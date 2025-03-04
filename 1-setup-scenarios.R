# A328
# Sarah Chisholm (ApexRMS)
# Run with R-4.4.1

# This script generates wisdm scenarios for each unique combination of model 
# parameter values specified in the configuration file (i.e. config.yaml). A 
# .csv file summarizing each scenario's ID, name, and model parameters is also 
# generated and saved to a user-specified output directory. 

# Workspace ----
# Load Libraries 
library(rsyncrosim)
library(tidyverse)
library(yaml)

# Load Configuration 
config <- read_yaml("./config.yaml")

# Create directory to save sensitivity analysis outputs
if(!dir.exists(config$outputPath)) dir.create(config$outputPath)

# Load SyncroSim library data ----
mySession        <- session(config$ssimLocation)
myLibrary        <- ssimLibrary(config$libraryPath, session = mySession)
myProject        <- project(myLibrary, config$projectName)
templateScenario <- scenario(myProject, config$templateScenarioId)

# Parameters ----

# List boolean parameters in Random Forest
booleanRfParameters <- c("EvaluateCovariateImportance", "CalculateCasewiseImportance",
                          "NormalizeVotes", "CalculateProximity", "SampleWithReplacement")

# Initialize model summary table
modelSummaryTable <- tibble()

# Prep parameter combinations ----

# Create dataframe of unique combinations of parameter values
if(config$modelType == "BRT"){
  
  parameterCombinations <- expand_grid(
    fittingMethod = config$wisdm_BRT[[1]]$fittingMethod %>% unlist,
    learningRate = config$wisdm_BRT[[2]]$learningRate %>% unlist,
    numberOfTrees = config$wisdm_BRT[[3]]$numberOfTrees %>% unlist,
    bagFraction = config$wisdm_BRT[[4]]$bagFraction %>% unlist,
    maximumTrees = config$wisdm_BRT[[5]]$maximumTrees %>% unlist)
  
} else if(config$modelType == "RF"){
  
  parameterCombinations <- expand_grid(
    evaluateCovariateImportance = config$wisdm_RF[[1]]$evaluateCovariateImportance %>% unlist,
    calculateCasewiseImportance = config$wisdm_RF[[2]]$calculateCasewiseImportance %>% unlist,
    numberOfVariablesSampled = config$wisdm_RF[[3]]$numberOfVariablesSampled %>% unlist,
    maximumNodes = config$wisdm_RF[[4]]$maximumNodes %>% unlist,
    numberOfTrees = config$wisdm_RF[[5]]$numberOfTrees %>% unlist,
    nodeSize = config$wisdm_RF[[6]]$nodeSize %>% unlist,
    normalizeVotes = config$wisdm_RF[[7]]$normalizeVotes %>% unlist,
    calculateProximity = config$wisdm_RF[[8]]$calculateProximity %>% unlist,
    sampleWithReplacement = config$wisdm_RF[[9]]$sampleWithReplacement %>% unlist)
}

# Generate scenarios ----

# Check if >2 parameters were set to vary
if(parameterCombinations %>% ncol() > 2){
  stop("Only two parameters may vary across scenarios.")
} else {
  
  # Create a vector of the number of unique parameter combinations
  uniqueCombinations <- seq(1:nrow(parameterCombinations))
  
  # Create a new scenario for each unique combination
  for(uniqueCombination in uniqueCombinations){
    
    # Get scenario-specific parameter values
    scenarioParameterCombinations <- parameterCombinations %>% 
      filter(row_number() == uniqueCombination) %>% 
      pivot_longer(seq(1, ncol(.)), # Get all columns in the data frame
                   names_to = "Parameter",
                   values_to = "Values") %>% 
      mutate(Parameter = Parameter %>% str_to_title())
    
    if(config$modelType == "BRT"){
      
      # Name of new scenario
      scenarioName <- templateScenario %>% 
        name() %>% 
        str_c(": BRT Variation ", uniqueCombination %>% as.character())
      
      # Create new scenario from template
      newScenario <- scenario(myProject, scenarioName, sourceScenario = templateScenario)
      
      # Check that the template scenario contains data in it's wisdm_BRT datasheet
      templateBrtSettings <- datasheet(templateScenario, "wisdm_BRT")
      
      if(nrow(templateBrtSettings) == 0) {
        stop("The wisdm_BRT datasheet in the template scenario is empty. Please add values to this dataheet to act as a baseline for the sensitivity analysis.")
      } else {
        
        # Pull model parameters from template scenario and update with new values
        wisdmBrtValues <- templateBrtSettings %>% 
          # Convert string parameters to integer values to be able to use pivot_longer
          mutate(FittingMethod = case_when(FittingMethod == "Use defaults and tuning" ~ 0,
                                           FittingMethod == "Use values provided below" ~ 1)) %>% 
          # zzz: pivot_longer converts all values to a common datatype - this will break if all values can't be converted to a common data type
          pivot_longer(seq(1, ncol(.)), # Get all columns in the data frame
                       names_to = "SyncroSimParameter",
                       values_to = "SyncroSimValues") %>% 
          mutate(Parameter = SyncroSimParameter %>% str_to_title()) %>% 
          left_join(scenarioParameterCombinations) %>% 
          mutate(SyncroSimValues = case_when(!is.na(Values) ~ Values,
                                             TRUE ~ SyncroSimValues)) %>% 
          dplyr::select(SyncroSimParameter, SyncroSimValues)
        
        # Prep datasheet
        wisdmBrtDatasheet <- wisdmBrtValues %>% 
          pivot_wider(names_from = SyncroSimParameter,
                      values_from = SyncroSimValues) %>% 
          # Convert string parameters back to string values 
          mutate(FittingMethod = case_when(FittingMethod == 0 ~ "Use defaults and tuning",
                                           FittingMethod == 1 ~ "Use values provided below")) %>% 
          as.data.frame()
        
        # Update wisdm_BRT datasheet
        saveDatasheet(newScenario, wisdmBrtDatasheet, "wisdm_BRT", append = FALSE)
        
        # Update tabular model summary output
        modelSummaryTable <- modelSummaryTable %>% 
          bind_rows(tibble(
            ScenarioId = scenarioId(newScenario),
            ScenarioName = scenarioName,
            FittingMethod = wisdmBrtDatasheet$FittingMethod,
            LearningRate = wisdmBrtDatasheet$LearningRate,
            NumberOfTrees = wisdmBrtDatasheet$NumberOfTrees,
            BagFraction = wisdmBrtDatasheet$BagFraction,
            MaximumTrees = wisdmBrtDatasheet$MaximumTrees)) 
      }
      
    } else if(config$modelType == "RF"){
      
      # Name of new scenario
      scenarioName <- templateScenario %>% 
        name() %>% 
        str_c(": RF Variation ", uniqueCombination %>% as.character())
      
      # Create new scenario from template
      newScenario <- scenario(myProject, scenarioName, sourceScenario = templateScenario)
      
      # Check that the template scenario contains data in it's wisdm_BRT datasheet
      templateRfSettings <- datasheet(templateScenario, "wisdm_RF")
      
      if(nrow(templateRfSettings) == 0) {
        stop("The wisdm_RF datasheet in the template scenario is empty. Please add values to this dataheet to act as a baseline for the sensitivity analysis.")
      } else {
        
        # Pull model parameters from template scenario and update with new values
        wisdmRfValues <- templateRfSettings %>% 
          # zzz: pivot_longer converts all values to a common datatype - this will break if all values can't be converted to a common data type
          pivot_longer(seq(1, ncol(.)), # Get all columns in the data frame
                       names_to = "SyncroSimParameter",
                       values_to = "SyncroSimValues") %>% 
          mutate(Parameter = SyncroSimParameter %>% str_to_title()) %>% 
          left_join(scenarioParameterCombinations) %>% 
          mutate(SyncroSimValues = case_when(!is.na(Values) ~ Values,
                                             TRUE ~ SyncroSimValues)) %>% 
          dplyr::select(SyncroSimParameter, SyncroSimValues) %>% 
          # Convert boolean parameters values back to logical data type
          mutate(SyncroSimBooleanValues = case_when(SyncroSimParameter %in% booleanRfParameters ~ SyncroSimValues %>% as.logical,
                                                    TRUE ~ NA))
        
        # Prep boolean parameters  
        wisdmRfBoolean <- wisdmRfValues %>% 
          filter(SyncroSimParameter %in% booleanRfParameters) %>% 
          dplyr::select(-SyncroSimValues) %>% 
          pivot_wider(names_from = SyncroSimParameter,
                      values_from = SyncroSimBooleanValues) %>% 
          as.data.frame()
        
        # Prep integer parameters
        wisdmRfInteger <- wisdmRfValues %>%
          filter(!SyncroSimParameter %in% booleanRfParameters) %>% 
          dplyr::select(-SyncroSimBooleanValues) %>% 
          pivot_wider(names_from = SyncroSimParameter,
                      values_from = SyncroSimValues) %>% 
          as.data.frame()
        
        # Prep datasheet
        wisdmRfDatasheet <- wisdmRfBoolean %>% bind_cols(wisdmRfInteger)
        
        # Update wisdm_RF datasheet
        saveDatasheet(newScenario, wisdmRfDatasheet, "wisdm_RF", append = FALSE)
        
        # Update tabular model summary output
        modelSummaryTable <- modelSummaryTable %>% 
          bind_rows(tibble(
            ScenarioId = scenarioId(newScenario),
            ScenarioName = scenarioName,
            EvaluateCovariateImportance = wisdmRfDatasheet$EvaluateCovariateImportance,
            CalculateCasewiseImportance = wisdmRfDatasheet$CalculateCasewiseImportance,
            NormalizeVotes = wisdmRfDatasheet$NormalizeVotes,
            CalculateProximity = wisdmRfDatasheet$CalculateProximity,
            SampleWithReplacement = wisdmRfDatasheet$SampleWithReplacement,
            NumberOfVariablesSampled = wisdmRfDatasheet$NumberOfVariablesSampled,
            MaximumNodes = wisdmRfDatasheet$MaximumNodes,
            NumberOfTrees = wisdmRfDatasheet$NumberOfTrees,
            NodeSize = wisdmRfDatasheet$NodeSize))
      }
    }
  }
  
  # Save model summary table to disk
  outputName <- str_c(config$modelType, " Model Summary.csv")
  outputPath <- file.path(config$outputPath, outputName) %>% normalizePath() %>% suppressWarnings()
  
  if(!file.exists(outputPath)){
    modelSummaryTable %>% write_csv(outputPath, append = FALSE)
  } else stop("A model summary file already exists in this location.")
}
