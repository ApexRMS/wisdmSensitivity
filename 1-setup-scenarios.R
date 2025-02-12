# A328
# Sarah Chisholm (ApexRMS)
# Run with R-4.4.1

# This script generates wisdm scenarios for each unique combination of model 
# parameter values specified in the configuration file (i.e. config.yaml)

# Workspace ----
# Load Libraries 
library(rsyncrosim)
library(tidyverse)
library(yaml)

# Load Configuration 
config <- read_yaml("./config.yaml")

# Load SyncroSim library data ----
mySession        <- session(config$ssimLocation)
myLibrary        <- SsimLibrary(config$libraryPath, mySession)
myProject        <- project(myLibrary, config$projectName)
templateScenario <- scenario(myProject, config$templateScenarioId)

# Parameters ----

# List boolean parameters in Random Forest
booleanRfParameters <- c("EvaluateCovariateImportance", "CalculateCasewiseImportance",
                          "NormalizeVotes", "CalculateProximity", "SampleWithReplacement")

# Prep parameter combinations ----

# Get type of model being run
# zzz: only one type of model can be specified in the scenario pipeline
modelType <- datasheet(templateScenario, "core_Pipeline") %>% 
  filter(StageNameId %>% str_detect("7")) %>% 
  pull(StageNameId) %>% 
  str_replace("7 - ", "")

# Create dataframe of unique combinations of parameter values
# zzz: add an error message if >2 or 3 parameters are set to vary
if(modelType == "Boosted Regression Trees"){
  
  parameterCombinations <- expand_grid(
    fittingMethod = config$wisdm_BRT[[1]]$fittingMethod %>% unlist,
    learningRate = config$wisdm_BRT[[2]]$learningRate %>% unlist,
    numberOfTrees = config$wisdm_BRT[[3]]$numberOfTrees %>% unlist,
    bagFraction = config$wisdm_BRT[[4]]$bagFraction %>% unlist,
    maximumTrees = config$wisdm_BRT[[5]]$maximumTrees %>% unlist)
  
} else if(modelType == "Random Forest"){
  
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
  
  if(modelType == "Boosted Regression Trees"){
    
    # Name of new scenario
    scenarioName <- templateScenario %>% 
      name() %>% 
      str_c(": BRT Variation ", uniqueCombination %>% as.character())
    
    # Create new scenario from template
    newScenario <- scenario(myProject, scenarioName, sourceScenario = templateScenario)
    
    # Pull model parameters from template scenario and update with new values
    wisdmBrtValues <- templateScenario %>% 
      datasheet("wisdm_BRT") %>% 
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
    
  } else if(modelType == "Random Forest"){
    
    # Name of new scenario
    scenarioName <- templateScenario %>% 
      name() %>% 
      str_c(": RF Variation ", uniqueCombination %>% as.character())
    
    # Create new scenario from template
    newScenario <- scenario(myProject, scenarioName, sourceScenario = templateScenario)
    
    # Pull model parameters from template scenario and update with new values
    wisdmRfValues <- templateScenario %>% 
      datasheet("wisdm_RF") %>% 
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
  }
}