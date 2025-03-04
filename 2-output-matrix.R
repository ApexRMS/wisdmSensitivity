# A328
# Sarah Chisholm (ApexRMS)
# Run with R-4.4.1

# This script generates matrices of the wisdm image outputs, where each dimension
# displays a parameter that varied across scenarios.

# Workspace ----
# Load Libraries 
library(rsyncrosim)
library(tidyverse)
library(yaml)
library(png)
library(magick)

# Load Configuration 
config <- read_yaml("./config.yaml")

# Load SyncroSim library data ----
mySession        <- session(config$ssimLocation)
myLibrary        <- ssimLibrary(config$libraryPath, session = mySession)
myProject        <- project(myLibrary, config$projectName)

# Load tabular data ----
modelSummaryTable <- read_csv(file.path(config$outputPath, "Model Summary.csv") %>% 
                                normalizePath() %>% 
                                suppressWarnings())

# Functions ----

# Get parameters that varied across scenarios
getVariedParameters <- function(df){
  
  variedParameters <- c()
  
  for(i in seq(1:ncol(df))){
    
    uniqueColumnValues <- df[,i] %>% 
      n_distinct()
    
    if(uniqueColumnValues > 1){
      
      variedParameters <- df[,i] %>% 
        colnames() %>% 
        append(variedParameters, .)
      
    }
  }
  return(variedParameters)
}

# Parameters ----

# Get scenario IDs
scenarioIds <- modelSummaryTable$ScenarioId

# Get parameters that varied across scenarios
parameters <- modelSummaryTable %>% 
  dplyr::select(-ScenarioId, -ScenarioName) %>% 
  getVariedParameters()

# Get outputs to generate matrices for
if(length(config$modelOutputs) == 0){
  modelOutputs <- c("ResponseCurves","ResidualsPlot","ResidualsSmoothPlot",
                    "CalibrationPlot","ROCAUCPlot","AUCPRPlot","ConfusionMatrix",
                    "VariableImportancePlot")
} else {
  modelOutputs <- config$modelOutputs %>% unlist
}

# Initialize table of output images
modelOutputsTable <- tibble()

# Crosswalk of parameter names to clean names
if(config$modelType == "BRT"){
  paramterNames <- tibble(
    Name = c("FittingMethod", "LearningRate", "NumberOfTrees", "BagFraction", "MaximumTrees"),
    CleanName = c("Fitting Method", "Learning Rate", "Number of Trees Added Per Stage", "Bag Fraction", "Maximum Number of Trees"))
} else if(config$modelType == "RF"){
  parameterNames <- tibble(
    Name = c("EvaluateCovariateImportance", "CalculateCasewiseImportance",
             "NormalizeVotes", "CalculateProximity", "SampleWithReplacement",
             "NumberOfVariablesSampled", "MaximumNodes", "NumberOfTrees", "NodeSize"),
    CleanName = c("Evaluate Covariate Importance", "Calculate Casewise Importance",
                  "Normalize Votes", "Calculate Proximity", "Sample with Replacement",
                  "Number of Variables Sampled at Split", "Maximum Number of Nodes", 
                  "Number of Trees", "Node Size"))
}

# Prep Output Matrix ----

# Loop over scenarios to get their output images
for(scenarioId in scenarioIds){
  
  # Get result scenario ID
  resultScenarioId <- scenario(myProject) %>% 
    filter(ParentId == scenarioId) %>% 
    pull(ScenarioId)
  
  # Load result scenario
  resultScenario <- scenario(myProject, resultScenarioId)
  
  # Get model outputs
  wisdmOutputModel <- datasheet(resultScenario, "wisdm_OutputModel", returnInvisible = TRUE) %>% 
    dplyr::select(-"ModelsID", -"ModelRDS", -"ResidualSmoothRDS", -"TextOutput", -"VariableImportanceData", -"MaxentFiles") %>% 
    pivot_longer(cols = seq(1,ncol(.)),
                 names_to = "ModelOutput",
                 values_to = "ImageFile") %>% 
    right_join(data.frame(ModelOutput = modelOutputs))
  
  # Get parameter values to organize images by
  modelOutputsTable <- modelOutputsTable %>% 
    bind_rows(
    modelSummaryTable %>% 
      filter(ScenarioId == scenarioId) %>% 
      dplyr::select(ScenarioId, all_of(parameters)) %>% 
      bind_cols(wisdmOutputModel))
}

# Create output matrices
for(modelOutput in modelOutputs){
  
  # Get data for model output 
  modelOutputData <- modelOutputsTable %>% 
    filter(ModelOutput == modelOutput)
  
  # Check if data is available for the modelOutput type
  if(modelOutputData$ImageFile %>% unique() %>% length == 1){
    if(is.na(modelOutputData$ImageFile %>% unique())){
      print(str_c(modelOutput, " was not a wisdm output."))
    }
  } else {
    
    # Get # of columns and rows for matrix
    # - NB: if only one parameter varied across scenarios, output are arrange in columns
    if(length(parameters) > 1){
      
      numberOfColumns <- modelOutputData %>% 
        dplyr::select(parameters[2] %>% all_of()) %>% 
        n_distinct()
      
      columnNames <- modelOutputData %>% 
        pull(parameters[2] %>% all_of()) %>% 
        unique() %>% 
        as.character()
      
      numberOfRows <- modelOutputData %>% 
        dplyr::select(parameters[1] %>% all_of()) %>% 
        n_distinct()
      
      rowNames <- modelOutputData %>% 
        pull(parameters[1] %>% all_of()) %>% 
        unique() %>% 
        as.character()
      
    } else {
      numberOfColumns <- modelOutputData %>% 
        dplyr::select(parameters[1] %>% all_of()) %>% 
        n_distinct() 
      
      columnNames <- modelOutputData %>% 
        pull(parameters[1] %>% all_of()) %>% 
        unique() %>% 
        as.character()
    }
    
    # Read wisdm png files
    imageList <- image_read(modelOutputData$ImageFile)
    
    # Get width and height of an image
    imageWidth <- imageList[1] %>% 
      image_info() %>% 
      pull(width)
    
    imageHeight <- imageList[1] %>% 
      image_info() %>% 
      pull(height)
    
    # Define font sizes of annotations
    individualPanelFontSize <- imageHeight * 0.04
    matrixFontSize <- imageHeight * 0.02
    
    # Annotate images with column and row names
    # - NB: if only one parameter varied across scenarios, output are arrange in columns
    if(length(parameters) > 1){
      
      # Create a white column to add space for row annotations
      whiteColumn <- image_blank(width = 200,
                                 height = imageHeight,
                                 color = "white")
      
      # Add white space to the left edge of images in first column
      for(i in seq(1:(numberOfRows - 1))){
        
        columnIndex <- (i * numberOfColumns) + 1
        
        if(i == 1){
          # Append and annotate the first image
          imageList[i] <- image_append(c(whiteColumn, imageList[i])) %>% 
            image_annotate(text = rowNames[i],
                           gravity = "West",
                           location = "+100+0",
                           degrees = 270,
                           size = individualPanelFontSize)
          
          # Append and annotate the first image in the second row
          imageList[columnIndex] <- image_append(c(whiteColumn, imageList[columnIndex])) %>% 
            image_annotate(text = rowNames[i + 1],
                           gravity = "West",
                           location = "+100+0",
                           degrees = 270,
                           size = individualPanelFontSize)
        } else
          # Append the first image of remaining rows
          imageList[columnIndex] <- image_append(c(whiteColumn, imageList[columnIndex])) %>% 
            image_annotate(text = rowNames[i + 1],
                           gravity = "West",
                           location = "+100+0",
                           degrees = 270,
                           size = individualPanelFontSize)
      }
    }
    
    # Create a white row to add space for column annotations
    whiteRow <- image_blank(width = imageWidth,
                            height = 200,
                            color = "white")
    
    # Add white space to the top edge of all images to ensure consistent image sizes
    for(i in seq(1:length(imageList))){
      imageList[i] <- image_append(c(whiteRow, imageList[i]), stack = TRUE)
    }
    
    # Annotate images in first row with column names
    imageList[1:numberOfColumns] <- imageList[1:numberOfColumns] %>% 
      image_annotate(text = columnNames,
                     gravity = "North",
                     location = "+0+100",
                     size = individualPanelFontSize)
    
    # Define matrix tiles
    if(length(parameters) > 1){
      maxtrixTiles <- numberOfRows %>% as.character() %>% 
        str_c("x", numberOfColumns %>% as.character())
    } else {
      maxtrixTiles <- numberOfColumns %>% as.character() %>% 
        str_c("x1")
    }
    
    # Define matrix panel sizes and spacing
    matrixGeometry <- "x" %>% 
      str_c((imageWidth/numberOfColumns) %>% as.character(),
            "+0+0")
    
    if(length(parameters) > 1){
      # Create matrix
      outputMatrix <- imageList %>% 
        image_montage(geometry = matrixGeometry, tile = maxtrixTiles) %>% 
        image_annotate(
          text = parameterNames %>% filter(Name == parameters[1]) %>% pull(CleanName),
          gravity = "North",
          location = "+0+20",
          size = matrixFontSize)%>% 
        image_annotate(
          text = parameterNames %>% filter(Name == parameters[2]) %>% pull(CleanName),
          gravity = "West",
          location = "+45+150",
          degrees = 270,
          size = matrixFontSize)
    } else {
      # Create matrix
      outputMatrix <- imageList %>% 
        image_montage(geometry = matrixGeometry, tile = maxtrixTiles) %>% 
        image_annotate(
          text = parameterNames %>% filter(Name == parameters[1]) %>% pull(CleanName),
          gravity = "North",
          location = "+0+20",
          size = matrixFontSize)
    }
    
    outputName <- config$modelType %>% str_to_lower() %>% 
      str_c(modelOutput, "Matrix.png")
    
    if(!file.exists(file.path(config$outputPath, outputName))){
      image_write(outputMatrix,
                  path = file.path(config$outputPath, outputName),
                  format = "png",
                  quality = 100)
    } else stop("The output matrix already exists. Delete the file before rewriting to disk.")
  }
}