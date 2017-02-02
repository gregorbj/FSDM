#server.R
#Author: Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#License: Apache 2


#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(jsonlite)
library(DT)
library(ggplot2)
library(plotly)
library(DiagrammeR)
#Helper.R script
source("helper.R")


#SHINY SERVER FUNCTION
#---------------------
shinyServer(function(input, output, session) {

    
  #------------------------------------------------
  #CREATE OBJECTS TO STORE MODEL AND SCENARIO STATE
  #------------------------------------------------
  #Reactive object to store current state that interface responds to
  model <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)
  #Reactive object to store model history (unlimited undo)
  history <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)
  #Reactive object to represent concepts table
  conceptstable <- reactiveValues(concepts = NULL)
  #Create a reactive object to store current selected effects
  effects <- 
    reactiveValues(
      variable = "",
      name = "",
      direction = "",
      strength = "",
      description = "")
  #Create a reactive object to store scenario data in
  scenario <- 
    reactiveValues(status = NULL, values = NULL, history = NULL)
  #Create a reactive object to represent scenario table
  scenariotable <- reactiveValues(values = NULL)
  #Create a reactive object to store names of scenarios
  scenariolist <- reactiveValues(valid = "", invalid = "", run = "", all = "")
  #Create a reactive object to keep track of various conditions
  is <- reactiveValues(newconcept = FALSE)

  
  #-----------------------------------------------------
  #DEFINE COMMON FUNCTIONS FOR MODIFYING REACTIVE VALUES
  #-----------------------------------------------------
  #Function to save the model in history
  saveLastState <- function() {
    history$status <- model$status
    history$concepts <- model$concepts
    history$relations <- model$relations
  }
  #Function to swap model and history (i.e. undo)
  swapState <- function() {
    Status <- model$status
    Concepts <- model$concepts
    Relations <- model$relations
    model$status <- history$status
    model$concepts <- history$concepts
    model$relations <- history$relations
    history$status <- Status
    history$concepts <- Concepts
    history$relations <- Relations
  }
  #Function to undo concept edit
  undoConceptEdit <- function() {
    swapState()
  }
  #Function to undo relation edit
  undoRelationEdit <- function() {
    swapState()
  }
  #Function to save last scenario state
  saveLastScenarioState <- function() {
    scenario$history <- scenario$values
  }
  #Function to swap scenario history and present values
  swapScenarioState <- function() {
    Values <- scenario$values
    scenario$values <- scenario$history
    scenario$history <- Values
  }
  #Function to undo scenario edit
  undoScenarioEdit <- function() {
    swapScenarioState()
  }
  #Function to update concepts table with model
  updateConceptsTable <- function() {
    conceptstable$concepts <- model$concepts
  }
  #Function to update scenario table with scenario values
  updateScenarioTable <- function() {
    scenariotable$values <- scenario$values
  }
  #Function to update concept form inputs
  updateConceptForm <- function(RowNum) {
    updateTextInput(session, "conceptName",
                    value = conceptstable$concepts$name[RowNum])
    updateTextInput(session, "varName",
                    value = conceptstable$concepts$variable[RowNum])
    updateTextInput(session, "conceptDesc",
                    value = conceptstable$concepts$description[RowNum])
    updateTextInput(session, "minValue",
                    value = conceptstable$concepts$values$min[RowNum])
    updateTextInput(session, "maxValue",
                    value = conceptstable$concepts$values$max[RowNum])
    updateTextInput(session, "valuesDesc",
                    value = conceptstable$concepts$values$description[RowNum])
    updateTextInput(session, "conceptGroup",
                    value = conceptstable$concepts$group[RowNum])
  }
  #Function to update scenario concept form inputs
  updateScenarioForm <- function(RowNum) {
    output$scenarioConcept <- renderText({scenariotable$values$name[RowNum]})
    updateTextInput(session, "conceptStartValue",
                    value = scenariotable$values$startvalue[RowNum])
    updateTextInput(session, "conceptStartChange",
                    value = scenariotable$values$startchange[RowNum])
    updateTextInput(session, "conceptValuesDescription",
                    value = scenariotable$values$description[RowNum])
  }
  #Function to clear reactive data when when changing model
  resetModel <- function(){
    history$status = NULL
    history$concepts = NULL
    history$relations = NULL
    conceptstable$concepts = NULL
    effects$variable <- ""
    effects$name <- ""
    effects$direction <- ""
    effects$strength <- ""
    effects$description <- ""
    scenario$status <- NULL
    scenario$values <- NULL
    scenario$history <- NULL
    scenariotable$values <- NULL
    scenariolist$valid <- ""
    scenariolist$invalid <- ""
    scenariolist$all <- ""
    scenariolist$run <- ""
  }
  #Function to clear scenario form
  clearScenarioForm <- function() {
    updateTextInput(session, "conceptVarName",
                    value = "")
    updateTextInput(session, "conceptStartValue",
                    value = "")
    updateTextInput(session, "conceptStartChange",
                    value = "")
    updateTextInput(session, "conceptValuesDescription",
                    value = "")
  } 


  #--------------------------------------
  #IMPLEMENT INTERFACE FOR STARTING MODEL
  #--------------------------------------
  #Define GUI element to select model from a list
  output$selectModelFile <- renderUI({
    selectInput(
      inputId = "modelFileName",
      label = switch(
        input$modelAction,
        "copyModel" = "Select model to copy",
        "editModel" = "Select model to edit",
        "runModel" = "Select model to run"
      ),
      choices = dir(path = "../models")[dir(path = "../models") != "templates"]
    )
  })
  #Choose model start option and initialize model
  observeEvent(
    input$startModeling,
    {
      if (input$modelAction == "newModel") {
        #Check that there is a model name
        if (input$modelName == "") {
          createAlert(session = session, anchorId = "nonameAlert", 
                      title = "Missing Name", 
                      content = "Model name is missing. Enter a name.")
          return()
        }
        #Check that model name does not duplicate an existing model name
        ExistingModels_ <- dir("../models")
        if (input$modelName %in% ExistingModels_) {
          createAlert(session = session, anchorId = "duplicateModel",
                      title = "Duplicate Model",
                      content = "Model name is same as existing model name. Enter a different name.")
          return()
        }
        #Check that the model author information is present
        NoFirstName <- all(unlist(strsplit(input$firstName, "")) == " ")
        NoLastName <- all(unlist(strsplit(input$lastName, "")) == " ")
        if (NoFirstName | NoLastName) {
          createAlert(session = session, anchorId = "noAuthorInfo",
                      title = "Missing Author Info",
                      content = "Author information missing. Enter first and last name in 'User Information' tab.")
          return()
        }
        resetModel()
        ModelAuthor <- 
          paste0(input$firstName, " ", input$lastName, " (", input$organization, ")")
        model$status <- initializeNewModel(input$modelName, ModelAuthor)
        model$concepts <- loadModelConcepts(input$modelName)
        model$relations <- list()
        updateConceptsTable()
        updateConceptForm(1)
        saveLastState()
        scenariolist$runs <- listScenarios(model$status$name)$runs
        clearScenarioForm()
      }
      if (input$modelAction == "copyModel") {
        if (input$modelName == "") {
          createAlert(session = session, anchorId = "nonameAlert", 
                      title = "Missing Name", 
                      content = "Model name is missing. Enter a name.")
          return()
        }
        #Check that model name does not duplicate an existing model name
        ExistingModels_ <- dir("../models")
        if (input$modelName %in% ExistingModels_) {
          createAlert(session = session, anchorId = "duplicateModel",
                      title = "Duplicate Model",
                      content = "Model name is same as existing model name. Enter a different name.")
          return()
        }
        #Check that the model author information is present
        NoFirstName <- all(unlist(strsplit(input$firstName, "")) == " ")
        NoLastName <- all(unlist(strsplit(input$lastName, "")) == " ")
        if (NoFirstName | NoLastName) {
          createAlert(session = session, anchorId = "noAuthorInfo",
                      title = "Missing Author Info",
                      content = "Author information missing. Enter first and last name in 'User Information' tab.")
          return()
        }
        resetModel()
        ModelAuthor <- 
          paste0(input$firstName, " ", input$lastName, " (", input$organization, ")")
        model$status <- 
          initializeCopyModel(input$modelName, input$modelFileName, ModelAuthor, input$copyScenarios)
        model$concepts <- loadModelConcepts(input$modelName)
        model$relations <- loadModelRelations(input$modelName)
        updateConceptsTable()
        updateConceptForm(1)
        saveLastState()
        scenariolist$run <- listScenarios(model$status$name)$Run
        clearScenarioForm()
      }
      if (input$modelAction == "editModel") {
        #Check that the model author information is present
        NoFirstName <- all(unlist(strsplit(input$firstName, "")) == " ")
        NoLastName <- all(unlist(strsplit(input$lastName, "")) == " ")
        if (NoFirstName | NoLastName) {
          createAlert(session = session, anchorId = "noAuthorInfo",
                      title = "Missing Author Info",
                      content = "Author information missing. Enter first and last name in 'User Information' tab.")
          return()
        }
        resetModel()
        ModelAuthor <- 
          paste0(input$firstName, " ", input$lastName, " (", input$organization, ")")
        model$status <- loadModelStatus(input$modelFileName, ModelAuthor)
        model$concepts <- loadModelConcepts(input$modelFileName)
        model$relations <- loadModelRelations(input$modelFileName)
        updateConceptsTable()
        updateConceptForm(1)
        saveLastState()
        scenariolist$run <- listScenarios(model$status$name)$Run
        clearScenarioForm()
      }
      if (input$modelAction == "runModel") {
        resetModel()
        model$status <- loadModelStatus(input$modelFileName)
        model$concepts <- loadModelConcepts(input$modelFileName)
        model$relations <- loadModelRelations(input$modelFileName)
        updateConceptsTable()
        updateConceptForm(1)
        saveLastState()
        scenariolist$run <- listScenarios(model$status$name)$Run
        clearScenarioForm()
      }
    }
  )
  #Output model status information
  output$modelName <- renderText({model$status$name})
  output$modelParent <- renderText({model$status$parent})
  output$modelCreated <- renderText({model$status$created})
  output$modelEdited <- renderText({model$status$lastedit})
  output$modelAttribution <- renderText({paste(model$status$attribution, collapse = "\n")})
  
    
  #----------------------------------------------
  #IMPLEMENT INTERFACE FOR EDITING MODEL CONCEPTS
  #----------------------------------------------
  #Update concept form based on what is selected in table
  observeEvent(
    input$conceptsTable_rows_selected,
    {
      RowNum <- input$conceptsTable_rows_selected
      updateConceptForm(RowNum)
    }
  )
  #Implement the new concept button
  observeEvent(
    input$addConcept,
    {
      is$newconcept <- TRUE
      conceptstable$concepts <- conceptstable$concepts[c(1,1:nrow(conceptstable$concepts)),]
      conceptstable$concepts$name[1] <- ""
      conceptstable$concepts$variable[1] <- ""
      conceptstable$concepts$description[1] <- ""
      conceptstable$concepts$values$min[1] <- ""
      conceptstable$concepts$values$max[1] <- ""
      conceptstable$concepts$values$description[1] <- ""
      conceptstable$concepts$group[1] <- ""
      RowNum <- input$conceptsTable_rows_selected
      updateConceptForm(RowNum)
    }
  )
  #Implement the update concept button
  observeEvent(
    input$updateConcept,
    {
      #Check whether if new concept and duplicate name or variable
      IsDupName <- input$conceptName %in% model$concepts$name
      IsDupVar <- input$varName %in% model$concepts$variable
      if (is$newconcept & IsDupName) {
        createAlert(session = session, anchorId = "duplicateConceptName", 
                    title = "Duplicate Concept Name", 
                    content = "New concept name is the same as name for an existing concept. Rename before updating.")
        return()        
      }
      if (is$newconcept & IsDupVar) {
        createAlert(session = session, anchorId = "duplicateConceptVariable", 
                    title = "Duplicate Concept Label", 
                    content = "New concept label name is the same as label name for an existing concept. Rename before updating.")
        return()        
      }
      #Save state of current model
      saveLastState()
      #Modify conceptstable
      RowNum <- input$conceptsTable_rows_selected
      conceptstable$concepts$name[RowNum] <- input$conceptName
      conceptstable$concepts$variable[RowNum] <- input$varName
      conceptstable$concepts$description[RowNum] <- input$conceptDesc
      conceptstable$concepts$values$min[RowNum] <- input$minValue
      conceptstable$concepts$values$max[RowNum] <- input$maxValue
      conceptstable$concepts$values$description[RowNum] <- input$valuesDesc
      conceptstable$concepts$group[RowNum] <- input$conceptGroup
      #Update model concepts
      model$concepts <- conceptstable$concepts
      model$status$lastedit <- as.character(Sys.time())
      #Reset Concept$IsNew
      is$newconcept <- FALSE
      #Initialize model relations for new concept
      AnyRelations <- length(model$relations) != 0
      if (AnyRelations) {
        ExistingRelations_ <- unlist(lapply(model$relations, function(x) x$name))
        if (!(input$varName %in% ExistingRelations_)) {
          model$relations[[length(model$relations) + 1]] <- 
            list(name = input$varName, affects = list())
        }
      } else {
        model$relations[[1]] <- list(name = input$varName, affects = list())
      }
    }
  )
  #Implement the undo button
  observeEvent(
    input$undoConceptAction,
    {
      undoConceptEdit()
      conceptstable$concepts <- model$concepts
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the delete button
  observeEvent(
    input$deleteConcept,
    {
      #Save last model state in redobuffer
      saveLastState()
      #Modify conceptstable
      RowNum <- input$conceptsTable_rows_selected
      Var <- conceptstable$concepts$variable[RowNum]
      conceptstable$concepts <- conceptstable$concepts[-RowNum,]
      #Update model concepts
      model$concepts <- conceptstable$concepts
      model$status$lastedit <- as.character(Sys.time())
      #conceptstable$concepts <- model$concepts
      #Update model relations
      Relations_ls <- model$relations
      RelationsNames_ <- 
        unlist(lapply(Relations_ls, function(x) x$name))
      RelationIdx <- which(RelationsNames_ == Var)
      Relations_ls[[RelationIdx]] <- NULL
      for (i in 1:length(Relations_ls)) {
        Affects_ls <- Relations_ls[[i]]$affects
        IsVar <- unlist(lapply(Affects_ls, function(x) x$variable == Var))
        if (any(IsVar)) {
          Affects_ls[[which(IsVar)]] <- NULL
        }
        Relations_ls[[i]]$affects <- Affects_ls
      }
      model$relations <- Relations_ls
      #Update the input form
      updateConceptForm(RowNum)
    }
  )
  #Output model concepts table
  output$conceptsTable <- DT::renderDataTable(
    formatConceptTable(conceptstable$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row', selected = 1),
    options = list(pageLength = 20)
  )
  

  #-----------------------------------------------
  #IMPLEMENT INTERFACE FOR EDITING MODEL RELATIONS
  #-----------------------------------------------
  #Define dropdown element to select causal group
  output$selectCausalGroup <- renderUI({
    selectInput(
      inputId = "causalGroup",
      label = "Causal Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define dropdown element to select affected group
  output$selectAffectedGroup <- renderUI({
    selectInput(
      inputId = "affectedGroup",
      label = "Affected Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define dropdown element to select causal concept from a list
  output$selectCausalConcept <- renderUI({
    selectInput(
      inputId = "causalConcept",
      label = "Causal Concept",
      choices = sort(model$concepts$variable)
      #choices = sort(model$concepts$name)
    )
  })
  #Define dropdown element to select affected concept from a list
  output$selectAffectedConcept <- renderUI({
    selectInput(
      inputId = "affectedConcept",
      label = "Affected Concept",
      choices = sort(model$concepts$variable)
      #choices = sort(model$concepts$name)
    )
  })
  #On change of selected causal concept, update causal info in GUI
  observeEvent(
    input$causalConcept,
    {
      Effects_df <- getEffects(model, input$causalConcept)
      if (!is.null(Effects_df)) {
        effects$variable <- Effects_df$variable
        effects$name <- Effects_df$name
        effects$direction <- Effects_df$direction
        effects$strength <- Effects_df$weight
        effects$description <- Effects_df$description
        #in following if, change effects$name to effects$variable
        if (input$affectedConcept %in% effects$variable) {
          updateTextInput(session, "causalDirection",
                          value = effects$direction[effects$variable == input$affectedConcept])
          updateTextInput(session, "causalStrength",
                          value = effects$strength[effects$variable == input$affectedConcept])
          updateTextInput(session, "causalDesc",
                          value = effects$description[effects$variable == input$affectedConcept])
        } else {
          updateTextInput(session, "causalDirection", value = "")
          updateTextInput(session, "causalStrength", value = "")
          updateTextInput(session, "causalDesc", value = "")
        }
      } else {
        effects$variable <- ""
        effects$name <- ""
        effects$direction <- ""
        effects$strength <- ""
        effects$description <- ""
        updateTextInput(session, "causalDirection", value = "")
        updateTextInput(session, "causalStrength", value = "")
        updateTextInput(session, "causalDesc", value = "")
      }
    }
  )
  #On change of selected affected concept, update causal info in GUI
  observeEvent(
    input$affectedConcept,
    {
      #in following if, change effects$name to effects$variable 
      if (input$affectedConcept %in% effects$variable) {
          updateTextInput(session, "causalDirection",
                          value = effects$direction[effects$variable == input$affectedConcept])
          updateTextInput(session, "causalStrength",
                          value = effects$strength[effects$variable == input$affectedConcept])
          updateTextInput(session, "causalDesc",
                          value = effects$description[effects$variable == input$affectedConcept])
      } else {
        updateTextInput(session, "causalDirection", value = "")
        updateTextInput(session, "causalStrength", value = "")
        updateTextInput(session, "causalDesc", value = "")        
      }
    }
  )
  #Implement the update relations button
  observeEvent(
    input$updateRelation,
    {
      #Save last model state in redobuffer
      saveLastState()
      #Update Relation
      #change concepts$name to concepts$variable
      CausalConcept <- 
        model$concepts$variable[model$concepts$variable == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$name))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$variable[model$concepts$variable == input$affectedConcept]
      NewEffect_ls <-
        list(variable = AffectedConcept,
             direction = input$causalDirection,
             weight = input$causalStrength,
             description = input$causalDesc)
      Effects_ls <- 
        model$relations[[CausalIdx]]$affects
      if (length(Effects_ls) != 0) {
        AffectedConcepts_ <- unlist(lapply(Effects_ls, function(x) x$variable))
        if (AffectedConcept %in% AffectedConcepts_) {
          Effects_ls[[which(AffectedConcepts_ == AffectedConcept)]] <-
            NewEffect_ls
        } else {
          Effects_ls[[length(Effects_ls) + 1]] <- NewEffect_ls
        }
      } else {
        Effects_ls[[1]] <- NewEffect_ls
      }
      model$relations[[CausalIdx]]$affects <- Effects_ls
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the delete relation
  observeEvent(
    input$deleteRelation,
    {
      #Save last model state and relations inputs
      saveLastState()
      #Remove relation from model
      #change concepts$name to concepts$variable
      CausalConcept <- 
        model$concepts$variable[model$concepts$variable == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$name))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$variable[model$concepts$variable == input$affectedConcept]
      Effects_ls <- 
        model$relations[[CausalIdx]]$affects
      EffectIdx <- 
        which(unlist(lapply(Effects_ls, function(x) x$variable)) == AffectedConcept)
      if (length(EffectIdx) != 0) {
        Effects_ls[[EffectIdx]] <- NULL
        model$relations[[CausalIdx]]$affects <- Effects_ls
      }
      #Update text fields
      updateTextInput(session, "causalDirection", value = "")
      updateTextInput(session, "causalStrength", value = "")
      updateTextInput(session, "causalDesc", value = "")
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Undo relations edit
  observeEvent(
    input$undoRelationAction,
    {
      #change effects$name to effects$variable
      undoRelationEdit()
      updateTextInput(session, "causalDirection",
                      value = effects$direction[effects$variable == input$affectedConcept])
      updateTextInput(session, "causalStrength",
                      value = effects$strength[effects$variable == input$affectedConcept])
      updateTextInput(session, "causalDesc",
                      value = effects$description[effects$variable == input$affectedConcept])
      model$status$lastedit <- as.character(Sys.time())
    }
  )
  #Output relations map
  output$relations_map <- renderPlot({
    Map <-
      mapRelations(model,
                   FromConcept = input$causalConcept,
                   FromGroup = input$causalGroup,
                   ToGroup = input$affectedGroup)
    plot(Map$XVals, c(Map$YVals1, Map$YVals2),
         axes = FALSE,
         xlim = Map$XLim, ylim = Map$YLim,
         xlab = "", ylab = "")
    text(2, Map$YVals1, labels = Map$Labels1, pos = 2)
    text(6, Map$YVals2, labels = Map$Labels2, pos = 4)
    text(2, Map$TitlePosY, labels = "Cause", pos = 2, cex = 1.5)
    text(6, Map$TitlePosY, labels = "Effect", pos = 4, cex = 1.5)
    arrows(Map$X0, Map$Y0, Map$X1, Map$Y1, col = Map$Col, lwd = Map$Lwd, lty = Map$Lty, length = 0)
  },
  width = 800, height = 700)
  #Implement relations graph
  output$relations_graph <- renderGrViz({
    Dot_ <-
      makeDot(Relations_ls = model$relations,
              Concepts_df = model$concepts,
              RowGroup = input$causalGroup,
              ColGroup = input$affectedGroup,
              orientation = input$graphOrientation,
              rankdir = input$graphLayout,
              shape = input$nodeShape,
              Show = input$edgeLabel)
    grViz(Dot_)
  })


  #------------------------------------
  #IMPLEMENT INTERFACE FOR SAVING MODEL
  #------------------------------------
  #Implement save model button
  observeEvent(
    input$saveModel,
    {
      showNotification(
        ui = "Saving Model",
        duration = 1, 
        closeButton = TRUE,
        type = "message"
      )
      Author <- 
        paste0("Edited By: ", input$firstName, " ", input$lastName, " (", input$organization, ")")
      Timestamp <-
        paste0("When: ", model$status$lastedit)
      Notation <- 
        paste0("Notes: ", input$modelNotes)
      CurrentNote <-
        paste(Author, Timestamp, Notation, sep = " | ")
      model$status$notes <- c(CurrentNote, model$status$notes) 
      saveModel(model)
      updateTextAreaInput(session = session, inputId = "modelNotes", value = "")
    }
  )
  

  #-----------------------------------------
  #IMPLEMENT INTERFACE FOR CHOOSING SCENARIO
  #-----------------------------------------
  #Define GUI element to select scenario from a list
  output$selectScenarioFile <- renderUI({
    selectInput(
      inputId = "scenarioFileName",
      label = switch(
        input$scenarioAction,
        "copyScenario" = "Select Scenario to Copy",
        "editScenario" = "Select Scenario to Edit"
      ),
      choices = dir(path = paste0("../models/", model$status$name, "/scenarios"))
    )
  })
  #Choose model start option and initialize model
  observeEvent(
    input$startScenario,
    {
      if (input$scenarioAction == "newScenario") {
        if (input$scenarioName == "") {
          createAlert(session = session, anchorId = "noscenarioAlert",
                      title = "Missing Name",
                      content = "Scenario name is missing. Enter a name.")
          return()
        }
        ScenInit_ls <- 
          initializeNewScenario(model$status$name, input$scenarioName, model$concepts)
        scenario$status <- ScenInit_ls$status
        scenario$values <- ScenInit_ls$values
        updateScenarioTable()
        saveLastScenarioState()
        updateScenarioForm(1)
      }
      if (input$scenarioAction == "copyScenario") {
        if (input$scenarioName == "") {
          createAlert(session = session, anchorId = "noscenarioAlert",
                      title = "Missing Name",
                      content = "Scenario name is missing. Enter a name.")
          return()
        }
        ScenInit_ls <- 
          initializeCopyScenario(
            model$status$name,
            model$concepts$variable,
            input$scenarioName, 
            input$scenarioFileName
            )
        scenario$status <- ScenInit_ls$status
        scenario$values <- ScenInit_ls$values
        updateScenarioTable()
        saveLastScenarioState()
        updateScenarioForm(1)
      }
      if (input$scenarioAction == "editScenario") {
        ScenInit_ls <- 
          loadScenario(
            model$status$name, 
            model$concepts$variable,
            input$scenarioFileName
            )
        scenario$status <- ScenInit_ls$status
        scenario$values <- ScenInit_ls$values
        updateScenarioTable()
        saveLastScenarioState()
        updateScenarioForm(1)
      }
    }
  )
  #Output scenario status information
  output$scenarioName <- renderText({scenario$status$name})
  output$scenarioParent <- renderText({scenario$status$parent})
  output$scenarioModelName <- renderText({model$status$name})
  output$scenarioCreated <- renderText({scenario$status$created})
  output$scenarioEdited <- renderText({scenario$status$lastedit})
  output$scenarioValidated <- renderText({scenario$status$validated})

  
  #----------------------------------------
  #IMPLEMENT INTERFACE FOR EDITING SCENARIO
  #----------------------------------------
  #Update concept form based on what is selected in table
  observeEvent(
    input$scenarioTable_rows_selected,
    {
      RowNum <- input$scenarioTable_rows_selected
      updateScenarioForm(RowNum)
    }
  )
  #Implement the updateScenario button
  observeEvent(
    input$updateScenario,
    {
      #Save state of current model
      saveLastScenarioState()
      #Modify conceptstable
      RowNum <- input$scenarioTable_rows_selected
      #scenariotable$values$name[RowNum] <- input$conceptVarName
      scenariotable$values$startvalue[RowNum] <- input$conceptStartValue
      scenariotable$values$startchange[RowNum] <- input$conceptStartChange
      scenariotable$values$description[RowNum] <- input$conceptValuesDescription
      #Update scenario values
      scenario$values <- scenariotable$values
      scenario$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the undoScenarioAction button
  observeEvent(
    input$undoScenarioAction,
    {
      undoScenarioEdit()
      scenariotable$values <- scenario$values
      RowNum <- input$scenarioTable_rows_selected
      updateScenarioForm(RowNum)
      scenario$status$lastedit <- as.character(Sys.time())
    }
  )
  #Implement the validateScenario button
  observeEvent(
    input$validateScenario,
    {
      Validation_ls <- validateScenario(scenario$values, model$concepts)
      IsValid <- Validation_ls$Valid
      if (IsValid) {
        scenario$status$validated <- Validation_ls$TimeStamp
        VMsg <- "Congratulations, the scenario was successfully validated!"
        showNotification(
          ui = VMsg,
          duration = 2, 
          closeButton = TRUE,
          type = "message"
        )
        saveScenario(scenario)
      } else {
        scenario$status$validated <- ""
        VMsg <- paste(Validation_ls$Errors, collapse = "\n")
        showNotification(
          ui = VMsg,
          duration = 15, 
          closeButton = TRUE,
          type = "error"
        )
        saveScenario(scenario)
      }
    }
  )
  #Output scenario table
  output$scenarioTable <- DT::renderDataTable(
    scenariotable$values,
    server = FALSE,
    selection = list(mode = 'single', target = 'row', selected = 1),
    options = list(pageLength = 20)
  )
  

  #-----------------------------------------
  #IMPLEMENT INTERFACE FOR RUNNING THE MODEL
  #-----------------------------------------
  #Implement action button to list scenarios for model
  observeEvent(
    input$listScenarios,
    {
      ScenarioList_ls <- listScenarios(model$status$name)
      scenariolist$valid <- ScenarioList_ls$Valid
      scenariolist$invalid <- ScenarioList_ls$Invalid
    }
  )
  #Define checkbox GUI element to select valid scenarios from list
  output$selectScenariosToRun <- renderUI({
    Scenarios_ <- 
      dir(path = file.path("../models/", model$status$name, "scenarios"))
    checkboxGroupInput(
      inputId = "scenariosToRun",
      label = "Check Scenarios to Run",
      choices = scenariolist$valid
    )
  })
  #List scenarios that are not validated
  output$invalidScenarios <- 
    renderText(scenariolist$invalid)
  #Implement the model run button
  observeEvent(
    input$runModel,
    {
      withProgress(
        message = "Model is Running",
        detail = "This may take a while",
        value = 0,
        {
          Sc <- input$scenariosToRun
          Sys.sleep(0.2)
          for (sc in Sc) {
            ModelPath <- file.path("../models", model$status$name)
            ScenarioPath <- file.path(ModelPath, "scenarios", sc)
            Model_ls <- createFuzzyModel(ModelPath)
            Scenario_ls <- createFuzzyScenario(ScenarioPath, Model_ls, OpRange = c(0.1,99.9))
            Outputs_ls <- runFuzzyModel(Model_ls, Scenario_ls, OpRange = c(0.1,99.9), Pow = 10)
            save(Outputs_ls, file = file.path(ScenarioPath, "Outputs_ls.RData"))
            incProgress(1 / length(Sc))
          }          
        }
      )
      showNotification(
        ui = "Model Runs Complete",
        duration = 5,
        closeButton = TRUE,
        type = "message"
      )
    }
  )
  #Implement the run reset button
  # observeEvent(
  #   input$resetRun,
  #   {
  #     output$runMessage <- renderText({""})
  #     scenariolist$valid <- ""
  #     scenariolist$invalid <- ""      
  #   }
  # )
  #Implement the revalidate scenarios button
  observeEvent(
    input$revalidate,
    {
      Sc <- listScenarios(model$status$name)$All
      for (sc in Sc) {
        Scenario <- loadScenario(
          model$status$name, 
          model$concepts$variable,
          sc
        )
        Validation_ls <- validateScenario(Scenario$values, model$concepts)
        IsValid <- Validation_ls$Valid
        if (IsValid) {
          Scenario$status$validated <- Validation_ls$TimeStamp
          saveScenario(Scenario)
        } else {
          Scenario$status$validated <- ""
          saveScenario(Scenario)
        }
      }
    }
  )
  
    
  #------------------------------------------
  #IMPLEMENT INTERFACE FOR DISPLAYING RESULTS
  #------------------------------------------
  #Implement action button to update list of scenarios that have been run
  observeEvent(
    input$listRunScenarios,
    {
      scenariolist$run <- listScenarios(model$status$name)$Run
    }
  )
  #Define GUI element to select scenario 1 from a list
  output$selectScenarioPlot1 <- renderUI({
    Sc <- scenariolist$run
    selectInput(
      inputId = "scenarioPlot1",
      label = "Select Scenario 1",
      choices = Sc
    )
  })
  #Define GUI element to select scenario 2 from a list
  output$selectScenarioPlot2 <- renderUI({
    Sc <- scenariolist$run
    selectInput(
      inputId = "scenarioPlot2",
      label = "Select Scenario 2",
      choices = Sc
    )
  })
  #Define checkbox GUI element to select variables to plot
  output$selectVarsToPlot <- renderUI({
    checkboxGroupInput(
      inputId = "variablesToPlot",
      label = "Check Variables to Plot",
      choices = sort(model$concepts$variable)
    )
  })
  #Implement results plots
  output$resultsPlot <- renderPlotly({
    Sc <- c(input$scenarioPlot1, input$scenarioPlot2)
    Vn <- input$variablesToPlot
    if (length(Vn) >= 2) {
      PlotData_df <- formatOutputData(model$status$name, Sc, Vn)
      plot <- ggplot(PlotData_df, aes(x=Iteration, y=Scaled, color=Concept)) +
        geom_line() +
        facet_wrap(~Scenario)      
      ggplotly(plot)
    } 
  })
  #Implement saving data
  observeEvent(
    input$saveResults,
    {
      Sc <- c(input$scenarioPlot1, input$scenarioPlot2)
      Vn <- input$variablesToPlot
      if (length(Vn) >= 2) {
        PlotData_df <- formatOutputData(model$status$name, Sc, Vn)
        Plot <- ggplot(PlotData_df, aes(x=Iteration, y=Scaled, color=Concept)) +
          geom_line() +
          facet_wrap(~Scenario)
        if (input$analysisSaveName == "") {
          createAlert(session = session, anchorId = "noAnalysisNameAlert", 
                      title = "Missing Name", 
                      content = "Analysis save name is missing. Enter a name.")
          return()
        } else {
          AnalysisPath <- 
            file.path("../models", model$status$name, "analysis", input$analysisSaveName)
          if (!dir.exists(AnalysisPath)) {
            dir.create(AnalysisPath)
          }
          ggsave(file.path(AnalysisPath, "plot.png"), plot = Plot, device = "png")
          write.csv(PlotData_df, file = file.path(AnalysisPath, "data.csv"), row.names = FALSE)
        }        
      } 
    }
  )
  
  

  
})