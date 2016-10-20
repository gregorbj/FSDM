#server.R
#Author: Brian Gregor, Oregon Systems Analytics LLC


#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(jsonlite)
library(DT)
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
  #Create a reactive object to store scenario validation results
  validscenarios <- reactiveValues(valid = "", invalid = "")

  
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
  #Function to update concept form inputs
  updateScenarioForm <- function(RowNum) {
    updateTextInput(session, "conceptVarName",
                    value = scenariotable$values$name[RowNum])
    updateTextInput(session, "conceptStartValue",
                    value = scenariotable$values$startvalue[RowNum])
    updateTextInput(session, "conceptStartChange",
                    value = scenariotable$values$startchange[RowNum])
    updateTextInput(session, "conceptValuesDescription",
                    value = scenariotable$values$description[RowNum])
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
        "editModel" = "Select model to edit"
      ),
      choices = dir(path = "../models")[dir(path = "../models") != "templates"]
    )
  })
  #Choose model start option and initialize model
  observeEvent(
    input$startModeling,
    {
      if (input$modelAction == "newModel") {
        if (input$modelName == "") {
          createAlert(session = session, anchorId = "nonameAlert", 
                      title = "Missing Name", 
                      content = "Model name is missing. Enter a name.")
          return()
        }
        model$status <- initializeNewModel(input$modelName)
        model$concepts <- loadModelConcepts(input$modelName)
        model$relations <- list() #loadModelRelations(input$modelName)
        updateConceptsTable()
        saveLastState()
      }
      if (input$modelAction == "copyModel") {
        if (input$modelName == "") {
          createAlert(session = session, anchorId = "nonameAlert", 
                      title = "Missing Name", 
                      content = "Model name is missing. Enter a name.")
          return()
        }
        model$status <- initializeCopyModel(input$modelName, input$modelFileName, input$copyScenarios)
        model$concepts <- loadModelConcepts(input$modelName)
        model$relations <- loadModelRelations(input$modelName)
        updateConceptsTable()
        saveLastState()
      }
      if (input$modelAction == "editModel") {
        model$status <- loadModelStatus(input$modelFileName)
        model$concepts <- loadModelConcepts(input$modelFileName)
        model$relations <- loadModelRelations(input$modelFileName)
        updateConceptsTable()
        saveLastState()
      }
    }
  )
  #Output model status information
  output$modelName <- renderText({model$status$name})
  output$modelParent <- renderText({model$status$parent})
  output$modelCreated <- renderText({model$status$created})
  output$modelEdited <- renderText({model$status$lastedit})
  
    
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
  #Implement the save model button
  observeEvent(
    input$saveModel,
    {
      saveModel(model)
    }
  )
  #Output model concepts table
  output$conceptsTable <- DT::renderDataTable(
    formatConceptTable(conceptstable$concepts), 
    server = FALSE, 
    selection = list(mode = 'single', target = 'row', selected = 1)
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
      choices = sort(model$concepts$name)
    )
  })
  #Define dropdown element to select affected concept from a list
  output$selectAffectedConcept <- renderUI({
    selectInput(
      inputId = "affectedConcept",
      label = "Affected Concept",
      choices = sort(model$concepts$name)
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
        if (input$affectedConcept %in% effects$name) {
          updateTextInput(session, "causalDirection",
                          value = effects$direction[effects$name == input$affectedConcept])
          updateTextInput(session, "causalStrength",
                          value = effects$strength[effects$name == input$affectedConcept])
          updateTextInput(session, "causalDesc",
                          value = effects$description[effects$name == input$affectedConcept])
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
      if (input$affectedConcept %in% effects$name) {
          updateTextInput(session, "causalDirection",
                          value = effects$direction[effects$name == input$affectedConcept])
          updateTextInput(session, "causalStrength",
                          value = effects$strength[effects$name == input$affectedConcept])
          updateTextInput(session, "causalDesc",
                          value = effects$description[effects$name == input$affectedConcept])
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
      CausalConcept <- 
        model$concepts$variable[model$concepts$name == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$name))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$variable[model$concepts$name == input$affectedConcept]
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
      CausalConcept <- 
        model$concepts$variable[model$concepts$name == input$causalConcept]
      CausalConcepts_ <-
        unlist(lapply(model$relations, function(x) x$name))
      CausalIdx <- which(CausalConcepts_ == CausalConcept)
      AffectedConcept <- 
        model$concepts$variable[model$concepts$name == input$affectedConcept]
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
      undoRelationEdit()
      updateTextInput(session, "causalDirection",
                      value = effects$direction[effects$name == input$affectedConcept])
      updateTextInput(session, "causalStrength",
                      value = effects$strength[effects$name == input$affectedConcept])
      updateTextInput(session, "causalDesc",
                      value = effects$description[effects$name == input$affectedConcept])
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
    arrows(Map$X0, Map$Y0, Map$X1, Map$Y1, col = Map$Col, lwd = Map$Lwd, length = 0.1)
  }, 
  width = 800, height = 700)
  
    
  #-----------------------------------------
  #IMPLEMENT INTERFACE FOR CHOOSING SCENARIO
  #-----------------------------------------
  #Define GUI element to select scenario from a list
  output$selectScenarioFile <- renderUI({
    selectInput(
      inputId = "scenarioFileName",
      label = switch(
        input$scenarioAction,
        "copyScenario" = "Select scenario to copy",
        "editScenario" = "Select scenario to edit"
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
          initializeCopyScenario(model$status$name, input$scenarioName, input$scenarioFileName)
        scenario$status <- ScenInit_ls$status
        scenario$values <- ScenInit_ls$values
        updateScenarioTable()
        saveLastScenarioState()
        updateScenarioForm(1)
      }
      if (input$scenarioAction == "editScenario") {
        ScenInit_ls <- 
          loadScenario(model$status$name, input$scenarioFileName)
        scenario$status <- ScenInit_ls$status
        scenario$values <- ScenInit_ls$values
        updateScenarioTable()
        saveLastScenarioState()
        updateScenarioForm(1)
      }
    }
  )

  
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
      scenariotable$values$name[RowNum] <- input$conceptVarName
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
      } else {
        scenario$status$validated <- ""
        VMsg <- Validation_ls$Errors
      }
      output$validationMsg <- renderText({VMsg})
    }
  )
  #Implement the saveScenario button
  observeEvent(
    input$saveScenario,
    {
      saveScenario(scenario)
    }
  )
  #Output scenario table
  output$scenarioTable <- DT::renderDataTable(
    scenariotable$values,
    server = FALSE,
    selection = list(mode = 'single', target = 'row', selected = 1)
  )
  

  #-----------------------------------------
  #IMPLEMENT INTERFACE FOR RUNNING THE MODEL
  #-----------------------------------------
  #Implement action button to list scenarios for model
  observeEvent(
    input$listScenarios,
    {
      ScenarioValidation_ls <- listScenarios(model$status$name)
      validscenarios$valid <- ScenarioValidation_ls$Valid
      validscenarios$invalid <- ScenarioValidation_ls$Invalid
    }
  )
  #Define checkbox GUI element to select valid scenarios from list
  output$selectScenariosToRun <- renderUI({
    Scenarios_ <- 
      dir(path = file.path("../models/", model$status$name, "scenarios"))
    checkboxGroupInput(
      inputId = "scenariosToRun",
      label = "Check Scenarios to Run",
      choices = validscenarios$valid
    )
  })
  #List scenarios that are not validated
  output$invalidScenarios <- 
    renderText(validscenarios$invalid)
  #Implement the model run button
  observeEvent(
    input$runModel,
    {
      output$runMessage <- renderText({
      withProgress(
        message = "Model is Running",
        detail = "This may take a while",
        value = 0,
        {
          Sc <- input$scenariosToRun
          Sys.sleep(0.5)
          for (sc in Sc) {
            ModelPath <- file.path("../models", model$status$name)
            ScenarioPath <- file.path(ModelPath, "scenarios", sc)
            Model_ls <- createFuzzyModel(ModelPath)
            Scenario_ls <- createFuzzyScenario(ScenarioPath, Model_ls)
            Outputs_ls <- runFuzzyModel(Model_ls, Scenario_ls)
            save(Outputs_ls, file = file.path(ScenarioPath, "Outputs_ls.RData"))
            incProgress(1 / length(Sc))
          }          
        }
      )
        "Model Runs Completed"
      })
    }
  )
  #Implement the run reset button
  observeEvent(
    input$resetRun,
    {
      output$runMessage <- renderText({""})
      validscenarios$valid <- ""
      validscenarios$invalid <- ""      
    }
  )
  
    
  #------------------------------------------
  #IMPLEMENT INTERFACE FOR DISPLAYING RESULTS
  #------------------------------------------
  #Define GUI element to select scenario 1 from a list
  output$selectScenarioPlot1 <- renderUI({
    ScenarioValidation_ls <- listScenarios(model$status$name)
    validscenarios$valid <- ScenarioValidation_ls$Valid
    selectInput(
      inputId = "scenarioPlot1",
      label = "Select Scenario 1",
      choices = validscenarios$valid
    )
  })
  #Define GUI element to select scenario 2 from a list
  output$selectScenarioPlot2 <- renderUI({
    ScenarioValidation_ls <- listScenarios(model$status$name)
    validscenarios$valid <- ScenarioValidation_ls$Valid
    selectInput(
      inputId = "scenarioPlot2",
      label = "Select Scenario 2",
      choices = c("None", validscenarios$valid)
    )
  })
  #Define checkbox GUI element to select variables to plot
  output$selectVarsToPlot <- renderUI({
    FilePath <- file.path("../models", model$status$name, "scenarios", input$scenarioPlot1, "Outputs_ls.RData")
    Summary_mx <- assignLoad(FilePath)$Summary
    checkboxGroupInput(
      inputId = "variablesToPlot",
      label = "Check Variables to Plot",
      choices = colnames(Summary_mx)
    )
  })
  #Implement results plots
  output$resultsPlot <- renderPlot({
    ScenPath <- file.path("../models", model$status$name, "scenarios")
    Data1Path <- file.path(ScenPath, input$scenarioPlot1, "Outputs_ls.RData")
    Summary1_mx <- assignLoad(Data1Path)$Summary
    DoOnlyOne <- input$scenarioPlot2 == "None"
    if (DoOnlyOne) {
      if (length(input$variablesToPlot != 0)) {
        Data1_mx <- Summary1_mx[,input$variablesToPlot]
        layout(
          matrix(1:2, ncol = 2),
          widths = c(4,1)
        )
        par(mar = c(5,4,4,0.5))
        matplot(Data1_mx,
                type = "l", xlab = "Iteration",
                ylab = "Percentage of Maximum", main = input$scenarioPlot1,
                cex.main = 1.5)
        par(mar = c(5,0,4,0))
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "Legend")
        legend("topleft", col = 1:length(input$variablesToPlot),
               lty = 1:length(input$variablesToPlot), bty = "n",
               legend = input$variablesToPlot)
      }
    } else {
      Data2Path <- file.path(ScenPath, input$scenarioPlot2, "Outputs_ls.RData")
      Summary2_mx <- assignLoad(Data2Path)$Summary
      if (length(input$variablesToPlot != 0)) {
        Data1_mx <- Summary1_mx[,input$variablesToPlot]
        Data2_mx <- Summary2_mx[,input$variablesToPlot]
        YLim_ <- range(c(range(Data1_mx), range(Data2_mx)))
        layout(
          matrix(1:3, ncol = 3),
          widths = c(3.3,3,1)
        )
        par(mar = c(5,4,4,0))
        matplot(Data1_mx, type = "l", xlab = "Iteration",
                ylab = "Percentage of Maximum", ylim = YLim_, 
                main = input$scenarioPlot1, cex.main = 1.5)
        par(mar = c(5,0.5,4,0))
        matplot(Data2_mx, type = "l", xlab = "Iteration",
                ylab = "Percentage of Maximum", ylim = YLim_, 
                main = input$scenarioPlot2, cex.main = 1.5,
                axes = FALSE)
        box()
        axis(1)
        par(mar = c(5,0,4,0))
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "Legend")
        legend("topleft", col = 1:length(input$variablesToPlot),
               lty = 1:length(input$variablesToPlot), bty = "n",
               legend = input$variablesToPlot)
      }
    }
  }, 
  width = 800, height = 700)  
  

  
  

  
  
})