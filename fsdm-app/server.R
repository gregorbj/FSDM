#server.R

library(shiny)
library(shinyBS)
library(jsonlite)
library(DT)

source("fsdm.R")
source("helper.R")


#Shiny Server Function
#=====================
shinyServer(function(input, output, session) {

    
  #Create objects to store model state
  #-----------------------------------
  #Reactive object to store current state that interface responds to
  model <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)
  #Reactive object to store model history (unlimited undo)
  history <- reactiveValues(status = NULL, concepts = NULL, relations = NULL)
  #Reactive object to represent concepts table
  conceptstable <- reactiveValues(concepts = NULL)
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
  #Function to update concepts table with model
  updateConceptsTable <- function() {
    conceptstable$concepts <- model$concepts
  }
  #Function to undo relation edit
  undoRelationEdit <- function() {
    swapState()
  }


  #Start new model
  #---------------
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

    
  #Edit model concepts
  #-------------------
  #Select concept to edit from table
  observeEvent(
    input$conceptsTable_rows_selected,
    {
      RowNum <- input$conceptsTable_rows_selected
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
  )
  #Add new concept to table
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
  )
  #Update concept data
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
  #Undo edit
  observeEvent(
    input$undoConceptAction,
    {
      undoConceptEdit()
      conceptstable$concepts <- model$concepts
    }
  )
  #Delete concept
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
      conceptstable$concepts <- model$concepts
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
    }
  )
  

  #Edit model relations
  #--------------------
  #Define GUI element to select causal group
  output$selectCausalGroup <- renderUI({
    selectInput(
      inputId = "causalGroup",
      label = "Causal Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define GUI element to select affected group
  output$selectAffectedGroup <- renderUI({
    selectInput(
      inputId = "affectedGroup",
      label = "Affected Group",
      choices = c("All", model$concepts$group)
    )
  })
  #Define GUI element to select causal concept from a list
  output$selectCausalConcept <- renderUI({
    selectInput(
      inputId = "causalConcept",
      label = "Causal Concept",
      choices = sort(model$concepts$name)
    )
  })
  #Define GUI element to select affected concept from a list
  output$selectAffectedConcept <- renderUI({
    selectInput(
      inputId = "affectedConcept",
      label = "Affected Concept",
      choices = sort(model$concepts$name)
    )
  })
  #Create a reactive values object to store current selected effects
  effects <- 
    reactiveValues(
      variable = "",
      name = "",
      direction = "",
      strength = "",
      description = "")
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
  #Update relations data
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
    }
  )
  #Delete relation
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
    }
  )

  
  #Plot relations map
  #------------------
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
  }, width = 800, height = 700)
      

  #Save model
  #----------
  observeEvent(
    input$saveModel,
    {
      saveModel(model)
    }
  )

    
  #Output the model status information
  #-----------------------------------
  output$modelName <- renderText({model$status$name})
  output$modelParent <- renderText({model$status$parent})
  output$modelCreated <- renderText({model$status$created})
  output$modelEdited <- renderText({model$status$lastedit})
  output$modelStatus <- renderText({head(model$status)})
  output$modelConcepts <- renderText({head(model$concepts)})
  output$modelRelations <- renderText({head(model$relations)})
  output$conceptsTable <- DT::renderDataTable(
    formatConceptTable(model$concepts), 
    server = FALSE, selection = list(mode = 'single', target = 'row', selected = 1)
    )

  
  
})