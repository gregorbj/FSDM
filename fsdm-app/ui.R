#ui.R
#Author: Brian Gregor, Oregon Systems Analytics LLC
#Copyright: Oregon Department of Transportation 2016
#License: Apache 2


#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(DiagrammeR)
#Function to support text area inputs
textareaInput <- function(id, label, value="", rows=5, cols=40, class="form-control"){
  tags$div(
    class="form-group shiny-input-container",
    tags$label('for'=id,label),
    tags$textarea(id=id,class=class,rows=rows,cols=cols,value))
}


#SHINY UI FUNCTION
#-----------------
shinyUI(
  navbarPage(
    "Logic Laboratory",
    
    #Introduction Screen
    #-------------------
    tabPanel(
      "Introduction",
      fluidPage(
        titlePanel(span("Introduction", img(src="OSA_Logo2.png", height=50, width=75, align="right"))),
        br(),
        h3("What is the FSDM Modeler", style = "color: blue"),
        p("The FSDM Modeler is an application for building and running fuzzy systems dynamics models (FSDM). FSDMs are variants of fuzzy cognitive maps (FCM). A FSDM is represented as a directed graph whose ", strong("nodes are concepts"), " that are being modeled and whose ", strong("edges specify relationships between concepts"), ". The following figure illustrates a simple FSDM."),
        img(src = "simple_fsdm.png", height = 200, width = 275, style = "display: block; margin-left: auto; margin-right: auto"),
        p("The direction of each edge (i.e. the direction of the arrow) specifies the relationship between causal and affected concepts. In the example, the ", strong("Proximity"), " concept is shown as affecting the ", strong("Auto Trip Distance"), " concept. Edge weights specify the strength and directionality of causal effects. A positive sign for an edge weight means that an increase in the causing concept causes an increase in the affected concept. In the example, an increase in the ", strong("Auto Trip Distance"), " concept causes a decrease in the ", strong("Auto Trip Rate"), " concept and an increase in the ", strong("VMT"), " concept. In the FSDM, as in the FCM, the strength of causal effects is expressed in ‘fuzzy’ terms. In the example, the weights are L (low), M (moderate), and H (high), as opposed to specific numeric values."),
        p("At its core, FSDM involves defining the concepts and edges. Each concept has a name, a description, and a range of possible values. Each relation defined by a causal concept and an affected concept, the sign of the relationship (+ or -), and the strength of the relationship. One or many scenarios may be run on a FSDM. Each scenario is defined by the starting values assigned to every concept and by the final values assigned to one or more of the concepts. When the FSDM is run, the concepts that have assigned final values have their values incremented in small steps between the starting and final values. With each increment, the orders of causal effects are calculated until every causal effect is accounted for. For example, if the ", strong("Proximity"), " concept in the example FSDM above is being incremented, then with each increment the following are calculated:"),
        tags$ol(
          tags$li("The effect of a change in ", strong("Proximity"), " on ", strong("Auto Trip Distance"), " is calculated."),
          tags$li("The effects of a change in ", strong("Auto Trip Distance"), " on ", strong("Auto Trip Rate"), " and ", strong("VMT"), " are calculated."),
          tags$li("The effect of a change in ", strong("Auto Trip Rate"), " on ", strong("VMT"), " is calculated.")
        ),
        p("If the FSDM includes one or more cycles (i.e. feedback loops), these calculations are repeated until the changes in concept values with each repeated set of calculations is very small or until a specified maximum number of repeat calculations is reached."),
        hr(),
        h3("How to Build and Run a FSDM", style = "color: blue"),
        p("The FSDM Modeler interface will guide you through the process of 1) building a FSDM model, 2) creating scenarios to model with the FSDM, 3) running the FSDM on the scenarios, and 4) analyzing the model results. The four numbered 'tabs' at the top of the page correspond to these tasks. Clicking on a 'tab' changes the interface to guide you through the process of carrying out the task. Following are summaries of what is done in each task:"),
        tags$ol(
          tags$li(strong("Build a Model:"), " Building a FSDM model involves four steps that are listed as menu items: Select Model, Edit Concepts, Edit Relations, Save Model. The first step, selecting a model, shows a view which enables you to start a model from scratch, or copy an existing model to serve as the starting point for your model, or edit an existing model. The second step, editing concepts, shows a listing of all the concepts in the model and provides input boxes to enable you to edit the values. It also enables you to add new concepts to the model. The third step, editing relations, shows you several views of the specified relationships between concepts and allows you to edit existing relationships and specify new ones. The final step, saving the model, permanently saves all of the model edits."),
          tags$li(strong("Create Scenarios:"), " One or more model scenarios can be created for a FSDM. This 'tab' of the application assists you with creating single or multiple scenarios, and with editing existing scenarios. A new scenario can be created from an existing scenario. The interface enables multiple scenarios to be created by specifying minimum and maximum final values for a concept, and the number of increments between those values. The interface shows a table of concept values as the scenario is being edited."),
          tags$li(strong("Run the Model:"), " The FSDM may be run for one or more of the scenarios. This 'tab' enables you to specify which of the scenarios to run. The interface shows a checkbox list of all of the scenarios that have been created for a model. Entering a check in a checkbox marks the corresponding scenario to be run. A checkbox is also presented that enables you to select all of the scenarios at once. A button starts the model runs. Progress of the model run is shown as the model run proceeds."),
          tags$li(strong("Analyze Results:"), " This 'tab' of the application enables users to produce several prescribed analyses of scenario results. These include analyses that show the results for a single scenario and analyses that compare the results of several scenarios. In addition, allows user-defined functions to be called to carry out an analysis.")
        )
      )
    ),
    
    #Build a Model Screen
    #--------------------
    navbarMenu( "1) Build a Model",
                tabPanel( "Select Model",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Select Model"),
                              hr(),
                              radioButtons(
                                inputId = "modelAction", 
                                label = "Model Action",
                                choices = list("Create New Model From Scratch" = "newModel",
                                               "Create New Model From Copy" = "copyModel",
                                               "Edit Existing Model" = "editModel")
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'newModel' || input.modelAction == 'copyModel'",
                                textInput("modelName", "Model Name", "")
                              ),
                              bsAlert(
                                "nonameAlert"
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'copyModel' || input.modelAction == 'editModel'",
                                uiOutput("selectModelFile")
                              ),
                              conditionalPanel(
                                condition = "input.modelAction == 'copyModel'",
                                checkboxInput("copyScenarios", "Copy scenarios too?")
                              ),
                              actionButton("startModeling", "Start Working on Model")
                            ),
                            mainPanel(
                              h4("Model Name: ", textOutput("modelName", inline = TRUE)),
                              h4("Parent Model: ", textOutput("modelParent", inline = TRUE)),
                              h4("Created: ", textOutput("modelCreated", inline = TRUE)),
                              h4("Last Edited: ", textOutput("modelEdited", inline = TRUE))
                            )
                          )
                ),
                tabPanel( "Edit Concepts",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Edit Concepts"),
                              hr(),
                              textInput("conceptName", "Concept Name"),
                              textInput("varName", "Concept Label"),
                              textareaInput("conceptDesc", "Concept Description"),
                              textInput("minValue", "Minimum Value"),
                              textInput("maxValue", "Maximum Value"),
                              textareaInput("valuesDesc", "Values Description"),
                              textInput("conceptGroup", "Concept Group"),
                              actionButton("addConcept", "New"),
                              actionButton("updateConcept", "Update"),
                              actionButton("deleteConcept", "Delete"),
                              actionButton("undoConceptAction", "Undo")
                            ),
                            mainPanel(
                              tabPanel("Concepts", DT::dataTableOutput("conceptsTable"), value = "table")
                            )
                          )
                          
                ),
                tabPanel( "Edit Relations",
                          sidebarLayout(
                            sidebarPanel(
                            tabsetPanel(
                              tabPanel(
                                title = "Edit Relations",
                                br(),
                                uiOutput("selectCausalGroup"),
                                uiOutput("selectAffectedGroup"),
                                uiOutput("selectCausalConcept"),
                                uiOutput("selectAffectedConcept"),
                                selectInput(inputId = "causalDirection", 
                                            label = "Causal Direction", 
                                            choices = c("" ,"Positive", "Negative")),
                                selectInput(inputId = "causalStrength", 
                                            label = "Causal Strength", 
                                            choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH")),
                                textareaInput("causalDesc", "Causal Description"),
                                actionButton("updateRelation", "Update"),
                                actionButton("deleteRelation", "Delete"),
                                actionButton("undoRelationAction", "Undo")                                
                              ),
                              tabPanel(
                                title = "Relations Graph Format",
                                br(),
                                selectInput(inputId = "graphOrientation",
                                            label =  "Graph Orientation",
                                            choices = c("Landscape", "Portrait"),
                                            selected = "Portrait"),
                                selectInput(inputId = "graphLayout",
                                            label = "Graph Layout",
                                            choices = c("Left-to-Right", "Top-to-Bottom"),
                                            selected = "Top-to-Bottom"),
                                selectInput(inputId = "nodeShape",
                                            label = "Node Shape",
                                            choices = c("box", "oval", "circle"),
                                            selected = "box"),
                                selectInput(inputId = "edgeLabel",
                                            label = "Edge Label",
                                            choices = c("label", "value"),
                                            selected = "Level"),
                                actionButton(inputId = "saveRelationsGraph",
                                             label = "Save Graph")
                              )
                            )

                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "Relations Graph",
                                  grVizOutput('relations_graph', width = "100%", height = "800px")
                                ),
                                tabPanel(
                                  title = "Relations Map",
                                  plotOutput("relations_map")
                                )
                              )
                            )
                          )
                ),
                tabPanel( "Save Edits",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Save Edits"),
                              p("Pressing the ", strong("Save Edits"), " button will save the save the edited model, overwriting the Concepts and Relations files and updating the status file."),
                              actionButton("saveModel", "Save Model")
                            ),
                            mainPanel(
                              
                            )
                          )
                )
    ),
    
    #Create Scenarios Screen
    #-----------------------
    tabPanel(
      "2) Create Scenarios",
      titlePanel("Create Scenarios"),
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel(
              title = "Select Scenario",
              br(),
              radioButtons(
                inputId = "scenarioAction", 
                label = "Scenario Action",
                choices = list("Create New Scenario From Scratch" = "newScenario",
                               "Create New Scenario From Copy" = "copyScenario",
                               "Edit Existing Scenario" = "editScenario")
              ),
              conditionalPanel(
                condition = "input.scenarioAction == 'newScenario' || input.scenarioAction == 'copyScenario'",
                textInput("scenarioName", "Scenario Name", "")
              ),
              bsAlert(
                "noscenarioAlert"
              ),
              conditionalPanel(
                condition = "input.scenarioAction == 'copyScenario' || input.scenarioAction == 'editScenario'",
                uiOutput("selectScenarioFile")
              ),
              actionButton("startScenario", "Start Working on Scenario")              
            ),
            tabPanel(
              title = "Edit Scenario Values",
              br(),
              textInput("conceptVarName", "Concept Variable Name"),
              textInput("conceptStartValue", "Concept Starting Value"),
              textInput("conceptStartChange", "Concept Starting Change"),
              textareaInput("conceptValuesDescription", "Concept Values Description"),
              actionButton("updateScenario", "Update"),
              actionButton("undoScenarioAction", "Undo"),
              actionButton("validateScenario", "Validate"),
              actionButton("saveScenario", "Save Scenario")
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Scenario Values", 
              DT::dataTableOutput("scenarioTable"), 
              value = "table"
              ),
            tabPanel(
              "Validation Results",
              verbatimTextOutput("validationMsg")
              )
          )
        )
      )
    ),
    
    #Run the Model Screen
    #--------------------
    tabPanel(
      "3) Run the Model",
      titlePanel("Run the Model"),
      sidebarLayout(
        sidebarPanel(
          h4("List Model Scenarios"),
          actionButton("listScenarios", "List Scenarios"),
          hr(),
          h4("Scenarios that have been validated and may be run"),
          uiOutput("selectScenariosToRun"),
          hr(),
          h4("Scenarios that must be validated before they can be run"),
          verbatimTextOutput("invalidScenarios"),
          hr(),
          p("Pressing the ", strong("Run Model"), " button will run the model for all the scenarios that are checked in the list above. Outputs will be saved in the respective scenario directories."),
          actionButton("runModel", "Run Model"),
          actionButton("resetRun", "Reset")
        ),
        mainPanel(
          h4(textOutput("runMessage"))
        )
      )
    ),
    
    #Analyze Results Screen
    #----------------------
    tabPanel(
      "4) Analyze Results",
      titlePanel("Analyze Results"),
      sidebarLayout(
        sidebarPanel(
          actionButton("listRunScenarios", "Update Scenario Selection Set"),
          hr(),
          uiOutput("selectScenarioPlot1"),          
          uiOutput("selectScenarioPlot2"),
          uiOutput("selectVarsToPlot"),
          hr(),
          actionButton("saveResults", "Save Results"),
          textInput("analysisSaveName", "Analysis Save Name", ""),
          bsAlert(
            "noAnalysisNameAlert"
          )          
        ),
        mainPanel(
          plotOutput("resultsPlot")
        )
      )
    )
    
  ))