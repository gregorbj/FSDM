#ui.R
#Author: Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#Copyright: 2019, Brian Gregor
#License: Apache 2


#LOAD RESOURCES
#--------------
#Packages
library(shiny)
library(shinyBS)
library(DT)
library(ggplot2)
library(DiagrammeR)
library(shinyFiles)
library(fs)
library(plotly)

#Function to support text area inputs
textareaInput <-
  function(id,
           label,
           value = "",
           rows = 5,
           cols = 40,
           class = "form-control") {
    tags$div(
      class = "form-group shiny-input-container",
      tags$label('for' = id, label),
      tags$textarea(
        id = id,
        class = class,
        rows = rows,
        cols = cols,
        value
      )
    )
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
        titlePanel(span(
          "Introduction",
          img(
            src = "OSA_Logo2.png",
            height = 60,
            width = 85,
            align = "right"
          )
        )),
        br(),
        h3("What is the Logic Laboratory", style = "color: blue"),
        p(
          "The Logic Laboratory is an application for building and running
          fuzzy systems dynamics models (FSDM). FSDMs are variants of fuzzy
          cognitive maps (FCM). A FSDM is represented as a directed graph whose ",
          strong("nodes are concepts"),
          " that are being modeled and whose ",
          strong("edges specify relationships between concepts"),
          ". The following figure illustrates a simple FSDM."
        ),
        img(
          src = "simple_fsdm.png",
          height = 200,
          width = 275,
          style = "display: block; margin-left: auto; margin-right: auto"
        ),
        p(
          "The direction of each edge (i.e. the direction of the arrow)
          specifies the causal relationship between concepts. In the example, the ",
          strong("Proximity"),
          " concept is shown as affecting the ",
          strong("Auto Trip Distance"),
          " concept. Edge weights specify the strength and directionality of
          causal effects. A positive sign for an edge weight means that an
          increase in the causing concept causes an increase in the affected
          concept. In the example, an increase in the ",
          strong("Auto Trip Distance"),
          " concept causes a decrease in the ",
          strong("Auto Trip Rate"),
          " concept and an increase in the ",
          strong("VMT"),
          " concept. In the FSDM, as in the FCM, the strength of causal effects
          is expressed in ‘fuzzy’ terms. In the example, the weights are L
          (low), M (moderate), and H (high), as opposed to specific numeric
          values."
        ),
        p(
          "At its core, FSDM involves defining the concepts and edges. Each
          concept has a name, a description, and a range of possible values.
          Each relation defined by a causal concept and an affected concept, the
          sign of the relationship (+ or -), and the strength of the relationship.
          One or many scenarios may be run on a FSDM. Each scenario is defined by
          the starting values assigned to every concept and by the final values
          assigned to one or more of the concepts. When the FSDM is run, the
          concepts that have assigned final values have their values incremented
          in small steps between the starting and final values. With each
          increment, the orders of causal effects are calculated until every
          causal effect is accounted for. For example, if the ",
          strong("Proximity"),
          " concept in the example FSDM above is being incremented, then with
          each increment the following are calculated:"
        ),
        tags$ol(
          tags$li(
            "The effect of a change in ",
            strong("Proximity"),
            " on ",
            strong("Auto Trip Distance"),
            " is calculated."
          ),
          tags$li(
            "The effects of a change in ",
            strong("Auto Trip Distance"),
            " on ",
            strong("Auto Trip Rate"),
            " and ",
            strong("VMT"),
            " are calculated."
          ),
          tags$li(
            "The effect of a change in ",
            strong("Auto Trip Rate"),
            " on ",
            strong("VMT"),
            " is calculated."
          )
        ),
        p(
          "If the FSDM includes one or more cycles (i.e. feedback loops), these
          calculations are repeated until the changes in concept values with
          each repeated set of calculations is very small or until a specified
          maximum number of repeat calculations is reached."
        ),
        hr(),
        h3("How to Build and Run a FSDM", style = "color: blue"),
        p(
          "The Logic Laboratory interface will guide you through the process of
          1) building a FSDM model,
          2) creating scenarios to model with the FSDM,
          3) running the FSDM on the scenarios, and
          4) analyzing the model results.
          The four numbered 'tabs' at the top of the page correspond to these
          tasks. Clicking on a 'tab' changes the interface to guide you through
          the process of carrying out the task. Following are summaries of what
          is done in each task:"
        ),
        tags$ol(
          tags$li(
            strong("Build a Model:"),
            " Building a FSDM model involves five steps that are listed as menu
            items: User Info, Edit Concepts, Edit Relations, Save Model. The
            first step, entering user information is necessary in order to
            attribute models and model edits. The second step, selecting a model,
            shows a view which enables you to start a model from scratch, copy
            an existing model to serve as the starting point for your model,
            edit an existing model, or run an existing model without editing.
            The third step, editing concepts, is where you define new concepts
            or edit existing concepts and the range of values that they may
            have. The fourth step, editing relations, is where you define the
            relationships between concepts (magnitude and direction). The final
            step, saving the model, permanently saves all of the model edits
            along with notes to document the model or edits to the model."
          ),
          tags$li(
            strong("Create Scenarios:"),
            " One or more model scenarios can be created for a FSDM. This 'tab'
            of the application assists you with creating single or multiple
            scenarios, and with editing existing scenarios. A new scenario can
            be started from scratch or created from an existing scenario.
            Scenarios are validated against the defined model to assure that
            scenario values are consistent with defined concept value ranges."
          ),
          tags$li(
            strong("Run the Model:"),
            " The FSDM may be run for one or more of the scenarios. This 'tab'
            enables you to specify which of the scenarios to run. The interface
            shows a checkbox list of all of the scenarios that have been created
            for a model. Entering a check in a checkbox marks the corresponding
            scenario to be run. A button starts the model runs. Progress of the
            model run is shown as the model run proceeds."
          ),
          tags$li(
            strong("Analyze Results:"),
            " This 'tab' of the application enables users to view the results
            for one or two scenarios. The user can choose which model scenarios
            and which concept values to display. The results are displayed in
            graphs. Users can choose to save the displayed graphs and
            corresponding data."
          )
        ),
        hr(),
        h3("Copyright and License", style = "color: blue"),
        p(
          "The Logic Laboratory was developed by Brian Gregor (Oregon Systems
          Analytics) with funding from the Oregon Department of Transportation.
          It is licensed with the Apache 2 open source license. Terms of the
          license are included in the LICENSE text file. Additional notices are
          included in the NOTICE text file."
        ),
        h4("Copyright 2016 Oregon Department of Transportation"),
        h4("License Apache 2")
      )
    ),

    #Build a Model Screen
    #--------------------
    navbarMenu(
      "1) Build a Model",
      tabPanel("User Information",
               sidebarLayout(
                 sidebarPanel(
                   h4("User Information"),
                   hr(),
                   p(
                     "The user information entered below is used to attribute
                     model creation and editing."
                   ),
                   textInput("firstName", "First Name"),
                   textInput("lastName", "Last Name"),
                   textInput("organization", "Organization")
                 ),
                 mainPanel()
               )),
      tabPanel("Select Model",
               sidebarLayout(
                 sidebarPanel(
                   h4("Select Project Folder"),
                   shinyDirButton("ProjectFolder", "Folder select", "Please select a folder"),
                   hr(),
                   h4("Select Model"),
                   conditionalPanel(
                     condition = "output.projectFolder != ''",
                     radioButtons(
                       inputId = "modelAction",
                       label = "Model Action",
                       choices = list(
                         "Create New Model From Scratch" = "newModel",
                         "Create New Model From Copy" = "copyModel",
                         "Edit Existing Model" = "editModel",
                         "Run Existing Model Without Editing" = "runModel"
                       )
                     ),
                     conditionalPanel(condition = "input.modelAction == 'newModel' ||
                                    input.modelAction == 'copyModel'",
                                      textInput("modelName", "Model Name", "")),
                     bsAlert("nonameAlert"),
                     bsAlert("duplicateModel"),
                     bsAlert("noAuthorInfo"),
                     conditionalPanel(condition = "input.modelAction == 'copyModel' ||
                                    input.modelAction == 'editModel' ||
                                    input.modelAction == 'runModel'",
                                      uiOutput("selectModelFile")),
                     conditionalPanel(
                       condition = "input.modelAction == 'copyModel'",
                       checkboxInput("copyScenarios", "Copy scenarios too?")
                     ),
                     actionButton("startModeling", "Start Working on Model")
                   )
                 ),
                 mainPanel(
                   h4("Project Folder: ", textOutput("projectFolder", inline = TRUE)),
                   h4("Model Name: ", textOutput("modelName", inline = TRUE)),
                   h4("Parent Model: ", textOutput("modelParent", inline = TRUE)),
                   h4("Created: ", textOutput("modelCreated", inline = TRUE)),
                   h4("Last Edited: ", textOutput("modelEdited", inline = TRUE)),
                   h4("Attribution History", verbatimTextOutput("modelAttribution")
                   )
                 )
               )),
      tabPanel("Edit Concepts",
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
                   conditionalPanel(
                     condition = "input.modelAction != 'runModel'",
                     wellPanel(
                       actionButton("addConcept", "New"),
                       actionButton("updateConcept", "Update"),
                       actionButton("deleteConcept", "Delete"),
                       actionButton("undoConceptAction", "Undo"),
                       bsAlert("duplicateConceptName"),
                       bsAlert("duplicateConceptVariable")
                     )
                   )
                 ),
                 mainPanel(tabPanel(
                   "Concepts", DT::dataTableOutput("conceptsTable"), value = "table"
                 ))
               )),
      tabPanel("Edit Relations",
               sidebarLayout(
                 sidebarPanel(tabsetPanel(
                   tabPanel(
                     title = "Edit Relations",
                     br(),
                     uiOutput("selectCausalGroup"),
                     uiOutput("selectAffectedGroup"),
                     uiOutput("selectCausalConcept"),
                     uiOutput("selectAffectedConcept"),
                     selectInput(
                       inputId = "causalDirection",
                       label = "Causal Direction",
                       choices = c("" , "Positive", "Negative")
                     ),
                     selectInput(
                       inputId = "causalStrength",
                       label = "Causal Strength",
                       choices = c("", "VL", "L", "ML", "M", "MH", "H", "VH")
                     ),
                     textareaInput("causalDesc", "Causal Description"),
                     conditionalPanel(
                       condition = "input.modelAction != 'runModel'",
                       wellPanel(
                         actionButton("updateRelation", "Update"),
                         actionButton("deleteRelation", "Delete"),
                         actionButton("undoRelationAction", "Undo")
                       )
                     )
                   ),
                   tabPanel(
                     title = "Relations Graph Format",
                     br(),
                     selectInput(
                       inputId = "graphOrientation",
                       label =  "Graph Orientation",
                       choices = c("Landscape", "Portrait"),
                       selected = "Portrait"
                     ),
                     selectInput(
                       inputId = "graphLayout",
                       label = "Graph Layout",
                       choices = c("Left-to-Right", "Top-to-Bottom"),
                       selected = "Top-to-Bottom"
                     ),
                     selectInput(
                       inputId = "nodeShape",
                       label = "Node Shape",
                       choices = c("box", "oval", "circle"),
                       selected = "box"
                     ),
                     selectInput(
                       inputId = "edgeLabel",
                       label = "Edge Label",
                       choices = c("label", "value"),
                       selected = "Level"
                     ),
                     actionButton(inputId = "saveRelationsGraph",
                                  label = "Save Graph")
                   )
                 )),
                 mainPanel(tabsetPanel(
                   tabPanel(
                     title = "Relations Graph",
                     grVizOutput('relations_graph', width = "100%", height = "800px")
                   ),
                   tabPanel(title = "Relations Map",
                            plotOutput("relations_map"))
                 ))
               )),
      tabPanel(
        "Save Model",
        conditionalPanel(condition = "input.modelAction == 'runModel'",
                         wellPanel(
                           h4("Run Only Mode"),
                           p("Saving is disabled because program is in run only mode.")
                         )),
        conditionalPanel(condition = "input.modelAction != 'runModel'",
                         wellPanel(
                           h4("Save Model Edits"),
                           p(
                             "Pressing the ",
                             strong("Save Model"),
                             " button will save the save the edited model,
                             overwriting the Concepts and Relations files and
                             updating the status file. The notes entered into
                             the notes text area will overwrite any notes since
                             the last save in this session."
                           ),
                           textareaInput("modelNotes", "Model Notes"),
                           actionButton("saveModel", "Save Model")
                         ))
      )
    ),

    #Create Scenarios Screen
    #-----------------------
    navbarMenu(
      "2) Create Scenarios",
      tabPanel("Select Scenario",
               sidebarLayout(
                 sidebarPanel(
                   br(),
                   radioButtons(
                     inputId = "scenarioAction",
                     label = "Scenario Action",
                     choices = list(
                       "Create New Scenario From Scratch" = "newScenario",
                       "Create New Scenario From Copy" = "copyScenario",
                       "Edit Existing Scenario" = "editScenario"
                     )
                   ),
                   conditionalPanel(condition = "input.scenarioAction == 'newScenario' ||
                                    input.scenarioAction == 'copyScenario'",
                                    textInput("scenarioName", "Scenario Name", "")),
                   bsAlert("noscenarioAlert"),
                   conditionalPanel(condition = "input.scenarioAction == 'copyScenario' ||
                                    input.scenarioAction == 'editScenario'",
                                    uiOutput("selectScenarioFile")),
                   actionButton("startScenario", "Start Working on Scenario")
                 ),
                 mainPanel(
                   h4("Scenario Name: ", textOutput("scenarioName", inline = TRUE)),
                   h4("Parent Scenario: ", textOutput("scenarioParent", inline = TRUE)),
                   h4("Model Name: ", textOutput("scenarioModelName", inline = TRUE)),
                   h4("Created: ", textOutput("scenarioCreated", inline = TRUE)),
                   h4("Last Edited: ", textOutput("scenarioEdited", inline = TRUE)),
                   h4("Validated: ", textOutput("scenarioValidated", inline = TRUE))
                 )
               )),
      tabPanel(
        "Edit Scenario Values",
        sidebarLayout(
          sidebarPanel(
            h4("Set Number of Change Increments"),
            h4("Number of Increments: ", textOutput("scenarioincrements", inline = TRUE)),
            numericInput("increments", "Change Increments To:",
                         value = 10,
                         min = 1, max = 100, step = 1),
            br(),
            hr(),
            h4("Edit Concept Values"),
            h4("Concept: ", textOutput("scenarioConcept", inline = TRUE)),
            textInput("conceptStartValue", "Concept Starting Value"),
            textInput("conceptStartChange", "Concept Ending Value"),
            textareaInput("conceptValuesDescription", "Concept Values Description"),
            actionButton("updateScenario", "Update"),
            actionButton("undoScenarioAction", "Undo"),
            actionButton("validateScenario", "Validate and Save")
          ),
          mainPanel(DT::dataTableOutput("scenarioTable"),
                    value = "table")
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
          h4("Growth Rate"),
          uiOutput("selectGrowthRateType"),
          hr(),
          p(
            "Pressing the ",
            strong("Run Model"),
            " button will run the model for all the scenarios that are checked
            in the list above. Outputs will be saved in the respective scenario
            directories."
          ),
          actionButton("runModel", "Run Model"),
          # actionButton("resetRun", "Reset"),
          actionButton("revalidate", "Revalidate Scenarios")
        ),
        mainPanel()
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
          bsAlert("noAnalysisNameAlert")
        ),
        mainPanel(
          # plotOutput("resultsPlot")
          plotlyOutput("resultsPlot")
          )
      )
    )

  )
)
