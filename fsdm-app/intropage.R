# Front page intro 
fluidPage(
  titlePanel(span("Introduction", img(src="OSA_Logo2.png", height=60, width=85, align="right"))),
  br(),
  h3("What is the Logic Laboratory", style = "color: blue"),
  p("The Logic Laboratory is an application for building and running fuzzy 
    systems dynamics models (FSDM). FSDMs are variants of fuzzy cognitive maps 
    (FCM). A FSDM is represented as a directed graph whose ", 
    strong("nodes are concepts"), 
    " that are being modeled and whose ", 
    strong("edges specify relationships between concepts"), 
    ". The following figure illustrates a simple FSDM."), 
    img(src = "simple_fsdm.png", height = 200, width = 275, 
        style = "display: block; margin-left: auto; margin-right: auto"),
  p("The direction of each edge (i.e. the direction of the arrow) specifies the 
    relationship between causal and affected concepts. In the example, the ", 
    strong("Proximity"), " concept is shown as affecting the ", 
    strong("Auto Trip Distance"), " concept. Edge weights specify the strength 
    and directionality of causal effects. A positive sign for an edge weight 
    means that an increase in the causing concept causes an increase in the 
    affected concept. In the example, an increase in the ", 
    strong("Auto Trip Distance"), " concept causes a decrease in the ", 
    strong("Auto Trip Rate"), " concept and an increase in the ", strong("VMT"), 
    " concept. In the FSDM, as in the FCM, the strength of causal effects is 
    expressed in ‘fuzzy’ terms. In the example, the weights are L (low), 
    M (moderate), and H (high), as opposed to specific numeric values."), 
  p("At its core, FSDM involves defining the concepts and edges. Each concept 
    has a name, a description, and a range of possible values. Each relation 
    defined by a causal concept and an affected concept, the sign of the 
    relationship (+ or -), and the strength of the relationship. One or many 
    scenarios may be run on a FSDM. Each scenario is defined by the starting 
    values assigned to every concept and by the final values assigned to one or 
    more of the concepts. When the FSDM is run, the concepts that have assigned 
    final values have their values incremented in small steps between the 
    starting and final values. With each increment, the orders of causal effects 
    are calculated until every causal effect is accounted for. For example, if 
    the ", strong("Proximity"), " concept in the example FSDM above is being 
    incremented, then with each increment the following are calculated:"),
  tags$ol(
    tags$li("The effect of a change in ", strong("Proximity"), " on ", 
            strong("Auto Trip Distance"), " is calculated."),
    tags$li("The effects of a change in ", strong("Auto Trip Distance"), " on ", 
            strong("Auto Trip Rate"), " and ", strong("VMT"), " are calculated."),
    tags$li("The effect of a change in ", strong("Auto Trip Rate"), " on ", 
            strong("VMT"), " is calculated.")),
  p("If the FSDM includes one or more cycles (i.e. feedback loops), these 
    calculations are repeated until the changes in concept values with each 
    repeated set of calculations is very small or until a specified maximum 
    number of repeat calculations is reached."),
  hr(),
  h3("How to Build and Run a FSDM", style = "color: blue"),
  p("The Logic Laboratory interface will guide you through the process of 
    1) building a FSDM model, 2) creating scenarios to model with the FSDM, 
    3) running the FSDM on the scenarios, and 4) analyzing the model results. 
    The four numbered 'tabs' at the top of the page correspond to these tasks. 
    Clicking on a 'tab' changes the interface to guide you through the process 
    of carrying out the task. Following are summaries of what is done in each task:"),
  tags$ol(
    tags$li(strong("Build a Model:"), " Building a FSDM model involves five steps that 
            are listed as menu items: User Info, Edit Concepts, 
            Edit Relations, Save Model. The first step, entering user 
            information is necessary in order to attribute models and model 
            edits.  The second step, selecting a model, shows a view which 
            enables you to start a model from scratch, copy an existing model to
            serve as the starting point for your model, edit an existing model, 
            or run an existing model without editing. The third step, editing 
            concepts, is where you define new concepts or edit existing concepts 
            and the range of values that they may have. The fourth step, editing
            relations, is where you define the relationships between concepts 
            (magnitude and direction). The final step, saving the model, 
            permanently saves all of the model edits along with notes to 
            document the model or edits to the model."),
    tags$li(strong("Create Scenarios:"), " One or more model scenarios can be 
            created for a FSDM. This 'tab' of the application assists you with 
            creating single or multiple scenarios, and with editing existing 
            scenarios. A new scenario can be started from scratch or created 
            from an existing scenario. Scenarios are validated against the 
            defined model to assure that scenario values are consistent with 
            defined concept value ranges."),
    tags$li(strong("Run the Model:"), " The FSDM may be run for one or more of 
            the scenarios. This 'tab' enables you to specify which of the 
            scenarios to run. The interface shows a checkbox list of all of the 
            scenarios that have been created for a model. Entering a check in a 
            checkbox marks the corresponding scenario to be run. A button starts
            the model runs. Progress of the model run is shown as the model run 
            proceeds."),
    tags$li(strong("Analyze Results:"), " This 'tab' of the application enables 
            users to view the results for one or two scenarios. The user can 
            choose which model scenarios and which concept values to display. 
            The results are displayed in graphs. Users can choose to save the 
            displayed graphs and corresponding data.")),
  hr(),
  h3("Copyright and License", style = "color: blue"),
  p("The Logic Laboratory was developed by Brian Gregor (Oregon Systems 
    Analytics) with funding from the Oregon Department of Transportation. It is 
    licensed with the Apache 2 open source license. Terms of the license are 
    included in the LICENSE text file. Additional notices are included in the 
    NOTICE text file."),
  h4("Copyright 2016 Oregon Department of Transportation"),
  h4("License Apache 2")
)