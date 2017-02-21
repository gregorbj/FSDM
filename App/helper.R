#helper.R
#Author: Brian Gregor, Oregon Systems Analytics LLC
#Copyright: 2016, Oregon Department of Transportation 2016
#License: Apache 2


############################################
#------------------------------------------#
# INITIALIZING, LOADING, AND SAVING MODELS #
#------------------------------------------#
############################################

#----------------------
#Initialize a New Model
#----------------------
#' Initialize a new model.
#'
#' \code{initializeNewModel} initializes a new model by creating a directory
#' with the model name. Creates and saves a model status list.
#'
#' This function initializes a new model with the given model name. It does this
#' by creating a directory with the model name and a list to store the model
#' status. This list is saved in the model directory in JSON format and is
#' also returned by the function. The status list contains the name of the 
#' model, the parent (none), the date and time is was created, and the date
#' and time it was last edited (same as creation time).
#'
#' @param ModelName a string representation of the model name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
initializeNewModel <- function(ModelName, Author) {
  #Create directory for model
  NewDir <- file.path("../models", ModelName)
  dir.create(NewDir)
  #Create and save a status list
  Attribution <- 
    paste0("Model: ", ModelName, "   Author: ", Author, "   Created: ", as.character(Sys.time()))
  status_ls <- list(name = ModelName,
                    parent = "none",
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()),
                    attribution = Attribution,
                    notes = character(0))
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  #Copy and save the concept and relations template files
  CopyDir <- "../models/templates"
  FilesToCopy_ <- file.path(
    CopyDir, c("concepts.json", "relations.json")
    )
  file.copy(FilesToCopy_, NewDir, recursive = TRUE)
  #Create scenarios directory if does not exist
  ScenarioPath <- file.path(NewDir, "scenarios")
  if (!dir.exists(ScenarioPath)) {
    dir.create(ScenarioPath)
  }
  #Create analysis directory if does not exist
  AnalysisPath <- file.path(NewDir, "analysis")
  if (!dir.exists(AnalysisPath)) {
    dir.create(AnalysisPath)
  }
  #Return the status list  
  status_ls
}

#---------------------------------------------------
#Initialize a New Model by Copying an Existing Model
#---------------------------------------------------
#' Initialize a new model by copying an existing model.
#'
#' \code{initializeCopyModel} initializes a new model by creating a directory
#' with the model name and copying the contents of an existing model into that
#' directory. Creates and saves a model status list.
#'
#' This function initializes a new model with the given model name from an
#' existing model. It does this by creating a directory with the model name and
#' copying the model files for an existing model into it. It creates a new
#' model status list which identifies the name of the new model and the name
#' of the parent model it is a copy of. The status list also identifies the 
#' date and time is was created. The function can be used to copy only the 
#' model or all the scenarios as well as the model.
#'
#' @param ModelName a string representation of the model name.
#' @param CopyModelName a string representation of the name of the model to copy.
#' @param CopyScenarios a logical to determine whether to copy the model 
#' scenarios.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
initializeCopyModel <- function(ModelName, CopyModelName, Author, CopyScenarios = FALSE) {
  NewDir <- file.path("../models", ModelName)
  dir.create(NewDir)
  CopyDir <- file.path("../models", CopyModelName)
  if(CopyScenarios) {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json", "scenarios")
    )
    file.copy(FilesToCopy_, NewDir, recursive = TRUE)
    Sc <- dir(file.path(NewDir, "scenarios"))
    for (sc in Sc) {
      ScenDir <- file.path(NewDir, "scenarios", sc)
      status_ls <- fromJSON(paste0(ScenDir, "/status.json"))
      status_ls$model <- ModelName
      writeLines(toJSON(status_ls), file.path(ScenDir, "status.json"))
    }
  } else {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json")
    )
    file.copy(FilesToCopy_, NewDir)
    #Create scenarios directory if does not exist
    ScenarioPath <- file.path(NewDir, "scenarios")
    if (!dir.exists(ScenarioPath)) {
      dir.create(ScenarioPath)
    }
  }
  #Create analysis directory if does not exist
  AnalysisPath <- file.path(NewDir, "analysis")
  if (!dir.exists(AnalysisPath)) {
    dir.create(AnalysisPath)
  }
  #Copy and edit the model status file
  status_ls <- as.list(fromJSON(file.path(NewDir, "status.json")))
  Attribution <- 
    paste0("Model: ", ModelName, "   Author: ", Author, "   Copy Created: ", as.character(Sys.time()))
  status_ls$name <- ModelName
  status_ls$parent <- CopyModelName
  status_ls$created <- as.character(Sys.time())
  status_ls$lastedit <- as.character(Sys.time())
  status_ls$attribution <- c(Attribution, status_ls$attribution)
  status_ls$notes <- status_ls$notes
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  status_ls
}

#----------------------
#Load Model Status File
#----------------------
#' Load the model status file for a model.
#'
#' \code{loadModelStatus} reads a model status file and returns a list
#' containing the status information.
#'
#' This function reads the model status JSON file for a specified model and 
#' creates a list containing the model status information.
#'
#' @param ModelName a string representation of the model name.
#' @return a list containing values for name, parent, created, and lastedit.
#' @export
loadModelStatus <- function(ModelName, Author = NULL){
  ModelDir <-  file.path("../models", ModelName)
  status_ls <- as.list(fromJSON(file.path(ModelDir, "status.json")))
  if (!is.null(Author)) {
    Attribution <- 
      paste0("Model: ", ModelName, "   Author: ", Author, "   Edited: ", as.character(Sys.time()))
    status_ls$attribution <- c(Attribution, status_ls$attribution)
    status_ls$notes <- status_ls$notes
  }
  status_ls
}

#------------------------
#Load Model Concepts File
#------------------------
#' Load the concept file for a model.
#'
#' \code{loadModelConcepts} reads the file that contains model concept information
#' and returns a data frame representation.
#'
#' This function reads the model concept file for a specified model and returns
#' a data frame containing the information.
#'
#' @param ModelName a string representation of the model name.
#' @return a data frame containing the model concept information.
#' @export
loadModelConcepts <- function(ModelName){
  ModelDir <-  file.path("../models", ModelName)
  fromJSON(file.path(ModelDir, "concepts.json"))
}

#-------------------------
#Load Model Relations File
#-------------------------
#' Load the relations file for a model.
#'
#' \code{loadModelRelations} reads the file that contains model relations
#' information and returns a data frame representation.
#'
#' This function reads the model relations file for a specified model and 
#' returns a data frame containing the information.
#'
#' @param ModelName a string representation of the model name.
#' @return a data frame containing the model relations information.
#' @export
loadModelRelations <- function(ModelName){
  ModelDir <-  file.path("../models", ModelName)
  fromJSON(file.path(ModelDir, "relations.json"), simplifyDataFrame = FALSE)
}

#-------------------------
#Save All Model Components
#-------------------------
#' Saves all the model components as JSON files.
#'
#' \code{saveModel} saves the model status, model concepts, and model relations
#' as JSON files.
#'
#' Models are composed of 3 objects: a model status list, a model concepts
#' data frame, and a model relations data frame. This function saves these 
#' objects as JSON-formatted files.
#'
#' @param ModelData a model.
#' @return no return value. Has side effect of saving the model status list,
#' model concepts data frame, and model relations data frame as JSON-formatted
#' files.
#' @export
saveModel <- function(ModelData) {
  ModelName <- ModelData$status$name
  ModelDir <- file.path("../models", ModelName)
  writeLines(toJSON(ModelData$status), file.path(ModelDir, "status.json"))
  writeLines(toJSON(ModelData$concepts), file.path(ModelDir, "concepts.json"))
  writeLines(toJSON(ModelData$relations), file.path(ModelDir, "relations.json"))
}


############################################
#------------------------------------------#
#             EDITING MODELS               #
#------------------------------------------#
############################################

#----------------------------------
#Format a Concept Table for Display
#----------------------------------
#' Formats a concept table to be displayed in the GUI.
#'
#' \code{formatConceptTable} formats a concept data frame to be displayed as a
#' table in the GUI.
#'
#' The GUI summarizes information about model concepts in a table. Not all of 
#' the concept data needs to be shown and some of the data is difficult to
#' show in table form. This function extracts and formats the concept data that
#' is to be displayed in a table. 
#'
#' @param Concepts_df a data frame containing the concepts data.
#' @return a data frame containing the concepts data to be shown in a table.
#' @export
formatConceptTable <- function(Concepts_df) {
  data.frame(Name = Concepts_df$name,
             Variable = Concepts_df$variable,
             Minimum = Concepts_df$values$min,
             Maximum = Concepts_df$values$max,
             Group = Concepts_df$group,
             stringsAsFactors = FALSE)
}

#----------------------------------------------
#Make an Adjacency Matrix from a Relations List
#----------------------------------------------
#' Creates an adjacency matrix from an FSDM relations list
#' 
#' \code{makeAdjacencyMatrix} creates an adjacency matrix from an FSDM relations
#' list
#' 
#' This function creates an adjacency matrix from an FSDM relations list. The
#' adjacency matrix is a square matrix with as many rows and columns as the
#' number of concepts in the model. The rows and columns are named with the
#' concept variable names. The rows represent the causal side of the
#' relationship and the columns represent the affected side. The values in the
#' matrix are logicals with TRUE meaning that a relationship exists and FALSE
#' meaning that it does not.
#' 
#' @param Relations_ls a list of FSDM relations
#' @param Type a string identifying the type of matrix to create. "Logical"
#' produces a matrix where the existence of a relationship is noted with TRUE
#' and FALSE. "Values" produces a list of two matrices where one matrix 
#' identifies the causal weitht of each relationship and a second matrix 
#' identifies the causal direction of each relationship. 
#' @return a matrix of logical values or string values
#' @export
makeAdjacencyMatrix <- function(Relations_ls, Type = "Logical") {
  Var_ <- unlist(lapply(Relations_ls, function(x) x$name))
  #If type is "Logical" return a logical matrix
  if (Type == "Logical") {
    Affected_ls <- lapply(Relations_ls, function(x) {
      Affected_ <- unlist(lapply(x$affects, function(y) y$variable))
    })
    names(Affected_ls) <- Var_
    Relations_mx <- 
      array(FALSE, 
            dim = c(length(Var_), length(Var_)),
            dimnames = list(Var_, Var_))
    for (name in names(Affected_ls)) {
      Relations_mx[name, Affected_ls[[name]]] <- TRUE
    }
    return(Relations_mx)
  }
  #If type is "Values" return a list of matrices of direction and values
  if (Type == "Values") {
    #Matrix of effects directions
    Direction_ls <- lapply(Relations_ls, function(x) {
      Direction_ <- unlist(lapply(x$affects, function(y) y$direction))
      names(Direction_) <- unlist(lapply(x$affects, function(y) y$variable))
      Direction_
    })
    names(Direction_ls) <- Var_
    Direction_mx <- 
      array(NA, 
            dim = c(length(Var_), length(Var_)),
            dimnames = list(Var_, Var_))
    for (name in names(Direction_ls)) {
      Direction_mx[name, names(Direction_ls[[name]])] <- Direction_ls[[name]]
    }
    #Matrix of effects weights
    Weight_ls <- lapply(Relations_ls, function(x) {
      Weight_ <- unlist(lapply(x$affects, function(y) y$weight))
      names(Weight_) <- unlist(lapply(x$affects, function(y) y$variable))
      Weight_
    })
    names(Weight_ls) <- Var_
    Weight_mx <- 
      array(NA, 
            dim = c(length(Var_), length(Var_)),
            dimnames = list(Var_, Var_))
    for (name in names(Weight_ls)) {
      Weight_mx[name, names(Weight_ls[[name]])] <- Weight_ls[[name]]
    }
    return(list(Direction = Direction_mx, Weight = Weight_mx))
  }
}

#-----------------------------------------------------------------
#Extract Data on Effect Relationships of a Selected Causal Concept
#-----------------------------------------------------------------
#' Get data on effect relationships of a causal concept
#' 
#' \code{getEffects} extracts data on all the effect relationships of an
#' identified causal concept. 
#' 
#' The function extracts the data on the effect relationships of an identified
#' causal concept. This includes the names of the affected concepts and the 
#' respective effect directions, magnitudes, and descriptions.
#' 
#' @param Model_ls a list that includes components for model concepts and
#' relations
#' @param ConceptName a string containing the name of the causal concept
#' @return a data frame containing the names, directions, magnitudes, and
#' descriptions of all the effects or NULL if there are no effects.
#' @export
getEffects <- function(Model_ls, ConceptName) {
  #Extract all the concept names and variable names
  Nn <- Model_ls$concepts$name
  Nv <- Model_ls$concepts$variable
  #Functions to translate from name to variable name and vice versa
  nameToVar <- 
    function(Names_) {
      Nv[match(Names_, Nn)]
    }
  varToName <-
    function(Vars_) {
      Nn[match(Vars_, Nv)]
    }
  #Extract the effects portion of the relations list for the concept
  VarName <- ConceptName
  #VarName <- nameToVar(ConceptName)
  VarNames_ <- unlist(lapply(Model_ls$relations, function(x) x$name))
  Idx <- which(VarNames_ == VarName)
  SelectEffects_ls <- Model_ls$relations[[Idx]]$affects
  #Prepare results
  if (length(SelectEffects_ls) == 0) {
    Result <- NULL
  } else {
    Result <- 
      data.frame(
      variable = unlist(lapply(SelectEffects_ls, function(x) x$variable)),
      direction = unlist(lapply(SelectEffects_ls, function(x) x$direction)),
      weight = unlist(lapply(SelectEffects_ls, function(x) x$weight)),
      description = unlist(lapply(SelectEffects_ls, function(x) x$description))
    )
    Result$name <- varToName(Result$variable)
  }
  #Return the result
  Result
}

#------------------------------
#Initialize New Relations Entry
#------------------------------
#' Initialize a initial relations entry for a new concept
#' 
#' \code{initRelationsEntry} creates a initial relations entry for concept
#' that is being added to the model
#' 
#' This function creates an initial relations entry data for a concept. This
#' entry data has the concept variable name and empty fields for all other data
#' items. The server script calls this function when a concept is created and
#' adds the resulting entry to the relations table.
#' 
#' @param VarName a string representation of the concept variable name
#' @return a data frame which includes all the mandatory relations fields with
#' the name field populated with the concept variable name and all other fields
#' populated with empty fields
#' @export
initRelationsEntry <- function(VarName){
  Lst <- 
    list(name = VarName,
         affects = list("")
    )
  Lst$affects[[1]] <- data.frame(
    variable = "",
    direction = "",
    weight = "",
    description = ""
  )
  Lst
}

#------------------
#Plot Relationships
#------------------
#' Plot relationships
#' 
#' \code{mapRelations} displays a plot of concept relations, highlighting the
#' relations from or to a highlighted concept.
#' 
#' The function maps relationships between concepts by plotting concepts in
#' two parallel vertical lines and then showing relationships between concepts
#' by drawing lines between the related concepts. The selected concept and
#' related concepts are placed at the top of the plot and the lines are 
#' highlighted.
#' 
#' @param Model_ls a list that includes components for model concepts and 
#' relations
#' @param FromConcept the name of a selected causal concept or NULL
#' @param ToConcept the name of a selected receiving concept or NULL
#' @param FromGroup the name of the group of the causal concepts to display or
#' NULL
#' @param ToGroup the name of the group of the receiving concepts to display or
#' NULL
#' @return a logical value identifying whether the plot can be created
#' @export
Model_ls <- list()
Model_ls$concepts <- loadModelConcepts("Gopher")
Model_ls$relations <- loadModelRelations("Gopher")
FromConcept = NULL
FromGroup = "All"
ToGroup = "All"
mapRelations <- 
  function(Model_ls, FromConcept = NULL, FromGroup = "All", ToGroup = "All") {
    #Extract all the concept names, variable names, and group names
    Nn <- Model_ls$concepts$name
    Nv <- Model_ls$concepts$variable
    Ng <- Model_ls$concepts$group
    #Functions to translate from name to variable name and vice versa
    nameToVar <- 
      function(Names_) {
        Nv[match(Names_, Nn)]
      }
    varToName <-
      function(Vars_) {
        Nn[match(Vars_, Nv)]
      }
    #Check whether function arguments are proper
    # if (!is.null(FromConcept)) {
    #   FromConcept <- nameToVar(FromConcept)
    # }
    #Make a matrix of relations
    Relations_mx <- makeAdjacencyMatrix(Model_ls$relations, Type = "Values")$Direction[Nv,Nv]
    #Function to select portion of matrix and return a matrix regardless of how
    #many rows and columns are selected
    selectMatrix <- function(Matrix, RowSelect, ColSelect) {
      NRow <- length(RowSelect)
      NCol <- length(ColSelect)
      if (NRow == 1 & NCol > 1) {
        Result <- 
          matrix(Matrix[RowSelect, ColSelect], 
                 byrow = TRUE, 
                 nrow = 1)
        rownames(Result) <- RowSelect
        colnames(Result) <- ColSelect
      }
      if (NCol == 1 & NRow > 1) {
        Result <- 
          matrix(Matrix[RowSelect, ColSelect],
                 ncol = 1)
        colnames(Result) <- ColSelect
        rownames(Result) <- RowSelect
      }
      if (NRow == 1 & NCol == 1){
        Result <- 
          matrix(Matrix[RowSelect, ColSelect],
                 nrow = 1)
        rownames(Result) <- RowSelect
        colnames(Result) <- ColSelect
      }
      if (NRow > 1 & NCol > 1) {
        Result <- Matrix[RowSelect, ColSelect]
      }
      Result
    }
    #Select portions of relations matrix corresponding to FromGroup and ToGroup
    if (FromGroup == "All") RowSelect <- Nv else RowSelect <- Nv[Ng == FromGroup]
    if (ToGroup == "All") ColSelect <- Nv else ColSelect <- Nv[Ng == ToGroup]
    Selected_mx <- selectMatrix(Relations_mx, RowSelect, ColSelect)
    #Order rows and columns according to FromConcept
    if (!is.null(FromConcept)) {
      if (FromConcept %in% rownames(Selected_mx)) {
        RowSelect <- c(FromConcept, RowSelect[RowSelect != FromConcept])
        if (sum(!is.na(Selected_mx[FromConcept,])) != 0) {
          ColSelect <- 
            c(
              ColSelect[which(!is.na(Selected_mx[FromConcept,]))],
              ColSelect[-which(!is.na(Selected_mx[FromConcept,]))]
            )
        }
        if (FromConcept %in% rownames(Selected_mx)) {
          NumHighlighted <- sum(!is.na(Selected_mx[FromConcept,]))
        } else {
          NumHighlighted <- 0
        }
        Selected_mx <- selectMatrix(Selected_mx, RowSelect, ColSelect)
      }
    }
    #Return list of plot parameters
    MaxVal <- length(Nv)
    YLim_ <- c(0, 1.1 * MaxVal)
    XLim_ <- c(-3,11)
    YVals1_ <- (nrow(Selected_mx):1 + MaxVal - nrow(Selected_mx)) * 1
    YVals2_ <- (ncol(Selected_mx):1 + MaxVal - ncol(Selected_mx)) * 1
    XVals_ <- c(rep(2, length(YVals1_)), rep(6, length(YVals2_)))
    Y0_ <- rep(YVals1_, apply(!is.na(Selected_mx), 1, sum))
    X0_ <- rep(2, length(Y0_))
    Y1_ <- rep(YVals2_, nrow(Selected_mx))[as.vector(t(!is.na(Selected_mx)))]
    X1_ <- rep(6,length(Y1_))
    if (exists("NumHighlighted")) {
      Col_ <- as.vector(t(Selected_mx))
      Col_ <- Col_[!is.na(Col_)]
      Col_[Col_ == "Negative"] <- "red"
      Col_[Col_ == "Positive"] <- "black"
      Lwd_ <-
        c(rep(3, NumHighlighted), rep(1, length(X0_) - NumHighlighted))
      Lty_ <- as.vector(t(Selected_mx))
      Lty_ <- Lty_[!is.na(Lty_)]
      Lty_[Lty_ == "Negative"] <- "2"
      Lty_[Lty_ == "Positive"] <- "1"
      Lty_ <- as.numeric(Lty_)
    } else {
      Col_ <- rep("grey", length(X0_))
      Lwd_ <- rep(1, length(X0_))
      Lty_ <- rep(1, length(X0_))
    }
    Labels1_ <- varToName(rownames(Selected_mx))
    Labels2_ <- varToName(colnames(Selected_mx))
    Map <- list(
      XVals = XVals_,
      YVals1 = YVals1_,
      YVals2 = YVals2_,
      XLim = XLim_,
      YLim = YLim_,
      Col = Col_,
      Lwd = Lwd_,
      Lty = Lty_,
      Labels1 = Labels1_,
      Labels2 = Labels2_,
      X0 = X0_,
      Y0 = Y0_,
      X1 = X1_,
      Y1 = Y1_,
      TitlePosY = MaxVal * 1.1
    )
    Map
  }    

#---------------------------------------------------------------
#Define function to create a dot file for plotting with GraphViz
#---------------------------------------------------------------
#' Create a DOT file for GraphViz
#'
#' \code{makeDotFile} create and save a dot file to be displayed by GraphViz
#'
#' This function writes out a dot file to be rendered using GraphViz.
#'
#' @param Relations_ls a list of model relations.
#' @param Concepts_df a data frame with model concepts.
#' @param RowGroup a string identifying the names of the name of the group that
#' selects rows from the relationship table to plot. The default 'All' selects
#' all the rows.
#' @param ColGroup a string identifying the name of the group that selects
#' columns from the relationship table to plot. The default 'All' selects all
#' the columns.
#' @param orientation a string identifying the GraphViz layout orientation
#' ('Portrait' or 'Landscape').
#' @param rankdir a string identifying the graph orientation:
#' 'TB' for top to bottom, 'LR' for left to right.
#' @param shape a string identifying the shape of the graph nodes (e.g. 'box').
#' @param Show a string identifying how to label the edges. The default value
#' "label" results in showing the fuzzy label (e.g. VL, L, M, H, VH). The
#' alternative, "value", results in showing the equivalent numeric value.
#' @return A string specification of a DOT.
#' @export
makeDot <-
  function(Relations_ls, Concepts_df, RowGroup = "All", ColGroup = "All",
           orientation = "Portrait", rankdir = "Top-to-Bottom", shape = "box",
           Show = "label")
  {
    #Make matrices of relations and labels
    Relates_ls <- makeAdjacencyMatrix(Relations_ls, Type = "Values")
    Cn <- Concepts_df$variable
    Vals <- c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675,
             H = 0.75, VH = 0.95)
    Signs <- c(Positive = 1, Negative = -1)    
    Relates.CnCn <- 
      apply(Relates_ls$Weight[Cn,Cn], 2, function(x) Vals[x]) *
      apply(Relates_ls$Direction[Cn,Cn], 2, function(x) Signs[x])
    rownames(Relates.CnCn) <- Cn
    Labels.CnCn <- Relates_ls$Weight[Cn,Cn]
    #Create row and column indices for selected row and column groups
    if (RowGroup == "All") {
      Cr <- Cn
    } else {
      Cr <- Cn[Concepts_df$group %in% RowGroup]
    }
    if (ColGroup == "All") {
      Cc <- Cn
    } else {
      Cc <- Cn[Concepts_df$group %in% ColGroup]
    }
    #Select relations and labels matrices for selected rows and columns
    Relates.CrCc <- Relates.CnCn[Cr,Cc]
    Labels.CrCc <- Labels.CnCn[Cr,Cc]
    Concepts. <- unique(Cr)
    #Remove rows and columns that are all NA values
    # AllNARows_ <- apply(Relates.CrCc, 1, function(x) all(is.na(x)))
    # AllNACols_ <- apply(Relates.CrCc, 2, function(x) all(is.na(x)))
    # Relates.CrCc <- Relates.CrCc[!AllNARows_, !AllNACols_]
    # Labels.CrCc <- Labels.CrCc[!AllNARows_, !AllNACols_]
    #Update Cr and Cc and identify unique concepts
    Cr <- rownames(Relates.CrCc)
    Cc <- colnames(Relates.CrCc)
    #Convert rankdir argument
    if (rankdir == "Top-to-Bottom") rankdir <- "TB"
    if (rankdir == "Left-to-Right") rankdir <- "LR"
    #Make DOT data
    Dot_ <-
      paste("digraph {\n orientation =", orientation, ";\n rankdir =", rankdir, ";\n")
    for (concept in Concepts.) {
      Dot_ <- paste(Dot_, concept, "[ shape =", shape, "];\n")
    }
    for (cr in Cr) {
      for (cc in Cc) {
        Value <- Relates.CrCc[cr,cc]
        if (!is.na(Value)) {
          if (Show == "label") {
            Label <- Labels.CrCc[cr,cc]
          } else {
            Label <- Value
          }
          if (Value != 0) {
            if (Value > 0) {
              Dot_ <- paste0(Dot_, cr, " -> ", cc, "[ label=", Label, " ];\n")
            } else {
              Dot_ <-
                paste0(
                  Dot_, cr, " -> ", cc, "[ color=red, fontcolor=red, style=dashed, label=", Label, " ];\n"
                )
            }
          }
        }
      }
    }
    Dot_ <- paste(Dot_, "}")
    #Return the resulting DOT description
    Dot_
  }


###############################################
#---------------------------------------------#
# INITIALIZING, LOADING, AND SAVING SCENARIOS #
#---------------------------------------------#
###############################################

#--------------------------
#Initialize an New Scenario
#--------------------------
#' Initialize a new scenario.
#'
#' \code{initializeNewScenario} initializes a new scenario by creating a 
#' directory with the scenario name and returns a list containing scenario
#' status and scenario values components.
#'
#' This function initializes a new scenario with the given scenario name. It 
#' does this by creating a directory with the scenario name and a data frame
#' to store the scenario data. This list is returned and the components are
#' used in the scenario reactive object. The components are also saved in the
#' scenario directory in JSON format.
#'
#' @param ModelName a string representation of the model name.
#' @param ScenarioName a string representation of the scenario name.
#' @param Concepts_df a data frame containing the concept data.
#' @return a list containing a status list and a scenario values data frame.
#' @export
initializeNewScenario <- function(ModelName, ScenarioName, Concepts_df) {
  #Create directory for scenario
  NewDir <- paste0("../models/", ModelName, "/scenarios/", ScenarioName)
  dir.create(NewDir)
  #Create and save a status list
  status_ls <- list(name = ScenarioName,
                    model = ModelName,
                    parent = "none",
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()),
                    validated = ""
                    )
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  #Create and save a scenario values data frame
  Concepts_ <- Concepts_df$variable
  values_df <-
    data.frame(name = Concepts_,
               startvalue = rep("NA", length(Concepts_)),
               startchange = rep("NA", length(Concepts_)),
               description = rep("", length(Concepts_)),
               stringsAsFactors = FALSE
               )
  writeLines(toJSON(values_df), file.path(NewDir, "scenario.json"))
  #Return the list of status and values
  list(status = status_ls,
       values = values_df)
}


#---------------------------------------------------------
#Modify an Existing Scenario to Conform with Model Changes
#---------------------------------------------------------
#' Modify a scenario to conform with a changed model
#' 
#' \code{conformScenario} modifies an existing scenario definition so that it
#' conforms to the current model definition.
#' 
#' This function modifies a scenario definition so that it conforms to the
#' the current model definition. It does this my removing entries for concepts
#' that are not part of the model and by adding "blank" entries for concepts
#' that are in the model but not in the scenario. The function is designed to be
#' called by the 'initializeCopyScenario' function and the 'loadScenario'
#' function. Note that the function does not check whether the values for 
#' concepts are consistent with the concept definition. The user needs to check
#' consistency of values and validation will check for inconsistencies as well.
#' 
#' @param ConceptVars_ a string vector of the variable names of model concepts
#' @param ScenarioValues_df a data frame of scenario concept values
#' @return a data frame of scenario concept values that is consistent with the
#' model concepts
#' @export
conformScenario <- function(ConceptVars_, ScenarioValues_df){
  #Identify scenario concepts in model
  IsConceptInModel <- ScenarioValues_df$name %in% ConceptVars_
  #Identify model concepts not in scenario
  MissingConcepts_ <- 
    ConceptVars_[!(ConceptVars_ %in% ScenarioValues_df$name)]
  #If any concepts are missing from the scenario, add blank entries
  if (length(MissingConcepts_) != 0) {
    AddValues_df <- 
      data.frame(
        name = MissingConcepts_,
        startvalue = rep("NA", length(MissingConcepts_)),
        startchange = rep("NA", length(MissingConcepts_)),
        description = rep("", length(MissingConcepts_)),
        stringsAsFactors = FALSE
      )
    Result_df <- 
      rbind(
        ScenarioValues_df[IsConceptInModel,], 
        AddValues_df
        )
  } else {
    Result_df <- ScenarioValues_df[IsConceptInModel,]
  }
  #Return the modified scenario data frame
  Result_df
}


#---------------------------------------------------------
#Initialize a New Scenario by Copying an Existing Scenario
#---------------------------------------------------------
#' Initialize a new scenario by copying an existing scenario.
#'
#' \code{initializeCopyScenario} initializes a new scenario by creating a 
#' directory with the scenario name and copying the contents of an existing 
#' scenario into that directory. Creates and saves a scenario status list.
#'
#' This function initializes a new scenario with the given scenario name from an
#' existing scenario. It does this by creating a directory with the scenario 
#' name and copying the scenario files for an existing scenario into it. It 
#' creates a new scenario status list which identifies the name of the new 
#' scenario and the name of the parent scenario it is a copy of.
#'
#' @param ModelName a string representation of the model name.
#' @param ConceptVars_ a string vector of the model concept variable names.
#' @param ScenarioName a string representation of the new scenario name.
#' @param CopyScenarioName a string representation of the name of the scenario
#' to copy.
#' @return a list containing a status list and a scenario values data frame.
#' @export
initializeCopyScenario <- 
  function(ModelName, ConceptVars_, ScenarioName, CopyScenarioName) {
  #Create directory for scenario
  NewDir <- file.path("../models", ModelName, "scenarios", ScenarioName)
  dir.create(NewDir)
  #Load the scenario values file to be copied and make conform to model
  CopyFromDir <- file.path("../models", ModelName, "scenarios", CopyScenarioName)
  CopyValues_df <- fromJSON(file.path(CopyFromDir, "scenario.json"))
  values_df <- conformScenario(ConceptVars_, CopyValues_df)
  #Save the scenario values
  writeLines(toJSON(values_df), file.path(NewDir, "scenario.json"))
  #Create and save the status list
  status_ls <- list(name = ScenarioName,
                    model = ModelName,
                    parent = CopyScenarioName,
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()),
                    validated = ""
  )
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  #Return the list of status and values
  list(status = status_ls,
       values = values_df)
}

#-------------------------
#Load an Existing Scenario
#-------------------------
#' Loads the files for a scenario.
#'
#' \code{loadScenario} reads the files that contain scenario information
#' and returns a list containing scenario status and values information.
#'
#' This function reads the scenario status file and scenario file for a 
#' specified scenario and returns a list whose components are a list containing
#' the scenario status information and a data frame containing the scenario
#' values information.
#'
#' @param ModelName a string representation of the model name.
#' @param ConceptVars_ a string vector of the model concept variable names.
#' @param ScenarioName a string representation of the scenario name.
#' @return a list containing a status list and a scenario values data frame.
#' @export
loadScenario <- function(ModelName, ConceptVars_, ScenarioFileName){
  #Identify the scenario directory
  Dir <- paste0("../models/", ModelName, "/scenarios/", ScenarioFileName)
  #Load the scenario values file to be copied and make conform to model
  CopyValues_df <- fromJSON(file.path(Dir, "scenario.json"))
  values_df <- conformScenario(ConceptVars_, CopyValues_df)
  #Load the scenario status
  status_ls <- fromJSON(paste0(Dir, "/status.json"))
  #Return list of the scenario status and values
  list(status = status_ls,
       values = values_df)
}

#-------------
#Save Scenario
#-------------
#' Saves all the scenario components as JSON files.
#'
#' \code{saveScenario} saves the scenario status and values as JSON files.
#'
#' This function saves the scenario status and values as JSON files in the
#' scenario directory for the model. The status information is saved in the 
#' status.json file and the scenario values are saved in the scenarios.json
#' file.
#'
#' @param ScenarioData a list having status and values components.
#' @return no return value. Has side effect of saving the scenario status list
#' and values data frame.
#' @export
saveScenario <- function(ScenarioData) {
  ModelName <- ScenarioData$status$model
  ScenarioName <- ScenarioData$status$name
  ScenarioDir <- file.path("../models", ModelName, "scenarios", ScenarioName)
  writeLines(toJSON(ScenarioData$status), file.path(ScenarioDir, "status.json"))
  writeLines(toJSON(ScenarioData$values), file.path(ScenarioDir, "scenario.json"))
}

#-----------------
#Validate Scenario
#-----------------
#' Validates scenario with model.
#' 
#' \code{validateScenario} validates the scenario with the model.
#' 
#' This function compares the values for the scenario with the model and
#' determines whether the scenario has values for all concepts, whether the
#' values are consistent with the value ranges for the concepts, and whether
#' the starting changes are consistent with the ranges and don't vary too many.
#' 
#' @param Values_df a data frame containing the scenario values
#' @param Concepts_df a data frame containing the model concepts
#' @return a list having three components, a logical identifying whether the
#' scenario validates, a list of all validation errors, and a character string
#' with the time stamp.
#' @export
validateScenario <- function(Values_df, Concepts_df) {
  Cn <- Concepts_df$variable
  #Convert Values_df to analyze
  Values_mx <- as.matrix(Values_df[, c("startvalue", "startchange")])
  Values_mx[Values_mx == "NA"] <- NA
  Values_mx <- apply(Values_mx, 2, function(x) as.numeric(x))
  rownames(Values_mx) <- Values_df$name
  #Extract the value range for all concepts
  ValRng_df <- Concepts_df$values[,c("min","max")]
  ValRng_df$min <- as.numeric(ValRng_df$min)
  ValRng_df$max <- as.numeric(ValRng_df$max)
  rownames(ValRng_df) <- Cn
  #Initialize errors
  HasErrors <- FALSE
  Errors_ <- c("Scenario is not valid for the following reasons:")
  #Check that all rownames correspond to concept names
  if (!setequal(Values_df$name, Concepts_df$variable)) {
    ErrMsg <- "The variable names for the scenario don't all correspond to the concepts variable names in the model."
    Errors_ <- c(Errors_, "\n", ErrMsg)
    HasErrors <- TRUE
  }
  #Check that there are numeric values for all startvalue
  if (any(is.na(Values_mx[, "startvalue"]))) {
    ErrMsg <- "One or more values for 'startvalue' are NA. Numeric entries are required for all values."
    Errors_ <- c(Errors_, "\n", ErrMsg)
    HasErrors <- TRUE
  }
  #Check that the values for StartValue are within range
  for (cn in Cn) {
    StartVal <- as.numeric(Values_mx[cn, "startvalue"])
    MinVal <- as.numeric(ValRng_df[cn,"min"])
    MaxVal <- as.numeric(ValRng_df[cn,"max"])
    if (!is.na(StartVal)) {
      if ((StartVal < MinVal) | (StartVal > MaxVal)) {
        ErrMsg <- paste("'startvalue' value for", cn, "is outside the range of acceptable values.")
        Errors_ <- c(Errors_, "\n", ErrMsg)
        HasErrors <- TRUE
      }
    }
  }
  #Check whether there is at least one startchange that is not NA
  if (all(is.na(Values_df$startchange))) {
    ErrMsg <- "All values for 'startchange' are NA. At least one must be a number."
    Errors_ <- c(Errors_, "\n", ErrMsg)
    HasErrors <- TRUE
  }
  #Check whether all the startchange values are within range
  for (cn in Cn) {
    ChangeVal <- as.numeric(Values_mx[cn, "startchange"])
    MinVal <- as.numeric(ValRng_df[cn,"min"])
    MaxVal <- as.numeric(ValRng_df[cn,"max"])
    if (!is.na(ChangeVal)) {
      if ((ChangeVal < MinVal) | (ChangeVal > MaxVal)) {
        ErrMsg <- paste("'startchange' value for", cn, "is outside the range of acceptable values.")
        Errors_ <- c(Errors_, "\n", ErrMsg)
        HasErrors <- TRUE
      }
    }
  }
  list(
    Valid = !HasErrors,
    Errors = Errors_,
    TimeStamp = as.character(Sys.time())
  )
}


#--------------
#List Scenarios
#--------------
#' Creates a list of scenarios and whether validated.
#' 
#' \code{listScenarios} creates a list of scenarios and identifies which have
#' been validated.
#' 
#' This function creates a list of scenarios for a model and checks whether each
#' scenario has been validated. Scenarios are identified as being validated if
#' they were successfully validated and if the validation time stamp is later
#' than the 'lastedit' time stamp for the model and the 'lastedit' time stamp
#' for the scenario. The returned list has two components. The first is a vector
#' containing the names that have been successfully validated. The second is a
#' vector of the names that have not been validated; either because validation
#' was not successful or because changes were made to the model or the scenario
#' after validation was done.
#' 
#' @param ModelName the name of the model
#' @return a list having four components, a vector of the names of scenarios 
#' that were properly validated, a vector of the names of scenarios that were
#' not validated, a vector of the names of all scenarios, and a vector
#' of the names of scenarios that have outputs.
#' @export
listScenarios <- function(ModelName) {
  ScenariosDir <- file.path("../models", ModelName, "scenarios")
  Sc <- dir(ScenariosDir)
  if (length(Sc) == 0) {
    return(list(
      Valid = "",
      Invalid = "",
      All = "",
      Run = ""
    ))
  } else {
    ModelEdited <- loadModelStatus(ModelName)$lastedit
    Validation_mx <- sapply(Sc, function(x) {
      ScenDir <- file.path(ScenariosDir, x)
      ScenValidated <- fromJSON(file.path(ScenDir, "status.json"))$validated
      ScenEdited <- fromJSON(file.path(ScenDir, "status.json"))$lastedit
      c(
        Validated = ScenValidated != "",
        AfterModelEdited = ScenValidated > ModelEdited,
        AfterScenarioEdited = ScenValidated > ScenEdited
      )
    })
    HasOutputs_ <- names(sapply(Sc, function(x) {
      file.exists(file.path(ScenariosDir, x, "Outputs_ls.RData"))
    }))
    Valid_Sc <- apply(Validation_mx, 2, all)
    return(list(
      Valid = names(Valid_Sc)[Valid_Sc],
      Invalid = names(Valid_Sc)[!Valid_Sc],
      All = Sc,
      Run = HasOutputs_
    ))
  }
}

  
###############################################
#---------------------------------------------#
#             RUNNING THE MODEL               #
#---------------------------------------------#
###############################################

#----------------------------
#Calculate Causal Sensitivity
#----------------------------
#' Calculate causal sensitivity
#'
#' \code{powIncr} calculates a sensitivity value which modifies the causal
#' effect of a concept as a function of the concept's value. The form of the
#' fuction is an increasing power function.
#'
#' This function calculates causal sensitivity as a function of the relative
#' value of a causal concept. The value increases from 0 to 1 as the input
#' increases from 0 to 100. The value of the 'Pow' argument determines the
#' shape of the function. The function is linear when the value is 1. When the
#' value is greater than 1, the rate of change in the output decreases as the
#' value of the concept increases from 0 to 100.
#'
#' @param x a numeric value that is in the range from 0 to 100. This is the
#'   relative value of a causal concept.
#' @param Pow the exponent which determines how rapidly sensitivity rises to the
#'   maximum as the concept value increases. A value of 1 is a linear
#'   relationship. Higher values modify the rate of change as the the input
#'   increases from 0 to 100.
#' @return a numeric value in the range of 0 to 1.
#' @export
powIncr <- function(x, Pow, OpRange) {
  sx <- rescale(x, OpRange, c(1,99))
  Result <- 1 - ((100 - sx) / 100)^Pow
  #Result[sx > 50] <- 1 / (1 - (sx[sx > 50] / 100)^Pow)
  #Result[is.infinite(Result)] <- 1 / (1 - (99 / 100)^Pow)
  Result
}


#-------------------------------
#Calculate Receiving Sensitivity
#-------------------------------
#' Calculate receiving sensitivity
#'
#' \code{powDecr} calculates a sensitivity value which modifies the receiving
#' effect of a concept as a function of the concept's value. The form of the
#' fuction is a decreasing power function.
#'
#' This function calculates causal sensitivity as a function of the relative
#' value of a causal concept. The value decreases from 1 to 0 as the input
#' increases from 0 to 100. The value of the 'Pow' argument determines the
#' shape of the function. The function is linear when the value is 1. When the
#' value is greater than 1, the rate of change in the output increases as the
#' value of the concept increases from 0 to 100.
#'
#' @param x a numeric value that is in the range from 0 to 100. This is the
#' relative value of a receiving concept.
#' @param Pow the exponent which determines how rapidly sensitivity declines to
#' the minimum as the concept value increases. A value of 1 is a linear
#' relationship. Higher values increase the rate of decline.
#' @return a numeric value in the range of 0 to 1.
#' @export
powDecr <- function(x, Pow, OpRange) {
  sx <- rescale(x, OpRange, c(1,99))
  Result <- 1 / (1 - ((100 - sx) / 100)^Pow)
  Result[sx > 50] <- 1 - (sx[sx > 50] / 100)^Pow
  Result
}


#--------------------------------------------------------
#Rescaling a Value from an Input Range to an Output Range
#--------------------------------------------------------
#' Rescale value
#'
#' \code{rescale} rescales a value from a specified input range to a specified
#' output range.
#'
#' This function rescales a value from a specified input range to a specified
#' output range. The default output range is 0 to 100 because that is the range
#' used to represent concepts in FSDM models.
#'
#' @param Value a numeric value to be rescaled from the input range to the
#' output range.
#' @param FromRange a numeric vector of length 2 in which the first value is
#' the minimum value in the range and the second value is the maximum value in
#' the range.
#' @param ToRange a numeric vector of length 2 in which the first value is the
#' minimum value in the range and the second value is the maximum value in the
#' range.
#' @return A numeric value in the output range.
#' @export
rescale <- function(Value, FromRange, ToRange) {
  ToRange[1] + diff(ToRange) * (Value - FromRange[1]) / diff(FromRange)
}

#-------------------------------------------
#Create a Fuzzy Model from Proper JSON Files
#-------------------------------------------
#' Create fuzzy model
#'
#' \code{createFuzzyModel} creates the representation of a FSDM model as an R
#' object from JSON-formatted text files.
#'
#' This function reads in JSON-formatted text files which contains all of the
#' information needed to specify a FSDM model and makes an object (a list) which
#' contains components in the data structures needed to apply the FSDM functions
#' to compute an output given a scenario which specifies initial conditions.
#'
#' @param Dir a string identifying the path to the directory where the
#' JSON-formatted text files that specify a model are located.
#' @param FuzzyVals a named numeric vector that relates linguistic relationships
#' to numeric values.
#' @return A list containing the following components:
#' Cn = a string vector containing the names of the model concepts;
#' Group = a named string vector containing the group name for each concept;
#' Relates = a numeric matrix whose dimensions are equal to the number of
#' concepts and values are the numeric weights in the model;
#' Labels = a string matrix, with the same dimensions as Relates, which contains
#' the fuzzy relationships between concepts (e.g. low, medium, high);
#' ValueRange = a data frame which provides the minimum and maximum values of
#' each concept.
#' @export
# Vals = c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675, H = 0.75, VH = 1),
# Vals = c(VL = 0.125, L = 0.25, ML = 0.5, M = 0.75, MH = 1, H = 1.25, VH = 1.5),

createFuzzyModel <- 
  function(Dir,
           Vals = c(VL = 0.1, L = 0.25, ML = 0.375, M = 0.5, MH = 0.675, H = 0.75, VH = 0.95),
           Signs = c(Positive = 1, Negative = -1)
           ) 
    {
  #Read model concept files and identify variable names and groups
  #---------------------------------------------------------------
  Concepts_df <- fromJSON(file.path(Dir, "concepts.json"))
  Cn <- Concepts_df$variable
  Group.Cn <- Concepts_df$group
  names(Group.Cn) <- Cn
  #Create relationships matrix and assign numeric values
  #-----------------------------------------------------
  Relations_ls <- 
    makeAdjacencyMatrix(fromJSON(file.path(Dir, "relations.json"), simplifyDataFrame = FALSE), 
                        Type = "Values")
  #Adjacency matrix of numeric values
  Relates.CnCn <- 
    apply(Relations_ls$Weight[Cn,Cn], 2, function(x) Vals[x]) *
    apply(Relations_ls$Direction[Cn,Cn], 2, function(x) Signs[x])
  rownames(Relates.CnCn) <- Cn
  #Make labels for graph
  Labels.CnCn <- Relations_ls$Weight[Cn,Cn]
  #Extract the value range for all concepts
  ValRng_df <- Concepts_df$values[,c("min","max")]
  ValRng_df$min <- as.numeric(ValRng_df$min)
  ValRng_df$max <- as.numeric(ValRng_df$max)
  rownames(ValRng_df) <- Cn
  # Return all the model components in a list
  list(Cn=Cn, Group=Group.Cn, Relates=Relates.CnCn, Labels=Labels.CnCn, ValueRange=ValRng_df)
}

#--------------------------------------
#Create a Scenario Object to be Modeled
#--------------------------------------
#' Create a scenario object to be modeled
#'
#' \code{createFuzzyScenario} loads a scenario file into object used in model
#' application
#'
#' This function reads a JSON formatted text file which describes a scenario. A
#' scenario is defined by the starting values of all concepts and starting
#' changes in one or more concepts.
#'
#' @param Dir the a string identifying the path to the scenario directory.
#' @param M the FSDM model object that is created by the 'createFuzzyModel'
#' function.
#' @return a list containing the following components:
#' Cn a vector of concept variable names in the same order as the list in the
#' fuzzy model. 
#' StartValues a numeric vector of concept starting values scaled 
#' to the range of 0 to 100 and in the order of Cn. 
#' ChangeTo a numeric vector of concept starting changes scaled to the range
#' of 0 to 100 and in the order of Cn.
#' @export
createFuzzyScenario <- function(Dir, M, OpRange) {
  #Load table of starting concept values, check values, and create vectors for each
  Values_df <- fromJSON(file.path(Dir, "scenario.json"))
  rownames(Values_df) <- Values_df$name
  Values_df$startvalue <- as.numeric(Values_df$startvalue)
  Values_df$startchange[Values_df$startchange == "NA"] <- NA
  Values_df$startchange <- as.numeric(Values_df$startchange)
  #Convert input starting values to range of 0 - 100
  StartValues_Cn <- numeric(length(M$Cn))
  names(StartValues_Cn) <- M$Cn
  #Extract model value range
  ValRng_df <- M$ValueRange
  #Scale start values
  for(cn in M$Cn) {
    StartVal <- Values_df[cn, "startvalue"]
    MinVal <- as.numeric(ValRng_df[cn,"min"])
    MaxVal <- as.numeric(ValRng_df[cn,"max"])
    StartValues_Cn[cn] <- rescale(StartVal, c(MinVal, MaxVal), OpRange )
  }
  #StartValues_Cn[StartValues_Cn == 0] <- 0.1
  #Record the change to targets
  ChangeTo_Cn <- numeric(length(M$Cn))
  names(ChangeTo_Cn) <- M$Cn
  for(cn in M$Cn) {
    if (!is.na(Values_df[cn, "startchange"])) {
      ChangeVal <- Values_df[cn, "startchange"]
      MinVal <- as.numeric(ValRng_df[cn,"min"])
      MaxVal <- as.numeric(ValRng_df[cn,"max"])
      ChangeTo_Cn[cn] <- rescale(ChangeVal, c(MinVal, MaxVal), OpRange )
    } else {
      ChangeTo_Cn[cn] <- NA
    }
  }
  #Return all the model components in a list
  list(
    Cn=M$Cn, 
    StartValues=StartValues_Cn, 
    ChangeTo=ChangeTo_Cn
  )
}

#-------------------------------------------------------
#Calculate the initial effects of inputs on each concept
#-------------------------------------------------------
#' Calculate intial effects
#'
#' \code{calcInitEffects} calculates the initial changes to concepts as a
#' result of changes in causal concepts
#'
#' This function takes the change in concept levels, calculates the causal
#' sensitivities, calculates the adjusted effect of each causal concept as the
#' product of the sensitivity and causal weight, and calculates the joint
#' effects of multiple causes. The result is the initial proportional changes
#' to receiving concepts.
#'
#' @param Levels_Cn a numeric vector of concept levels
#' @param LevelChg_Cn a numeric vector of the percentage changes in concept.
#' levels.
#' @param M a fuzzy model object created with the createFuzzyModel function.
#' @param Pow a numeric value that is the exponent used in the causal
#' sensitivity function.
#' @return a numeric vector containing the initial percentage change to each
#' concept.
#' @export
calcInitEffects <- function(Levels_Cn, LevelChg_Cn, M, Pow, OpRange) {
  # Calculate sensitivity of each concept to cause change
  CausalSens_Cn <- powIncr(Levels_Cn, Pow = Pow, OpRange = OpRange)
  # Adjust the weights by the causal sensitivity
  CausalWts_CnCn <- sweep(M$Relates, 1, CausalSens_Cn, "*")
  # Convert percentage change into proportions
  InputChg_Cn <- LevelChg_Cn / 100
  # Calculate the component changes
  Changes_CnCn <- sweep(CausalWts_CnCn, 1, InputChg_Cn, "*")
  # Calculate the product of the changes
  Changes_Cn <- apply(Changes_CnCn + 1, 2, prod, na.rm = TRUE) - 1
  # Return the result
  Changes_Cn * 100
}

#--------------------------------------------------------
#Adjust the response to inputs as function of sensitivity
#--------------------------------------------------------
#' Adjust concept response to change
#'
#' \code{adjustResponse} uses sensitivity function to adjust concept response to
#' causal changes
#'
#' This function calculates receiving sensitivity values and uses them to
#' adjusts the causal changes to concepts
#'
#' @param InputChg_Cn a numeric vector of the unadjusted percentage changes
#' calculated for each concept.
#' @param Levels_Cn a numeric vector of the levels of each concept in the range
#' of 0 to 100.
#' @param Pow a numeric value that is the exponent used in the receiving
#' sensitivity function.
#' @return a numeric vector containing the percentage change to each concept.
#' @export
adjustResponse <- function(InputChg_Cn, Levels_Cn, Pow, OpRange){
  # Calculate the sensitivity of each concept to be changed
  ReceiveSens_Cn <- powDecr(Levels_Cn, Pow = Pow, OpRange = OpRange)
  # Calculate the adjusted input change
  InputChg_Cn * ReceiveSens_Cn
}
# 
# InputChg_Cn <- InitEffect_Cn
# 
# 
# adjustResponse(InitEffect_Cn, Levels_Cn, Pow, OpRange)

#--------------------------------
#Update the level of each concept
#--------------------------------
#' Adjust concept level
#'
#' \code{adjustLevel} calculates the values of all the concepts as a result of
#' the applied changes
#'
#' This function calculates the values of all the concepts as a result of
#' applying the percentage changes calculated by the adjustResponse function.
#' The function constrains the result to the range of 0.001 to 100.
#'
#' @param InputChg_Cn a numeric vector of the percentage change in the level of
#' each concept.
#' @param Level_Cn a numeric vector of the level of each concept.
#' @return a numeric vector of the level of each concept after it has been
#' adjusted.
#' @export
adjustLevel <- function(InputChg_Cn, Level_Cn, OpRange) {
  AdjLevel_Cn <- Level_Cn * (1 + InputChg_Cn / 100)
  # Constrain to the operating range
  AdjLevel_Cn[AdjLevel_Cn < OpRange[1]] <- OpRange[1]
  AdjLevel_Cn[AdjLevel_Cn > OpRange[2]] <- OpRange[2]
  # Return the result
  AdjLevel_Cn
}

#---------------------------------------------------------
#Define a function that runs the fuzzy cognitive map model
#---------------------------------------------------------
#' Run the fuzzy model
#'
#' \code{runFuzzyModel} runs a fuzzy model given scenario inputs for starting
#' values and starting changes.
#'
#' This function runs a FCM model that was created using the createFuzzyModel
#' function with a scenarios that was created using the createFuzzyScenario function.
#' The function uses a nested loop to calculate final values. The outer loop
#' applies the starting changes in small increments so that the changes
#' appropriate for applying 'point elasticities'. The inner loop iterates the
#' model until a stable equilibrium result is achieved for each increment.
#'
#' @param M a fuzzy model created using the create FuzzyModel function.
#' @param S a scenario created using the loadScenario function.
#' @param NumIncr a numeric value identifying the number of increments to use
#' for building up the starting changes.
#' @param MaxIter a numeric value identifying the maximum number of iterations
#' for the inner loop.
#' @param Pow a numeric value that is the exponent used in the causal and
#' receiving sensitivity functions.
#' @return A list containing 3 components:
#' Summary - the final relative concept values (e.g. 0 - 100) for each concept
#' and increment;
#' ScaleSummary - the final concept values in the nominal measurement units for
#' each concept and increment;
#' Full - the relative concept values (e.g. 0 - 100) for each increment and
#' each iteration.
#' @export
runFuzzyModel <- function(M, S, OpRange, Pow = 10, NumIncr = 100, MaxIter=100){
  #Iterate through number of increments to increase inputs
  Final_ <- list()
  ChangeTargets_Cn <- S$ChangeTo
  ChangeTargets_Cn[is.na(S$ChangeTo)] <- S$StartValues[is.na(S$ChangeTo)]
  LastResults_Cn <- S$StartValues
  ChangeDir_Cn <- sign(ChangeTargets_Cn - LastResults_Cn)
  #ChangeDir.Cn[is.na(ChangeDir.Cn)] <- 1
  for (n in 1:NumIncr) {
    # Set up list to store calculations for each iteration
    Results_ <- list()
    # Iterate the model
    for (i in 1:MaxIter) {
      if (i == 1){
        PrevLevels_Cn <- LastResults_Cn
        ChangeRates_Cn <- (ChangeTargets_Cn / PrevLevels_Cn)^(1/(NumIncr - n)) - 1
        ChangeRates_Cn[ChangeDir_Cn == 0] <- 0
        Levels_Cn <- PrevLevels_Cn * (1 + ChangeRates_Cn)
        #Levels.Cn <- PrevLevels.Cn + ChangeDir.Cn * abs(ChangeTargets.Cn - PrevLevels.Cn) * n / NumIncr
        Results_[[i]] <- Levels_Cn
      } else {
        LevelChg_Cn <- 100 * (Levels_Cn - PrevLevels_Cn) / PrevLevels_Cn
        LevelChg_Cn[is.nan(LevelChg_Cn)] <- 0
        InitEffect_Cn <- calcInitEffects(Levels_Cn, LevelChg_Cn, M, Pow, OpRange)
        AdjEffect_Cn <- adjustResponse(InitEffect_Cn, Levels_Cn, Pow, OpRange)
        PrevLevels_Cn <- Levels_Cn
        Levels_Cn <- adjustLevel(AdjEffect_Cn, Levels_Cn, OpRange)
        Results_[[i]] <- Levels_Cn
        # If absolute values of all of the adjusted effects are less than 0.01 then break out of the loop
        if( all( abs(AdjEffect_Cn) < 1 ) & i > 10 ) break
      }
    }
    # Put the results into a matrix and return
    if (n == 1) {
      Results_ItCn <- rbind( S$StartValues, do.call(rbind, Results_) )
      rownames(Results_ItCn) <- 0:(nrow(Results_ItCn)- 1)
    } else {
      Results_ItCn <- do.call(rbind, Results_)
    }
    Final_[[n]] <- Results_ItCn
    LastResults_Cn <- Results_ItCn[nrow(Results_ItCn),]
    #Break out if any targets achieved
    PosDir <- ChangeDir_Cn == 1
    NegDir <- ChangeDir_Cn == -1
    if (any(LastResults_Cn[PosDir] > ChangeTargets_Cn[PosDir], na.rm = TRUE))
      break()
    if (any(LastResults_Cn[NegDir] < ChangeTargets_Cn[NegDir], na.rm = TRUE))
      break()
    #Break out if all values are close to targets
    LastResults_Cx <- LastResults_Cn[PosDir | NegDir]
    ChangeTargets_Cx <- ChangeTargets_Cn[PosDir | NegDir]
    if (all(abs(LastResults_Cx - ChangeTargets_Cx) / ChangeTargets_Cx < 0.01))
      break()
  }
  #Prepare summary table of final results
  FinalResults_ItCn <- rbind(Final_[[1]][1,], do.call(rbind, lapply(Final_, function(x) x[nrow(x),])))
  rownames(FinalResults_ItCn) <- as.character(0:(nrow(FinalResults_ItCn)-1))
  #Rescale summary table to initial input range
  RescaleResults_ItCn <- FinalResults_ItCn * 0
  for (cn in M$Cn) {
    RescaleResults_ItCn[,cn] <-
      sapply(FinalResults_ItCn[,cn], function(x) {
        rescale(x, OpRange, unlist(M$ValueRange[cn,]))
      })
  }
  list(
    ScaledSummary = rescale(FinalResults_ItCn, OpRange, c(0,100)),
    RescaledSummary = RescaleResults_ItCn,
    ScaledFull = lapply(Final_, function(x) rescale(x, OpRange, c(0,100)))
  )
}


###############################################
#---------------------------------------------#
#           ANALYZE MODEL RESULTS             #
#---------------------------------------------#
###############################################

#---------------------------------------------------------------
#Define a function to identify model scenarios that have outputs
#---------------------------------------------------------------
#' Identify scenarios that have model outputs
#' 
#' \code{idScenWithOutputs} returns the names of scenarios that have model run
#' outputs.
#' 
#' This function takes the name of a model and returns a vector of the names
#' of all the model scenarios that have model outputs.
#' 
#' @param Model a string representation of the model name.
#' @return a string vector of the names of scenarios having model outputs.
#' @export
idScenWithOutputs <- function(ModelName) {
  ScenPath <- file.path("../models", ModelName, "scenarios")
  Sc <- dir(ScenPath)
  HasOutputs_ <- sapply(Sc, function(x) {
    file.exists(file.path(ScenPath, x, "Outputs_ls.RData"))
  })
  Sc[HasOutputs_]
}

#------------------------------------------------------
#Define function to format output data to plot and save
#------------------------------------------------------
#' Create data frame of selected scenarios and concepts to plot and save
#'
#' \code{formatOutputData} makes a data frame of the summary results for selected
#' scenarios and selected concepts.
#'
#' This function creates a data frame of model results for selected scenarios
#' and selected concepts. The data frame is in 'flat' format where values are
#' in one column and the corresponding concept names, scenario names, and 
#' iterations are in separate columns.
#'
#' @param Model a string representation of the model name.
#' @param Sc a vector of the names of scenarios to include.
#' @param Vn the variable names for the concepts to include.
#' @return A data frame having columns identifying the scenario, concept,
#' iteration, scaled values, and rescaled values.
#' @export
formatOutputData <- function(ModelName, Sc, Vn) {
  ScenPath <- file.path("../models", ModelName, "scenarios")
  ModelOut_ls <- list()
  for (sc in Sc) {
    DataPath <- file.path(ScenPath, sc, "Outputs_ls.RData")
    if (file.exists(DataPath)) {
      load(DataPath)
      ModelOut_ls[[sc]]$Scaled <- Outputs_ls$ScaledSummary
      ModelOut_ls[[sc]]$Rescaled <- Outputs_ls$RescaledSummary
    }
  }
  Results_ls <- 
    lapply(ModelOut_ls, function(x) {
      Sc <- rownames(x$Scaled)
      Scaled_mx <- x$Scaled[Sc, Vn]
      Rescaled_mx <- x$Rescaled[Sc, Vn]
      data.frame(
        Concept = rep(Vn, each = length(Sc)),
        Iteration = rep(1:length(Sc), length(Vn)),
        Scaled = as.vector(Scaled_mx),
        Rescaled = as.vector(Rescaled_mx)
      )
    })
  for (i in 1:length(Results_ls)) {
    Results_ls[[i]]$Scenario <- names(Results_ls)[i]
  }
  Results_df <- do.call(rbind, Results_ls)
  rownames(Results_df) <- NULL
  Results_df
}

