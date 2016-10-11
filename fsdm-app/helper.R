#helper.R

#INITIALIZE A NEW MODEL
#======================
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
initializeNewModel <- function(ModelName) {
  #Create directory for model
  NewDir <- file.path("../models", ModelName)
  dir.create(NewDir)
  #Create and save a status list
  status_ls <- list(name = ModelName,
                    parent = "none",
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()))
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  #Copy and save the concept and relations template files
  CopyDir <- "../models/templates"
  FilesToCopy_ <- file.path(
    CopyDir, c("concepts.json", "relations.json")
    )
  file.copy(FilesToCopy_, NewDir, recursive = TRUE)
  #Return the status list  
  status_ls
}


#INITIALIZE A NEW MODEL BY COPYING AN EXISTING MODEL
#===================================================
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
initializeCopyModel <- function(ModelName, CopyModelName, CopyScenarios = FALSE) {
  NewDir <- file.path("../models", ModelName)
  dir.create(NewDir)
  CopyDir <- file.path("../models", CopyModelName)
  if(CopyScenarios) {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json", "scenarios")
    )
    file.copy(FilesToCopy_, NewDir, recursive = TRUE)
  } else {
    FilesToCopy_ <- file.path(
      CopyDir, c("status.json", "concepts.json", "relations.json")
    )
    file.copy(FilesToCopy_, NewDir)      
  }
  status_ls <- list(name = ModelName,
                    parent = CopyModelName,
                    created = as.character(Sys.time()),
                    lastedit = as.character(Sys.time()))
  writeLines(toJSON(status_ls), file.path(NewDir, "status.json"))
  status_ls
}


#LOAD MODEL STATUS
#=================
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
loadModelStatus <- function(ModelName){
  ModelDir <-  file.path("../models", ModelName)
  as.list(fromJSON(file.path(ModelDir, "status.json")))
}


#LOAD MODEL CONCEPTS
#===================
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


#LOAD MODEL RELATIONS
#====================
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


#INITIALIZE NEW RELATIONS ENTRY
#==============================
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

#SAVE MODEL
#==========
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


#FORMAT A CONCEPT TABLE FOR DISPLAY
#==================================
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


#MAKE AN ADJACENCY MATRIX FROM A RELATIONS LIST
#==============================================
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
#' @return a matrix of logical values
#' @export
makeAdjacencyMatrix <- function(Relations_ls) {
  Var_ <- unlist(lapply(Relations_ls, function(x) x$name))
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
  Relations_mx
}


#EXTRACT DATA ON EFFECT RELATIONSHIPS OF A SELECTED CAUSAL CONCEPT
#=================================================================
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
  VarName <- nameToVar(ConceptName)
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


#MAP RELATIONS
#=============
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
    if (!is.null(FromConcept)) {
      FromConcept <- nameToVar(FromConcept)
    }
    #Make a matrix of relations
    Relations_mx <- makeAdjacencyMatrix(Model_ls$relations)[Nv,Nv]
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
        if (sum(Selected_mx[FromConcept,]) != 0) {
          ColSelect <- 
            c(
              ColSelect[which(Selected_mx[FromConcept,])],
              ColSelect[-which(Selected_mx[FromConcept,])]
            )
        }
        if (FromConcept %in% rownames(Selected_mx)) {
          NumHighlighted <- sum(Selected_mx[FromConcept,])
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
    Y0_ <- rep(YVals1_, apply(Selected_mx, 1, sum))
    X0_ <- rep(2, length(Y0_))
    Y1_ <- rep(YVals2_, nrow(Selected_mx))[as.vector(t(Selected_mx))]
    X1_ <- rep(6,length(Y1_))
    if (exists("NumHighlighted")) {
      Col_ <- 
        c(rep("red", NumHighlighted), rep("grey", length(X0_) - NumHighlighted))
      Lwd_ <-
        c(rep(2, NumHighlighted), rep(1, length(X0_) - NumHighlighted))
    } else {
      Col_ <- rep("grey", length(X0_))
      Lwd_ <- rep(1, length(X0_))
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
