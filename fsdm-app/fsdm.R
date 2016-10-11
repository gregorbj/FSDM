#fuzzy_systems_dynamics_modeler.r
#Version 0.7

# Copyright 2014-2015 Brian Gregor, Oregon Systems Analytics LLC

# Licensed under the Apache License, Version 2.0 (the "License"); you may not use
# this file except in compliance with the License. You may obtain a copy of the
# License at http://www.apache.org/licenses/LICENSE-2.0. Unless required by
# applicable law or agreed to in writing, software distributed under the License
# is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the specific language
# governing permissions and limitations under the License.


#Overview
#========
# This script implements an alternative formulation for fuzzy cognitive map
# (FCM) models. Fuzzy cognitive maps are models of systems of interrelated
# concepts represented graphically as directed graphs and programmatically as
# adjacency matrices. The nodes of the graph represent concepts and the directed
# edges represent causal effects. Weights on the edges represent the strength of
# the effect. Fuzzy congnitive maps are applied to problem domains that have
# system characteristics but are relatively data poor and so specifying the
# system depends on expert knowledge. In standard fuzzy cognitive maps the
# causal linkage is between the value of one concept and the value of another
# concept mediated by the weight on the causal link. In the case where a node is
# affected by one other node, the value of the affected node is the product of
# the value of the affected node and the weight of the causal link. When a node
# is affected by more than one link, the value is the sum of the products of the
# values of the respective causal links and nodes. This resulting value is
# processed by what is commonly called an activation function to limit the value
# of the node to the interval (0,1].

# There are several semantic issues with standard fuzzy cognitive map
# representations of systems.  A number of these have been documented by
# Carvalho (Joao Paulo Carvalho, On the Semantics and the Use of Fuzzy Cognitive
# Maps in Social Sciences, WCCI 2010 IEEE World Congress on Computational
# Intelligence, July 18-23, 2010 - CCIB, Barcelona, Spain). In the new fuzzy
# cognitive map formulation implemented by this script, rather than the value of
# the concepts affecting one another, it is the change in the values of concepts
# that affect each other. The link weights can therefore be thought of as
# elasticities. The joint effect of multiple causal nodes on an affected node is
# calculated as the product of the individual proportional effects (e.g. 1.1 *
# 0.8 * 0.9). The activation function is replaced by a sensitivity functions on
# causal and receiving nodes of the functions. This replaces the method used in
# version 0.4 where sensitivity functions were applied only to the receiving
# nodes. In the case of causal sensitivity, the causal strength increases from
# zero, when the value of the causal node is at it's minimum value, to a value
# of 1, when the causal node is at its maximum value. In the case of the
# receiving sensitivity, the sensitivity is 1, when the value of the receiving
# node is at its minimum value, to a value of 0, when the receiving note is at
# its maximum value.


#Set Up
#======

#Load packages
#-------------
library(RColorBrewer)
library(jsonlite)

#Define utility functions
#========================

#Causal sensitivity function
#---------------------------
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
powIncr <- function(x, Pow) {
  #Check inputs
  if (!(class(x) %in% c("numeric", "integer"))) {
    Msg <- "x argument must be a numeric value"
    stop(Msg)
  }
  if (any((x < 0) | (x > 100))) {
    Msg <- "x must not be less than 0 or greater than 100"
    stop(Msg)
  }
  if (class(Pow) != "numeric") {
    Msg <- "Pow argument must be a numeric value"
    stop(Msg)
  }
  if (Pow < 1) {
    Msg <- "Pow argument must be a number greater than 1."
    stop(Msg)
  }
  if (Pow > 10) {
    Msg <- "Do you really intend Pow to be greater than 10?"
    warning(Msg)
  }
  #Calculate sensitivity
  1 - ((100 - x) / 100)^Pow
}

#Define receiving sensitivity function
#-------------------------------------
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
powDecr <- function(x, Pow) {
  #Check inputs
  if (!(class(x) %in% c("numeric", "integer"))) {
    Msg <- "x argument must be a numeric value"
    stop(Msg)
  }
  if (any((x < 0) | (x > 100))) {
    Msg <- "x must not be less than 0 or greater than 100"
    stop(Msg)
  }
  if (class(Pow) != "numeric") {
    Msg <- "Pow argument must be a numeric value"
    stop(Msg)
  }
  if (Pow < 1) {
    Msg <- "Pow argument must be a number greater than 1."
    stop(Msg)
  }
  if (Pow > 10) {
    Msg <- "Do you really intend Pow to be greater than 10?"
    warning(Msg)
  }
  #Calculate sensitivity
  1 - (x / 100)^Pow
}

#Define a function to rescale a number from input range to output range
#----------------------------------------------------------------------
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
rescale <- function(Value, FromRange, ToRange=c(0,100)) {
  if ((Value < min(FromRange)) | (Value > max(FromRange))) {
    ErrMsg <- "Value is outside FromRange."
    stop(ErrMsg)
  }
  if (FromRange[1] > FromRange[2]) {
    ErrMsg <- "FromRange is improperly specified."
    stop(ErrMsg)
  }
  if (ToRange[1] > ToRange[2]) {
    ErrMsg <- "ToRange is improperly specified."
    stop(ErrMsg)
  }
  #Return the scaled number or 0.01 if the scaled number is 0
  ToRange[1] + diff(ToRange) * (Value - FromRange[1]) / diff(FromRange)
}


#Define functions to load models and scenarios
#=============================================

#Define a function to create a model from JSON description
#---------------------------------------------------------
#' Create FSDM model
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
#' @param ConceptFile a string identifying the path to a JSON-formatted text
#' file which specifies the characteristics of the concepts in a FSDM.
#' @param RelationsFile a string identifying the path to a JSON-formatted text
#' file which specifies the characteristics of the causal relationships in a
#' FSDM.
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
createFuzzyModel <- function(Dir, ConceptFile, RelationsFile,
                             FuzzyVals = c(VL = 0.1, L = 0.25, ML = 0.375,
                                           M = 0.5, MH = 0.675, H = 0.75,
                                           VH = 0.99)) {
  #Read in model JSON files and identify variable names and groups
  #---------------------------------------------------------------
  Concepts.. <- fromJSON(file.path(Dir, ConceptFile))
  Relations.. <- fromJSON(file.path(Dir, RelationsFile))
  Cn <- Concepts..$variable
  Group.Cn <- Concepts..$group
  names(Group.Cn) <- Cn
  #Create relationships matrix and assign numeric values
  #-----------------------------------------------------
  #Add variable name to the Relations.. data frame
  Relations..$variable <- Concepts..$variable[match(Concepts..$name, Relations..$name)]
  #Extract direction and fuzzy magnitude values from model into matrices
  RelSign.CnCn <- RelWt.CnCn <- array("", dim=c(length(Cn), length(Cn)), dimnames=list(Cn, Cn))
  for(cn in Cn) {
    Affects.. <- Relations..$affects[[which(Relations..$variable == cn)]]
    RelSign.CnCn[cn, Affects..$variable] <- Affects..$direction
    RelWt.CnCn[cn, Affects..$variable] <- Affects..$weight
  }
  #Convert direction and fuzzy magnitude values into matrix of numerical values
  Relates.CnCn <- array(0, dim=c(length(Cn), length(Cn)), dimnames=list(Cn, Cn))
  Vals. <- FuzzyVals
  Fl <- names(Vals.)
  for(fl in Fl) {
    Relates.CnCn[ RelWt.CnCn == fl ] <- Vals.[fl]
  }
  Relates.CnCn[RelSign.CnCn == "Negative"] <- -1 * Relates.CnCn[RelSign.CnCn == "Negative"]
  #Make labels for graph
  Labels.CnCn <- RelWt.CnCn
  #Extract the value range for all concepts
  ValRng.. <- Concepts..$values[,c("min","max")]
  ValRng..$min <- as.numeric(ValRng..$min)
  ValRng..$max <- as.numeric(ValRng..$max)
  rownames(ValRng..) <- Cn
  # Return all the model components in a list
  list(Cn=Cn, Group=Group.Cn, Relates=Relates.CnCn, Labels=Labels.CnCn, ValueRange=ValRng..)
}

#Define a function which creates an object to define a scenario
#--------------------------------------------------------------
#' Load scenario
#'
#' \code{loadScenario} loads a scenario file into object used in model
#' application
#'
#' This function reads a CSV formatted text file which describes a scenario. A
#' scenario is defined by the starting values of all concepts and starting
#' changes in one or more concepts.
#'
#' @param Dir the a string identifying the path to the directory where the
#' CSV-formatted text file that specifies a scenario is located.
#' @param ScenarioFile a string identifying the CSV-formatted text file that
#' meets specifications for describing a scenario.
#' @param M the FSDM model object that is created by the 'createFuzzyModel'
#' function.
#' @return a list containing the following components:
#' StartValues a numeric vector of concept starting values scaled to the range
#' of 0 to 100;
#' @export
loadScenario <- function(Dir, ScenarioFile, M) {
  #Load table of starting concept values, check values, and create vectors for each
  Starts.. <- read.csv(file.path(Dir, ScenarioFile), row.names=1, as.is=TRUE)
  #Check that all rownames correspond to concept names
  if (!setequal(rownames(Starts..), M$Cn)) {
    stop( "The rownames of the ScenarioFile don't correspond to the concepts names in the model." )
  }
  #Check that the values for StartValue are within range
  ValRng.. <- M$ValueRange
  for(cn in M$Cn) {
    StartVal <- Starts..[cn, "StartValue"]
    ChangeVal <- Starts..[cn, "ChangeTo"]
    MinVal <- as.numeric(ValRng..[cn,"min"])
    MaxVal <- as.numeric(ValRng..[cn,"max"])
    if( (StartVal < MinVal) | (StartVal > MaxVal) ) {
      ErrMsg <- paste( "StartValue value for", cn, "is outside the range of acceptable values." )
      stop( ErrMsg )
    }
    if( !is.na(ChangeVal) ) {
      if( (ChangeVal < MinVal) | (ChangeVal > MaxVal) ) {
        ErrMsg <- paste( "ChangeTo value for", cn, "is outside the range of acceptable values." )
        stop( ErrMsg )
      }
    }
  }
  #Convert input starting values to range of 0 - 100
  StartValues.Cn <- numeric(length(M$Cn))
  names(StartValues.Cn) <- M$Cn
  for(cn in M$Cn) {
    StartVal <- Starts..[cn, "StartValue"]
    MinVal <- as.numeric(ValRng..[cn,"min"])
    MaxVal <- as.numeric(ValRng..[cn,"max"])
    StartValues.Cn[cn] <- rescale(StartVal, c(MinVal, MaxVal) )
  }
  #Record the change to targets
  ChangeTo.Cn <- numeric(length(M$Cn))
  names(ChangeTo.Cn) <- M$Cn
  for(cn in M$Cn) {
    if (!is.na(Starts..[cn, "ChangeTo"])) {
      ChangeVal <- Starts..[cn, "ChangeTo"]
      MinVal <- as.numeric(ValRng..[cn,"min"])
      MaxVal <- as.numeric(ValRng..[cn,"max"])
      ChangeTo.Cn[cn] <- rescale(ChangeVal, c(MinVal, MaxVal) )
    } else {
      ChangeTo.Cn[cn] <- NA
    }
  }
  #Return all the model components in a list
  list( Cn=M$Cn, StartValues=StartValues.Cn, ChangeTo=ChangeTo.Cn )
}


#Define functions to run a fuzzy model
#=====================================

#Define function to calculate the initial effects of inputs on each concept
#--------------------------------------------------------------------------
#' Calculate intial changes
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
#' @param Levels.Cn a numeric vector of concept levels
#' @param LevelChg.Cn a numeric vector of the percentage changes in concept.
#' levels.
#' @param M a fuzzy model object created with the createFuzzyModel function.
#' @param Pow a numeric value that is the exponent used in the causal
#' sensitivity function.
#' @return a numeric vector containing the initial percentage change to each
#' concept.
#' @export
calcInitEffects <- function(Levels.Cn, LevelChg.Cn, M, Pow) {
  # Calculate sensitivity of each concept to cause change
  CausalSens.Cn <- powIncr(Levels.Cn, Pow = Pow)
  # Adjust the weights by the causal sensitivity
  CausalWts.CnCn <- sweep(M$Relates, 1, CausalSens.Cn, "*")
  # Convert percentage change into proportions
  InputChg.Cn <- LevelChg.Cn / 100
  # Calculate the component changes
  Changes.CnCn <- sweep(CausalWts.CnCn, 1, InputChg.Cn, "*")
  # Calculate the product of the changes
  Changes.Cn <- apply(Changes.CnCn + 1, 2, prod) - 1
  # Return the result
  Changes.Cn * 100
}

#Define a function which adjusts the response to inputs as function of sensitivity
#---------------------------------------------------------------------------------
#' Adjust concept response to change
#'
#' \code{adjustResponse} uses sensitivity function to adjust concept response to
#' causal changes
#'
#' This function calculates receiving sensitivity values and uses them to
#' adjusts the causal changes to concepts
#'
#' @param InputChg.Cn a numeric vector of the unadjusted percentage changes
#' calculated for each concept.
#' @param Levels.Cn a numeric vector of the levels of each concept in the range
#' of 0 to 100.
#' @param Pow a numeric value that is the exponent used in the receiving
#' sensitivity function.
#' @return a numeric vector containing the percentage change to each concept.
#' @export
adjustResponse <- function(InputChg.Cn, Levels.Cn, Pow){
  # Calculate the sensitivity of each concept to be changed
  ReceiveSens.Cn <- powDecr(Levels.Cn, Pow = Pow)
  # Calculate the adjusted input change
  InputChg.Cn * ReceiveSens.Cn
}

#Define a function to update the level of each concept
#-----------------------------------------------------
#' Adjust concept level
#'
#' \code{adjustLevel} calculates the values of all the concepts as a result of
#' the applied changes
#'
#' This function calculates the values of all the concepts as a result of
#' applying the percentage changes calculated by the adjustResponse function.
#' The function constrains the result to the range of 0.001 to 100.
#'
#' @param InputChg.Cn a numeric vector of the percentage change in the level of
#' each concept.
#' @param Level.Cn a numeric vector of the level of each concept.
#' @return a numeric vector of the level of each concept after it has been
#' adjusted.
#' @export
adjustLevel <- function(InputChg.Cn, Level.Cn) {
  AdjLevel.Cn <- Level.Cn * (1 + InputChg.Cn / 100)
  # Constrain to range of 0.001 to 100
  AdjLevel.Cn[AdjLevel.Cn < 0] <- 0.001
  AdjLevel.Cn[AdjLevel.Cn > 100] <- 100
  # Return the result
  AdjLevel.Cn
}

#Define a function that runs the fuzzy cognitive map model
#---------------------------------------------------------
#' Run the fuzzy model
#'
#' \code{runFuzzyModel} runs a fuzzy model given scenario inputs for starting
#' values and starting changes.
#'
#' This function runs a FCM model that was created using the createFuzzyModel
#' function with a scenarios that was created using the loadScenario function.
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
runFuzzyModel <- function(M, S, Pow, NumIncr = 100, MaxIter=100){
  #Iterate through number of increments to increase inputs
  Final_ <- list()
  ChangeTargets.Cn <- S$ChangeTo
  ChangeTargets.Cn[is.na(S$ChangeTo)] <- S$StartValues[is.na(S$ChangeTo)]
  LastResults.Cn <- S$StartValues
  ChangeDir.Cn <- sign(ChangeTargets.Cn - LastResults.Cn)
  #ChangeDir.Cn[is.na(ChangeDir.Cn)] <- 1
  for (n in 1:NumIncr) {
    # Set up list to store calculations for each iteration
    Results_ <- list()
    # Iterate the model
    for (i in 1:MaxIter) {
      if (i == 1){
        PrevLevels.Cn <- LastResults.Cn
        ChangeRates.Cn <- (ChangeTargets.Cn / PrevLevels.Cn)^(1/(NumIncr - n)) - 1
        ChangeRates.Cn[ChangeDir.Cn == 0] <- 0
        Levels.Cn <- PrevLevels.Cn * (1 + ChangeRates.Cn)
        #Levels.Cn <- PrevLevels.Cn + ChangeDir.Cn * abs(ChangeTargets.Cn - PrevLevels.Cn) * n / NumIncr
        Results_[[i]] <- Levels.Cn
      } else {
        LevelChg.Cn <- 100 * (Levels.Cn - PrevLevels.Cn) / PrevLevels.Cn
        LevelChg.Cn[is.nan(LevelChg.Cn)] <- 0
        InitEffect.Cn <- calcInitEffects(Levels.Cn, LevelChg.Cn, M, Pow)
        AdjEffect.Cn <- adjustResponse(InitEffect.Cn, Levels.Cn, Pow)
        PrevLevels.Cn <- Levels.Cn
        Levels.Cn <- adjustLevel(AdjEffect.Cn, Levels.Cn)
        Results_[[i]] <- Levels.Cn
        # If absolute values of all of the adjusted effects are less than 0.01 then break out of the loop
        if( all( abs(AdjEffect.Cn) < 1 ) & i > 10 ) break
      }
    }
    # Put the results into a matrix and return
    if (n == 1) {
      Results.ItCn <- rbind( S$StartValues, do.call(rbind, Results_) )
      rownames(Results.ItCn) <- 0:(nrow(Results.ItCn)- 1)
    } else {
      Results.ItCn <- do.call(rbind, Results_)
    }
    Final_[[n]] <- Results.ItCn
    LastResults.Cn <- Results.ItCn[nrow(Results.ItCn),]
    #Break out if any targets achieved
    PosDir <- ChangeDir.Cn == 1
    NegDir <- ChangeDir.Cn == -1
    if (any(LastResults.Cn[PosDir] > ChangeTargets.Cn[PosDir], na.rm = TRUE))
      break()
    if (any(LastResults.Cn[NegDir] < ChangeTargets.Cn[NegDir], na.rm = TRUE))
      break()
    #Break out if all values are close to targets
    LastResults.Cx <- LastResults.Cn[PosDir | NegDir]
    ChangeTargets.Cx <- ChangeTargets.Cn[PosDir | NegDir]
    if (all(abs(LastResults.Cx - ChangeTargets.Cx) / ChangeTargets.Cx < 0.01))
      break()
  }
  #Prepare summary table of final results
  FinalResults.ItCn <- rbind(Final_[[1]][1,], do.call(rbind, lapply(Final_, function(x) x[nrow(x),])))
  rownames(FinalResults.ItCn) <- as.character(0:(nrow(FinalResults.ItCn)-1))
  #Rescale summary table to initial input range
  RescaleResults.ItCn <- FinalResults.ItCn * 0
  for (cn in M$Cn) {
    RescaleResults.ItCn[,cn] <-
      sapply(FinalResults.ItCn[,cn], function(x) {
        rescale(x, c(0,100), unlist(M$ValueRange[cn,]))
      })
  }
  list(Summary = FinalResults.ItCn, ScaleSummary = RescaleResults.ItCn, Full = Final_)
}


#Define plotting and tabulation functions
#========================================

#Define a function to plot the level values of each concept for all iterations
#-----------------------------------------------------------------------------
#' Plot concept changes by iteration
#'
#' \code{plotIterationResponse} plots the values of all concepts that change for
#' each iteration
#'
#' This function plots the results of a fuzzy model run, showing the values of
#' all the concepts that change for each iteration.
#'
#' @param Levels.ItCn a numeric matrix of fuzzy model outputs where the rows
#' correspond to iterations and the columns correspond to the concepts in the
#' model.
#' @param ... other arguments passed to matplot.
#' @return none. Function has the side effect of producing a plot.
#' @export
plotIterationResponse <- function( Levels.ItCn, ... ){
  LevelChg.Cn <- Levels.ItCn[nrow(Levels.ItCn),] / Levels.ItCn[1,]
  ToPlot.Cn <- (LevelChg.Cn > 1.01) | (LevelChg.Cn < 0.99)
  Levels.ItCn <- Levels.ItCn[,ToPlot.Cn]
  Cn <- colnames( Levels.ItCn )
  XAxisLabels. <- rownames( Levels.ItCn )
  YRange. <- range( min(Levels.ItCn), max(Levels.ItCn) )
  Num <- length(Cn)
  Reps <- ceiling( Num / 9 )
  if( Num < 3 ) {
    Colors. <- c( "red", "blue" )
    Lty. <- rep(1, Num)
  }
  if( (Num >= 3) & (Num <= 9) ) {
    Colors. <- brewer.pal( length(Cn), "Set1" )
    Lty. <- rep(1, Num)
  }
  if( Num > 9 ) {
    Colors. <- rep( brewer.pal( 9, "Set1" ), Reps )[1:Num]
    Lty. <- rep( 1:Reps, each=9 )[1:Num]
  }
  layout( matrix(1:2, ncol=2), widths=c(6,2.5), heights=6 )
  matplot( as.numeric( rownames(Levels.ItCn ) ), Levels.ItCn,
           type="l", lwd=2, lty=Lty., col=Colors.,
           xlab="Iteration", ylab="Percent of Maximum",
           ... )
  Opar_ <- par( mar=c(1,2,1,2) )
  plot( 0, 0, type="n", axes=FALSE, xlab="", ylab="" )
  legend( "left", lty=Lty., lwd=2, col=Colors., legend=Cn, ncol=1, cex=0.85,
          bty = "n")
  par(Opar_)
}

# Define function to create a dot file for plotting with GraphViz
#----------------------------------------------------------------
#' Display a fuzzy model using GraphViz
#'
#' \code{makeDotFile} create a dot file to use in GraphViz and a graphics file
#' displaying the model
#'
#' This function writes out a dot file to use in GraphViz and also a graphics
#' file (in png) produced by dot. You can specify a whole graph or different
#' groups of the graph.
#'
#' @param FileName a string identifying the file path name without an extension
#' (function will add the 'dot' extension for the dot file and 'png' extension
#' for the png file).
#' @param FSDM_ a fuzzy model created using the create FuzzyModel function.
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
#' @return none. The function has the side effect of saving a dot file, used by
#' GraphViz to produce a graphic representation of the model, and a png file
#' that is the graphic representation.
#' @export
makeDotFile <-
  function(FileName, FSDM_, RowGroup = "All", ColGroup = "All",
           orientation = "Portrait", rankdir = "TB", shape = "box",
           Show = "label")
  {
    if (!(Show %in% c("label", "value"))) {
      stop("The value of the 'Show' argument must be 'label' or 'value'")
    }
    if (RowGroup == "All")
      RowGroup <- FSDM_$Group
    if (ColGroup == "All")
      RowGroup <- FSDM_$Group
    Cr <- FSDM_$Cn[FSDM_$Group %in% RowGroup]
    Cc <- FSDM_$Cn[FSDM_$Group %in% ColGroup]
    Relates.CrCc <- FSDM_$Relates[Cr,Cc]
    Cr <- Cr[rowSums(abs(Relates.CrCc)) != 0]
    Cc <- Cc[colSums(abs(Relates.CrCc)) != 0]
    Relates.CrCc <- Relates.CrCc[Cr,Cc]
    Labels.CrCc <- FSDM_$Labels[Cr,Cc]
    Concepts. <- unique(c(Cr,Cc))
    Dot. <-
      paste("digraph {\n orientation =", orientation, ";\n rankdir =", rankdir, ";\n")
    for (concept in Concepts.) {
      Dot. <- paste(Dot., concept, "[ shape =", shape, "];\n")
    }
    for (cr in Cr) {
      for (cc in Cc) {
        Value <- Relates.CrCc[cr,cc]
        if (Show == "label") {
          Label <- Labels.CrCc[cr,cc]
        } else {
          Label <- Value
        }
        if (Value != 0) {
          if (Value > 0) {
            Dot. <- paste0(Dot., cr, " -> ", cc, "[ label=", Label, " ];\n")
          } else {
            Dot. <-
              paste0(
                Dot., cr, " -> ", cc, "[ color=red, fontcolor=red, style=dashed, label=", Label, " ];\n"
              )
          }
        }
      }
    }
    Dot. <- paste(Dot., "}")
    #Save dot file
    DotFile <- paste0(FileName, ".dot")
    writeLines(Dot., DotFile)
    #Write out graphics file
    PngFile <- paste0(FileName, ".png")
    Cmd <- paste0("dot -Tpng ", DotFile, " -o ", PngFile)
    system(Cmd)
  }

#Define a function to make a table documenting concepts
#------------------------------------------------------
#' Make table documenting concepts
#'
#' \code{makeConceptTable} save a tab-delimited text file which documents concepts
#'
#' This function formats model concept information in a table (a tab delimited
#' text file) which documents concept names, corresponding model variable names,
#' and corresponding descriptions.
#'
#' @param ConceptFile a string identifying the file path to the concept
#' decription file.
#' @param Group a string identifying the concept group to document. The default
#' 'All' documents all concepts in the model.
#' @param SaveFile a string identifying the file path name without an extension
#' to use for saving the file. The function adds a 'txt' suffix to the name.
#' @return None. The function has a side effect of saving the concept
#' documentation in a tab-delimited text file.
#' @export
makeConceptTable <- function(ConceptFile, Group="All", SaveFile) {
  ConceptData.. <- fromJSON(ConceptFile)
  if(Group == "All"){
    Concepts.. <- ConceptData..[,c("name", "variable", "description")]
  } else {
    ToSelect. <- ConceptData..$group %in% Group
    Concepts.. <- ConceptData..[ToSelect., c("name", "variable", "description")]
  }
  colnames(Concepts..) <- c("Concept Name", "Variable Name", "Description")
  write.table(Concepts.., file=paste0(SaveFile, ".txt"), col.names=TRUE, row.names=FALSE, sep="\t")
}

#Define a function to make a table describing relationships between concepts
#---------------------------------------------------------------------------
#' Make table documenting causal relationships
#'
#' \code{makeRelationsTable} save a tab-delimited file which documents the
#' causal relationships in a fuzzy model.
#'
#' This function reads the causal relationship information in a fuzzy model and
#' documents it in a table that is saved as a tab-delimited text file.
#'
#' @param ModelFile a string identifying the file path to the model file.
#' @param CauseGroup a string identify the group of causal concepts to include
#' in the table. The default value 'All' selects all concepts.
#' @param EffectGroup a string identifying the group of affected concepts to
#' include in the table. The default value 'All' selects all concepts.
#' @param SaveFile a string identifying the file path name without an extension
#' to use for saving the file. The function adds a 'txt' suffix to the name.
#' @return None. The function has a side effect of saving the concept causal
#' relations documentation in a tab-delimited text file.
#' @export
makeRelationsTable <- function(ModelFile, CauseGroup="All", EffectGroup="All",
                               SaveFile) {
  ModelData.. <- fromJSON(ModelFile)
  #Identify causal and effects concepts
  Cc <- ModelData..$variable[ModelData..$group %in% CauseGroup]
  Ce <- ModelData..$variable[ModelData..$group %in% EffectGroup]
  if(CauseGroup == "All"){
    Relates.. <- ModelData..[,c("variable", "affects")]
  } else {
    ToSelect. <- ModelData..$group %in% CauseGroup
    Relates.. <- ModelData..[ToSelect., c("variable", "affects")]
  }
  Extract_ <- list()
  for(cc in Cc){
    CauseData.. <- Relates..[Relates..$variable == cc, 2][[1]]
    if( nrow(CauseData..) == 0 ) break
    CauseData.. <- CauseData..[CauseData..$variable %in% Ce,]
    CauseData.. <- cbind(cc, CauseData..)
    names(CauseData..) <- c("Concept", "Affected Concept", "Causal Direction", "Causal Weight", "Description")
    Extract_[[cc]] <- CauseData..
  }
  CauseTable.. <- do.call(rbind, Extract_)
  write.table(CauseTable.., file=paste0(SaveFile, ".txt"), col.names=TRUE, row.names=FALSE, sep="\t")
}

#Define function to make table describing value ranges
#-----------------------------------------------------
#' Document concept value range
#'
#' \code{makeValueRangeTable} save a tab-delimited file which documents the
#' concept value ranges in a fuzzy model.
#'
#' This function documents the nominal value ranges for concepts in a fuzzy
#' model. The minimum and maximum assumed values and an explanation for those
#' values is extracted from the model and saved in a tab-delimited file.
#'
#' @param ModelFile a string identifying the file path to the model file.
#' @param Group a string identifying the concept group to document. The default
#' 'All' documents all concepts in the model.
#' @param SaveFile a string identifying the file path name without an extension
#' to use for saving the file. The function adds a 'txt' suffix to the name.
#' @return None. The function has a side effect of saving the concept value
#' range documentation in a tab-delimited text file.
#' @export
makeValueRangeTable <- function(ModelFile, Group="All", SaveFile) {
  ModelData.. <- fromJSON(ModelFile)
  Cn <- ModelData..$variable
  if(Group == "All") {
    Values.. <- ModelData..[,"values"]
    Cx <- Cn
  } else {
    ToSelect. <- ModelData..$group %in% Group
    Values.. <- ModelData..[ToSelect.,"values"]
    Cx <- Cn[ToSelect.]

  }
  Values.. <- cbind(Cx, Values..)
  names(Values..) <- c("Variable", "Minimum", "Maximum", "Description")
  write.table(Values.., file=paste0(SaveFile, ".txt"), col.names=TRUE,
              row.names=FALSE, sep="\t")
}

#Function to save summary table of results
#-----------------------------------------
#' Save summary table of results
#'
#' \code{saveSummaryTable} save a tab-delimited file that tabulates starting
#' and ending values and percentage change for all concepts for a scenario
#'
#' The function creates a table of model results for a scenario that includes
#' the starting value of each concept, the ending value, and the percentage
#' change from the starting value. This table is saved as a tab-delimited file.
#'
#' @param MResults_ls a list containing the results produced by application of
#' the "runFuzzyModel" function.
#' @param File a string identifying the relative path name for the file to be
#' saved.
#' @return None. Function saves a tab-delimited file of summary model results.
#' @export
saveSummaryTable <- function(MResults_ls, File) {
  Data <- MResults_ls$ScaleSummary
  Start <- as.vector(head(Data, 1))
  End <- round(as.vector(tail(Data, 1)), 2)
  PctChg <- round( 100 * (End / Start - 1), 1)
  OutTable <- data.frame(list(Start = Start, End = End, Change = PctChg))
  rownames(OutTable) <- colnames(TestResults$ScaleSummary)
  write.table(OutTable, file = File, row.names = TRUE, col.names = TRUE, sep = "\t")
}

