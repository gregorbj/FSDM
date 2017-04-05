#fuzzy_systems_dynamics_modeler.r
#Version 0.4

#Copyright 2014-2015 Brian Gregor, Oregon Systems Analytics LLC

#Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

#http://www.apache.org/licenses/LICENSE-2.0

#Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.


#Overview
#========
# This script implements an alternative formulation for fuzzy cognitive map (FCM) models. Fuzzy cognitive maps are models of systems of interrelated concepts represented graphically as directed graphs and programmatically as adjacency matrices. The nodes of the graph represent concepts and the directed edges represent causal effects. Weights on the edges represent the strength of the effect. Fuzzy congnitive maps are applied to problem domains that have system characteristics but are relatively data poor and so specifying the system depends on expert knowledge. 
# In standard fuzzy cognitive maps the causal linkage is between the value of one concept and the value of another concept mediated by the weight on the causal link. In the case where a node is affected by one other node, the value of the affected node is the product of the value of the affected node and the weight of the causal link. When a node is affected by more than one link, the value is the sum of the products of the values of the respective causal links and nodes. This resulting value is processed by what is commonly called an activation function to limit the value of the node to the interval (0,1]. There are several semantic issues with standard fuzzy cognitive map representations of systems.  A number of these have been documented by Carvalho (Joao Paulo Carvalho, On the Semantics and the Use of Fuzzy Cognitive Maps in Social Sciences, WCCI 2010 IEEE World Congress on Computational Intelligence, July 18-23, 2010 - CCIB, Barcelona, Spain).
# In the new fuzzy cognitive map formulation implemented by this script, rather than the value of the concepts affecting one another, it is the change in the values of concepts that affect each other. The link weights can therefore be thought of as elasticities. The joint effect of multiple causal nodes on an affected node is calculated as the product of the individual proportional effects (e.g. 1.1 * 0.8 * 0.9). The activation function is replaced by a sensitivity function which conditions the change in an affected node as a function of the value of the affected node. For example, in the prototypical model of the diffusion of new technology, market share over time is sigmoidal (e.g. logistic) with the rate of change in market share being highest when the market share is at the midpoint of the maximum market share and lowest when market share is near zero or near the maximum market share. The sensitivity functions for a concept which follows this pattern would be concave downward with values near zero at concept levels of 0 and 100 and one at a concept level of 50.


#Load resources
#==============

library(RColorBrewer)


#Read in a fuzzy model
#=====================

#Define a function that creates a sensitivity response curve from a set of input parameters describing the curve
#---------------------------------------------------------------------------------------------------------------
#The sensitivity of a concept to change can be made to vary depending on the concept level. For example in a situation where a concept exhibits diminishing returns or market saturation, the sensitivity of the concept would decrease towards zero as the level of the concept approaches 100.  Concept sensitivity is represented using trapezoidally shaped functions that are described by a vector of 4 parameters. The first parameter specifies the value when the concept level is 0. A value less than 0 produces a flat leading edge value of 0.01. The larger the negative value, the longer the leading edge. The second parameter establishes where the sensitivity level reaches a value of 1. The third parameter is the concept level at which the value starts falling below 1 at the top end. The fourth parameter specifies the value when the concept level is 100. 
#This function takes a vector of the 4 parameters and returns a function which calculates a sensitivity value for any input between 0 and 100. The function will also plot the sensitivity function is the "Plot" argument is set to TRUE.  
#Arguments:
#SensParm. - A vector composed of 4 values as described above
#Plot - If Plot = TRUE the function plots the sensitivity function response over the range of 0:100.
#Return:
#A function that returns the sensitivity value for one or more input values.
makeSensitivityFunction <- function( SensParm., Plot=FALSE ) {
  A <- SensParm.[1]
  B <- SensParm.[2]
  C <- SensParm.[3]
  D <- SensParm.[4]
  SensResponse. <- numeric( length(0:100) )
  for( i in 0:100 ) {
    X <- i
    if( X < B ) Y <- X * ( 1 - A ) / B + A
    if( X >= B & X <= C ) Y <- 1
    if( X > C ) Y <- ( X - C ) * ( D - 1 ) / ( 100 - C ) + 1
    if( Y < 0.01 ) Y <- 0.01
    SensResponse.[i + 1] <- Y
    rm( X, Y )
  }
  names( SensResponse. ) <- 0:100
  if( Plot ) {
      plot( 0:100, SensResponse., ylim=c(0,1),
            main=paste( "Sensitivity Response Function with Parameters\nA =", A, ", B =", B, ", C =", C, ", D = ", D ) )
  }          
  function( X ) {
    SensResponse.[ as.character( round( X ) ) ]
  }
}

#Define a function to create a model from input data
#---------------------------------------------------
#A model is fully described by 3 input text files in csv format.  The first file is an adjacency matrix of the causal relationships between causing concept (rows) and affected concept (columns).  The numbers in the matrix can be values between -1 and 1.  The rownames and columnames need to be the names or abbreviations of the names of the concepts.  All of the input files need to be labeled with these same names.  The second file is a matrix of the concept sensitivity parameters where the rows correspond to the concepts and the columns (A,B,C,D) correspond to the four parameters described above.  The third is a matrix which contains the starting levels for the concepts and the starting changes. The rows are the concepts, the first column is the starting concept values (StartValues) and the second column is the starting concept changes (StartChanges).
#Arguments:
#RelatesFile - The path name of the file which contains an adjacency matrix of the causal relationships.
#SensitivityFile - The path name of the file which contains the parameters for the sensitivity functions.
#StartsFile - The path name of the file which contains the starting values and starting changes for all of the concepts.
#Return:
#A list which contains all of the FCM model information.
createFuzzyModel <- function( RelatesFile, SensitivityFile, StartsFile ) {
  # Load relationships table, check values, and convert to matrix
  #--------------------------------------------------------------
  Relates.CnCn <- as.matrix( read.csv( RelatesFile, row.names=1, as.is=TRUE ) )
  Cn <- rownames( Relates.CnCn )
  # Check that row and column names are the same
  if( any( colnames( Relates.CnCn ) != Cn ) ) {
    stop( "Row and columns names in the relationships table do not correspond." )
  }
  # Check that relations are between -1 and 1
  if( any( Relates.CnCn < -1 ) | any( Relates.CnCn > 1 ) ) {
    stop( "One or more values in the RelatesFile are outside the acceptable range (-1 to 1)" )
  }
  # Load sensitivity function parameters and create vector of sensitivity functions
  #--------------------------------------------------------------------------------
  Sensitivity.CnP <- as.matrix( read.csv( SensitivityFile, row.names=1, as.is=TRUE ) )
  # Check that rownames correspond to names in the relationships file
  if( any( rownames( Sensitivity.CnP ) != Cn ) ) {
    stop( "The rownames of the SensitivityFile don't correspond to the names in the RelatesFile." )
  }
  # Check that the sensitivity function parameters are within bounds
  if( any( Sensitivity.CnP[,c(1,4)] > 1 ) ) {
    stop( "Values for parameters A and D can not exceed 1" )
  }
  if( any( Sensitivity.CnP[,c(2,3)] < 0 ) | any( Sensitivity.CnP[,c(2,3)] > 100 ) ) {
    stop( "Values for parameters B and C can't be less than 0 or greater than 100" )
  }
  Sensitivity.Cn <- apply( Sensitivity.CnP, 1, makeSensitivityFunction )
  # Load table of starting concept values, check values, and create vectors for each
  #---------------------------------------------------------------------------------
  Starts.. <- read.csv( StartsFile, row.names=1, as.is=TRUE )
  # Check that rownames correspond to names in the relationships file
  if( any( rownames( Starts.. ) != Cn ) ) {
    stop( "The rownames of the StartsFile don't correspond to the names in the RelatesFile." )
  }
  # Create vectors for start value and start changes
  StartValues.Cn <- Starts..$StartValue
  StartChanges.Cn <- Starts..$StartChange
  names( StartValues.Cn ) <- names( StartChanges.Cn ) <- Cn
  # Check that the starting values are in range of 0 to 100
  if( any( StartValues.Cn < 0 ) | any( StartValues.Cn > 100 ) ) {
    stop( "Values for StartValues can't be less than 0 or greater than 100" )
  }
  # Warn if any starting values are 0 because there will be no changes for those values
  if( any( StartValues.Cn == 0 ) ) {
      ZeroValueConcepts. <- Cn[ StartValues.Cn == 0 ]
      sapply( ZeroValueConcepts., function(x) {
        Msg <- paste( x, "has starting 0 value. It will remain 0 unless increased to value greater than 0." ) 
        warning( Msg ) } )
  }
  # Check that no starting changes are less than -100 (can't have more than 100% decrease)
  if( any( StartChanges.Cn < -100 ) ) {
    stop( "Values for StartChanges can't be less than -100." )
  }
  # If any of the starting changes will cause concept values to exceed 100, reduce to keep within bound and warn
  FirstIterConceptValues. <- StartValues.Cn * ( 1 + StartChanges.Cn/100 )
  if( any( FirstIterConceptValues. > 100 ) ) {
      TooHighChanges. <- Cn[ FirstIterConceptValues. > 100 ]
      for( Name in TooHighChanges. ) {
        StartValue <- StartValues.Cn[ Name ]
        RevChange <- floor( 100 / StartValue - 1 )
        StartChanges.Cn[ Name ] <- RevChange
        Msg <- paste( "Starting change for", Name, 
                      "will make initial value exceed 100. Starting percentage change reduced to", 
                      RevChange * 100 )
        warning( Msg ) 
        rm( StartValue, RevChange, Msg )
      }
  }
  # Return all the model components in a list
  list( Relates=Relates.CnCn, Sensitivity=Sensitivity.Cn, StartValues=StartValues.Cn, StartChanges=StartChanges.Cn )
}
  

#Define functions to run a fuzzy model and to display the results
#================================================================

#Define function to calculate the initial effects of inputs on each concept
#--------------------------------------------------------------------------
#This function calculates the initial percentage changes affecting each concept resulting from the StartChanges specified for the model.
#Arguments:
#FuzzyModel_ - a FCM model list that was created using the createFuzzyModel function.
#Return: A vector of the percentage change in the effect on each concept
calcInitEffects <- function( LevelChg.Cn, FuzzyModel_ ) {
  # Convert percentages into proportions
  InputChg.Cn <- LevelChg.Cn / 100
  # Calculate the component changes
  Changes.CnCn <- sweep( FuzzyModel_$Relates, 1, InputChg.Cn, "*") 
  # Calculate the product of the changes
  Changes.Cn <- apply( Changes.CnCn + 1, 2, prod ) - 1
  # Return the result
  Changes.Cn * 100
}

#Define a function which adjusts the response to inputs based on the level and sensitivity
#-----------------------------------------------------------------------------------------
#Arguments:
#InputChg.Cn: An input vector of the unadjusted percentage changes calculated for each concept
#Level.Cn: The level of each concept in the range of 0 to 100.
#FuzzyModel_ - a FCM model list that was created using the createFuzzyModel function.  
#Return: A vector of the percentage change that will be applied to each concept.
adjustResponse <- function( InputChg.Cn, Level.Cn, FuzzyModel_ ){
  # Calculate the sensitivity of each concept to change
  Sensitivities_Cn <- FuzzyModel_$Sensitivity
  Cn <- names( Sensitivities_Cn )
  SensResponse.Cn <- numeric( length(Cn) )
  names( SensResponse.Cn ) <- Cn
  for( cn in Cn ) {
    SensResponse.Cn[cn] <- Sensitivities_Cn[[cn]]( Level.Cn[cn] )
  }
  # Calculate the adjusted input change
  InputChg.Cn * SensResponse.Cn
}

#Define a function to update the level of each concept based on the input percentage change
#------------------------------------------------------------------------------------------
#Name: adjustLevel
#Arguments:
#InputChg.Cn: An input vector of the percentage change in the level of each concept
#Level.Cn: The level of each concept
#Return: The new level of each concept
adjustLevel <- function( InputChg.Cn, Level.Cn ) {
  AdjLevel.Cn <- Level.Cn * ( 1 + InputChg.Cn / 100 )
  # Constrain to range of 1 to 100
  AdjLevel.Cn[ AdjLevel.Cn < 1 ] <- 1
  AdjLevel.Cn[ AdjLevel.Cn > 100 ] <- 100
  # Return the result
  AdjLevel.Cn
}

#Define a function that runs the fuzzy cognitive map model
#---------------------------------------------------------
#This function runs a FCM model that was created using the createFuzzyModel function.  
#Name: runCognitiveMapModel
#Arguments: 
#FuzzyModel_ - a FCM model list that was created using the createFuzzyModel function.
#MaxIter: The maximum number of iterations.
#Return:
#A matrix containing the concept level values (columns) for each iteration (rows).
runFuzzyModel <- function( FuzzyModel_, MaxIter=100 ){
  # Set up list to store calculations for each iteration
  Results_ <- list()
  # Iterate the model
  for( i in 1:MaxIter ) {
    if( i == 1 ){
      PrevLevels.Cn <- FuzzyModel_$StartValues
      Levels.Cn <- PrevLevels.Cn * ( 1 + FuzzyModel_$StartChanges / 100 )
      Results_[[i]] <- Levels.Cn
    } else {
      LevelChg.Cn <- 100 * ( Levels.Cn - PrevLevels.Cn ) / PrevLevels.Cn
      LevelChg.Cn[is.nan(LevelChg.Cn)] <- 0
      InitEffect.Cn <- calcInitEffects( LevelChg.Cn, FuzzyModel_ )
      AdjEffects.Cn <- adjustResponse( InitEffect.Cn, Levels.Cn, FuzzyModel_ )
      PrevLevels.Cn <- Levels.Cn
      Levels.Cn <- adjustLevel( AdjEffects.Cn, Levels.Cn )
      Results_[[i]] <- Levels.Cn
      # If absolute values of all of the adjusted effects are less than 0.01 then break out of the loop
      if( all( abs(AdjEffects.Cn) < 0.001 ) & i > 10 ) break
      
    }
  }
  # Put the results into a matrix and return
  Results.ItCn <- rbind( FuzzyModel_$StartValues, do.call( rbind, Results_ ) )
  rownames( Results.ItCn ) <- 0:( nrow(Results.ItCn)- 1 )
  Results.ItCn
}

#Define a function to plot the level values of each concept for all iterations
#-----------------------------------------------------------------------------
#Name: plotIterationResponse
#Argments:
#Results.ItCn - The results returned by runFuzzyModel.  This is a matrix containing the level values for each iteration where iterations are rows and concepts are columns.
#... - The function can take any other standard named arguments used by the plot function
#Return:
#None.  The function has a side effect of producing a plot.
plotIterationResponse <- function( Levels.ItCn, ... ){
  Cn <- colnames( Levels.ItCn )
  XAxisLabels. <- rownames( Levels.ItCn )
  YRange. <- range( min(Levels.ItCn), max(Levels.ItCn) )
  if( length(Cn) < 3 ) {
    Colors. <- c( "red", "blue" )
  } else {
    Colors. <- brewer.pal( length(Cn), "Set1" )
  }
  layout( matrix(1:2, ncol=1), widths=6, heights=c(6,2.5) )
  matplot( as.numeric( rownames(Levels.ItCn ) ), Levels.ItCn, 
           type="l", lwd=2, lty=1, col=Colors., 
           xlab="Iteration", ylab="Percent of Maximum",
           ... )
  Opar_ <- par( mar=c(1,2,1,2) )
  plot( 0, 0, type="n", axes=FALSE, xlab="", ylab="" )
  legend( "topleft", lty=1, lwd=2, col=Colors., legend=Cn, ncol=3, cex=0.8 )
  par(Opar_)
}

