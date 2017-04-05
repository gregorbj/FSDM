#Sensitivity_Test_Joint.R
#========================
#Author: Alex Bettinardi

#This script creates and models 216,000 scenarios from combinations of changes in the nine technology concepts in the fuzzy cognitive map model. The change levels for these concepts are defined in the "Starts_Levels.csv" file in the "inputs" directory. All combinations of levels are modeled. The results are saved in a file called "ManyRuns.csv" in the "results" directory. The time to model all 216,000 scenarios is reported out.

#Source in fuzzy systems dynamics modeler functions
#--------------------------------------------------
source("../code/fuzzy_systems_dynamics_modeler.R" )

#Create a base model
#-------------------
BaseModel <- createFuzzyModel( "inputs/Relations.csv", "inputs/Sensitivities.csv", "inputs/Starts.csv" )

#Read in the table of concept levels and create a data frame of all combinations
#-------------------------------------------------------------------------------
levels.. <- read.csv("inputs/Starts_Levels.csv", row.names=1, as.is=TRUE)
Levels_ <- apply(levels..[,grep("Level", colnames(levels..))], 1, function(x) x[!is.na(x)])
scens.. <- expand.grid( lapply(Levels_,function(x) gsub("Level","",names(x))) )

#Run model on all combinations
#-----------------------------
#Create data frame to store the resulting concept values for each scenario
results.. <- matrix(0,nrow=nrow(scens..), ncol=ncol(scens..))
colnames(results..) <- paste(names(scens..), "FinalIter", sep="_")
#Run the model and save the results. 
#Run time of 23.5 minutes on laptop with Intel i7 cpu and 8 GB ram
system.time({
  for(i in 1:nrow(scens..)){
    scenInputs <- mapply(function(x,y) y[x],scens..[i,],Levels_)
    BaseModel$StartChanges[] <- scenInputs
    Results <- runFuzzyModel(BaseModel)
    results..[i,] <- Results[nrow(Results),]
  }
})

#Save the results for analysis
#-----------------------------
JointSensitivityResults.. <- cbind(scens.., results..)
write.csv(JointSensitivityResults.., "results/JointSensitivityResults.csv")
save(JointSensitivityResults.., file="results/JointSensitivityResults.RData")
    
