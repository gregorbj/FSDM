#Sensitivity_Test_Individual.R
#=============================
#Author: Brian Gregor

#This script tests the sensitivity of each of the concepts to incremental changes in each technology concept. Each concept is increased from its starting value to its maximum value in 10 increments.  Values of the concepts at the end of one increment became the starting values for the next increment. Non-auto trips and vehicle miles are graphed. Elasticities between each concept and all other concepts are computed and graphed. Results are saved in "results" directory.

#Source in fuzzy systems dynamics modeler functions
#--------------------------------------------------
source("../code/fuzzy_systems_dynamics_modeler.R" )

#Create a base model
#-------------------
BaseModel <- createFuzzyModel( "inputs/Relations.csv", "inputs/Sensitivities.csv", "inputs/Starts.csv" )
#Names of all of the concepts (Ca for concepts all)
Ca <- rownames(BaseModel$Relates)
#Names of concepts that are treated as inputs for the test (Ci for concepts inputs)
Ci <- Ca[1:9]
#Names of concepts that are treated as outputs for the test (Co for concepts outputs)
Co <- Ca[10:13]
          
#Run model for all scenarios using the assumed sensitivities
#-----------------------------------------------------------
#Run 10 iterations for each input concept where each scenario has different starting change for concept but not any other concepts.  The starting values for each iteration build off of the ending values for each previous iteration.  The starting change for the iteration is calculated as that constant percentage change in the concept that would result in a value of 100 for the concept for the remaining number of periods.
calcIncrementChg <- function( StartVal, EndVal=100, NumPeriod ) {
     ( EndVal / StartVal )^( 1 / NumPeriod ) - 1 }     
Results_Ca <- list()
for( ci in Ci ) {
     StartVals.CaI <- array( 0, dim=c(length(Ca),10), dimnames=list(Ca,1:10) )
     StartVals.CaI[,1] <- BaseModel$StartValues
     EndVals.CaI <- array( 0, dim=c(length(Ca),10), dimnames=list(Ca,1:10) )
     StartChanges.I <- numeric(10)
     PctChg.CaI <- array( 0, dim=c(length(Ca),10), dimnames=list(Ca,1:10) ) 
     Elasticity.CaI <- array( 0, dim=c(length(Ca),10), dimnames=list(Ca,1:10) )
     for( i in 1:10 ) {
          NumPeriod <- (10 + 1) - i
          PctChg <- 100 * calcIncrementChg( StartVals.CaI[ci,i], 100, NumPeriod=NumPeriod )
          StartChanges. <- BaseModel$StartChanges
          StartChanges.[ci] <- PctChg
          StartChanges.I[i] <- PctChg
          Model <- BaseModel
          Model$StartChanges <- StartChanges.
          Model$StartValues <- StartVals.CaI[,i]
          ModelResults <- runFuzzyModel(Model)
          Values.Ca <- ModelResults[ nrow(ModelResults), ]
          if( i != 10 ) StartVals.CaI[,i+1] <- Values.Ca
          EndVals.CaI[,i] <- Values.Ca
          PctChg.CaI[,i] <- 100 * ( Values.Ca - StartVals.CaI[,i] ) / StartVals.CaI[,i]
          Elasticity.CaI[,i] <- PctChg.CaI[,i] / PctChg
     }
     Results_Ca[[ci]]$StartChanges.I <- StartChanges.I
     Results_Ca[[ci]]$StartVals.CaI <- StartVals.CaI
     Results_Ca[[ci]]$EndVals.CaI <- EndVals.CaI
     Results_Ca[[ci]]$PctChg.CaI <- PctChg.CaI
     Results_Ca[[ci]]$Elasticity.CaI <- Elasticity.CaI
}

#Plot elasticities
#-----------------
Abbr. <- c( "Autonomous\nVehicles", "Connected\nVehicles", "Development\nDensity", 
            "Demand Responsive\nTransit", "Fixed Route\nTransit", 
            "Intelligent\nInfrastructure", "Low Speed\nInfrastructure", 
            "LW Personal\nTransportation", "Mobile\nTechnologies",
            "Relatively Short\nAuto Convenience", "Relatively Long\nAuto Convenience",
            "Non-Auto Trips", "Vehicle Miles\nTraveled" ) 
pdf( file="results/concept_elasticities.pdf", width=11, height=8 )
par( mfrow=c(3,5), oma=c(1,1,3,1) )
for( i in 1:length(Ci) ) {
     for( j in 1:length(Ca) ) {
          ci <- Ci[i]
          cj <- Ca[j]
          if( i == j ) {
              Ylim <- c(0.95, 1.5)
          } else {
              Ylim <- c(-0.75, 0.75)
          }
          StartVals. <- Results_Ca[[ci]]$StartVals.CaI[ci,]
          plot( StartVals., Results_Ca[[ci]]$Elasticity.CaI[cj,], type="b", 
                ylim=Ylim, xlab="Starting Value", ylab="Elasticity",
                main=Abbr.[j] )
                abline( h=0, col="grey" )
          if( i == j ) {
              abline( h=1, col="red", lty=2 )
          } else {
              abline( h=BaseModel$Relates[ci,cj], col="red", lty=2 )
          }
          if(j == 9) plot( 0, 0, type="n", axes=FALSE, xlab="", ylab="" )
     }
     plot( 0, 0, type="n", axes=FALSE, xlab="", ylab="" )
     mtext( side=3, outer=TRUE, line=1, text=paste("Elasticity With Respect to Change in", ci) )
}
dev.off()

#Plot relationship of non-auto trips to each input concept
#---------------------------------------------------------
Abbr2. <- Abbr.[1:9]
Cn <- Ci 
win.metafile( file="results/nonauto_trips.wmf", width=6, height=4 )
par( mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4.5,4.5,3,3) )
for( n in 1:length(Cn) ) {
    cn <- Cn[n]
    Inputs. <- Results_Ca[[cn]]$EndVals.CaI[cn,]
    Outputs. <- Results_Ca[[cn]]$EndVals.CaI["Non_Auto_Trips",]
    plot( Inputs., Outputs., xlab="", ylab="", xlim=c(0,100), ylim=c(12,30), 
          type="l", lwd=2, axes=FALSE, col="red" )
    box()
    text( 50, 12, labels=Abbr2.[n], pos=3, cex=1 )
    if( n == 1 ) axis(side=2) 
    if( n == 2 ) axis(side=3) 
    if( n == 6 ) axis(side=4) 
    if( n == 7 ) axis(side=2)
    if( n == 7 ) axis(side=1)
    if( n == 9 ) axis(side=1) 
}
mtext( side=1, outer=TRUE, line=3, text="Concept Level", cex=0.9 )
mtext( side=2, outer=TRUE, line=3, text="Non-Motorized Trip Concept Level", cex=0.9 )
dev.off()

#Plot relationship of VMT to each input concept
#----------------------------------------------
win.metafile( file="results/vmt.wmf", width=6, height=4 )
par( mfrow=c(3,3), mar=c(0,0,0,0), oma=c(4.5,4.5,3,3) )
for( n in 1:length(Cn) ) {
    cn <- Cn[n]
    Inputs. <- Results_Ca[[cn]]$EndVals.CaI[cn,]
    Outputs. <- Results_Ca[[cn]]$EndVals.CaI["VMT",]
    plot( Inputs., Outputs., xlab="", ylab="", xlim=c(0,100), ylim=c(47,100), 
          type="l", lwd=2, axes=FALSE, col="red" )
    box()
    text( 50, 47, labels=Abbr2.[n], pos=3, cex=1 )
    if( n == 1 ) axis(side=2) 
    if( n == 2 ) axis(side=3) 
    if( n == 6 ) axis(side=4) 
    if( n == 7 ) axis(side=2)
    if( n == 7 ) axis(side=1)
    if( n == 9 ) axis(side=1) 
}
mtext( side=1, outer=TRUE, line=3, text="Concept Level", cex=0.9 )
mtext( side=2, outer=TRUE, line=3, text="Vehicle Miles Traveled Trip Concept Level", cex=0.9 )
dev.off()
