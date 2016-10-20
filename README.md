# FSDM_GUI
R Shiny GUI for Fuzzy Systems Dynamics Model

This project is a graphical user interface to the fuzzy systems dynamics model written in R using the R Shiny library. The Fuzzy Systems Dynamics Model is an alternative formulation of fuzzy cognitive map (FCM) models. Fuzzy cognitive maps are models of systems of interrelated concepts represented graphically as directed graphs and mathematically as adjacency matrices.  The nodes of the graph represent concepts and the directed edges represent causal effects.  Weights on the edges represent effect strengths.  Fuzzy congnitive maps are applied to problem domains that have system characteristics but are relatively data poor and so specifying the system depends on expert knowledge. This GUI is being built to make it easier for people to create and run FSDM models. The GUI supports model documentation and data checking. 

All aspects of the GUI have now been implemented.

To use this project R and Shiny must be installed. The following R packages must also be installed: shiny, shinyBS, jsonlite, DT. The application can be started by opening the "fsdm.Rproj" in R Studio and clicking on the "RunApp" button. 
