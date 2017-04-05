# FSDM
Fuzzy Systems Dynamics Model Implemented with a Graphical User Interface

This project is the latest version of the Fuzzy Systems Dynamics Model (FSDM) and includes a graphical user interface for building and running FSDM models. It includes technical documentation of the FSDM modeling approach and a user's guide for the GUI software.

The Fuzzy Systems Dynamics Model is an alternative formulation of fuzzy cognitive map (FCM) models. Fuzzy cognitive maps are models of systems of interrelated concepts represented graphically as directed graphs and mathematically as adjacency matrices.  The nodes of the graph represent concepts and the directed edges represent causal effects.  Weights on the edges represent effect strengths.  Fuzzy congnitive maps are applied to problem domains that have system characteristics but are relatively data poor and so specifying the system depends on expert knowledge. This GUI is being built to make it easier for people to create and run FSDM models. The GUI supports model documentation and data checking.

The FSDM approach follows the general approach in FCMs of representing concepts and connections between concepts using a directed graph and adjacency matrix.  It departs from the FCM in three important ways.  First, the effect of a concept on another is through the change in its value.  Edge weights can therefore be thought of as elasticities.  Second, in standard FCMs, when multiple concepts affect a concept, their combined effect is determined through summing the individual effects.  Since the FSDM works with percentage changes instead of absolute changes, the standard mathematical approach to combining percentage changes is used. Third, the activation function is replaced by sensitivity functions for causal and affected concepts. These functions modify the edge weights based on the respective values of the causal and affected concepts.

Documentation of the FSDM and GUI software is contained in the 'documentation' directory.

The FSDM GUI software is in the 'fsdm-app' directory. Note that to run the GUI, the R language must be installed as well as the following R packages: shiny, shinyBS, jsonlite, DT. If you also have R Studio installed, the application can be started by opening the "fsdm.Rproj" in R Studio and clicking on the "RunApp" button.

Example models can be found in the 'models' directory.

Legacy code corresponding to the journal article "Potential changes to travel behaviors & patterns: a fuzzy cognitive map modeling approach" published in Transportation 42(6) October 2015, is contained in the 'archive' directory.
