# FSDM_GUI
R Shiny GUI for Fuzzy Systems Dynamics Model

# Installation
  1. Install [R](https://cran.r-project.org) in a location where you have write access.
  2. Start R
  3. Run the following commands to download and install the required libraries and their dependencies:

```
install.packages("shiny")
install.packages("shinyBS")
install.packages("DT")
install.packages("ggplot2")
install.packages('DiagrammeR')
```

# Running FSDM_GUI
  1. Start R
  2. Run the following commands to download and run the FSDM GUI:

```
library("shiny")
runGitHub( "bstabler/FSDM_GUI", "bstabler")
```
