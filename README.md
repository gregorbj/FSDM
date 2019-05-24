# FSDM  
Fuzzy Systems Dynamics Modeling Package

This project is the latest version of the Fuzzy Systems Dynamics Modeling (FSDM) package. It includes a graphical user interface (Logic Laboratory) for building FSDM models, specifying scenarios, running the model on the scenarios, and displaying results. Using the GUI to build models and scenarios assures that the model and scenario files are properly formatted. It also simplifies the process of running the models and analyzing the results. Advanced users may also run model scenarios programmatically; loading models with the createFuzzyModel function, loading scenarios with the createFuzzyScenario function, and running the model with the runFuzzyModel function.

## Installing the Package and Dependencies
The Fuzzy Systems Dynamics Modeling package is a package for the R computing language environment. The incorporation of the FSDM into an R package simplifies installation of all the software needed to use it. The package was developed using the latest version of R at the time (3.6.0) and has not been tested with older versions. The R language can be installed from www.r-project.org. It is free and open source. Once R has been installed the FSDM by starting the R console and copying and pasting the following lines of code into it:

```
#Set the CRAN mirror for installing package dependencies
#Feel free to change to different mirror if desired
if (is.null(getOption("repos")["CRAN"])) {
  options(repos = c(CRAN = "https://cran.rstudio.com/"))
}
#Install the devtools package if not already installed
#Makes it easier to install FSDM package from GitHub and dependencies from CRAN
if (!library("devtools", logical.return = TRUE)) install.packages("devtools")
#Install the FSDM package from GitHub
#This also installs all other packages that FSDM depends on
devtools::install_github("gregorbj/FSDM")

```

## Running 
You may wish to start by first creating a project folder and then copying the demonstration models and scenarios from the installed package to your project folder. Once you have created your project folder, you can copy the demonstration models/scenarios to it by entering the following in the R console. Replace the *"PROJECT_FOLDER_PATH"* in the code with the full path to the project folder (e.g. "C:/My_FSDM_Project").
```
copyDemoModels("PROJECT_FOLDER_PATH")
```

The Logic Laboratory graphical user interface can be started by entering the following in the R console:
```
FSDM::runLogicLab()
```

Starting the Logic Laboratory in this way will open up the application in a web browser window using the default web browser for your computer. Note that the application does not work correctly in the Microsoft Edge browser because the graphs showing results will not be displayed. The application does work in the Chrome, Firefox, and Brave browsers. It may work in other browsers as well but has not been tested. If your default browser is Microsoft Edge, you will need to change it to one of the others.

Note that after you close the browser window that the Logic Laboratory is running in, when you return focus to the R console window you need to press the escape key on your keyboard to return to the console command line.

The users guide in the *documentation* folder of the installed package provides information on how to use the Logic Laboratory. It is out of date with respect to some of the features and look of some of the screens, but will provide you with some basic orientation. It will be updated in the coming weeks and made available as a standard R vignette that will be easy to call up from the R console.


