% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsdm.R
\name{formatOutputData}
\alias{formatOutputData}
\title{Create data frame of selected scenarios and concepts to plot and save}
\usage{
formatOutputData(ModelDir, ModelName, Sc, Vn)
}
\arguments{
\item{ModelDir}{a string identifying the path to the models folder in which}

\item{ModelName}{a string representation of the model name.}

\item{Sc}{a vector of the names of scenarios to include.}

\item{Vn}{the variable names for the concepts to include.}
}
\value{
A data frame having columns identifying the scenario, concept,
iteration, scaled values, and rescaled values.
}
\description{
\code{formatOutputData} makes a data frame of the summary results for selected
scenarios and selected concepts.
}
\details{
This function creates a data frame of model results for selected scenarios
and selected concepts. The data frame is in 'flat' format where values are
in one column and the corresponding concept names, scenario names, and
iterations are in separate columns.
}
