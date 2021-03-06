% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fsdm.R
\name{formatConceptTable}
\alias{formatConceptTable}
\title{Formats a concept table to be displayed in the GUI.}
\usage{
formatConceptTable(Concepts_df)
}
\arguments{
\item{Concepts_df}{a data frame containing the concepts data.}
}
\value{
a data frame containing the concepts data to be shown in a table.
}
\description{
\code{formatConceptTable} formats a concept data frame to be displayed as a
table in the Logic Laboratory GUI.
}
\details{
The GUI summarizes information about model concepts in a table. Not all of
the concept data needs to be shown and some of the data is difficult to
show in table form. This function extracts and formats the concept data that
is to be displayed in a table.
}
