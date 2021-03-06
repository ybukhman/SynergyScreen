% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeWide.R
\docType{methods}
\name{writeWide}
\alias{writeWide}
\alias{writeWide,ScreenDesign-method}
\title{Write out a data frame-like object in a wide format}
\usage{
writeWide(object, ...)

\S4method{writeWide}{ScreenDesign}(object, file = "", blank = "blank",
  untreated = "untreated", nothing = "NULL", ...)
}
\arguments{
\item{object}{a ScreenDesign object}

\item{...}{other parameters to pass to write.table}

\item{file}{either a character string naming a file or a connection open for writing. "" indicates output to the console.}

\item{blank}{string to use for blank wells}

\item{untreated}{string to use for untreated control wells, e.g. the name of the growth medium}

\item{nothing}{string to use in cases where nothing should be added to a well}
}
\description{
Write out a data frame-like object in a wide format
}
\details{
A ScreenDesign object is a data frame in a "long" format.  This function converts it to wide, e.g. 8X12 for a 96 well plate,
then writes it out to a file in CSV format
}
\section{Methods (by class)}{
\itemize{
\item \code{ScreenDesign}: Write out screen design data in a wide format
}}
\examples{
compounds = readCompoundFile(system.file("extdata/8_compounds/8_compounds.csv",
                                         package="SynergyScreen"))
screen = new("SynergyScreen", compound_list=compounds)
screen = generateDesign(screen, type="12 doses exp")
writeWide(design(screen), file="8_compounds_design_wide.csv")

}
\seealso{
\code{\linkS4class{ScreenDesign}}, \code{\link[utils]{write.table}}
}

