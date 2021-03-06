% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DRE.R
\docType{class}
\name{DRE-class}
\alias{DRE-class}
\title{An S4 class representing a dose-response experiment of an individual compound or a mixture}
\description{
An S4 class representing a dose-response experiment of an individual compound or a mixture
}
\section{Slots}{

\describe{
\item{\code{name}}{dose-response experiment name, character}

\item{\code{compound.name}}{vector of compound names, character}

\item{\code{fraction}}{vector of compound fractions, numeric, should add up to 1}

\item{\code{dose}}{vector of doses, numeric}

\item{\code{response}}{vector of responses, e.g. OD, numeric}

\item{\code{control.response}}{vector of [untreated] control responses, numeric}

\item{\code{effect}}{fractional effect of the drug(s), can range from 0 for untreated control to 1 for complete growth inhibition}

\item{\code{model}}{named list of fitted models of the dose-response curve, list of objects of class DRModel}

\item{\code{ic25}}{IC25 value inferred from the model, numeric}

\item{\code{ic50}}{IC50 value inferred from the model, numeric}

\item{\code{ic75}}{IC75 value inferred from the model, numeric}

\item{\code{true.ic50}}{"true" IC50 value, can be set for simulation purposes, numeric}

\item{\code{true.m}}{"true" slope parameter of the Hill's equation, can be set for simulation purposes, numeric}

\item{\code{comment}}{a vector of diagnostic messages and other comments, character}

\item{\code{dilution.factor}}{dilution factor, defined for exponential dose series}
}}
\examples{
data(sim15_screen3)
dre(sim15_screen3,"Cpd1")
plot(dre(sim15_screen3,"Cpd1"))

}

