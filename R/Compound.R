#  Compound class and methods

#### Compound class ####
#' An S4 class to represent a compound
#' 
#' @slot name compound name, character
#' @slot min.dose minimum dose to be used in the dose-response curves, numeric
#' @slot max.dose maximum dose to be used in the dose-response curves, numeric
#' @slot ic50 IC50 value, numeric
#' @slot m slope parameter in the Hill's equation, numeric
#' 
#' @examples
#' data(sim15_screen3)
#' compound(sim15_screen3,"Cpd1")
#' 
setClass("Compound",
         representation(
           name = "character",
           min.dose = "numeric",
           max.dose = "numeric",
           ic50 = "numeric",
           m = "numeric"
         ))

