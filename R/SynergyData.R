#  SynergyData class and methods

#### SynergyData class #####
#' S4 class for a synergy data
#' 
#' A SynergyData class object contains a special data frame containing interaction index values from a synergy screen  
#' 
#' @details 
#' SynergyData data frame must contain the following columns:
#' \enumerate{
#'   \item \code{experiment} the name of a synergy experiment, character or factor
#'   \item \code{model} the name of the model used to fit dose-resoponse curves
#'   \item \code{effect} effect value, numeric between 0 and 1
#'   \item \code{interaction.index} interaction index value, a positive number
#' }
#' Additional columns are allowed and will be ignored by SynergyScreen software.
#' Correctness of the data frame structure is verified by a validity method and can be checked by calling validObject().
#' 
#' @examples
#' data(sim15_screen3)
#' head(synergy_data(sim15_screen3))
#' str(synergy_data(sim15_screen3))
#' 
#' @seealso \code{\link[base]{data.frame}}, \code{\link[methods]{validObject}}, \code{\linkS4class{SynergyScreen}}
#' 
setClass("SynergyData", contains = "data.frame",
         validity = function(object) {
           if (!all(c("experiment","model","effect","interaction.index") %in% names(object))) {
             return("Not all required columns are present in SynergyData object")
           } else if (!(is.factor(object$experiment) || is.character(object$experiment))) {
             return("experiment must be factor or character")
           } else if (!(is.factor(object$model) || is.character(object$model))) {
             return("model must be factor or character")
           } else if (!all(is.numeric(object$effect), object$effect > 0, object$effect < 1)) {
             return("effect must be a number between 0 and 1, non-inclusive")
           } else if (!all(is.numeric(object$interaction.index),all(is.na(object$interaction.index) | object$interaction.index >= 0))) {
             return("interaction.index must be a positive number or NA")
           } else {
             return(TRUE)
           }
         }
)

####  SynergyData - coersion of data frames to SynergyData objects ####
#  Need this because as() does not check validity by default
setAs("data.frame", "SynergyData",
      function(from) {
        new("SynergyData",from)
      })
