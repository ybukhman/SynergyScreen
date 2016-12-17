#  ScreenDesign class and methods

#### ScreenDesign class #####
#' S4 class for a synergy screen design
#' 
#' A ScreenDesign class object contains a special data frame specifying design of a screen, i.e. what goes into each well
#' of a set of plates.  
#' 
#' @details Initial implementation supports 96-well, 8*12 plates and mixtures of 2 compounds.  
#' Plate rows should be named A-H and columns 1-12.  Design data frame must contain the following columns:
#' \enumerate{
#'   \item \code{plate} plate identifier
#'   \item \code{column} column number, numeric, integer between 1 and 12
#'   \item \code{row} row, character or factor with values or levels A-H
#'   \item \code{experiment} experiment name, character or factor
#'   \item \code{cpd1} compound 1 name, character or factor
#'   \item \code{dose1} compound 1 dose, numeric
#'   \item \code{cpd2} compound 2 name, character or factor
#'   \item \code{dose2} compound 2 dose, numeric
#' }
#' Additional columns are allowed and will be ignored by SynergyScreen software.
#' Correctness of the data frame structure is verified by a validity method and can be checked by calling validObject().
#' 
#' @examples
#' data(sim15_screen3)
#' head(design(sim15_screen3),n=30)  
#' str(design(sim15_screen3))
#' 
#' @seealso \code{\link[base]{data.frame}}, \code{\link[methods]{validObject}}
#' 
setClass("ScreenDesign", contains = "data.frame",
         validity = function(object) {
           if (!all(c("plate","column","row","experiment","cpd1","dose1","cpd2","dose2") %in% names(object))) {
             return("Not all required columns are present in ScreenDesign object")
           } else if (!all(is.numeric(object$column),
                           is.factor(object$row) || is.character(object$row),
                           is.factor(object$experiment) || is.character(object$experiment),
                           is.factor(object$cpd1) || is.character(object$cpd1),
                           is.numeric(object$dose1),
                           is.factor(object$cpd2) || is.character(object$cpd2),
                           is.numeric(object$dose2)
           )) {
             return("Incorrect column classes")
           } else if (!all(min(object$column)==1, max(object$column)==12)) {
             return("plate column numbers are expected to be between 1 and 12")
           } else if (!all(unique(object$row) %in% LETTERS[1:8])) {
             return("plate rows are expected to be labelled A-H")
           } else if (any(!is.na(object$dose1) & object$dose1 < 0) || any(!is.na(object$dose2) & object$dose2 < 0)) {
             return("dose values are expected to be positive")
           } else {
             return(TRUE)
           }
         }
)



####  Coersion of data frames to ScreenDesign objects ####
#  Need this because as() does not check validity by default
setAs("data.frame", "ScreenDesign",
      function(from) {
        new("ScreenDesign",from)
      })

