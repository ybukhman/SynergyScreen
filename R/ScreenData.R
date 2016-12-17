#  ScreenData class and methods


#### ScreenData class #####
#' S4 class for a synergy screen data
#' 
#' A ScreenData class object contains a special data frame containing response values for microtiter plate wells  
#' 
#' @details Initial implementation supports 96-well, 8*12 plates.  
#' Plate rows should be named A-H and columns 1-12.  ScreenData data frame must contain the following columns:
#' \enumerate{
#'   \item \code{plate} plate identifier
#'   \item \code{column} column number, numeric, integer between 1 and 12
#'   \item \code{row} row, character or factor with values or levels A-H
#'   \item \code{response} numeric response value, e.g. OD
#' }
#' Additional columns are allowed and will be ignored by SynergyScreen software.
#' Correctness of the data frame structure is verified by a validity method and can be checked by calling validObject().
#' 
#' @examples
#' data(sim15_screen3)
#' head(raw_data(sim15_screen3),30)
#' str(raw_data(sim15_screen3))
#' 
#' @seealso \code{\link[base]{data.frame}}, \code{\link[methods]{validObject}}
#' 
setClass("ScreenData", contains = "data.frame",
         validity = function(object) {
           if (!all(c("plate","column","row","response") %in% names(object))) {
             return("Not all required columns are present in ScreenData object")
           } else if (!all(is.numeric(object$column),
                           is.factor(object$row) || is.character(object$row),
                           is.numeric(object$response)
           )) {
             return("Incorrect column classes")
           } else if (!all(min(object$column)==1, max(object$column)==12)) {
             return("plate column numbers are expected to be between 1 and 12")
           } else if (!all(unique(object$row) %in% LETTERS[1:8])) {
             return("plate rows are expected to be labelled A-H")
           } else {
             return(TRUE)
           }
         }
)

####  ScreenData - coersion of data frames to ScreenData objects ####
#  Need this because as() does not check validity by default
setAs("data.frame", "ScreenData",
      function(from) {
        new("ScreenData",from)
      })
