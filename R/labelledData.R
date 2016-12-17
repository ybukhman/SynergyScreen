#  labelledData generic and methods
#' @include SynergyScreen.R
NULL

#' Retrieve labelled data
#' 
#' Retrieve synergy screen data as a data frame containing full screen design information along with response data
#' 
#' @param object object of class SynergyScreen
#' @param what type of data to retrieve, can be "raw" or "norm"
#' 
#' @return data frame containing full screen design information along with response data
#'  
#' @examples
#' data(sim15_screen3)
#' head(labelledData(sim15_screen3, what="raw"),n=30)
#'  
#' @seealso \code{\linkS4class{SynergyScreen}}
#' 
setGeneric("labelledData", function(object,what) standardGeneric("labelledData"))

#' @describeIn labelledData Retrieve labelled data
setMethod("labelledData","SynergyScreen",
          function (object, what="raw") {
            if (what == "raw") {
              data1 = raw_data(object)
            } else if (what == "norm") {
              data1 = norm_data(object)
            } else {
              stop("Unsupported option: what=", what)
            }
            design = design(object)
            data2 = merge(design,data1,by=c("plate","column","row"),all.x=T)
            data2 = data2[order(data2$plate,data2$column,data2$row),]
            return(data2)
          })
