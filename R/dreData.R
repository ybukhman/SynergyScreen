# dreData generic and methods
#' @include DRE.R SynergyScreen.R
NULL

#### dreData ####
#' Retrive doses, normalized responses and effects
#' 
#' @param object object of supported class, e.g. DRE or SynergyScreen
#' @param ... method-specific arguments: see below
#' @param names the names of the dose-response experiments to retrieve data for, character
#' 
#' @return data frame
#' 
#' @details
#' \describe{
#' \item{DRE method}{returns a data frame is generated from @@dose, @@response and @@effect slots}
#' \item{SynergyScreen method}{returns a data frame generated from @@dose, @@response and @@effect slots of DRE objects contained 
#' in @@dre_list slot of the SynergyScreen object.  Use parameter \code{names} to retrieve data for specific dose-response experiments.
#' If \code{names=NULL}, all dose-response data are retrieved}
#' }
#'  
#' @examples
#' data(sim15_screen3)
#' dreData(sim15_screen3)
#' dreData(dre(sim15_screen3,"Cpd1"))
#' 
#' @seealso \code{\linkS4class{DRE}}, \code{\linkS4class{SynergyScreen}}
#' 
setGeneric("dreData", function(object,...) standardGeneric("dreData"))

#### DRE dreData method ####
#' @describeIn dreData Retieve dose, normalized response and effect data from a DRE object
#' 
setMethod("dreData", "DRE", 
          function (object) {
            data.frame(dose = object@dose, response = object@response, effect = object@effect)
          })

#### SynergyScreen dreData method ####
#' @describeIn dreData Retieve dose, normalized response and effect data from DRE objects contained in the @@dre_list slot 
#' of a SynergyScreen object
#' 
setMethod("dreData", "SynergyScreen", 
          function (object, names=NULL) {
            if (is.null(names)) {
              names = names(object@dre_list)
            }
            
            data1 = data.frame(matrix(ncol=4,nrow=0))
            names(data1) = c("experiment","dose","response","effect")
            
            for (n in names) {
              data2 = data.frame(experiment = n, dreData(object@dre_list[[n]]))
              data1 = rbind(data1,data2)
            }
            
            return(data1)
          })
