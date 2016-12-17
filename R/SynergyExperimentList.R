#  SynergyExperimentList class and methods


#### SynergyExperimentList class ####
#' An S4 class to represent a list of SynergyExperiment objects
#' 
#' Extends class list but requires all members to be of class SynergyExperiment
#' 
#' @examples
#' data(sim15_screen3)
#' synergy_experiment_list(sim15_screen3)[[1]]
#' 
#' @seealso \code{\linkS4class{SynergyExperiment}}, \code{\link[base]{list}}
#' 
setClass("SynergyExperimentList", contains="list", 
         validity=function(object) { 
           checks = sapply(object, is, "SynergyExperiment")
           if(!all(checks)) {
             return(paste("The following elements are not of class SynergyExperiment:",paste(object[!checks],collapse="; ")))
           }
           #  Check names
           cnames = sapply(object, function(x) x@name)
           if (is.null(names(object)) || any(names(object) != cnames)) return("Inconsistent names")
           #  All done
           return(TRUE)
         }
)
####  SynergyExperimentList - coersion of lists to SynergyExperimentList objects ####
#  Need this because as() does not check validity by default
setAs("list", "SynergyExperimentList",
      function(from) {
        new("SynergyExperimentList",from)
      })

#### SynergyExperimentList show method #####
setMethod("show", "SynergyExperimentList",
          function(object) {
            cat("An object of class \"SynergyExperimentList\"\n")
            if (length(object)==0) {
              cat(".. Empty\n")
            } else {
              for (i in 1:length(object)) {
                cat(paste("$",names(object)[i],"\n",sep=""))
                show(object[[i]])
              }
            }
          })
