# Class DREList and its methods


#### DREList class ####
#' An S4 class to represent a list of DRE objects
#' 
#' Extends class list but requires all members to be of class DRE
#' 
#' @seealso \code{\linkS4class{DRE}}, \code{\link[base]{list}}
#' 
#' @examples
#' data(sim15_screen3)
#' dre_list(sim15_screen3)[1:3]
#' 
setClass("DREList", contains="list", 
         validity=function(object) { 
           checks = sapply(object, is, "DRE")
           if(!all(checks)) {
             return(paste("The following elements are not of class DRE:",paste(object[!checks],collapse="; ")))
           }
           #  Check names
           cnames = sapply(object, function(x) x@name)
           if (is.null(names(object)) || any(names(object) != cnames)) return("Inconsistent names")
           #  All done
           return(TRUE)
         }
)
####  DREList - coersion of lists to DREList objects ####
#  Need this because as() does not check validity by default
setAs("list", "DREList",
      function(from) {
        new("DREList",from)
      })

#### DREList show method #####
setMethod("show", "DREList",
          function(object) {
            cat("An object of class \"DREList\"\n")
            if (length(object)==0) {
              cat(".. Empty\n")
            } else {
              for (i in 1:length(object)) {
                cat(paste("$",names(object)[i],"\n",sep=""))
                show(object[[i]])
              }
            }
          })

