#  CompoundList class and methods

#### CompoundList class ####
#' An S4 class to represent a list of compounds
#' 
#' Extends class list but requires all members to be of class Compound
#' 
#' @seealso \code{\linkS4class{Compound}}, \code{\link[base]{list}}
#' 
#' @examples
#' data(sim15_screen3)
#' compound_list(sim15_screen3)
#'   
setClass("CompoundList", contains="list", 
         validity=function(object) { 
           #  Check that all members are of class Compound
           checks = sapply(object, is, "Compound")
           if(!all(checks)) {
             return(paste("The following elements are not of class Compound:",paste(object[!checks],collapse="; ")))
           }
           #  Check names
           cnames = sapply(object, function(x) x@name)
           if (is.null(names(object)) || any(names(object) != cnames)) return("Inconsistent names")
           #  All done
           return(TRUE)
         }
)
####  CompoundList - coersion of lists to CompoundList objects ####
#  Need this because as() does not check validity by default
setAs("list", "CompoundList",
      function(from) {
        new("CompoundList",from)
      })

#### CompoundList show method #####
setMethod("show", "CompoundList",
          function(object) {
            cat("An object of class \"CompoundList\"\n")
            if (length(object)==0) {
              cat(".. Empty\n")
            } else {
              for (i in 1:length(object)) {
                cat(paste("$",names(object)[i],"\n",sep=""))
                show(object[[i]])
              }
            }
          })

