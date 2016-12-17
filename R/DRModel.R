#  Class DRModel and its sub-classes

#' @include Model.R
NULL

#### DRModel class ####
#' A virtual S4 class representing a Dose-Response Model
#' 
#' Extended by sub-classes for specific types of model
#' 
#' @examples
#' data(sim15_screen3)
#' dre(sim15_screen3,"Cpd1")@@model
#' 
#' @seealso \linkS4class{DRModelLOESS}, \linkS4class{DRModelMEnls}, \linkS4class{DRModelMElm}
#' 
setClass("DRModel",contains="VIRTUAL")

##### DRModelLOESS class ####
#' An S4 class representing LOESS model of a dose-response curve
#' 
#' @slot model loess model object
#' @slot xlog specifies if the x (dose) axis was log transformed before fitting a loess model
#' 
#' @examples
#' data(sim15_screen3)
#' 
#' #  LOESS works
#' x = fit(dre(sim15_screen3,"Cpd10"),type="loess",span=1)
#' model(x)$loess
#' plot(x)
#' 
#' #  LOESS gives an odd result
#' x = fit(dre(sim15_screen3,"Cpd1"),type="loess",span=1)
#' model(x)$loess
#' plot(x)
#' 
setClass("DRModelLOESS",
         representation(
           model = "Model",
           xlog = "logical"
         ),
         contains = "DRModel")

##### DRModelMEnls class ####
#' An S4 class representing median effect model of a dose-response curve fitted using nls
#' 
#' @slot model object of class \code{\link[stats]{nls}}
#' 
#' @examples
#' data(sim15_screen3)
#' model(dre(sim15_screen3, "Cpd1"))
#' plot(dre(sim15_screen3, "Cpd1"))
#' 
setClass("DRModelMEnls",
         representation(
           model = "Model"
         ),
         contains = "DRModel")

##### DRModelMElm class ####
#' An S4 class representing median effect model of a dose-response curve fitted using lm
#' 
#' @slot model object of class \code{\link[stats]{lm}}
#' @slot control.response response value of untreated control, numeric
#' 
#' @examples
#' data(sim15_screen3)
#' x = fit(dre(sim15_screen3,"Cpd10"),type="melm")
#' model(x)$melm
#' plot(x)
#' 
setClass("DRModelMElm",
         representation(
           model = "Model",
           control.response = "numeric"
         ),
         contains = "DRModel")

