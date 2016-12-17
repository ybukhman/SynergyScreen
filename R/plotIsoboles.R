#### Plot isoboles ####
#' @include Compound.R DRE.R SynergyScreen.R
NULL

#' Plot isoboles for a specified synergy experiment
#' 
#' Plot isoboles for a synergy experiment that is part of a screen.  
#' 
#' @param object a SynergyScreen object
#' 
#' @details 
#' The experiment must be listed in the synergy_experiment_list slot of a SynergyScreen object
#' 
#' The effect values should be between 0 and 1, e.g. 0.5 to compute interaction index at IC50, 0.25 to compute IC25 (25% inhibition) etc.
#' Corresponding value of the response variable is defined as control.response*(1-effect)
#'
#' @examples
#' data(sim15_screen3)
#' plotIsoboles(sim15_screen3,"Cpd1-Cpd2")
#' 
#' @seealso \code{\link[=plot,SynergyExperiment,missing-method]{plot,SynergyExperiment-method}}, \code{\linkS4class{SynergyScreen}}
#' 
setGeneric("plotIsoboles", function(object, ...) standardGeneric("plotIsoboles"))

#' @param experiment string, name of the synergy experiment to plot
#' @param effect vector of fractional effect values to draw the isoboles for, numeric
#' @param model name of the growth curve model to use, character
#' @param xlab a title for the x axis.  If left unspecified, appropriate default is chosen.
#' @param ylab a title for the y axis.  If left unspecified, appropriate default is chosen.
#' @param ... additional arguments passed to the generic \code{\link[graphics]{plot}} function
#' @rdname plotIsoboles
setMethod("plotIsoboles", 
          signature(object = "SynergyScreen"),
          function(object, experiment, effect=c(0.25,0.5,0.75), model="menls", xlab=NULL, ylab=NULL, ...) {
            plot(object@synergy_experiment_list[[experiment]], dre.list=object@dre_list, 
                 effect=effect, model=model, xlab=xlab, ylab=ylab, ...)
          })
