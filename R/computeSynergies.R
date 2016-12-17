# computeSynergies generic and methods
#' @include SynergyScreen.R
NULL

#### computeSynergies ####
#' Compute synergy values
#' 
#' Synergy is measured as interaction index.  This function computes values of the interaction index for a set effective dose values for 
#' each synergy experiment in a screen.
#' 
#' @param object object of class SynergyScreen
#' @param effect numeric vector of effects, between 0 and 1
#' @param model character vector of model types.  Supported types include melm, menls and loess
#' @param control.weight weight to assing to the control.response, numeric
#' @param span span parameter for loess, numeric
#' @param silent logical: should warnings and error messages be suppressed?
#' @param ... other arguments passed to the model-fitting function, e.g. to loess or nls
#' 
#' @return A SynergyScreen object with @@synergies slot populated by a data frame of interaction index values
#' 
#' @details
#' See description of model types in \link{fit}
#' 
#' @examples
#' data(sim15_screen3)
#' sim15_screen3 = computeSynergies(sim15_screen3)
#' synergy_data(sim15_screen3)
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}
#' 
setGeneric("computeSynergies", function(object,...) standardGeneric("computeSynergies"))

#### SynergyScreen computeSynergies method ####
#' @describeIn computeSynergies Compute synergies in a screen
#' 
setMethod("computeSynergies", "SynergyScreen", 
          function (object, effect=c(0.25,0.5,0.75), model="menls", control.weight=10, span=1, silent=T, ...) {
            #  Debug
            # browser()
            
            #  Suppress warnings
            if (silent) {
              warn0 = getOption("warn")
              options(warn=-1)
            }
            
            #  Verify arguments
            stopifnot(is.numeric(effect), all(effect>0), all(effect<1))
            stopifnot(all(model %in% c("melm","menls","loess","loess2")))
            
            #  Fit dose-response curve models 
            dre_list = object@dre_list
            for (model1 in model) {
              dre_list = lapply(dre_list, fit, type=model1, control.weight=control.weight, span=span, silent=silent, ...)
            }
            
            #  Compute interaction index for each synergy experiment, model and effective dose
            result = data.frame(matrix(nrow=0,ncol=4))
            names(result) = c("experiment", "model", "effect", "interaction.index")
            for (exp1 in object@synergy_experiment_list){
              for (model1 in model) {
                for (eff1 in effect) {
                  ind1 = try(interactionIndex(experiment=exp1, effect=eff1, dre.list=dre_list, model=model1),silent=silent)
                  if (class(ind1) == "try-error") next
                  result = rbind(result,data.frame(experiment=exp1@name,model=model1,effect=eff1,interaction.index=ind1))
                }
              }
            }
            
            #  All done
            if (silent) {
              #  If warnings have been suppressed, restore session setting of the caller
              options(warn=warn0)
            }
            result = as(result,"SynergyData")
            object@dre_list = as(dre_list,"DREList")
            object@synergy_data = result
            return(object)
          })
