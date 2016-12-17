# fit generic and methods
#' @include DRE.R
NULL

#### fit generic ####
#' Fit a model to an object's data.  
#' 
#' Fit a model and append it to the @@model slot of the input object.
#' 
#' @return An object of the same class as the input with a model object appended to its @@model slot
#' 
#' @param object object of class DRE
#' @param type type of model to fit, character.  Currently supports "melm", "loess" and "menls": see Details
#' @param name a name to assign to the model, character
#' @param loess.xlog fit response vs. ln(dose) when using loess
#' @param control.weight weight to assing to the control.response when using loess with loess.xlog = F, numeric
#' @param silent logical: should the report of error messages be suppressed?
#' @param ... other arguments passed to the model-fitting function, e.g. to loess or nls
#' 
#' @details
#' The following model types can be specified by using the \code{type} argument:
#' \itemize{
#'   \item \code{melm} - fit a median effect equation model with parameters \emph{ic50} and \emph{m} using linear least 
#'   squares \code{\link[stats]{lm}}.
#'   The data are linearized as log(fa/fu) = m*log(dose) - m*log(ic50), 
#'   where fu is fraction unaffected, fu = response/control.response.  fa is fraction affected, fa = 1-fu.
#'   If fu > 1, it is set to 0.9999
#'   \item \code{menls} - fit a median effect equation model with parameters \emph{ic50} and \emph{m} using non-linear 
#'   least squares \code{\link[stats]{nls}}
#'   \item \code{loess} - fit a LOESS local regression model, using both dose-response data and the untreated control value, 
#'   i.e. \code{object@@control.response}.  
#'   It is recommended to assign a high weight to the control, if there is a relatively large number of untreated control 
#'   data points in an experiment.  This is determined by the \code{control.weight} argument.  
#'   Consider setting it to the ratio of the number of control data points to the number of replicates in the dose-response 
#'   data.  
#'   It is set to 10 by default.
#' }
#' 
#' @examples
#' data(sim15_screen3)
#' x = fit(dre(sim15_screen3,"Cpd10"),type="melm")
#' model(x)$melm
#' plot(x)
#' 
#' @seealso \code{\linkS4class{DRE}}, \code{\linkS4class{DRModel}}
#' 
setGeneric("fit", function(object,...) standardGeneric("fit"))

#### DRE fit method ####
#' @describeIn fit Fit a DRE object
#' 
#' This method fits a dose-response curve model and appends it to the model slot of a DRE object
#' 
setMethod("fit", "DRE", 
          function (object, type="menls", name=NULL, loess.xlog=TRUE, control.weight=10, silent=FALSE, ...) {
            #  debug
            # browser()
            #  Fit LOESS model
            if (type == "loess") {
              if (loess.xlog) {
                # Dose on log scale.  Do not use control response, since log(0) = -Inf
                model = try(loess(object@response ~ log(object@dose), ...), silent=T)
              } else {
                # Dose on linear scale. Can use control response
                model = try(loess(c(object@control.response,object@response) ~ c(0,object@dose), 
                                  weights=c(control.weight,rep(1,length(object@dose))), ...),
                            silent=T)
              }
              if (class(model) == "try-error") model = as.character(model) #  need this to make sure "new" succeeds
              model = new("DRModelLOESS", model=model, xlog=loess.xlog)
              if (is.null(name)) name = "loess"
              
              #  Median effect model
            } else {
              #  Fit linear model first
              
              #  debug
              #if(object@name == "Cpd13-Cpd15") browser()
              
              # fraction unaffected and fraction affected
              fu = object@response/object@control.response
              fu[fu > 1] = 0.9999 # need this to avoid negative fa/fu values
              fa = 1 - fu
              
              #  linear model
              log.d = log(object@dose)
              model.lm = try(lm(log(fa/fu)~log.d),silent=T)
              if (class(model.lm) == "try-error") model.lm = as.character(model.lm) #  need this to make sure "new" succeeds
                  
              #  if a model has been fit but is not well constrained, i.e. linear term not significantly > 0, 
              #  set it to an error message              
#              if (class(model.lm) == "lm") {
#                pval = summary(model.lm)$coefficients["log.d","Pr(>|t|)"]
#                if (!is.finite(pval) || pval > 0.1) {
#                  model.lm = "Insufficient data to constrain a median effect model"
#                }
#              }
              if (type == "melm") {
                #  Linear model is the final output
                model = new("DRModelMElm",model=model.lm,control.response=object@control.response)
                if (is.null(name)) name = "melm"
              } else if (type == "menls") {
                if (class(model.lm) == "lm") {
                  #  If linear model succeeded, use its parameters as initial guess for nls
                  #  debug
                  #if (object@name == "Cpd3-Cpd6") browser()
                  formula = as.formula(paste("y ~ ", object@control.response, "/((d/ic50)^m + 1)"))
                  data = list(y = object@response, d = object@dose)
                  start = list(ic50 = exp(-coef(model.lm)["(Intercept)"]/coef(model.lm)["log.d"]), m = coef(model.lm)["log.d"])
                  model = try(nls(formula = formula, data = data, start = start), silent=silent)
                  if (class(model) == "try-error") {
                    #  Try to fit again with different initial guesses, progressively going further away from the original one
                    model0 = model
                    start0 = start
                    rand1 = runif(20,-1,1)
                    rand2 = runif(20,-1,1)
                    for(i in 1:20) {
                      start$ic50 = start0$ic50*(1+ 0.05*i*rand1[i])
                      start$m = start0$m*(1+ 0.05*i*rand2[i])
                      model = try(nls(formula = formula, data = data, start = start), silent=silent)
                      if (class(model) == "nls") break
                    }
                    #  If succeeded, retain the fit.  If not, go back to the original error message
                    if (class(model) == "try-error") model = as.character(model0)
                  } 
                } else {
                  #  If linear model failed, just pass along the error message
                  model = model.lm
                }
                #  Final model object
                model = new("DRModelMEnls",model=model)
                if (is.null(name)) name = "menls"
              #  Unsupported model type  
              } else {
                stop("Unsupported model type:", type)
              }
            }
            #  Attach model to the input object
            model = list(model)
            names(model) = name
            object@model = c(object@model,model)
            return(object)
          })
