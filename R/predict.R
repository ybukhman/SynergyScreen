#  predict methods

#' @include DRModel.R
NULL

#### DRModelLOESS predict method ####
#' Predict response values from a DRModelLOESS object
#' 
#' Given a vector of dose values, computes response on linear scale
#' 
#' @param object object of class DRModelLOESS
#' @param dose a vector of dose values for which to compute response, numeric
#' @param ... other arguments to be passed to \code{\link[stats]{predict.loess}}
#' 
#' @return numeric vector of response values
#' 
#' @details
#' A DRModelLOESS object contains a loess model for log(response) vs. dose.  
#' This method Executes \code{\link[stats]{predict.loess}} and exponentiates to return response on linear scale
#' 
#' @examples
#' data(sim15_screen3)
#' x = fit(dre(sim15_screen3,"Cpd10"),type="loess")
#' model(x)$loess
#' plot(x)
#' dose(x)
#' y = predict(model(x)$loess, dose=seq(0.4,4,by=0.4))
#' data.frame(dose = seq(0.4,4,by=0.4), y)
#' 
#' 
setMethod("predict",
          signature(object = "DRModelLOESS"),
          function (object, dose, ...) {
            stopifnot(all(is.finite(dose) & dose>=0))
            #  If the input object contains a valid loess model, return predicted values.  Otherwise, return a vector of NAs.
            if (is(object@model,"loess")) {
              if (object@xlog) dose = log(dose)
              y.pred = predict(object@model,dose,...)
            } else {
              y.pred = rep(NA,length(dose))
            }            
            return(y.pred)
          }  
)

#### DRModelMEnls predict method ####
#' Predict response values from a DRModelMEnls object
#' 
#' Given a vector of dose values, computes response on linear scale
#' 
#' @param object object of class DRModelMEnls
#' @param dose a vector of dose values for which to compute response, numeric
#' @param ... other arguments to be passed to \code{\link[stats]{predict.nls}}
#' 
#' @return numeric vector of response values
#' 
#' @details
#' A DRModelMEnls object contains a nls model for log(response) vs. dose.  
#' This method Executes \code{\link[stats]{predict.nls}} and exponentiates to return response on linear scale
#' 
#' @examples
#' data(sim15_screen3)
#' y = predict(model(dre(sim15_screen3, "Cpd10"))$menls, dose=seq(0.4,4,by=0.4))
#' data.frame(dose = seq(0.4,4,by=0.4), y)
#' 
setMethod("predict",
          signature(object = "DRModelMEnls"),
          function (object, dose, ...) {
            stopifnot(all(is.finite(dose) & dose>=0))
            #  If the input object contains a valid nls model, return predicted values.  Otherwise, return a vector of NAs.
            if (is(object@model,"nls")) {
              y.pred = predict(object@model,list(d=dose),...)
            } else {
              y.pred = rep(NA,length(dose))
            }            
            return(y.pred)
          }  
)

#### DRModelMElm predict method ####
#' Predict response values from a DRModelMElm object
#' 
#' Given a vector of dose values, computes response on linear scale
#' 
#' @param object object of class DRModelMElm
#' @param dose a vector of dose values for which to compute response, numeric
#' @param ... other arguments to be passed to \code{\link[stats]{predict.lm}}
#' 
#' @return numeric vector of response values
#' 
#' @details
#' A DRModelMElm object contains a lm model for log(response) vs. dose.  
#' This method Executes \code{\link[stats]{predict.lm}} and exponentiates to return response on linear scale
#' 
#' @examples
#' data(sim15_screen3)
#' x = fit(dre(sim15_screen3,"Cpd10"),type="melm")
#' model(x)$melm
#' plot(x)
#' dose(x)
#' y = predict(model(x)$melm, dose=seq(0.4,4,by=0.4))
#' data.frame(dose = seq(0.4,4,by=0.4), y)
#' 
setMethod("predict",
          signature(object = "DRModelMElm"),
          function (object, dose, ...) {
            stopifnot(all(is.finite(dose) & dose>=0))
            #  If the input object contains a valid lm model, return predicted values converted to original response scale.  
            #  Otherwise, return a vector of NAs.
            if (is(object@model,"lm")) {
              dose2 = dose
              dose2[dose==0] = 1e-9 #  can't have log(0)
              y.pred = predict(object@model,data.frame(log.d=log(dose2)),...)
              fa.over.fu = exp(y.pred)
              resp = object@control.response/(1 + fa.over.fu)
              resp[dose==0] = object@control.response # return control response values for zero dose          
            } else {
              resp = rep(NA,length(dose))
            }            
            return(resp)
          }  
)


