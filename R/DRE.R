#  Class DRE and its methods

#### DRE class ####
#' An S4 class representing a dose-response experiment of an individual compound or a mixture
#' 
#' @slot name dose-response experiment name, character
#' @slot compound.name vector of compound names, character
#' @slot fraction vector of compound fractions, numeric, should add up to 1
#' @slot dose vector of doses, numeric
#' @slot response vector of responses, e.g. OD, numeric
#' @slot control.response vector of [untreated] control responses, numeric
#' @slot effect fractional effect of the drug(s), can range from 0 for untreated control to 1 for complete growth inhibition
#' @slot model named list of fitted models of the dose-response curve, list of objects of class DRModel
#' @slot ic25 IC25 value inferred from the model, numeric
#' @slot ic50 IC50 value inferred from the model, numeric
#' @slot ic75 IC75 value inferred from the model, numeric
#' @slot true.ic50 "true" IC50 value, can be set for simulation purposes, numeric
#' @slot true.m "true" slope parameter of the Hill's equation, can be set for simulation purposes, numeric
#' @slot comment a vector of diagnostic messages and other comments, character
#' @slot dilution.factor dilution factor, defined for exponential dose series
#' 
#' @examples
#' data(sim15_screen3)
#' dre(sim15_screen3,"Cpd1")
#' plot(dre(sim15_screen3,"Cpd1"))
#' 
setClass("DRE",
         representation(
           name = "character",
           compound.name = "character",
           fraction = "numeric",
           dose = "numeric",
           response = "numeric",
           control.response = "numeric",
           effect = "numeric",
           model = "list",
           ic25 = "numeric",
           ic50 = "numeric",
           ic75 = "numeric",
           true.ic50 = "numeric",
           true.m = "numeric",
           comment = "character",
           dilution.factor = "ANY"
         ))

#### DRE initialize method ####
#' Initialise a DRE object
#' 
#' DRE object represents a dose-response experiment.  This function creates a new DRE object from one or more \linkS4class{Compound} objects
#' 
#' @param .Object object of class DRE
#' @param name optional name of the dose-response experiment, character
#' @param compound.name names of compounds that are included in the dre object, character
#' @param compound a list of \linkS4class{Compound} objects
#' @param fraction optional vector of compound fractions, numeric, should add up to 1
#' @param dose optional vector of doses. In case of mixtures, these are the doses of an entire mixture.
#' @param min.dose optional minimal dose, numeric.  
#' @param max.dose optional maximal dose, numeric. 
#' @param num.doses number of different doses in the dose-response experiment
#' @param dose.series.type the type of dose series.  Supported values are "exp" and "linear".  
#' See the Details.
#' @param dilution.factor.decimals integer number of decimal points in the dilution factor
#' 
#' @return DRE object
#' 
#' @details
#' Generally, a dose-response experiment is conducted on a mixture of compounds.  
#' Suppose each compound is referred to by an index \emph{i} and there are \emph{n} compounds.
#' 
#' If parameters \code{fraction} and \code{dose} are both supplied by the caller, the following must be true:
#' dose[j] = SUM(fraction[i]*dose[ij])
#' where dose[j] is the j'th dose of the mixture and dose[ij] is the j'th dose of an individual compound 
#' 
#' If not supplied by the caller, parameters \code{fraction}, \code{min.dose} and \code{max.dose} are computed as follows:
#' \enumerate{
#'   \item fraction[i] = sqrt(min.dose[i]*max.dose[i]) / sum(sqrt(min.dose[1:n]*max.dose[1:n]))
#'   \item min.dose = 1/sum(fraction[1:n]/min.dose[1:n])
#'   \item max.dose = 1/sum(fraction[1:n]/max.dose[1:n])
#' }
#' 
#' Unless the \code{dose} parameter is supplied by the caller, the doses are calculated as a series of 
#' \code{num.doses} values from \code{min.dose} to \code{max.dose}.  The type of the series is determined
#'  by \code{dose.series.type}:
#' \enumerate{
#' \item \strong{"exp"} series is exponential, e.g. 1,2,4,8,16
#' \item \strong{"linear"} series is linear, e.g. 1,2,3,4,5
#' }
#'
#' In exponential dose series, the minimal dose will not be exactly the same as min.dose supplied 
#' by the caller.  The initialize method will round the dilution factor to the number of decimals 
#' specified by parameter dilution.factor.decimals.  Resulting minimal dose should be reasonably
#' close to the supplied min.dose parameter, but won't equal it exactly.
#'   
#' @examples
#' data(sim15_screen3)
#' compound(sim15_screen3,"Cpd1")
#' new("DRE", compound.name="Cpd1", compound=list(compound(sim15_screen3,"Cpd1")), 
#'     num.doses=5, dose.series.type="exp")
#' new("DRE", compound.name="Cpd1", compound=list(compound(sim15_screen3,"Cpd1")), 
#'     num.doses=5, dose.series.type="linear")
#' 
#' @seealso \linkS4class{DRE}, \linkS4class{Compound}
setMethod("initialize", "DRE", function(.Object, compound.name, name=NULL, compound, fraction=NULL, 
                                        dose=NULL, min.dose=NULL, max.dose=NULL, num.doses=5, 
                                        dose.series.type="exp", dilution.factor.decimals=2) {
  
  #  Check validity of arguments
  if (!all(sapply(compound,class)=="Compound")) stop("compound list must contain objects of class Compound")
  if (!all(compound.name %in% sapply(compound, function(x) x@name))) stop("compound list must contain all compounds listed in compound.name")
  if (!is.null(fraction) && length(compound.name) != length(fraction)) stop("compound.name and fraction must be of the same length")
  if (!is.null(fraction) && !all.equal(sum(fraction),1)) stop("fractions must add up to 1")
  if (!is.null(dose) && (!is.numeric(dose) || any(is.na(dose)))) stop("dose must be numeric and contain no NA values")
  stopifnot(dose.series.type %in% c("exp","linear"))
  stopifnot(all.equal(round(dilution.factor.decimals),dilution.factor.decimals))
  
  #  Use only compounds whose names are listed in this dose-response experiment object
  names(compound) = sapply(compound, function(x) x@name)
  compound = compound[compound.name]
  
  #  Assign compound.name slot
  .Object@compound.name = compound.name
  
  #  Compute or assign name slot
  if (is.null(name)) {
    .Object@name = paste(compound.name,collapse="-")
  } else {
    .Object@name = name
  }
  
  #  Compute or assign fraction slot  
  if (is.null(fraction)) {
    geomean.dose = sapply(compound, function(x) sqrt(x@min.dose * x@max.dose))
    .Object@fraction = geomean.dose / sum(geomean.dose)
  } else {
    .Object@fraction = fraction
  }
  
  #  Compute or assign dilution factor and dose slots
  if (is.null(dose)) {
    if (is.null(min.dose)) min.dose = 1 / sum(.Object@fraction/sapply(compound, function(x) x@min.dose))
    if (is.null(max.dose)) max.dose = 1 / sum(.Object@fraction/sapply(compound, function(x) x@max.dose))
    if (dose.series.type == "exp") {
      dilution.factor = (max.dose/min.dose)^(1/(num.doses-1))
      dilution.factor = round(dilution.factor, dilution.factor.decimals)
      dilution.factor(.Object) = dilution.factor
      min.dose = max.dose / dilution.factor^(num.doses-1)
      log.doses = seq(from = log(min.dose), to = log(max.dose), length.out = num.doses)
      dose(.Object) = exp(log.doses)
    } else if (dose.series.type == "linear") {
      .Object@dose = seq(from = min.dose, to = max.dose, length.out = num.doses)
    } else {
      stop("Unsupported dose.series.type: ", dose.series.type)
    }
  } else {
    .Object@dose = dose
  }
  
  
  #  All done
  return(.Object)
})

#### DRE plot method ####
#' Plot a DRE object
#' 
#' Plot dose-response data and, if known, a fitted curve and/or a "true" curve on the basis of which the data have been simulated
#' 
#' @param x object of class DRE
#' @param y no parameter \code{y} is defined for this method
#' @param linearize logical, defaults to FALSE (see Details)
#' @param xlab a title for the x axis.  If left unspecified, appropriate defaults are chosen for sigmoid and linearized plots.
#' @param ylab a title for the y axis.  If left unspecified, appropriate defaults are chosen for sigmoid and linearized plots.
#' @param log a character string which contains "x" if the x axis is to be logarithmic, "y" if the y axis is to be logarithmic 
#' and "xy" or "yx" if both axes are to be logarithmic.  If left unspecified, appropriate defaults are chosen for sigmoid and linearized plots.
#' @param ... additional arguments passed to the generic \code{\link[graphics]{plot}} function
#' 
#' @details
#' If "linearize" is set to FALSE (default), plot response vs. dose.  
#' If TRUE, plot log(fraction.affected/fraction.unaffected) vs. log(dose), which is linear if the dose-response relationship follows 
#' the median effect law
#' 
#' @examples
#' data(sim15_screen3)
#' plot(dre(sim15_screen3,"Cpd1"))
#' 
#' @seealso \code{\linkS4class{DRE}}, \code{\link[graphics]{plot}}
#' 
setMethod("plot",
          signature(x = "DRE", y="missing"),
          function (x, y, linearize=F, xlab=NULL, ylab=NULL, log=NULL, ...) 
          {
            #  Debug
            #  browser()
            
            #  Linearized plot
            if (linearize) {
              #  Plot raw data
              #  (exclude data where response is above the control value, as they become undefined due to variable transformation)
              filt = x@control.response-x@response > 0 
              y = log((x@control.response-x@response[filt])/x@response[filt])
              if (is.null(xlab)) xlab="log(dose)"
              if (is.null(ylab)) ylab="log(fa/fu)"
              plot(log(x@dose[filt]),y,xlab=xlab,ylab=ylab,...)
              
              #  Plot "true" line
              if (length(x@true.ic50) == 1 && length(x@true.m) == 1) {
                x.true = seq(min(x@dose),max(x@dose),length.out=100)
                y.true = x@control.response/((x.true/x@true.ic50)^x@true.m + 1)
                log.y.true = log((x@control.response-y.true)/y.true)
                lines(log(x.true),log.y.true,col="red")
              }
              #  Plot fitted model(s)
              if (length(x@model)>0) {
                for (i in 1:length(x@model)) {
                  model = x@model[[i]]
                  x.pred = seq(min(x@dose),max(x@dose),length.out=100)
                  y.pred = predict(model,x.pred)
                  filt2 = x@control.response-y.pred > 0 #  (exclude data where predicted response is above the control value)
                  log.y.pred = log((x@control.response-y.pred[filt2])/y.pred[filt2])
                  lines(log(x.pred[filt2]),log.y.pred,col=i+2)
                }
              }
              #  Sigmoid plot
            } else {
              #  Plot raw data
              if (is.null(xlab)) xlab="dose"
              if (is.null(ylab)) ylab="response"
              if (is.null(log)) log="x"
              plot(x@dose,x@response,ylim=c(0,max(x@response,x@control.response)),xlab=xlab,ylab=ylab,log=log,...)
              abline(h=x@control.response, lty=2)
              
              #  Plot "true" curve
              if (length(x@true.ic50) == 1 && length(x@true.m) == 1) {
                x.true = seq(min(x@dose),max(x@dose),length.out=100)
                y.true = x@control.response/((x.true/x@true.ic50)^x@true.m + 1)
                lines(x.true,y.true,col="red")
              }
              
              #  Plot fitted model(s)
              if (length(x@model)>0) {
                for (i in 1:length(x@model)) {
                  model = x@model[[i]]
                  x.pred = seq(min(x@dose),max(x@dose),length.out=100)
                  y.pred = try(predict(model,x.pred),silent=T)
                  if (class(y.pred)=="try-error") next
                  lines(x.pred,y.pred,col=i+2)
                }
              }
            }
            #  Build and plot the legend
            # - position
            pos = ifelse(linearize,"bottomright","topright")
            # - raw data
            legend = "raw data"
            pch = 1
            lty = NA
            col = 1
            #  - control response for sigmoid plots
            if (!linearize) {
              legend = c(legend,"control")
              pch = c(pch,NA)
              lty = c(lty,2)
              col = c(col,1)
            }
            #  - "true" line (for simulated datasets)
            if (length(x@true.ic50) == 1 && length(x@true.m) == 1) {
              legend = c(legend,"true line")
              pch = c(pch,NA)
              lty = c(lty,1)
              col = c(col,2)
            }
            #  - fitted models
            if (length(x@model)>0) {
              legend = c(legend,names(x@model))
              pch = c(pch,rep(NA,length(x@model)))
              lty = c(lty,rep(1,length(x@model)))
              col = c(col,seq(from=3,by=1,length.out=length(x@model)))
            }
            #  - plot the legend
            legend(x=pos,legend=legend,pch=pch,lty=lty,col=col,cex=0.85)
            
            # Add title
            title(main = paste(name(x), "dose-response curve"))
          })

