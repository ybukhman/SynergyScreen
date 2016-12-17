# SynergyExperiment class and methods

#### SynergyExperiment class #####
#' S4 class for a synergy experiment
#' 
#' A SynergyExperiment class object contains DRE objects for individual compounds 
#' and fixed-ratio mixture ray(s)
#' 
#' @slot name of the experiment
#' @slot mixture.dre.name a vector of dose-response experiment names of mixtures, character
#' @slot compound.dre.name a vector of dose-response experiment names of individual compounds, character
#' 
#' @examples
#' data(sim15_screen3)
#' synergy_experiment(sim15_screen3,"Cpd1-Cpd2")
#' 
#' @seealso \code{\linkS4class{DRE}}
#' 
setClass("SynergyExperiment",
         representation(
           name = "character",
           mixture.dre.name = "character",
           compound.dre.name = "character"
         ))

#### SynergyExperiment initialize method ####
#' Initialise a SynergyExperiment object
#' 
#' Create a new SynergyExperiment object from appropriate DRE objects,
#' making sure they form a coherent experiment
#' 
#' @param .Object object of class SynergyExperiment
#' @param name optional name of the experiment, character
#' @param mixture.dre.name name of the mixture dose-response experiment included in the synergy experiment, character
#' @param compound.dre.name names of individual compound dose-response experiments that are included in the synergy experiment, character
#' @param dre.list object of class DREList or list of DRE objects
#' 
#' @return SynergyExperiment object
#' 
#' @details 
#' A single ray experiment is assumed in the current implementation, i.e. there should be only one mixture
#' dose-response experiment.  There should also be 2 or more compound dose-response experiments.
#' 
#' The dre.list argument is used to check that compound.dre.name and mixture.dre.name
#' refer to valid dose-response experiments and make sure compounds are listed in the same order in the synergy experiment object as 
#' in the corresponding mixture dose-response experiment.  
#' 
#' @examples
#' data(sim15_screen3)
#' new("SynergyExperiment", name="Cpd1-Cpd2", mixture.dre.name="Cpd1-Cpd2", 
#'     compound.dre.name=c("Cpd1","Cpd2"), dre_list(sim15_screen3))
#' 
#' @seealso \code{\linkS4class{DRE}}
#' 
setMethod("initialize", "SynergyExperiment", 
          function(.Object, name=NULL, mixture.dre.name, compound.dre.name, dre.list) {
            #  Check validity of arguments
            stopifnot(any(is.null(name),is.character(name)))
            stopifnot(is.character(mixture.dre.name), length(mixture.dre.name)==1)
            stopifnot(is.character(compound.dre.name), length(compound.dre.name)>1)
            stopifnot(all(sapply(dre.list, is, "DRE")))
            names(dre.list) = sapply(dre.list, function(x) x@name)
            stopifnot(mixture.dre.name %in% names(dre.list), all(compound.dre.name %in% names(dre.list)))
            stopifnot(length(dre.list[[mixture.dre.name]]@compound.name)>1)
            stopifnot(all(sapply(dre.list[compound.dre.name], function(x) length(x@compound.name)==1)))
            
            #  Set .Object@name and .Object@mixture.dre.name
            .Object@name = ifelse(is.null(name),mixture.dre.name,name)
            .Object@mixture.dre.name = mixture.dre.name
            
            #  Set .Object@compound.dre.name while making sure compound dose-response experiments are listed in the same order
            #  as the corresponding compounds in the mixture dose-response experiment object
            mixture.dre = dre.list[[mixture.dre.name]]
            mix.compounds = mixture.dre@compound.name
            compound.dre = dre.list[compound.dre.name]
            cs.names = names(compound.dre)
            cs.compounds = sapply(compound.dre, function(x) x@compound.name)
            .Object@compound.dre.name = character()
            for (mc in mix.compounds) {
              .Object@compound.dre.name = c(.Object@compound.dre.name,cs.names[which(cs.compounds==mc)])
            }
            
            #  All done
            return(.Object)
          })

#### SynergyExperiment plot method ####
#' Plot a SynergyExperiment object
#' 
#' Isobole plot of a single ray experiment
#' 
#' @param x object of class SynergyExperiment
#' @param y no parameter \code{y} is defined for this method
#' @param dre.list a list of DRE objects where the data come from
#' @param effect vector of fractional effect values to draw the isoboles for, numeric
#' @param model name of the growth curve model to use, character
#' @param xlab a title for the x axis.  If left unspecified, appropriate default is chosen.
#' @param ylab a title for the y axis.  If left unspecified, appropriate default is chosen.
#' @param ... additional arguments passed to the generic \code{\link[graphics]{plot}} function
#' 
#' @details 
#' \link{plotIsoboles} is a convenient wrapper for this method.
#' 
#' effect value should be between 0 and 1, e.g. 0.5 to compute interaction index at IC50, 0.25 to compute IC25 (25% inhibition) etc.
#' Corresponding value of the response variable is defined as control.response*(1-effect)
#' 
#' @examples
#' data(sim15_screen3)
#' plot(synergy_experiment(sim15_screen3,"Cpd1-Cpd2"), dre.list=dre_list(sim15_screen3))
#' 
#' @seealso \link{plotIsoboles} \code{\linkS4class{SynergyExperiment}}, \code{\linkS4class{DRE}}, \code{\link{effectiveDose}}, \code{\link[graphics]{plot}}
#' 
setMethod("plot",
          signature(x = "SynergyExperiment", y="missing"),
          function (x, y, dre.list, effect=c(0.25,0.5,0.75), model="menls", xlab=NULL, ylab=NULL, ...) 
          {
            #  Validate the arguments
            stopifnot(all(sapply(dre.list, is, "DRE")))
            names(dre.list) = sapply(dre.list, function(x) x@name)
            stopifnot(x@mixture.dre.name %in% names(dre.list), all(x@compound.dre.name %in% names(dre.list)))
            stopifnot(is.numeric(effect))
            stopifnot(is.character(model), length(model)==1)
            
            #  Plot individual compound doses D1 and D2
            # - individual compound ray data
            compound.dre = dre.list[x@compound.dre.name]
            D1 = c(0,compound.dre[[1]]@dose)
            D2 = c(0,compound.dre[[2]]@dose)
            #  - mixture ray data
            mix.dre = dre.list[[x@mixture.dre.name]]
            d1 = mix.dre@dose * mix.dre@fraction[1]
            d2 = mix.dre@dose * mix.dre@fraction[2]
            
            #  - plot 
            if (is.null(xlab)) xlab = compound.dre[[1]]@compound.name
            if (is.null(ylab)) ylab = compound.dre[[2]]@compound.name
            plot(x=c(0,D1), y=rep(0,length(D1)+1), xlim=c(0,max(d1)), ylim=c(0,max(d2)), xlab=xlab, ylab=ylab, type="b", ...)
            lines(x=rep(0,length(D2)+1),y=c(0,D2),type="b")
            lines(c(0,d1),c(0,d2),type="b")
            
            #  Plot isoboles
            for (i in 1:length(effect)) {
              eff.D1 = effectiveDose(compound.dre[[1]],effect[i],model=model)
              eff.D2 = effectiveDose(compound.dre[[2]],effect[i],model=model)
              eff.mix.dose = effectiveDose(mix.dre,effect[i],model=model)
              eff.d1 = eff.mix.dose * mix.dre@fraction[1]
              eff.d2 = eff.mix.dose * mix.dre@fraction[2]
              lines(c(0,eff.d1,eff.D1),c(eff.D2,eff.d2,0),col=i+1,lwd=2)
            }
            
            #  Plot the legend
            legend("top",legend=effect,lty=1,lwd=2,col=seq(2,length(effect)+1,by=1),cex=0.8)
          })

