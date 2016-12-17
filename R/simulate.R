# simulate generic and methods
#' @include Compound.R DRE.R SynergyScreen.R
NULL

#### simulate generic ####
#' Simulate an object.  
#' 
#' Depending on the input object, randomly set ic50 and m parameters of median effect equations and simulate dose-response data
#' for a single experiment or an entire screen
#' 
#' @param object object of a supported class, e.g. Compound, DRE or SynergyScreen
#' @param ... method-specific arguments: see below
#'
#' @param log.ic50.cv coefficient of variation, i.e sd/mean, of the distribution that log(IC50) is selected from, numeric
#' @param m.range the range of values to select m from, numeric vector of length 2
#' @param replace replace pre-existing values of @@ic50 and @@m? logic
#' 
#' @param compound list of Compound objects (DRE method)
#' @param control.response "true" response value (e.g. OD) of untreated control, numeric
#' @param rel.resp.noise average relative noise of the response variable, i.e. mean(random.error/response)
#' 
#' @param plate.bias.sd standard deviation of plate bias values, see Details, numeric
#' @param blank.response average response value of blank wells, numeric
#' @param digits number of decimal places in dose values (SynergyScreen method)
#' 
#' @return object of the same class as the input object
#'  
#' @details
#' ic50 and m are parameters of the median effect equations for compounds and mixtures.  They are simulated as follows:
#' \enumerate{
#'   \item log(ic50) of a compound is chosen from a normal distribution centered around 
#'   0.5*(log(compound@@min.dose) + log(compound@@max.dose)) 
#'   with coefficient of variation specified by parameter log.ic50.cv
#'   \item IC50 of a mixture is chosen from a log-normal distribution centered around 1/(sum(fraction[i]/compound[i]@@ic50)) 
#'   with coefficient of variation specified by parameter log.ic50.cv
#'   \item m is chosen from a uniform distribution between m.range[1] and m.range[2]
#' }
#' 
#' If Compound object(s) in the input already have their @@ic50 and @@m slots populated and \code{replace} is set to FALSE, the pre-existing
#' values are retained.
#'  
#' For dose-response experiments, represented by DRE objects, response at each dose is chosen from a normal distribution with mean 
#' determined by the median response equation, i.e. expected.response = control.response / ((dose/ic50)^m + 1), 
#' and standard.deviation = expected.response*rel.resp.noise
#' 
#' In a multi-plate screen, plate bias is multiplicative.  All wells that contain cell cultures are multiplied by a bias value.  
#' The bias values are chosen from a normal distribution with the mean of 1 and standard deviation of plate.bias.sd
#' 
#' @examples
#' #  Load a set of compounds, initialize a SynergyScreen object, and generate a design
#' cpds3 = readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_3.csv",
#'                                      package="SynergyScreen"))
#' screen3 = new("SynergyScreen",compound_list=cpds3)
#' screen3 = generateDesign(screen3)
#' 
#' #  Simulate dose-response characteristics of compounds, mixtures and dose-response data
#' screen3_sim = simulate(screen3)
#' 
#' #  Simulation sets ic50 and m
#' compound(screen3,"Cpd1")
#' compound(screen3_sim,"Cpd1")
#' true.ic50(dre(screen3,"Cpd1"))
#' true.ic50(dre(screen3_sim,"Cpd1"))
#' 
#' #  Simulation sets response values
#' response(dre(screen3,"Cpd1"))
#' response(dre(screen3_sim,"Cpd1"))
#' 
#' @seealso \linkS4class{SynergyScreen}, \linkS4class{Compound}, \linkS4class{DRE}
#' 
setGeneric("simulate", function(object,...) standardGeneric("simulate"))

#### Compound simulate method ####
#' @describeIn simulate Simulate Compound dose-response characteristics
#' 
#' Randomly set ic50 and m parameter of the median effect equation for compound object
#' 
setMethod("simulate","Compound",
          function (object, log.ic50.cv=1, m.range=c(0.5,2.5), replace=F) {
            #  Assign IC50
            if (length(object@ic50)==0 || replace) {
              log.ic50.mean = 0.5*(log(object@max.dose) + log(object@min.dose))
              log.ic50 = rnorm(1, mean=log.ic50.mean, sd=abs(log.ic50.mean*log.ic50.cv))
              object@ic50 = exp(log.ic50)
            }
            
            #  Assign m
            if (length(object@m)==0 || replace) {
              object@m = runif(1, min = m.range[1], max = m.range[2])
            }
            
            #  All done
            return(object)
          }  
)

#### DRE simulate method ####
#' @describeIn simulate Simulate DRE data
#' 
#' Simulate dose-response experiment data based on the median effect equation with randomly set parameters IC50 and m
#' 
setMethod("simulate","DRE",
          function (object, compound, control.response=1, log.ic50.cv=1, m.range=c(0.5,2.5), rel.resp.noise=0.1, replace=F) {
            
            #  Debug
            # if (object@name == "Cpd2") browser()
            
            #  Use only compounds whose names are listed in this dose-response experiment object
            names(compound) = sapply(compound, function(x) x@name)
            if (!all(object@compound.name %in% names(compound))) stop("compound list must contain all compounds listed in object@compound.name")
            compound = compound[object@compound.name]
            
            #  Assign control.response
            object@control.response = control.response
            
            #  Set "true" IC50 if it is not pre-set in the input object
            if (length(object@true.ic50)==0 || replace) {
              if (length(compound) == 1) {
                #  - if we have a single compound, true IC50 of the dose-response experiment is the same as that compound's
                object@true.ic50 = compound[[1]]@ic50
              } else {
                #  - if more than one compound, simulate
                inv.ic50.mean = 0 # inverse of the expected IC50 in the absense of an interaction
                for (i in 1:length(object@fraction)) {
                  inv.ic50.mean = inv.ic50.mean + object@fraction[i]/compound[[i]]@ic50
                }
                log.ic50.mean = -log(inv.ic50.mean)
                log.ic50 = rnorm(1, mean=log.ic50.mean, sd=abs(log.ic50.mean*log.ic50.cv))
                object@true.ic50 = exp(log.ic50)
              }              
            }
            
            #  Set "true" m  if it is not pre-set in the input object
            if (length(object@true.m)==0 || replace) {
              if (length(compound) == 1) {
                #  - if we have a single compound, m of the dose-response experiment is the same as that compound's
                object@true.m = compound[[1]]@m
              } else {
                object@true.m = runif(1, min = m.range[1], max = m.range[2])
              }
            }
            
            #  Simulate response values
            respf = function(dose,control.response,ic50,m,rel.noise) {
              expectation = control.response / ((dose/ic50)^m + 1)
              resp = rnorm(1, mean = expectation, sd = expectation*rel.noise)
              return(resp)
            }
            object@response = sapply(object@dose, respf, object@control.response, object@true.ic50, object@true.m, rel.resp.noise)
            
            #  All done
            return(object)
          }  
)

#### SynergyScreen simulate method ####
#' @describeIn simulate Simulate a synergy screening experiment
#' 
#' Simulate an entire screen, including plate biases, control wells, median effect equation parameters for compounds
#' and mixtures, and dose-response experiment data
#' 
setMethod("simulate","SynergyScreen",
          function (object, control.response=1, plate.bias.sd = 0.2, blank.response=0.05, rel.resp.noise=0.1,
                    log.ic50.cv=1, m.range=c(0.5,2.5), digits=3, replace=F) {
            # Debug
            #browser()
            
            # Assign IC50 and m parameters of the median effect equation to each compound
            cpds = object@compound_list
            cpds = lapply(cpds, simulate, log.ic50.cv = log.ic50.cv, m.range = m.range, replace=replace)
            cpds = as(cpds,"CompoundList")
            
            # Simulate dose-response experiments
            dre_list = object@dre_list
            dre_list = lapply(dre_list, simulate, compound=cpds, control.response=control.response, log.ic50.cv=log.ic50.cv,
                              m.range=m.range, rel.resp.noise=rel.resp.noise, replace=replace)
            dre_list = as(dre_list,"DREList")
            
            #  Put simulated responses in a data frame
            data1 = data.frame(experiment = I(rep(sapply(dre_list, function(x) x@name),each=5)),
                               dose1 = round(as.vector(sapply(dre_list, function(x) x@fraction[1]*x@dose)), digits),
                               response = as.vector(sapply(dre_list, function(x) x@response)))
            
            #  Merge with design
            design = object@design
            data2 = merge(design,data1,by=c("experiment","dose1"),all.x=T)
            data2 = data2[order(data2$plate,data2$column,data2$row),]
            data2 = data2[c(names(design),"response")]
            
            #  Simulate controls
            filt = data2$experiment == "untreated"
            data2$response[filt] = rnorm(sum(filt), control.response, control.response*rel.resp.noise)
            
            #  Simulate blank data and add it to all wells
            filt = data2$experiment == "blank"
            data2$response[filt] = 0
            blank.data = rnorm(nrow(data2), blank.response, blank.response*rel.resp.noise)
            data2$response = data2$response + blank.data
            
            # Simulate plate bias
            plates = unique(data2$plate)
            plate.bias = rnorm(length(plates),1,plate.bias.sd)
            
            #  Apply plate bias to response values
            for (i in 1:length(plates)) {
              filt = data2$plate == plates[i]
              data2$response[filt] = data2$response[filt]*plate.bias[i]
            }
            
            #  Replace negative values with 0
            data2$response[data2$response<0] = 0
            
            #  Update and return the object
            object@compound_list = cpds
            object@dre_list = dre_list
            object@raw_data = as(data2[,c("plate","column","row","response")],"ScreenData")
            object
          })
