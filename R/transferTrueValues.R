# simulate generic and methods
#' @include Compound.R DRE.R SynergyScreen.R
NULL

#### transferTrueValues generic ####
#' Transfer "true" values of various parameters from one object to another
#' 
#' @details
#' These methods are useful in multi-step simulations, when "ground truth" values, e.g. "true" values of \emph{IC50} and \emph{m} parameters of 
#' dose-response curves, need to be transferred from one step to the next
#' 
#' @param from object to transfer values from
#' @param to object to transfer values to
#'  
#' @return Object \code{"to"} with modified slots
#'
#' ##  Stage 1: simulate individual compounds and their dose-response experiments
#' # Load a set of compounds and initialize a SynergyScreen object
#' cpds = readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_1.csv",
#'                                     package="SynergyScreen"))
#' screen1 = new("SynergyScreen",compound_list=cpds)
#' 
#' #  Generate screen design, only testing single compounds at first
#' screen1 = generateDesign(screen1,pairs=FALSE)
#' 
#' #  Simulate compound growth curve characteristics and raw dose-response data
#' set.seed(20141120)
#' screen1 = simulate(screen1)
#' 
#' #  Normalize screen data, adjusting for plate bias
#' screen1 = normalize(screen1)
#' 
#' #  Check ranges of dose-response experiments
#' screen1 = checkRange(screen1)
#' 
#' ## Stage 2: re-do dose-response experiments for which dose range checks failed, 
#' ## with corrected dose ranges
#' #  Load the new set of compounds, initialize a SynergyScreen object and generate a design
#' cpds2 = readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_2.csv",
#'                                      package="SynergyScreen"))
#' screen2 = new("SynergyScreen",compound_list=cpds2)
#' screen2 = generateDesign(screen2,pairs=FALSE)
#' 
#' #====================================================================================#
#' ### Transfer previously simulated "true" IC50 and m values to compounds in screen2 ###
#' screen2 = transferTrueValues(screen1,screen2)
#' compound(screen1,"Cpd1")
#' compound(screen2,"Cpd1")
#' #====================================================================================##
#' 
#' #  Simulate dose-response data again
#' set.seed(20141121)
#' screen2 = simulate(screen2)
#' screen2 = normalize(screen2)
#' screen2 = checkRange(screen2)
#'
#' @seealso \code{\linkS4class{Compound}}, \code{\linkS4class{DRE}}, \code{\linkS4class{SynergyScreen}},
#' 
setGeneric("transferTrueValues", function(from,to) standardGeneric("transferTrueValues"))

#### Compound transferTrueValues method ####
#' @describeIn transferTrueValues Transfer "true" \emph{IC50} and \emph{m} values between simulated \code{Compound} objects
#' 
setMethod("transferTrueValues",
          signature = c(from="Compound", to="Compound"),
          function (from,to) {
            to@ic50 = from@ic50
            to@m = from@m
            return(to)
          })

#### DRE transferTrueValues method ####
#' @describeIn transferTrueValues Transfer previously simulated "true" \emph{IC50} and \emph{m} values from one DRE object to another
#' 
setMethod("transferTrueValues",
          signature = c(from="DRE", to="DRE"),
          function (from,to) {
            to@true.ic50 = from@true.ic50
            to@true.m = from@true.m
            return(to)
          })

#### SynergyScreen transferTrueValues method ####
#' @describeIn transferTrueValues Transfer previously simulated "true" \emph{IC50} and \emph{m} values from one SynergyScreen object to another
#' 
setMethod("transferTrueValues",
          signature = c(from="SynergyScreen", to="SynergyScreen"),
          function (from,to) {
            #  Transfer values for compound objects
            cpd_names = intersect(names(from@compound_list),names(to@compound_list))
            for (cn in cpd_names) {
              to@compound_list[[cn]] = transferTrueValues(from@compound_list[[cn]],to@compound_list[[cn]])
            }
            #  Transfer values for DRE objects
            dre_names = intersect(names(from@dre_list),names(to@dre_list))
            for (dn in dre_names) {
              to@dre_list[[dn]] = transferTrueValues(from@dre_list[[dn]],to@dre_list[[dn]])
            }
            #  All done
            return(to)
          })
