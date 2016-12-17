#  checkRange generic and methods
#' @include DRE.R SynergyScreen.R
NULL

#### checkRange generic #####
#' Check response ranges of dose-response experiments.  
#' 
#' A good titration should include doses that produce an informative range of effect values. This method checks if that is the case, prints 
#' diagnostic messages and populates @@effect and @@comment slots of the input object.  In case of a SynergyScreen object, these are inside 
#' individual members of its @@dre_list slot
#' 
#' @param object object of a supported class, e.g. DRE or SynergyScreen
#' 
#' @return Object of the same class with @@effect slot(s) populated with effect values and 
#' diagnostic message string(s) appended to the @@comment slot(s)
#' 
#' @details
#' The assessment of a dose-response experiment's dose range is based on the range of produced effects.  
#' 
#' The effect is defined as fractional decrease in the response value, e.g. OD of the cell culture.  It ranges between 0 and 1, 
#' 0 being untreated control and 1 complete growth inhibition and is defined as follows:
#' 
#' \emph{effect = 1 - response/control.response}
#' 
#' A dose-response experiment's dose range is considered "good" if all of the following is true
#' \itemize{
#'   \item the experiment includes observed effect values below 0.25 and above 0.75
#'   \item at least 30\% of all observed effect values fall between 0.25 and 0.75
#' }
#' 
#' The following diagnostics can be produced:
#' \itemize{
#'   \item "dose range is OK" - all criteria of good range specified above are met 
#'   \item "dose range too narrow" - the difference between the highest and the lowest effect values is < 0.5
#'   \item "poor coverage of mid-range effect region" - less than 30% of effect values fall between 0.25 and 0.75
#'   \item "high dose region not covered" - no effect values above 0.75
#'   \item "low dose region not covered" - no effect values below 0.25
#' }
#' 
#' @examples
#' data(sim15_screen3)
#' 
#' #  Check the ranges of all dose-response experiments in a screen
#' sim15_screen3 = checkRange(sim15_screen3)
#' 
#' #  checkRange populates @@comment attributes of dre objects
#' comment(dre(sim15_screen3,"Cpd7"))
#' 
#' #  see all comments
#' lapply(dre_list(sim15_screen3), function(x) comment(x))
#' 
#' #  apply checkRange to an individual dose-response experiment
#' x = checkRange(dre(sim15_screen3,"Cpd1-Cpd2"))
#' 
#' @seealso \code{\linkS4class{DRE}}, \code{\linkS4class{SynergyScreen}}
#' 
setGeneric("checkRange", function(object) standardGeneric("checkRange"))

#### DRE checkRange method ####
#' @describeIn checkRange Assess dose range of a DRE object
#' 
setMethod("checkRange", "DRE", 
          function (object) {
            #  Initialize comments to an empty vector
            comment = character()
            
            #  Compute effect values
            effect = 1 - object@response/object@control.response
            
            #  Check if the range is wide enough
            range = max(effect) - min(effect)
            if (range < 0.5) comment = c(comment,"dose range too narrow")
            
            #  Check coverage of mid-range effect region
            eff.in.iqr = effect > 0.25 & effect < 0.75
            frac.in.iqr = length(effect[eff.in.iqr])/length(effect)
            if (frac.in.iqr < 0.3) comment = c(comment,"poor coverage of mid-range effect region")
            
            #  Check if the range is too low or too high
            if (max(effect) < 0.75) comment = c(comment,"high dose region not covered")
            if (min(effect) > 0.25) comment = c(comment,"low dose region not covered")
            
            #  If all checks succeeded, say that the range is good
            if (length(comment)==0) comment = "dose range is OK"
            
            # All done
            cat("dose-response experiment ", object@name, " diagnostics: ", paste(comment,collapse="; "), "\n")
            object@effect = effect
            object@comment = comment
            return(object)
          })

#### SynergyScreen checkRange method ####
#' @describeIn checkRange Assess dose range of a SynergyScreen object
#' 
#' Assess dose ranges of all dose-response experiments included in a synergy screen, i.e. individual members of the @@dre_list slot
#' of a SynergyScreen object.
#' 
setMethod("checkRange", "SynergyScreen", 
          function (object) {
            dre_list = object@dre_list
            dre_list = lapply(dre_list, checkRange)
            dre_list = as(dre_list,"DREList")
            object@dre_list = dre_list
            return(object)
          })
