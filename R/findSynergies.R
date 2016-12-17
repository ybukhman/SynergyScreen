# find_synergies generic and methods
#' @include SynergyScreen.R
NULL

#### findSynergies ####
#' Find pairs of compounds that show synergy or antagonism
#' 
#' @param object object of class SynergyScreen
#' @param ... method-specific arguments: see below
#' @param statistic type of statistic to use to find synergistic or antagonistic compound pairs.  Can be "min", "max", "mean" or "median".
#' @param threshold threshold value that needs to be surpassed by the statistic
#' @param direction can be "higher" or "lower".  See Details. 
#' @param model character.  Only consider results from the specified model.  If NULL, consider all models.
#' 
#' @return object of type SynergyData
#' 
#' @details
#' This function filters the output of synergy_data(object) to report only compounds that are synergistic or antagonistic.
#' This is done by computing a statistic of the set of interaction index values for each pair of compounds and comparing it to 
#' the threshold.  
#' 
#' \strong{Example 1}.  To find pairs of compounds that show consistent synergy for all analyzed effect sizes, 
#' one may use statistic = "max", threshold = 0.7 and direction = "lower".  This will select pairs of compounds whose interaction
#' index does not exceed 0.7.
#' 
#' \strong{Example 2}.  To find pairs of compounds that show strong synergy that may or may not be consistent for all effect sizes,
#' one may use statistic = "min", threshold = 0.3 and direction = "lower".  This will select pairs of compounds whose interaction
#' index is below 0.3 for at least one effect size.
#' 
#' @examples
#' data(sim15_screen3)
#' 
#' #  Find strong synergies
#' findSynergies(sim15_screen3, statistic="max", threshold=0.5, direction="lower")
#' 
#' #  Find strong antagonisms
#' findSynergies(sim15_screen3, statistic="min", threshold=2, direction="higher")
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}, \code{\link[=SynergyScreen-accessors]{synergy_data}}
#' 
setGeneric("findSynergies", function(object, ...) standardGeneric("findSynergies"))

#### SynergyScreen findSynergies method ####
#' @describeIn findSynergies Find pairs of compounds that show synergy or antagonism
setMethod("findSynergies", "SynergyScreen", 
          function (object, statistic, threshold, direction, model=NULL) {
            #  Debug
            # browser()
            
            #  Filter by model
            data1 = synergy_data(object)
            if (!is.null(model)) {
              m1 = model
              data1 = subset(data1, model == m1)
            }
            
            # Drop NA values
            data1 <- data1[is.finite(data1$interaction.index),]
            
            #  Compute statistic for each experiment
            expr = paste("with(data1,tapply(interaction.index,experiment,",statistic,"))",sep="")
            stats = eval(parse(text=expr))
            stats = stats[is.finite(stats)]
            
            #  Find experiments that pass selection criteria
            if (direction=="lower") {
              hits = names(stats)[stats<threshold]
            } else if (direction=="higher") {
              hits = names(stats)[stats>threshold]
            } else {
              stop("Unsupported option: direction = ", direction)
            }
            
            #  Ouput results
            return(subset(data1, experiment %in% hits))
          })
