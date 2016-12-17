#  designSummary generic and methods
#' @include SynergyScreen.R
NULL

#### designSummary generic ####
#' Output a summary table for synergy screen design with exponential dose series.
#' 
#' The summary table simplifies setting up experiments in the lab by reporting dilution factors.
#' 
#' @param object object of appropriate class, i.e. \code{SynergyScreen}
#' @param ... method-specific arguments: see below
#' @param dilution.factor.decimals interger number of decimals in the dilution factor
#' 
#' @return Data frame describing dose-response experiments
#' 
#' @seealso \linkS4class{SynergyScreen}
#' 
setGeneric("designSummary", function(object, ...) standardGeneric("designSummary"))

#### Method for SynergyScreen ####
#' @describeIn designSummary 
setMethod("designSummary", "SynergyScreen", function(object, dilution.factor.decimals=2) {
  # browser()
  
  #  Verify that dilution.factor.decimals is an integer
  stopifnot(all.equal(round(dilution.factor.decimals),dilution.factor.decimals))
  
  #  Retrieve screen design
  design = design(object)
  
  #  Only keep design rows that correspond to dose-response experiments
  design = subset(design, ! experiment %in% c("untreated","blank"))
  design$row = as.character(design$row)
  
  #  Create and populate design data frame.  Each row corresponds to a dose-response experiment.
  #  Populate row by row.
  experiment = unique(design$experiment)
  result = data.frame(plate = rep(NA,length(experiment)), row = NA, column = NA, cpd1 = NA, 
                      cpd1_max_dose = NA, cpd2 = NA, cpd2_max_dose = NA, dilution_factor = NA)
  row.names(result) = experiment
  for (exp1 in experiment) {
    exp.design = subset(design, experiment == exp1)
    result[exp1,"plate"] = unique(exp.design$plate)
    result[exp1,"row"] = paste(min(exp.design$row),max(exp.design$row),sep="-")
    result[exp1,"column"] = paste(min(exp.design$column),max(exp.design$column),sep=" to ")
    result[exp1,"cpd1"] = unique(exp.design$cpd1)
    result[exp1,"cpd1_max_dose"] = max(exp.design$dose1)
    result[exp1,"cpd2"] = unique(exp.design$cpd2)
    result[exp1,"cpd2_max_dose"] = max(exp.design$dose2)
    dilution.factor = (max(exp.design$dose1)/min(exp.design$dose1))^(1/(nrow(exp.design)-1))
    result[exp1,"dilution_factor"] = round(dilution.factor, dilution.factor.decimals)
  }
  
  #  All done
  return(result)
})
