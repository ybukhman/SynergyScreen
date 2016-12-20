#  Functions that are not S4 methods

#### readCompoundFile function ####
#' Load a spreadsheet of compounds
#' 
#' Reads in a CSV file with columns "compound" and "max.dose" into a list of Compound objects
#' 
#' @param file file to read from
#' 
#' @return a list of Compound objects
#' 
#' @examples
#' cpds = readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_1.csv",
#'                                     package="SynergyScreen"))
#' cpds[[1]]
#' 
readCompoundFile <- function (file) {
  cpds = read.csv(file,as.is=T)
  stopifnot("compound" %in% names(cpds)  && "min.dose" %in% names(cpds) && "max.dose" %in% names(cpds))
  cpds2 = list()
  for (i in 1:nrow(cpds)) {
    cpds2[[i]] = new("Compound", name = cpds[i,"compound"], min.dose = cpds[i,"min.dose"], max.dose = cpds[i,"max.dose"])
  }
  names(cpds2) = sapply(cpds2, function(x) x@name)
  cpds2 = as(cpds2,"CompoundList")
  return(cpds2)
}

#### designFile2SynergyScreen ####
#' Initialize a SynergyScreen object from a design file
#' 
#' Read in a design file and build a SynergyScreen object from it
#' 
#' @param file char design file name
#' 
#' @return a SynergyScreen object
#' 
#' @details
#' This function creates a new SynergyScreen object and populates slots @@design, @@compound_list, @@dre_list, and 
#' @@synergy_experiment_list
#' 
#' Design file is expected in "long" format.  It must contain the columns specified in \code{\linkS4class{ScreenDesign}}.
#' 
#' @examples
#' screen1 <- designFile2SynergyScreen(system.file("extdata/8_compounds/8_compounds_design_actually_used.csv", 
#'                                                package="SynergyScreen"))
#' str(screen1)
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}, \code{\linkS4class{ScreenDesign}}
#' 
designFile2SynergyScreen <- function (file) {
  #  Read in design data, initialize a new screen and set the @design slot
  design = read.csv(file,as.is=T)
  screen = new("SynergyScreen")
  design(screen) = design
  
  #  Set screen@compound_list
  cpds = unique(c(design$cpd1,design$cpd2))
  cpds = cpds[!is.na(cpds)]
  cpds2 = lapply(cpds, function(x) new("Compound", name=x))
  names(cpds2) = sapply(cpds2, function(x) x@name)
  cpds2 = as(cpds2,"CompoundList")
  screen@compound_list = cpds2
  
  #  Extract dose-response experiment (DRE) names
  expts = unique(design$experiment)
  expts = expts[!expts %in% c("blank","untreated")]
  
  #  - A function to build a new DRE object from a slice of design
  .buildDRE = function(x) {
    #  Extract design slice for the current DRE
    slice = subset(design, design$experiment == x)
    
    #  Figure out compound names
    cpd1 = unique(slice$cpd1)
    cpd2 = unique(slice$cpd2)
    stopifnot(length(cpd1)==1, length(cpd2)==1, !is.na(cpd1))
    if (is.na(cpd2)) {
      cpd.names =  cpd1
    } else {
      cpd.names = c(cpd1,cpd2)
    }
    
    #  Figure out doses and fractions
    stopifnot(all(!is.na(slice$dose1)))
    if (is.na(slice$dose2[1])) {
      #  Single compound experiment
      stopifnot(all(is.na(slice$dose2[1])))
      dose = slice$dose1
      fraction = 1
    } else {
      #  Mixture experiment
      stopifnot(all(!is.na(slice$dose2)))
      dose = slice$dose1 + slice$dose2
      frac1 = slice$dose1/dose
      stopifnot(isTRUE(all.equal(frac1,rep(frac1[1],length(frac1)),tolerance=0.01)))
      fraction = c(frac1[1], 1-frac1[1])
    }
    
    #  Create new DRE object
    new("DRE", compound.name = cpd.names, name = x, compound = screen@compound_list, fraction=fraction, dose=dose)
  }
  
  #  Set screen@dre_list
  dre_list = lapply(expts, .buildDRE)
  names(dre_list) = sapply(dre_list, function(x) x@name)
  dre_list = as(dre_list,"DREList")
  screen@dre_list = dre_list
  
  #  Set screen@synergy_experiment_list
  if (!all(is.na(design$cpd2))) {
    synergy_experiment_list = list()
    pairs = sapply(dre_list, function(x) length(x@compound.name)==2)
    for (dre in dre_list[pairs]) {
      exp1 = new("SynergyExperiment", 
                 mixture.dre.name=dre@name, compound.dre.name=dre@compound.name, dre.list = dre_list)
      synergy_experiment_list = c(synergy_experiment_list,exp1)
    }
    names(synergy_experiment_list) = sapply(synergy_experiment_list, function(x) x@name)
    screen@synergy_experiment_list = as(synergy_experiment_list,"SynergyExperimentList")
  }
  
  #  All done 
  return(screen)
}

#### effectiveDose function ####
#' Compute the dose that causes specified fractional effect
#' 
#' Take a DRE (dose-response experiment) object and compute the dose that causes the specified fractional effect
#' 
#' @param dre a DRE object
#' @param effect fractional effect value, numeric
#' @param model name of the model to use, character
#' 
#' @return dose value that causes the effect, numeric
#' 
#' @details 
#' effect value should be between 0 and 1, e.g. 0.5 to compute interaction index at IC50, 0.25 to compute IC25 (25% inhibition) etc.
#' Corresponding value of the response variable is defined as control.response*(1-effect)
#' 
#' If the effect is not achieved by doses between 0 and max dose for the dose-response experiment,
#' effectiveDose produces a warning and returns NA
#' 
#' @examples
#' data(sim15_screen3)
#' effectiveDose(dre(sim15_screen3,"Cpd1"),0.5)
#' 
#' @seealso \code{\linkS4class{DRE}}
#' 
effectiveDose = function(dre, effect, model="menls") {
  
  #  debug
  # browser()
  
  #  Verify the arguments
  if (!is(dre,"DRE")) stop("dre argument must be a DRE object")
  if (!is.numeric(effect)) stop("effect argument must be numeric")
  if (length(effect) != 1) stop("effect must be of length 1")
  if (length(model) != 1) stop("model must be of length 1")
  if (!is(dre@model[[model]],"DRModel")) stop("dre object does not contain a valid model of the name ", model)
  
  #  Compute the effective dose value
  if (model == "loess2") {
    dose = seq(min(dre@dose), max(dre@dose), length.out=100)
  } else {
    dose = seq(0, max(dre@dose), length.out=100)
  }
  resp = predict(dre@model[[model]],dose)
  if (any(is.na(resp))) {
    warning("Cannot predict responses for dose-response experiment ", dre@name, " using model ", model)
    return(NA)
  }
  desired.resp = dre@control.response*(1-effect)
  if (desired.resp < min(resp)) {
    warning("effect ", effect, " out of range for dose-response experiment ", dre@name)
    return(NA)
  } else {
    diffs = abs(resp-desired.resp)
    effective.dose = dose[which(diffs == min(diffs))]
    return(effective.dose)
  }
}

#### interactionIndex function ####
#' Compute Interaction Index based on Loewe Additivity 
#' 
#' Compute Interaction Index from SynergyExperiment and the matching DRE (dose-response experiment) objects
#' 
#' @param experiment an object of class SynergyExperiment
#' @param effect fractional effect value, numeric
#' @param dre.list a list of DRE objects
#' @param model name of the model to use (the same model must be present in all dre.list members)
#' 
#' @return interaction index value, numeric
#' 
#' @details 
#' effect value should be between 0 and 1, e.g. 0.5 to compute interaction index at IC50, 0.25 to compute IC25 (25% inhibition) etc.
#' Corresponding value of the response variable is defined as control.response*(1-effect)
#' 
#' @examples
#' data(sim15_screen3)
#' interactionIndex(synergy_experiment(sim15_screen3,"Cpd1-Cpd2"), 0.5, dre_list(sim15_screen3))
#' 
#' @seealso \code{\linkS4class{SynergyExperiment}}, \code{\linkS4class{DRE}}
#' 
interactionIndex = function(experiment, effect, dre.list, model="menls"){
  
  #  Debug
  # browser()
  
  #  Verify arguments
  stopifnot(is(experiment,"SynergyExperiment"), length(experiment) == 1,
            is.numeric(effect), length(effect) == 1,
            all(sapply(dre.list, function(x) {is(x,"DRE")})))
  
  #  Check that all of the necessary dose-response experiment objects are supplied
  names(dre.list) = sapply(dre.list, function(x) x@name)
  if (!all(experiment@compound.dre.name %in% names(dre.list)) || !all(experiment@mixture.dre.name %in% names(dre.list))) 
    stop("All compound and mixture DRE listed in the SynergyExperiment object must be supplied in the dre.list argument")
  
  #  Compute equivalent dose values
  #  (at this point, assuming a single ray experiment)
  #  - Compounds in the mixture
  mix.dre = dre.list[[experiment@mixture.dre.name]]
  mix.dose = effectiveDose(mix.dre, effect, model)
  mix.cpd.dose = mix.dose*mix.dre@fraction
  #  - Individual compounds on their own
  cpd.dre = dre.list[experiment@compound.dre.name]
  cpd.dose = sapply(cpd.dre, effectiveDose, effect, model)
  
  #  Check that all effective doses are defined and positive
  eff.doses = c(mix.dose,cpd.dose)
  if (any(is.na(eff.doses), eff.doses <= 0)) return(NA)

  #  Compute Loewe additivity-based interaction index
  laii = sum(mix.cpd.dose/cpd.dose)
  #return(list(mix.dose=mix.dose,mix.cpd.dose=mix.cpd.dose,cpd.dose=cpd.dose,laii=laii))
  return(laii)
}

#### meq function ####
#' Median Effect Equation
#' 
#' Compute the value of the Median Effect Equation for dose d
#' 
#' @param d dose
#' @param ic50 IC50 parameter of the median effect equation
#' @param m m parameter of the median effect equation
#' @param control.response response value of untreated control
#' 
#' @return value of the Median Effect Equation for dose d
#'
#' @examples
#' meq(d=seq(0,20,by=5), ic50=2, m=1, control.response=1)
#' 
meq = function(d,ic50,m, control.response) control.response/((d/ic50)^m+1)

#### plotMEQ function ####
#' Plot median effect equation
#' 
#' Plot median effect equation
#' 
#' @param ic50 IC50 parameter of the median effect equation
#' @param m m parameter of the median effect equation
#' @param control.response response value of untreated control
#' @param dose.range range of doses, a vector of 2 real positive numbers
#'
#' @examples
#' plotMEQ(ic50=2, m=1, control.response=1, dose.range=c(0.1,30))
#' 
plotMEQ = function(ic50,m,control.response,dose.range) {
  logd = seq(from=log(dose.range[1]), to=log(dose.range[2]), length.out=100)
  d = exp(logd)
  plot(logd,meq(d,ic50,m,control.response))
}

#### readWideData function  ####
#' Read synergy screen data in a wide format
#' 
#' Read in synergy screen data in a wide format and reshape it into a long format
#' 
#' @param file character file
#' 
#' @return data frame suitable for conversion to a ScreenData object
#' 
#' @seealso \code{\linkS4class{ScreenData}}
#' 
readWideData <- function (file) {
  #  Read in the data
  wide = read.csv(file,as.is=T)
  
  #  Parse out plate names
  plate.ind = seq(from=1, to=nrow(wide), by=10)
  plates = wide[plate.ind,1]
  plates = sub("^Plate ","",plates)
  
  #  Initialize the long data frame
  long = data.frame(plate=rep(plates,each=96), column=rep(1:12,each=8), row=LETTERS[1:8], response=NA)
  
  #  Parse the response data  into a vector
  data.ind = seq(from=2, to=nrow(wide), by=10)
  response = numeric()
  for (ind in data.ind) {
    resp = unlist(wide[ind:(ind+7),2:13])
    response = c(response,resp)
  }
  
  #  Add the response data to the long data frame
  long$response = response
  
  #  All done
  return(long)
}

#### readWideDataFlex function  ####
#' Read synergy screen data in a wide format from a CSV file of arbitrary layout
#' 
#' Read in synergy screen data in a wide format and reshape it into a long format
#' 
#' @param file character file
#' @param row numeric row coordinates of plate data blocks
#' @param col character column coordinates of plate data blocks
#' 
#' @return data frame suitable for conversion to a ScreenData object
#' 
#' @details 
#' This function reads data from a CSV file.  Data for each plate are in 8 row X 12 column blocks.
#' Parameters \code{row} and \code{col} specify the coordinates of the upper left corner of each block.
#' \code{col} is type character, because the function expects Excel column names.
#' 
#' @seealso \code{\linkS4class{ScreenData}}
#' 
readWideDataFlex <- function (file, row, col) {
  #  Read in the data
  wide = read.table(file, sep=",", as.is = TRUE, fill = TRUE, blank.lines.skip = FALSE, comment.char = "")
  excel.cols = paste(rep(c("",LETTERS),each=26),LETTERS,sep="")
  names(wide) = excel.cols[1:ncol(wide)]
  
  #  Initialize the long data frame
  long = data.frame(plate=rep(1:length(row),each=96), column=rep(1:12,each=8), row=LETTERS[1:8], response=NA)
  
  #  Parse the response data  into a vector
  response = numeric()
  for (i in 1:length(row)) {
    col.num = which(names(wide) == col[i])
    resp = unlist(wide[row[i]:(row[i]+7),col.num:(col.num+11)])
    response = c(response,resp)
  }
  
  #  Add the response data to the long data frame
  long$response = as.numeric(response)
  
  #  All done
  return(long)
}

#### readTecanFile ####
#' Read Tecan file
#' 
#' Read in a Tecan file in CSV format and extract the difference in response values (e.g. OD) at a specified time point vs. zero time
#' 
#' @param file Tecan file name, character
#' @param time time point, hours, numeric
#' 
#' @return Data frame of response values in wells.  It is suitable for conversion to a SynergyData object upon addition of a 
#' \code{plate} column.
#' 
#' @seealso \link{readTecanData}
#' 
readTecanFile <- function (file,time) {
  #  Read in the data
  data1 = read.csv(file,skip=2)
  
  #  Remove comments at the bottom
  data1 = subset(data1, !is.na(data1$A1))
  
  #  Reformat times to numeric
  names(data1)[1] = "time"
  data1$time = sub("s$","",data1$time)
  data1$time = as.numeric(data1$time)
  
  #  Find the row that is closest to the target time and compute growth as its difference with the first row
  #  - find the relevant time point
  diffs = data1$time - time*3600
  end.time = which(diffs == min(abs(diffs)))
  
  #  - find the relevant data columns
  column = rep((1:12),each=8)
  row = rep(LETTERS[1:8],12)
  data.cols = paste(row,column,sep="")
  
  #  - find the difference in response values
  end.point = data1[end.time,data.cols] 
  zero.point = data1[1,data.cols]
  growth = end.point - zero.point
  
  #  Populate result data frame
  result = data.frame(column,row,response=unlist(growth))
  
  #  All done
  return(result)
}

#### readTecanData ####
#' Extract raw data from a set of Tecan files
#' 
#' Parse Tecan files one by one and assemble a data frame of results.  
#' This data frame is suitable for conversion to a SynergyData object.
#' 
#' @param path path to a folder with Tecan files, character.  The folder must contain Tecan files in CSV format only.
#' @param time time point, hours, numeric
#' 
#' @seealso \linkS4class{SynergyData}
#' 
readTecanData <- function (path, time) {
  #  Get a listing of Tecan files
  files = dir(path)
  files = paste(path,files,sep="/")
  
  #  Parse Tecan files one by one and assemble a data frame of results
  for (plate in 1:length(files)) {
    res1 = readTecanFile(file=files[plate], time=time)
    res1$plate = plate
    if (plate == 1) {
      result = res1
    } else {
      result = rbind(result,res1)
    }
  }
  
  #  All done
  result = result[c("plate","column","row","response")]
  return(result)
}
