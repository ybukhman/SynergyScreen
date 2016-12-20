#  generateDesign generic and methods
#' @include DREList.R SynergyScreen.R
NULL

#### generateDesign generic ####
#' Generate design of microtiter plate-based synergy screening experiments.
#' 
#' Design specifies compound treatments, untreated controls and blanks that go in each well of a set of 
#' microtiter plates.  
#'
#' @param object an object of supported type, e.g. \code{DREList} or \code{SynergyScreen}
#' @param ... method-specific arguments: see below
#' @param type design type.  Supported types include "5 doses exp", "8 doses linear", "12 doses exp", 
#' and "custom".  See Details.
#' @param dre.orientation character.  Must be set if \code{type}="custom". 
#' Specifies if dose-response experiments are to be in rows or columns.
#' Supported values are "rows" and "columns"
#' @param control.orientation character.  Must be set if \code{type}="custom". 
#' Specifies if untreated controls are to be in rows or columns.
#' Supported values are "rows" and "columns"
#' @param dose.series.type character type of dose series.  Must be set if \code{type}="custom".
#' Supported values are "linear" and "exp". 
#' @param use.edge logical.  Must be set if \code{type}="custom".
#' Specifies if wells at the edge of the plate, e.g. rows "A" and "H" and columns "1" and "12", 
#' are to be used for dose series and untreated controls or left blank.  
#' @param singles logical: if singles = T, include dose-response experiments for individual compounds 
#' (\code{SynergyScreen} method)
#' @param pairs logical: if pairs = T, include dose-response experiments for compound pairs 
#' (\code{SynergyScreen} method)
#' @param digits number of decimal places in dose values
#' 
#' @return
#' \describe{
#' \item{SynergyScreen method}{returns a SynergyScreen object with populated @@design, @@dre_list and, 
#' if \code{pairs=T}, @@synergy_experiment_list slots} 
#' \item{DREList method}{returns a data frame that represents experimental design}
#' } 
#' 
#' @details
#' Screen design is represented as a data frame.  A \code{\linkS4class{SynergyScreen}} object has slots 
#' @@design for a design data frame, @@dre_list for a \code{\linkS4class{DREList}} object 
#' and @@synergy_experiment_list for a \code{\linkS4class{SynergyExperimentList}} object.  
#' The SynergyScreen method populates all of these based on data in its @@compound_list slot, i.e. max 
#' and min doses of each compound, and the desired design type.
#' 
#' A \code{\linkS4class{DREList}} object contains a list of dose-response experiment objects (of class 
#' \code{\linkS4class{DRE}}).  The DREList method produces a design data frame based on the contents of 
#' a DREList object.
#' 
#' Plate layout is determined by parameter \emph{type}.  If \code{type="custom"}, the user must manually
#' set parameters \emph{dre.orientation, control.orientation, dose.series.type, and use.edge}.  These 
#' settings determine how many dose-response experiments can fit on a [96-well] plate.  
#' \emph{generateDesign} computes the minimum necessary number of plates.  Each plate must contain at 
#' least one row or column of untreated controls, where cell cultures are allowed to grow on media 
#' without any of the drugs or toxins beign investigated in the dose-response experiments.  If there is 
#' space for more than one row or column of controls per plate, \emph{generateDesign} distributes them 
#' evenly amongst the plates.
#' 
#' The following pre-configured plate layout types are supported:
#' \describe{
#' \item{5 doses exp}{
#' generates a 96-well plate design where border rows and columns are left blank (e.g. filled with 
#' buffer), row B is untreated controls and the rest of the plate contains 10 dose-response experiments.
#' Each dose-response experiment contains 5 doses of a compound or mixture. 
#' These 5 doses are in the same column, in wells C-G.  The doses form exponential series, 
#' e.g. 1,2,4,8,16.
#' This design corresponds to \code{dre.orientation="columns"; control.orientation="rows"; 
#' dose.series.type="exp"; use.edge=FALSE}
#' }
#' \item{8 doses linear}{
#' generates a 96-well plate design where each column from 1 to 11 contains a dose-response experiment.  
#' Each experiment has 8 doses of a compound or mixture.  The doses form linear series, e.g. 
#' 1,2,3,4,5,6,7,8.  The last column (12) contains 8 untreated control wells.
#' This design corresponds to \code{dre.orientation="columns"; control.orientation="columns"; 
#' dose.series.type="linear"; use.edge=TRUE}
#' }
#' \item{12 doses exp}{
#' generates a 96-well plate design where each row from A to G contains a dose-response experiment.  
#' Each experiment has 12 doses of a compound or mixture.  The doses form exponential series, 
#' e.g. 1,2,4,8,16,32,64,128,256,512,1024,2048.  
#' The last row (H) contains 12 untreated control wells.
#' This design corresponds to \code{dre.orientation="rows"; control.orientation="rows"; 
#' dose.series.type="exp"; use.edge=TRUE}
#' }
#' }
#' 
#' The current implementation supports mixtures of no more than 2 compounds and no replicates.  However, 
#' if replicates are desired, the user may simply run the same design more than once.
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}, \code{\linkS4class{DREList}}, \code{\linkS4class{DRE}}, 
#' \code{\linkS4class{SynergyExperimentList}}, \code{\linkS4class{SynergyExperiment}}
#' 
setGeneric("generateDesign", function(object,...) standardGeneric("generateDesign"))

#### DREList generateDesign method ####
#' @describeIn generateDesign Generate design from a \code{DREList} object 
#' 
#' Taking a \code{DREList} object as input, generate a microtiter plate design showing what to put in 
#' each well.
#' 
setMethod("generateDesign", "DREList",
          function(object, dre.orientation, control.orientation, use.edge, digits=3) {
            #  Debug
            #browser()
            
            #  Check validity of arguments
            stopifnot(dre.orientation %in% c("rows","columns"), 
                      control.orientation %in% c("rows","columns"),
                      is.logical(use.edge),
                      is.numeric(digits), digits>0
            )

            #  compute the max number of dose-response experiments that can fit on one plate
            max.dres.per.plate = ifelse(dre.orientation == "rows",8,12)
            if (dre.orientation == control.orientation) max.dres.per.plate = max.dres.per.plate - 1
            if (!use.edge) max.dres.per.plate = max.dres.per.plate - 2

            #  compute the number of plates needed for the screen
            num.plates = length(object)/max.dres.per.plate

            #  number of plates must be an integer
            num.plates = ceiling(num.plates)

            #  compute the number of dose-response experiments per plate.  Some plates will have one 
            #  dre more if the number of dres is not a multiple of the number of plates
            dres.per.plate = floor(length(object)/num.plates)
            num.extra.dres = length(object)%%num.plates
            
            #  initialize design
            design = data.frame(plate = rep(1:num.plates,each=96),
                                column = rep(rep(1:12,each=8),num.plates),
                                row = rep(rep(LETTERS[1:8],12),num.plates)
            )
            
            #  fill out experiments, compounds and doses
            #  - initialize all wells to untreated controls
            design$experiment = "untreated"
            
            #  - if not using plate edges, overwrite them with blanks
            if (!use.edge) {
              design$experiment[design$column %in% c(1,12) | design$row %in% c("A","H")] = "blank"
            }
            
            #  - place compounds and doses
            design$cpd1 = as.character(NA)
            design$dose1 = as.numeric(NA)
            design$cpd2 = as.character(NA)
            design$dose2 = as.numeric(NA)
            counter = 0 # dose-response experiment counter
            
            if (dre.orientation == "columns") {
              # -- dose-response experiments in columns
              for (plate in 1:num.plates) {
                # -- compute first and last dose-response experiment columns
                first.dre.col = 1
                if (!use.edge) first.dre.col = 2
                last.dre.col = first.dre.col + dres.per.plate - 1
                if (plate <= num.extra.dres) last.dre.col = last.dre.col + 1

                #  -  compute rows occupied by dose-response experiments
                first.dre.row = 1
                if (!use.edge) first.dre.row = 2
                if (control.orientation == "rows") first.dre.row = first.dre.row + 1

                last.dre.row = 8
                if (!use.edge) last.dre.row = 7
                
                dre.rows = LETTERS[first.dre.row:last.dre.row]

                # -- Set up dose-response experiments one by one
                for (column in first.dre.col:last.dre.col) {
                  counter = counter + 1
                  filt = design$plate == plate & design$column == column & design$row %in% dre.rows
                  if (counter <= length(object)){
                    dre = object[[counter]]
                    design$experiment[filt] = dre@name
                    design$cpd1[filt] = dre@compound.name[1]
                    design$dose1[filt] = round(dre@fraction[1] * dre@dose, digits)
                    if (length(dre@compound.name)==2) {
                      design$cpd2[filt] = dre@compound.name[2]
                      design$dose2[filt] = round(dre@fraction[2] * dre@dose, digits)
                    }
                  }
                }
              }              
            } else if (dre.orientation == "rows") {
              # -- dose-response experiments in rows
              for (plate in 1:num.plates) {
                # -- compute first and last dose-response experiment rows
                first.dre.row = 1
                if (!use.edge) first.dre.row = 2
                last.dre.row = first.dre.row + dres.per.plate - 1
                if (plate <= num.extra.dres) last.dre.row = last.dre.row + 1
                
                
                
                #  -  compute columns occupied by dose-response experiments
                first.dre.column = 1
                if (!use.edge) first.dre.column = 2
                if (control.orientation == "columns") first.dre.column = first.dre.column + 1
                
                last.dre.column = 12
                if (!use.edge) last.dre.column = 11
                
                dre.columns = first.dre.column:last.dre.column
                
                # -- Set up dose-response experiments one by one
                for (row in LETTERS[first.dre.row:last.dre.row]) {
                  counter = counter + 1
                  filt = design$plate == plate & design$row == row & design$column %in% dre.columns
                  if (counter <= length(object)){
                    dre = object[[counter]]
                    design$experiment[filt] = dre@name
                    design$cpd1[filt] = dre@compound.name[1]
                    design$dose1[filt] = round(dre@fraction[1] * dre@dose, digits)
                    if (length(dre@compound.name)==2) {
                      design$cpd2[filt] = dre@compound.name[2]
                      design$dose2[filt] = round(dre@fraction[2] * dre@dose, digits)
                    }
                  }
                }
              }              
            } else {
              stop("Unsupported dre.orientation value", dre.orientation)
            }
            return(design)
          })

#### SynergyScreen generateDesign method ####
#' @describeIn generateDesign Taking a \code{SynergyScreen} object as input, generate a microtiter plate
#'  design showing what to put in each well.
#' 
#' This method also sets up dose-response experiment objects in object@@dre_list 
#' and synergy experiment objects in object@@synergy_experiment_list
#' 
setMethod("generateDesign", "SynergyScreen",
          function(object, type="5 doses exp", dre.orientation=NULL, control.orientation=NULL, 
                   dose.series.type=NULL, use.edge=NULL, singles=T, pairs=T, digits=3) {
            #  Verify validity of arguments
            stopifnot(type %in% c("5 doses exp","8 doses linear","12 doses exp","custom"))
            stopifnot(is.logical(singles))
            stopifnot(is.logical(pairs))
            stopifnot(is.numeric(digits), digits>0)
            
            #  Set parameters for pre-configured design types
            if (type=="5 doses exp") {
              dre.orientation="columns"
              control.orientation="rows"
              dose.series.type="exp"
              use.edge=FALSE
            } else if (type=="8 doses linear") {
              dre.orientation="columns"
              control.orientation="columns"
              dose.series.type="linear"
              use.edge=TRUE
            } else if (type=="12 doses exp") {
              dre.orientation="rows"
              control.orientation="rows"
              dose.series.type="exp"
              use.edge=TRUE
            } 
            
            #  Verify design parameters
            stopifnot(dre.orientation %in% c("rows","columns"), 
                      control.orientation %in% c("rows","columns"),
                      dose.series.type %in% c("exp","linear"),
                      is.logical(use.edge)
            )
            
            #  Compute number of doses in a dose-response experiment
            if (dre.orientation == "rows") {
              num.doses = 12
            } else if (dre.orientation == "columns") {
              num.doses = 8
            } else {
              stop("Unsupported dre.orientation value: ", dre.orientation)
            }
            if (!use.edge) num.doses = num.doses - 2
            if (dre.orientation != control.orientation) num.doses = num.doses - 1
            
            #  Create dose-response experiment objects
            dre_list = list()
            if (singles) {
              # Create single-compound dose-response experiments
              dre_list_s = lapply(object@compound_list, 
                                  function(x) new("DRE", compound.name = x@name, 
                                                  compound=object@compound_list, num.doses = num.doses, 
                                                  dose.series.type = dose.series.type)
                                  )
              dre_list = c(dre_list,dre_list_s)
            }
            if (pairs) {
              #  Create dose-response experiments for pairs of compounds
              dre_list_p = list()
              for (i in 1:(length(object@compound_list)-1)) {
                for (j in (i+1):length(object@compound_list)) {
                  dre = new("DRE", compound.name=names(object@compound_list)[c(i,j)], 
                            compound=object@compound_list,
                            num.doses = num.doses, dose.series.type = dose.series.type)
                  dre_list_p = c(dre_list_p,dre)
                }
              }
              dre_list = c(dre_list,dre_list_p)
            }
            
            #  Assign names to the dre_list members and populate object@dre_list
            names(dre_list) = sapply(dre_list, function(x) x@name)
            object@dre_list = as(dre_list,"DREList")
            
            #  Generate design
            design = generateDesign(object@dre_list, 
                                    dre.orientation=dre.orientation, 
                                    control.orientation=control.orientation, 
                                    use.edge=use.edge, 
                                    digits=digits)
            object@design = as(design,"ScreenDesign")
            
            #  Create SynergyExperiment objects
            if (pairs) {
              synergy_experiment_list = list()
              for (dre in dre_list_p) {
                exp1 = new("SynergyExperiment", 
                           mixture.dre.name=dre@name, compound.dre.name=dre@compound.name, dre.list = dre_list)
                synergy_experiment_list = c(synergy_experiment_list,exp1)
              }
              names(synergy_experiment_list) = sapply(synergy_experiment_list, function(x) x@name)
              object@synergy_experiment_list = as(synergy_experiment_list,"SynergyExperimentList")
            }
            
            #  All done
            return(object)
          })
