# writeWide generic and methods
#' @include ScreenDesign.R
NULL

#### writeWide ####
#' Write out a data frame-like object in a wide format
#' 
#' @param object a ScreenDesign object
#' @param file either a character string naming a file or a connection open for writing. "" indicates output to the console.
#' @param blank string to use for blank wells
#' @param untreated string to use for untreated control wells, e.g. the name of the growth medium
#' @param nothing string to use in cases where nothing should be added to a well
#' @param ... other parameters to pass to write.table
#' 
#' @details
#' A ScreenDesign object is a data frame in a "long" format.  This function converts it to wide, e.g. 8X12 for a 96 well plate,
#' then writes it out to a file in CSV format
#' 
#' @examples
#' compounds = readCompoundFile(system.file("extdata/8_cpds_1/8_compounds_1.csv",
#'                                          package="SynergyScreen"))
#' screen = new("SynergyScreen", compound_list=compounds)
#' screen = generateDesign(screen, type="8 doses linear")
#' writeWide(design(screen), file="8_compounds_1_design_wide.csv")
#' 
#' @seealso \code{\linkS4class{ScreenDesign}}, \code{\link[utils]{write.table}}
#' 
setGeneric("writeWide", function(object,...) standardGeneric("writeWide"))

#### writeWide method ####
#' @describeIn writeWide Write out screen design data in a wide format
#' 
setMethod("writeWide","ScreenDesign",
          function(object, file = "", blank = "blank", untreated = "untreated", nothing = "NULL", ...){
            
            # DEBUG
            #browser()
            
            #  Make sure the input is sorted
            object = object[order(object$plate,object$column,object$row),]
            
            #  Generate compound_dose strings
            cpd_dose1 = paste(object$cpd1,object$dose1,sep="_")
            cpd_dose2 = paste(object$cpd2,object$dose2,sep="_")
            cpd_dose1[object$experiment=="blank"] = blank
            cpd_dose2[object$experiment=="blank"] = blank
            cpd_dose1[object$experiment=="untreated"] = untreated
            cpd_dose2[object$experiment=="untreated"] = untreated
            cpd_dose2[cpd_dose2=="NA_NA"] = nothing
            
            #  Initialize output as a character matrix
            output = matrix(character(),nrow=0,ncol=25)
            
            #  Iterate over plates
            for (plate1 in unique(object$plate)) {
              out1 = matrix(character(),nrow=9,ncol=25)
              out1[1,] = c(paste("plate",plate1,sep="_"),rep(1:12,each=2))
              for (row1 in 1:8) {
                filt = object$plate == plate1 & object$row == LETTERS[row1]
                out1[row1+1,] = c(LETTERS[row1],as.vector(matrix(c(cpd_dose1[filt],cpd_dose2[filt]),nrow=2,byrow=T)))
              }
              output = rbind(output,out1)
              output = rbind(output,rep("",25))
            }
            
            #  Write output to a file
            write.table(output, file=file, sep=",", row.names=F, col.names=F)            
          })