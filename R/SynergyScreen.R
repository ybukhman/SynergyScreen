#  SynergyScreen package, class and methods

#### SynergyScreen package #####
#' Analyze and simulate synergy screen data.
#' 
#' SynergyScreen works with data from medium-throughput synergy screens where multiple pairs of compounds are tested in microtiter plates 
#' using simple single ray design.
#' 
#' @docType package
#' @name SynergyScreen-package
#' @example inst/extdata/15_cpds_simulation/15_cpds_simulation.R
#' 
NULL

#### SynergyScreen class #####
#' S4 class for a synergy screen
#' 
#' A SynergyScreen class object contains all data for an entire synergy screen
#' 
#' @slot design design of the screen, object of class ScreenDesign
#' @slot raw_data raw screen data, object of class ScreenData
#' @slot norm_data normalized screen data, object of class ScreenData
#' @slot compound_list list of compounds screened, object of class CompoundList
#' @slot dre_list list of dose-response experiments, object of class DREList
#' @slot synergy_experiment_list list of synergy experiments, object of class SynergyExperimentList
#' @slot synergy_data interaction index data
#' 
#' @examples
#' data(sim15_screen3)
#' head(design(sim15_screen3),n=30)
#' head(raw_data(sim15_screen3))
#' head(norm_data(sim15_screen3))
#' compound_list(sim15_screen3)[1:2]
#' dre_list(sim15_screen3)[1:2]
#' synergy_experiment_list(sim15_screen3)[1:2]
#' head(synergy_data(sim15_screen3))
#' 
#' @seealso \code{\linkS4class{ScreenDesign}}, \code{\linkS4class{ScreenData}}, 
#' \code{\linkS4class{Compound}}, \code{\linkS4class{CompoundList}}, 
#' \code{\linkS4class{DRE}}, \code{\linkS4class{DREList}}, 
#' \code{\linkS4class{SynergyExperiment}}, \code{\linkS4class{SynergyExperimentList}}, \code{\linkS4class{SynergyData}}
#' 
setClass("SynergyScreen",
         slots = c(compound_list = "CompoundList",
                   design = "ScreenDesign",
                   raw_data = "ScreenData",
                   norm_data = "ScreenData",
                   dre_list = "DREList",
                   synergy_experiment_list = "SynergyExperimentList",
                   synergy_data = "SynergyData"
                   )
         )

#### SynergyScreen boxplot method ####
#' Box plot
#' 
#' Produce box-and-whisker plot of a synergy screening experiment data
#' 
#' @param x object of class SynergyScreen
#' @param what type of data to plot, can be "raw" or "norm"
#' @param ... additional arguments passed to the generic boxplot function 
#' 
#' @return See \code{graphics::\link[graphics]{boxplot}}
#'  
#' @examples
#' data(sim15_screen3)
#' boxplot(sim15_screen3,"raw")
#' boxplot(sim15_screen3,"norm")
#' 
#' @seealso \code{BiocGenerics::\link[BiocGenerics]{boxplot}}, \code{graphics::\link[graphics]{boxplot}}, \code{\linkS4class{SynergyScreen}}
#' 
setMethod("boxplot","SynergyScreen",
          function (x, what="raw", ...) {
            #  Debug
            #browser()
            
            #  Set the style of axis labels and a bigger bottom margin
            las0 <- par("las")
            mar0 <- par("mar")
            par(las=2)
            par(mar=c(8,4,4,2)+0.1)
            
            #  Retrieve data for plotting
            data2 <- labelledData(x, what=what)
            
            #  Generate a box plot
            bxp <- boxplot(data2$response ~ paste(data2$plate,data2$experiment)) 
            
            #  Restore plotting parameter defaults
            par(las=las0)
            par(mar=mar0)
            
            #  Return boxplot value
            invisible(bxp)
          })

#### SynergyScreen normalize method ####
#' Normalize synergy screen data
#' 
#' Normalization corrects for plate bias and subtracts blank response
#' 
#' @param object object of class SynergyScreen
#' @param floor numeric, normalized non-blank response values smaller than this number are set to this number
#' 
#' @return object of class SynergyScreen with populated @@norm_data slot and @@response and @@control.response 
#' slots of @@dre_list members
#' 
#' @details
#' Normalization assumes that there is a multiplicative plate bias.  A bias factor is computed for each plate based on  
#' untreated control response values.  A plate's bias is its mean divided by the global mean.  To normalize, all response values 
#' in a plate are divided by the plate's bias factor.
#' 
#' @examples
#' # Initialize a screen
#' cpds <- readCompoundFile(system.file("extdata/15_cpds_simulation/compounds_1.csv",
#'                                     package="SynergyScreen"))
#' screen1 <- new("SynergyScreen",compound_list=cpds)
#' screen1 <- generateDesign(screen1,pairs=FALSE)
#' 
#' #  Simulate compound growth curve characteristics and raw dose-response data
#' set.seed(20141120)
#' screen1 <- simulate(screen1)
#' boxplot(screen1,"raw")
#' 
#' #  Normalize screen data, adjusting for plate bias
#' screen1 <- normalize(screen1)
#' boxplot(screen1,"norm")
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}, \code{BiocGenerics::\link[BiocGenerics]{normalize}}
#' 
setMethod("normalize","SynergyScreen",
          function (object, floor=0.001) {
            #  Debug
            #browser()
            
            #  Get raw data along with design info
            data1 <- labelledData(object,"raw")
            
            #  Compute mean of the control wells for each plate
            plate.means <- with(subset(data1, experiment=="untreated"), tapply(response, plate, mean))
            
            #  Compute plate bias factors
            bias.factors <- plate.means/mean(plate.means)
            
            #  Normalize the data by plate bias factors
            plates <- unique(data1$plate)
            for (i in 1:length(plates)) {
              filt <- data1$plate==plates[i]
              data1$response[filt] <- data1$response[filt]/bias.factors[plates[i]]
            }
            
            #  Subtract average blank value, if any
            if (any(data1$experiment=="blank")) {
              blank <- with(data1, mean(response[experiment=="blank"]))
              data1$response <- data1$response - blank
            }
            
            # Rescale to set mean of untreated controls to 1
            data1$response <- data1$response / with(data1, mean(response[experiment=="untreated"]))
            
            #  Replace small and negative non-blank response values with floor
            data1$response[data1$experiment!="blank" & data1$response < floor] <- floor
            
            #  Add normalized data to the object
            norm_data(object) <- data1[,c("plate","column","row","response")]
            
            #  All done
            return(object)
          })


#### SynergyScreen plot method ####
#' Plot a SynergyScreen object.
#' 
#' Plot matrix of pairwise interaction index values for a synergy screen.  
#' 
#' @param x a SynergyScreen object
#' @param y no parameter \code{y} is defined for this method
#' @param effect numeric, fractional effect value to plot the matrix for
#' @param model name of the growth curve model to use, character
#' @param ... additional arguments passed to the \code{\link[stats]{heatmap}} function
#' 
#' @details 
#' The matrix visualizes log2(interaction index) values for the specified effect size.  
#' The values are visualized on a red-yellow-blue color scale, red indicating synergy,
#' yellow no interaction and blue antagonism.
#' 
#' The index is visualized on log scale because it is a sum of two ratios.  
#' Distributions of ratios are highly assymetrical, e.g. 0 to infinity with a mode around 1
#' Log transform is commonly used to normalize them.
#' 
#' @examples
#' data(sim15_screen3)
#' plot(sim15_screen3)
#' 
#' @seealso \code{\linkS4class{SynergyScreen}}, \code{\link[stats]{heatmap}}
#' 
setMethod("plot", 
          signature(x = "SynergyScreen", y = "missing"),
          function(x, y, effect = 0.5, model="menls", ...) {
            #  Debug
            # browser()
            
            # Assign x to object to make downstream code work
            object <- x
            
            #  Verify arguments
            stopifnot(is.numeric(effect) && length(effect)==1)
            stopifnot(is.character(model) && length(model)==1 && model %in% c("menls","loess","melm","loess2"))
            
            #  Compute interaction index values 
            ii <- sapply(object@synergy_experiment_list, function(x) {
              ii1 <- try(interactionIndex(x, effect=effect, dre.list=object@dre_list, model=model),silent=T)
              ifelse(class(ii1)=="try-error", NA, ii1)
            })
            
            #  Extract compound names
            cpd1 <- sapply(object@synergy_experiment_list, function(x) x@compound.dre.name[1])
            cpd2 <- sapply(object@synergy_experiment_list, function(x) x@compound.dre.name[2])
            
            #  Build matrix of interaction index values
            cpd <- names(object@compound_list)
            ii.matrix <- matrix(NA, nrow=length(cpd), ncol=length(cpd), dimnames=list(cpd,cpd))
            for (c in cpd) ii.matrix[c,c] <- 1
            for (i in 1:length(ii)) {
              ii.matrix[cpd1[i],cpd2[i]] <- ii[i]
              ii.matrix[cpd2[i],cpd1[i]] <- ii[i]
            }
            
            # - drop failed compounds
            for (c in cpd) {
              if (sum(is.na(ii.matrix[c,])) == ncol(ii.matrix) - 1) {
                ii.matrix <- ii.matrix[! rownames(ii.matrix) == c, ! colnames(ii.matrix) == c]
                warning("No data for compound ", c, " with effect = ", effect)
              }
            }
            
            # - cluster the matrix of log-transformed interaction index values
            log.matrix <- log2(ii.matrix)
            no.na.vals <- sum(is.na(log.matrix)) == 0
            if (no.na.vals) {
              absmax <- max(abs(log.matrix))
              dd <- as.dist(scales::rescale(log.matrix, to=c(0,1), from=c(-absmax,absmax)))
              hc <- hclust(dd)
            }
            
            # - melt the matrix
            log.melt <- reshape2::melt(log.matrix)
            names(log.melt) <- c("cpd1","cpd2","log.ii")
            if (no.na.vals) {
              log.melt$cpd1 <- factor(log.melt$cpd1, levels = colnames(log.matrix)[hc$order])
              log.melt$cpd2 <- factor(log.melt$cpd2, levels = colnames(log.matrix)[hc$order])
            }
            
            # - plot the melted matrix
            plot1 <- ggplot(log.melt, aes(cpd1, cpd2, fill = log.ii)) + geom_tile(color = "white")
            plot1 <- plot1 + scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0,  
                                                           space = "Lab", name="log2(interaction index)")
            plot1 + labs(x="",y="", title = paste0("Interaction matrix at ", effect*100, "% inhibition")) + 
              theme(axis.text.x = element_text(angle = 330, hjust = 0))
          })

