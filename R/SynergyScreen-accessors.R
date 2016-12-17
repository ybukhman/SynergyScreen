#  Accessor methods for class SynergyScreen
#' @include SynergyScreen.R
NULL

#' Accessor methods for class SynergyScreen
#' 
#' @param object object of class SynergyScreen
#' @param name name of a list member to access, e.g. specific dose-response experiment or synergy experiment
#' @param value replacement value for a slot.  See Details
#' 
#' @details Details on specific methods
#'  
#' @section Replacement method for @@norm_data (norm_data<-):
#' Replacement value for @@norm_data can be a \linkS4class{ScreenData} object or a data frame
#' 
#' This method also sets @@response and @@control.response slots of @@dre_list members.
#' 
#' @section Replacement method for @@raw_data (raw_data<-):
#' Replacement value for @@norm_data can be a \linkS4class{ScreenData} object or a data frame
#' 
#' @section Replacement method for @@synergy_data (synergy_data<-):
#' Replacement value for @@synergy_data can be a \linkS4class{SynergyData} object or a data frame
#' 
#' @examples
#' data(sim15_screen3)
#' head(norm_data(sim15_screen3))
#' dre(sim15_screen3,"Cpd1")
#' 
#' @seealso \linkS4class{SynergyScreen}, \link{dreData}, \link{labelledData}
#' 
#' @name SynergyScreen-accessors
NULL

#### Generics ####
#' @rdname SynergyScreen-accessors 
setGeneric("norm_data", function(object) standardGeneric("norm_data"))

#' @rdname SynergyScreen-accessors 
setGeneric("norm_data<-", function(object,value) standardGeneric("norm_data<-"))

#' @rdname SynergyScreen-accessors
setGeneric("raw_data", function(object) standardGeneric("raw_data"))

#' @rdname SynergyScreen-accessors
setGeneric("raw_data<-", function(object,value) standardGeneric("raw_data<-"))

#' @rdname SynergyScreen-accessors
setGeneric("synergy_data", function(object) standardGeneric("synergy_data"))

#' @rdname SynergyScreen-accessors
setGeneric("synergy_data<-", function(object,value) standardGeneric("synergy_data<-"))

#' @rdname SynergyScreen-accessors
setGeneric("compound_list", function(object) standardGeneric("compound_list"))

#' @rdname SynergyScreen-accessors
setGeneric("compound_list<-", function(object,value) standardGeneric("compound_list<-"))

#' @rdname SynergyScreen-accessors
setGeneric("compound", function(object,name) standardGeneric("compound"))

#' @rdname SynergyScreen-accessors
setGeneric("dre_list", function(object) standardGeneric("dre_list"))

#' @rdname SynergyScreen-accessors
setGeneric("dre_list<-", function(object,value) standardGeneric("dre_list<-"))

#' @rdname SynergyScreen-accessors
setGeneric("dre", function(object,name) standardGeneric("dre"))

#' @rdname SynergyScreen-accessors
setGeneric("synergy_experiment_list", function(object) standardGeneric("synergy_experiment_list"))

#' @rdname SynergyScreen-accessors
setGeneric("synergy_experiment_list<-", function(object,value) standardGeneric("synergy_experiment_list<-"))

#' @rdname SynergyScreen-accessors
setGeneric("synergy_experiment", function(object,name) standardGeneric("synergy_experiment"))

#### design methods ####
#' @rdname SynergyScreen-accessors
setMethod("design", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@design
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("design", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "ScreenDesign")) value = new("ScreenDesign",value)
                   object@design = value
                   object
                 })

#### norm_data methods ####
#' @rdname SynergyScreen-accessors
setMethod("norm_data", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@norm_data
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("norm_data", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   # debug
                   # browser()
                   
                   if (!is(value, "ScreenData")) value = new("ScreenData",value)
                   #  Set object@norm_data
                   object@norm_data = value
                   
                   #  Join normalized data with screen design info
                   data1 = labelledData(object,"norm")
                   
                   #  Need this to be able to add doses:
                   data1$dose2[is.na(data1$dose2)] = 0
                   
                   #  set @response and @control.response slots of @dre_list members
                   exp_names = names(object@dre_list)
                   control.response = with(data1, mean(response[experiment=="untreated"]))
                   for (exp in exp_names) {
                     filt = data1$experiment == exp
                     object@dre_list[[exp]]@dose = data1$dose1[filt] + data1$dose2[filt]
                     object@dre_list[[exp]]@response = data1$response[filt]
                     object@dre_list[[exp]]@control.response = control.response
                   }   
                   
                   #  All done
                   return(object)
                 })

#### raw_data methods ####
#' @rdname SynergyScreen-accessors
setMethod("raw_data", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@raw_data
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("raw_data", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "ScreenData")) value = new("ScreenData",value)
                   object@raw_data = value
                   object
                 })

#### synergy_data methods ####
#' @rdname SynergyScreen-accessors
setMethod("synergy_data", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@synergy_data
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("synergy_data", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "SynergyData")) value = new("SynergyData",value)
                   object@synergy_data = value
                   object
                 })

#### compound methods ####
#' @rdname SynergyScreen-accessors
setMethod("compound_list", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@compound_list
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("compound_list", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "CompoundList")) value = new("CompoundList",value)
                   object@compound_list = value
                   object
                 })

#' @rdname SynergyScreen-accessors
setMethod("compound", 
          signature(object = "SynergyScreen"),
          function(object,name) {
            compound_list(object)[[name]]
          })

#### dre methods ####
#' @rdname SynergyScreen-accessors
setMethod("dre_list", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@dre_list
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("dre_list", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "DREList")) value = new("DREList",value)
                   object@dre_list = value
                   object
                 })

#' @rdname SynergyScreen-accessors
setMethod("dre", 
          signature(object = "SynergyScreen"),
          function(object,name) {
            dre_list(object)[[name]]
          })

#### synergy_experiment methods ####
#' @rdname SynergyScreen-accessors
setMethod("synergy_experiment_list", 
          signature(object = "SynergyScreen"),
          function(object) {
            object@synergy_experiment_list
          })

#' @rdname SynergyScreen-accessors
setReplaceMethod("synergy_experiment_list", 
                 signature(object = "SynergyScreen"),
                 function(object, value) {
                   if (!is(value, "SynergyExperimentList")) value = new("SynergyExperimentList",value)
                   object@synergy_experiment_list = value
                   object
                 })

#' @rdname SynergyScreen-accessors
setMethod("synergy_experiment", 
          signature(object = "SynergyScreen"),
          function(object,name) {
            synergy_experiment_list(object)[[name]]
          })
