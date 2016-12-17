#  Accessor methods for class DRE
#' @include DRE.R
NULL

#' Accessor methods for class DRE
#'
#' @param object object of class DRE
#' @param value replacement value for a slot
#'
#' @examples
#' #  Retrieve a model and a comment for a dose-response experiment
#' data(sim15_screen3)
#' model(dre(sim15_screen3,"Cpd1"))
#' comment(dre(sim15_screen3,"Cpd1"))
#' 
#' #  Assign a new comment
#' x = dre(sim15_screen3,"Cpd1")
#' comment(x) = "test"
#' comment(x)
#'
#' @aliases name compound.name fraction dose response control.response effect model ic25 ic50 ic75 true.ic50 true.m comment name<- compound.name<- fraction<- dose<- response<- control.response<- effect<- model<- ic25<- ic50<- ic75<- true.ic50<- true.m<- comment<-
#'
#' @name DRE-accessors
NULL


# Getter generics
if(!isGeneric("name")) setGeneric("name", function(object) standardGeneric("name"))
if(!isGeneric("compound.name")) setGeneric("compound.name", function(object) standardGeneric("compound.name"))
if(!isGeneric("fraction")) setGeneric("fraction", function(object) standardGeneric("fraction"))
if(!isGeneric("dose")) setGeneric("dose", function(object) standardGeneric("dose"))
if(!isGeneric("response")) setGeneric("response", function(object) standardGeneric("response"))
if(!isGeneric("control.response")) setGeneric("control.response", function(object) standardGeneric("control.response"))
if(!isGeneric("effect")) setGeneric("effect", function(object) standardGeneric("effect"))
if(!isGeneric("model")) setGeneric("model", function(object) standardGeneric("model"))
if(!isGeneric("ic25")) setGeneric("ic25", function(object) standardGeneric("ic25"))
if(!isGeneric("ic50")) setGeneric("ic50", function(object) standardGeneric("ic50"))
if(!isGeneric("ic75")) setGeneric("ic75", function(object) standardGeneric("ic75"))
if(!isGeneric("true.ic50")) setGeneric("true.ic50", function(object) standardGeneric("true.ic50"))
if(!isGeneric("true.m")) setGeneric("true.m", function(object) standardGeneric("true.m"))
if(!isGeneric("comment")) setGeneric("comment", function(object) standardGeneric("comment"))
if(!isGeneric("dilution.factor")) setGeneric("dilution.factor", function(object) standardGeneric("dilution.factor"))

# Getter methods
#' @rdname DRE-accessors
setMethod("name", "DRE", function(object) object@name)
#' @rdname DRE-accessors
setMethod("compound.name", "DRE", function(object) object@compound.name)
#' @rdname DRE-accessors
setMethod("fraction", "DRE", function(object) object@fraction)
#' @rdname DRE-accessors
setMethod("dose", "DRE", function(object) object@dose)
#' @rdname DRE-accessors
setMethod("response", "DRE", function(object) object@response)
#' @rdname DRE-accessors
setMethod("control.response", "DRE", function(object) object@control.response)
#' @rdname DRE-accessors
setMethod("effect", "DRE", function(object) object@effect)
#' @rdname DRE-accessors
setMethod("model", "DRE", function(object) object@model)
#' @rdname DRE-accessors
setMethod("ic25", "DRE", function(object) object@ic25)
#' @rdname DRE-accessors
setMethod("ic50", "DRE", function(object) object@ic50)
#' @rdname DRE-accessors
setMethod("ic75", "DRE", function(object) object@ic75)
#' @rdname DRE-accessors
setMethod("true.ic50", "DRE", function(object) object@true.ic50)
#' @rdname DRE-accessors
setMethod("true.m", "DRE", function(object) object@true.m)
#' @rdname DRE-accessors
setMethod("comment", "DRE", function(object) object@comment)
#' @rdname DRE-accessors
setMethod("dilution.factor", "DRE", function(object) object@dilution.factor)

# Setter generics
if(!isGeneric("name<-")) setGeneric("name<-", function(object, value) standardGeneric("name<-"))
if(!isGeneric("compound.name<-")) setGeneric("compound.name<-", function(object, value) standardGeneric("compound.name<-"))
if(!isGeneric("fraction<-")) setGeneric("fraction<-", function(object, value) standardGeneric("fraction<-"))
if(!isGeneric("dose<-")) setGeneric("dose<-", function(object, value) standardGeneric("dose<-"))
if(!isGeneric("response<-")) setGeneric("response<-", function(object, value) standardGeneric("response<-"))
if(!isGeneric("control.response<-")) setGeneric("control.response<-", function(object, value) standardGeneric("control.response<-"))
if(!isGeneric("effect<-")) setGeneric("effect<-", function(object, value) standardGeneric("effect<-"))
if(!isGeneric("model<-")) setGeneric("model<-", function(object, value) standardGeneric("model<-"))
if(!isGeneric("ic25<-")) setGeneric("ic25<-", function(object, value) standardGeneric("ic25<-"))
if(!isGeneric("ic50<-")) setGeneric("ic50<-", function(object, value) standardGeneric("ic50<-"))
if(!isGeneric("ic75<-")) setGeneric("ic75<-", function(object, value) standardGeneric("ic75<-"))
if(!isGeneric("true.ic50<-")) setGeneric("true.ic50<-", function(object, value) standardGeneric("true.ic50<-"))
if(!isGeneric("true.m<-")) setGeneric("true.m<-", function(object, value) standardGeneric("true.m<-"))
if(!isGeneric("comment<-")) setGeneric("comment<-", function(object, value) standardGeneric("comment<-"))
if(!isGeneric("dilution.factor<-")) setGeneric("dilution.factor<-", function(object, value) standardGeneric("dilution.factor<-"))

# Setter methods
#' @rdname DRE-accessors
setReplaceMethod("name", "DRE", function(object, value){ object@name <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("compound.name", "DRE", function(object, value){ object@compound.name <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("fraction", "DRE", function(object, value){ object@fraction <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("dose", "DRE", function(object, value){ object@dose <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("response", "DRE", function(object, value){ object@response <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("control.response", "DRE", function(object, value){ object@control.response <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("effect", "DRE", function(object, value){ object@effect <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("model", "DRE", function(object, value){ object@model <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("ic25", "DRE", function(object, value){ object@ic25 <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("ic50", "DRE", function(object, value){ object@ic50 <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("ic75", "DRE", function(object, value){ object@ic75 <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("true.ic50", "DRE", function(object, value){ object@true.ic50 <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("true.m", "DRE", function(object, value){ object@true.m <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("comment", "DRE", function(object, value){ object@comment <- value; object})
#' @rdname DRE-accessors
setReplaceMethod("dilution.factor", "DRE", function(object, value){ object@dilution.factor <- value; object})
