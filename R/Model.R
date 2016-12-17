#  Class Model

#### Model class ####
#  Model is a virtual S4 class representing a fitted model or an error message
#  Extended by lm, nls, loess, and character
#  Note: roxygen2 doesn't work with setClassUnion

#' S3 class registered as S4
#' 
#' See \link[stats]{lm}
#' @name lm-class
setOldClass("lm")

#' S3 class registered as S4
#' 
#' See \link[stats]{nls}
#' @name nls-class
setOldClass("nls")

#' S3 class registered as S4
#' 
#' See \link[stats]{loess}
#' @name loess-class
setOldClass("loess")

#' S3 class registered as S4
#' 
#' See \link[base]{character}
#' @name character-class
setOldClass("character")

#' A virtual S4 class representing a fitted model or an error message
#' 
#' Extended by lm, nls, loess, and character
#' @name Model-class
setClassUnion("Model",c("lm","nls","loess","character"))
