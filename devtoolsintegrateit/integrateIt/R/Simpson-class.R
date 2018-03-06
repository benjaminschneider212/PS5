#' An object with two vectors of Class Simpson
#' 
#' Object of class \code{Simpson}
#'
#' 
#' An object of the class `Simpson' has the following slots:
#' \itemize{
#' \item \code{result} The result of the integral
#' \item \code{x} a vector of values
#' \item \code{y} a vector of values of same dimensionality as \code{x} 
#' }
#'
#' @author Jacob M. Montgomery: \email{jacob.montgomery@@wustl.edu}
#' @aliases Simpson 
#' @rdname Simpson
#' @export
setClass(Class="Simpson", 
         representation = representation(
           result = "numeric",
           x="numeric",
           y="numeric"
         ),
         prototype = prototype(
           result =  c(),
           x= c(),
           y= c())
)

