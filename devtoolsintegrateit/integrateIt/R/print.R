#' Integrate values with estimation
#'
#' Finds the sum of squared numbers
#'
#' @param x A vector of values
#' @param y A vector of values with the same dimensionality as \code{x}
#' @param bounds A vector with two values that are the bounds of integration
#' @param type Either Trap or Simpson
#'
#' @return An object of class Squares containing
#'  \item{result}{the integrated value}
#'  \item{x}{The input vector of x's} 
#'  \item{y}{The input vector of y's}
#' @author Benjamin Schneider
#' @note This is a very simple function
#' @examples
#' 
#' X <- seq(0, 10) 
#' Y <- seq(0,20,2)
#' integrateIt(X, Y, c(0,10), type="Trap")
#' @seealso \code{\link{integrateIt}}
#' @rdname print
#' @aliases print,ANY-method
#' @export
setGeneric(name="print",
           def=function(x, y, bounds, rule,...)
           {standardGeneric("print")}
)
#' @export
setMethod(f="print",
          definition=function(x,y,bounds=c(a,b),rule, ...){
            x<-as.matrix(x)
            x<-t(x)
            y<-as.matrix(y)
            y<-t(y)
            if (ncol(x)==ncol(y)){
            if (rule=="Trap"){
              trapvec<-y*2
              trapvec[1]<-y[1]
              trapvec[ncol(y)]<-y[ncol(y)]
              h<-(bounds[2]-bounds[1]+1)/ncol(x)
              trapoutput<-h/2*(trapvec)
              return(result=sum(trapoutput))
            }
            if (rule=="Simpson"){
              simpvec<-y
              for (i in 1:ncol(y)){
                if((i %% 2) == 0){
                  simpvec[i]<-y[i]*4} 
                else {simpvec[i]<-y[i]*2}}
              simpvec[1]<-y[1]
              simpvec[ncol(y)]<-y[ncol(y)]
              h<-(bounds[2]-bounds[1]+1)/ncol(x)
              simpoutput<-h/3*(simpvec)
              return(result=sum(simpoutput))}
            else{return("Please select a valid rule")}}
            else{return("make sure your x and y are of equal length")}
          }
)