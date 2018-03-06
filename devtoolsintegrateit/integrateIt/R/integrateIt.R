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
#' @seealso \code{\link{print}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
setGeneric(name="integrateIt",
           def=function(x, y, bounds, rule)
           {standardGeneric("integrateIt")}
)

#' @export
setMethod(f="integrateIt",
          definition=function(x,y,bounds=c(a,b),rule){
            x<-as.matrix(x) #this is done for functionality
            x<-t(x) #same
            y<-as.matrix(y) #same
            y<-t(y) #same
            if (ncol(x)==ncol(y)){#ifr condition to make sure the function works
            if (rule=="Trap"){
              trapvec<-y*2 #setting up the doubled values
              trapvec[1]<-y[1] #replace the first
              trapvec[ncol(y)]<-y[ncol(y)] #replace the last
              h<-(bounds[2]-bounds[1]+1)/ncol(x) #creates the h with the bounds, the plus 1 is to make the math work
              trapoutput<-h/2*(trapvec) #setting up the series of numbers
              x<-as.vector(x) #transfering the x and y output back to how it was entered
              y<-as.vector(y)
              return(new("Trapezoid", result=sum(trapoutput), x = x, y = y)) #create the output, notice the sum of trap output for all of the values
            }
            if (rule=="Simpson"){
              simpvec<-y #Setting up the vector
              for (i in 1:ncol(y)){
                if((i %% 2) == 0){
                  simpvec[i]<-y[i]*4} #for loop mutiplying every ther value by 4
                else {simpvec[i]<-y[i]*2}} #doing the rest by two
              simpvec[1]<-y[1] #replace the first term
              simpvec[ncol(y)]<-y[ncol(y)] #replace the last term
              h<-(bounds[2]-bounds[1]+1)/ncol(x) #create the h agan
              simpoutput<-h/3*(simpvec) #same thing but divided by 3
              x<-as.vector(x) #transfering the x and y output back to how it was entered
              y<-as.vector(y)
              return(new("Simpson", result=sum(simpoutput), x = x, y = y))} #Create the output
            else{return("Please select a valid rule")}} #errors thrown
            else{return("make sure your x and y are of equal length")} #errors thrown
          }
)
