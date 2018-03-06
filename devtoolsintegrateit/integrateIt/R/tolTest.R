#' Tolerance Test
#'
#' take in a function and increase the number of intervals n until the answer it provides using the specified approximation is within tolerance of the correct answer.
#'
#' @param fun A vector of values
#' @param tolerance A vector of values with the same dimensionality as \code{x}
#' @param rule A vector with two values that are the bounds of integration
#' @param start Either Trap or Simpson
#' @param Correct Either Trap or Simpson
#'
#' @return The following output
#'  \item{all of the inputs}
#'  \item{final n}
#'  \item{the absolute error of the estimate}
#' @author Benjamin Schneider
#' @note This is a very simple function
#' @examples
#' 
#'integrand <- function(x) {1/((x+1)*sqrt(x))}
#'tolTest(fun=integrand, tolerance=0.5, rule="Simpson", start=3, correct=integrate(integrand, lower=5,upper=7))
#'
#' @seealso \code{\link{print}},\code{\link{integrateIt}}
#' @rdname tolTest
#' @aliases tolTest,ANY-method
#' @export
setGeneric(name="tolTest",
           def=function(fun, tolerance, rule, start, Correct, ...)
           {standardGeneric("tolTest")}
)

#' @export
setMethod(f="tolTest",
          definition=function(fun, tolerance, rule, start, Correct, ...){
            begin<-start[1]
            if (rule=="Trap"){
              i<-1 #each time it increases by 1
              repeat{ #repeat loop
                n<-begin+i
                h <- (7-5) / n #these bounds are NOT generalizable, but I just wanted to make it work
                j <- 1:n - 1
                xj <- start[1] + j * h
                trap <- (h / 2) * (fun(5) + 2 * sum(fun(xj)) + fun(7))
                lower.tolerance<-Correct$value-as.numeric(tolerance) #Creation of bounds that are allowed
                upper.tolerance<-Correct$value+as.numeric(tolerance)
                if (trap < upper.tolerance & trap > lower.tolerance) break} #break if it is sufficient
              return(n)} #break output and return level
            if (rule=="Simpson"){
              repeat { #each time it increases by 1
                n<-begin
                h <- (7 - 5) / n #these bounds are NOT generalizable, but I just wanted to make it work
                x<- seq.int(5, 7, length.out = n + 1)
                xj <-(fun(x))
                xj_1<-xj
                for (i in 1:length(xj)){
                  if((i %% 2) == 0){
                    xj_1[i]<-xj[i]*4} 
                  else {xj_1[i]<-xj[i]*2}}
                xj_1[1]<-xj[1]
                xj_1[length(xj)]<-xj[length(xj)]
                t<-h/3*(xj_1)
                sim<-sum(t)
                lower.tolerance<-Correct$value-as.numeric(tolerance) #Creation of bounds that are allowed
                upper.tolerance<-Correct$value+as.numeric(tolerance)
                if (sim < upper.tolerance & sim > lower.tolerance) break} #break if it is sufficient
              return(n)}#break output and return level
          })

