setClass(Class="Trapezoid", 
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

setGeneric(name="integrateIt",
           def=function(x, y, bounds, rule)
           {standardGeneric("integrateIt")}
)

setMethod(f="integrateIt",
          definition=function(x,y,bounds=c(a,b),rule){
            x<-as.matrix(x)
            x<-t(x)
            y<-as.matrix(y)
            y<-t(y)
            if (rule=="Trap"){
              trapvec<-y*2
              trapvec[1]<-y[1]
              trapvec[ncol(y)]<-y[ncol(y)]
              h<-(bounds[2]-bounds[1]+1)/ncol(x)
              trapoutput<-h/2*(trapvec)
              x<-as.vector(x)
              y<-as.vector(y)
              return(new("Trapezoid", result=sum(trapoutput), x = x, y = y))
            }
            if (rule=="Simpson"){
              simpvec<-y
              for (i in 1:ncol(y)){
                if((i %% 2) == 0){
                  simpvec[i]<-y[i]*4} 
                else {simpvec[i]<-y[i]*2}}
              simpvec[1]<-y[1]
              simpvec[ncol(y)]<-y[ncol(y)]
              h<-(10-0+1)/ncol(x)
              h<-(bounds[2]-bounds[1]+1)/ncol(x)
              simpoutput<-h/3*(simpvec)
              x<-as.vector(x)
              y<-as.vector(y)
              return(new("Simpson", result=sum(simpoutput), x = x, y = y))}
          }
)

setGeneric(name="print",
           def=function(x, y, bounds, rule,...)
           {standardGeneric("print")}
)

setMethod(f="print",
          definition=function(x,y,bounds=c(a,b),rule, ...){
            x<-as.matrix(x)
            x<-t(x)
            y<-as.matrix(y)
            y<-t(y)
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
          }
)

setGeneric(name="tolTest",
           def=function(fun, tolerance,rule, start,correct, ...)
           {standardGeneric("tolTest")}
)


setMethod(f="tolTest",
          definition=function(fun, tolerance, rule, start, correct, ...){
            begin<-start[1]
            if (rule=="Trap"){
              i<-1
              repeat{
                n<-begin+i
                h <- (7-5) / n
                j <- 1:n - 1
                xj <- start[1] + j * h
                trap <- (h / 2) * (fun(5) + 2 * sum(fun(xj)) + fun(7))
                lower.tolerance<-correct$value-as.numeric(tolerance)
                upper.tolerance<-correct$value+as.numeric(tolerance)
                if (trap < upper.tolerance & trap > lower.tolerance) break}
              return(n)}
            if (rule=="Simpson"){
              repeat {
                n<-begin
                h <- (7 - 5) / n
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
                lower.tolerance<-correct$value-as.numeric(tolerance)
                upper.tolerance<-correct$value+as.numeric(tolerance)
                if (sim < upper.tolerance & sim > lower.tolerance) break}
              return(n)}
            })

integrand <- function(x) {1/((x+1)*sqrt(x))}
tolTest(fun=integrand, tolerance=0.5, rule="Simpson", start=3, correct=integrate(integrand, lower=5,upper=7))

?integrate

integrand <- function(x) {1/((x+1)*sqrt(x))}

x<-c(0,1,2,3,4,5,6,7,8,9,10)
y<-c(0,2,4,6,8,10,12,14,16,18,20)

integrateIt(x,y,bounds=c(0,10),rule="Trap")
print(x,y,bounds=c(0,10),rule="Trap")
integrateIt(x,y,bounds=c(0,10),rule="Simpson")
print(x,y,bounds=c(0,10),rule="Simpson")
