setClass(Class="Trapezoid",
         representation = representation(
           x = "numeric",
           y = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c()
         )
)

setClass(Class="Simpson",
         representation = representation(
           x = "numeric",
           y = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c()
         )
)

setGeneric("integrateIt",
           function(object="Trapezoid")  {
             standardGeneric("intergrateIt")
           })
setMethod("integrateIt", "Trapezoid",
          function(object){
            return(object@square)
          })

setGeneric("integrateIt",
           function(object="Simpson")  {
             standardGeneric("intergrateIt")
           })
setMethod("integrateIt", "Simpson",
          function(object){
            return(object@square)
          })

integrateIt()<-function(x,y,type=NULL){
  a<-min(x)
  b<-max(x)
  h<-(b-a)/2
  if(type=="Trapezoid")(
    
    value<-(h/2)*sum()
    return(value)
  )
  if(type=="Simpson")(
    value<-(h/3)*sum()
    return(value)
  )
  if(type=="both")(
    
    return(value)
  )
  if(is.null(type))(
    print("You done messed up, pick a type")
  )
  else(print("uh oh boo, you messed up, pick a valid type"))
}
