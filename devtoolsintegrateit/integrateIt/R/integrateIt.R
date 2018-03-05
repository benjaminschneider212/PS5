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
              h<-(bounds[2]-bounds[1]+1)/ncol(x)
              simpoutput<-h/3*(simpvec)
              x<-as.vector(x)
              y<-as.vector(y)
              return(new("Simpson", result=sum(simpoutput), x = x, y = y))}
          }
)