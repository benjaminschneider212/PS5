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