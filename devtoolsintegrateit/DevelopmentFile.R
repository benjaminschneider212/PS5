
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd() #This will need to be changed to match your directory

## This is run once when the package strcuture is first created


## This can be run many times as the code is updates
current.code <- as.package("squaresPack")
load_all(current.code)
document(current.code)

## Let's look at a function

## Let's try it out
x<-c(1,2)
y<-c(3,4)
squaresObject <- addSquares(x, y)
squaresObject




