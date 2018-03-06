library(devtools)
library(roxygen2)
setwd("/Users/benjaminschneider/Documents/GitHub/PS5/devtoolsintegrateit/")
## This is run once when the package strcuture is first created


## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)
