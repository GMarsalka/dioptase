# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


library(zoo)
library(NMF)
library(tsne)
library(Rtsne)
library(vegan)
library(ade4)
library(stringr)
library(MALDIquant)


#################################################################################################
############### I/O functions ###################################################################
#################################################################################################


fcosts <- "tempc"

############################################################
## Read XRD data as XY-files, same angles in all files (faster)
read.xrd.xy.sameang <- function(f){
  lf2 <- list.files(f, pattern = "*.xy")
  lf <- sprintf("%s/%s",f,lf2)
  print(lf)
  ang <- read.table(lf[1])[,1]
  one <- read.table(lf[1])[,2]

  out <- matrix(ncol=length(one), nrow=length(lf))
  for(i in 1:length(lf)){
    out[i,] <- read.table(lf[i])[,2]
    #    out <- rbind(out,read.table(f)[,2])
  }
  rownames(out) <- lf2
  list(ang=ang,
       spec=out)
}


############################################################
## Read XRD data as XY-files, different angles in different files
read.xrd.xy.diffang <- function(f){
  xlist <- list.files(f,pattern = "*.xy")
  xlistm <- gsub('-', 'm', xlist)


  allfiles <- list()
  allfilesNames <- c()
  for (k in xlist){
    x <- read.table(sprintf("%s/%s",f,k))
    im <- gsub('-', 'm', k)
    colnames(x) <- c('angle', im)
    allfilesNames <- c(allfilesNames, im)
    allfiles[[k]] <- x
  }

  Spectra <-  Reduce(function(...) merge(..., by='angle', all=TRUE), lapply(1:length(xlist), function(i) allfiles[[i]]))

  #Gaps are filled average instead of missing values
  WithMissingValues <- zoo(Spectra)
  index(WithMissingValues) <- WithMissingValues[,1]
  SpectraWith <- na.approx(WithMissingValues)

  #NA rows removed. these are the first and last lines where interpolation does
  #not know how to create new values
  Clean0 <- na.omit(SpectraWith)
  Clean1 <- data.frame(Clean0)

  ang <- as.double(row.names(Clean1))
  list(
    ang=ang,
    spec=t(Clean1)
  )
}

# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages("rstudioapi")
# rstudioapi::isAvailable("0.99.149")
# devtools::install_github("r-lib/devtools")
