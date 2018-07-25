# Objective: Source necessary packages, libraries, and set directory for project

# Set working directory ---------------------------------------------------
setwd("/Users/cassandrabayer/Desktop/Estimating-Treatment-Effects")
options("scipen" =100, "digits" = 4)

# Load Packages -----------------------------------------------------------
# load basic packages
library(data.table)
library(tidyr)
library(tidyverse)

# visualization packages
library(ggplot2)
library(plotly)
library(corrplot)
library(stargazer)

# stats and prediction
library(stats)
library(forecast)
#library(fpp2)
library(tseries)
library(Hmisc)
library(MatchIt)

# Model Selection/Validation
library(MASS)
library(glmnet)
library(car)
library(ROCR)
library(pscl)

#Dates
library(zoo)
library(lubridate)

# Text analysis
library(RSiteCatalyst)
library(RTextTools)

# spell check
sorted_words <- names(sort(table(strsplit(tolower(paste(readLines("http://www.norvig.com/big.txt"), collapse = " ")), "[^a-z]+")), decreasing = TRUE))
# correct <- function(word) { c(sorted_words[ adist(word, sorted_words) <= min(adist(word, sorted_words), 2)], word)[1] }
library(qdap)
#devtools::install_github("ropensci/spelling")

# Load any custom functions -----------------------------------------------
rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

shift <- function(x, offset = 1, pad = NA) {
  r <- (1 + offset):(length(x) + offset)
  r[r<1] <- NA
  ans <- x[r]
  ans[is.na(ans)] <- pad
  return(ans)
}

maxMissing <- function(x){
  if(all(is.na(x))){
    return(NA_real_)
  } else{
    return(max(x, na.rm = T))
  }
}

nwords <- function(string, pseudo=F){
  ifelse( pseudo,
          pattern <- "\\S+",
          pattern <- "[[:alpha:]]+"
  )
  str_count(string, pattern)
}

wordParser <- function(word){
  unlist(lapply(seq(nchar(word)), function(x) substr(word, x, x)))
}
