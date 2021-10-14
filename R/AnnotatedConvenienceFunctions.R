library(stats)

#' Drop NAs by Columns
#' 
#' Remove NAs based on specified columns in the data
#' @param data data.frame object of variations
#' @param desiredCols list of columns from which incomplete cases should be dropped
#' 
#' @return dataframe with removed observations
#' @examples 
#' data<-data.frame(a=1:4,b=c("a","b","c","d"),c=c(NA,"keep",NA,"keep"))
#' completeFun(data,c("c"))
#' 
#' @export
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#' Mode
#' 
#' Function to calculate the mode of a variable
#' 
#' @param x numeric vector
#' 
#' @return numeric vector of modes
#' 
#' @examples 
#' x<-c(1,1,3,5,6,6)
#' Modes(x)
#' 
#' @export
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

#' Geometric mean
#' 
#' Function to calculate the geometric mean of a variable
#' 
#' @param x numeric vector
#' 
#' @return numeric value of geometric mean
#' 
#' @examples 
#' x<-c(1,1,3,5,6,6)
#' gm_mean(x)
#' 
#' @export
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' Factorial
#' 
#' Function to calculate the factorial of a variable
#' 
#' @param x numeric vector
#' 
#' @return numeric value of factorial
#' 
#' @examples 
#' factorial(5)
#' 
#' @export
factorial <- function(x){
  if(x==0)
    return(1)
  else
    return(x*factorial(x-1))
} 

#' Non-unique
#' 
#' Function that returns all non-unique values in a vector
#' 
#' @param x numeric or character vector
#' 
#' @return numeric or character vector of non-unique values
#' 
#' @examples 
#' x<-c(1,1,3,5,6,6)
#' nonUnique(x)
#' 
#' @export
nonUnique<-function(x){
  #return non-unique values
  return(unique(x[duplicated(x)]))
}