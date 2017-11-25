## There are 2 functions: 1)makeCaheMatrix() and 2)cacheSolve().
## These functions will create a special matrix, calculate its inverse and cache the inverse matrix

## This function makeCacheMatrix creates a special "matrix" object and cache its inverse , 

makeCacheMatrix <- function(x = matrix()) {
  Inverse_m <- NULL
  set <- function(y){
    x<<-y
    Inverse_m<<-NULL
  }
get<- function() x
set_inverse <- function(inverse) Inverse_m <<- inverse
get_inverse <- function() Inverse_m
list(set=set, get=get, set_inverse=set_inverse, 
     get_inverse=get_inverse)
}


## This function cacheSolve takes input from the previous function and calculates its inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse_m<-x$get_inverse()
  if(!is.null(Inverse_m)){
    message("getting cached data")
    return(Inverse_m)
  }
  data <- x$get()
  Inverse_m <- solve(data, ...)
  x$set_inverse(Inverse_m)
  Inverse_m
}
