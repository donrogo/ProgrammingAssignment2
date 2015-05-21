## this is an function using the <<- operator which is used to
## assign an value to an object, but not in the same environment
## but a different one.


## The makeCacheMatrix sets and gets the value of the matrix and
##  then it sets and gets the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  
  #create cache and initialize to NULL
  i <- NULL
  
   #create matrix in wd  
   set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if the matrix is not empty. If it is not
## empty then it return the cached date. If it is empty it
## computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
