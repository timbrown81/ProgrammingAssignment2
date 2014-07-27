## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
## set variables
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  ## this method will get the value of a matrix
  get <- function() x
  
  ## this method will set the inverse of a matrix
  setinverse <- function(solve) inv_x <<- solve
  getinverse <- function() inv_x
  
  ## this method will get the inveserse of a matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## get the inverse of matrix x
  inv_x <- x$getinverse()
  
  ## test to see if we already have cached the inverse
  if(!is.null(inv_x)) {
    message("fetching inverse from cache")
    return(inv_x)
  }
    ## if not already cached, calculate new inverse   
    data <- x$get()
    inv_x <- solve(data, ...)
    ## cache our newly calcurated inverse
    x$setinverse(inv_x)
    inv_x
}
