## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix (matrix):  This function creates a matrix object that can cache its 
## inverse. 
## cacheSolve : This function computes the inverse of the special "matrix" returned by   
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

## setinv (y) : This function will set the cached inverse matrix to y
## getinv (): This function will return the cached inverse matrix.   
## get (): This function will return the special matrix
## set(y)  : This fuction will set the special matrix to y


## Write a short comment describing this function

makeCacheMatrix <- function(matrix = matrix()) {
inv <-NULL   ## initialize inverse matrix to NULL 

set <- function(y) {     ## This fuction will set the special matrix to y
  matrix <<- y
  inv <<- NULL
}

get <- function() matrix       ## This function will return the special matrix
setinv <- function(w) inv <<- w    ##This function will set the cached inverse matrix to w
getinv <- function() inv    ## This function will return the cached inverse matrix.

list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv  
}
