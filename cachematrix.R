## Implementation of a special matrix ('cacheMatrix'), which caches 
## its inverse. This is done by using two functions:
## "makeCacheMatrix" for initialization and
## "cacheSolve" to get the inverse.
## The inverse will only be calculated on the first call of cacheSolve, 
## or if the matrix has been changed; otherwise the cached inverse 
## will be returned

## "makeCacheMatrix" initializes a 'cacheMatrix';
## also provides functions to set and get the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(newinverse) inverse <<- newinverse
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## "cacheSolve" returns the inverse of a 'cacheMatrix' x,
## which has been initialized using "makeCacheMatrix"

cacheSolve <- function(x, ...) {
     inverse <- x$getinverse()
     if(!is.null(inverse)) {        ## Test if there is a cached inverse
          return(inverse)           ## if yes return it
     }
     data <- x$get()                ## if not calculate, cache and return
     inverse <- solve(data, ...)    ## the new inverse
     x$setinverse(inverse)
     inverse     
}
