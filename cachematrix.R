## As matrix inversion is expensive in computation, the following two functions
## when utilized accordingly will help reduce the number of times the operation is
## performed when called multiple times with the help of caching.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) inv <<- solve
     getInverse <- function() inv
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)     
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse is already calculated, this function returns
## the cached result, provided that the value of the matrix is the same.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getInverse()
     if(!is.null(inv)) {
          message("Getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setInverse(inv)
     inv
}
