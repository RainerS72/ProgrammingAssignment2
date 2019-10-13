## Put comments here that give an overall description of what your
## functions do

## This function is created based on example makeVector from Assignment2
## The purpose of that function is to cache inverse results
makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    set <- function(y) {
        X <<- Y
        inv <<- NULL
    }
    get <- function() X
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## This function is to compute inverse of the matrix. In case inverse has been already calculated
## it will fetch inverse results from cache
cacheSolve <- function(X, ...) {
    inv <- X$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- X$get()
    inv <- solve(data, ...)
    X$setInverse(inv)
    inv
}