## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix <- function(x = matrix()) {
##
##}

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



## Write a short comment describing this function

##cacheSolve <- function(x, ...) {
##        ## Return a matrix that is the inverse of 'x'
##}

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