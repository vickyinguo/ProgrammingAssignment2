## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## get a special object that set, get the value of the matrix then set and get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the value of the matrix
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    ## getthe value of the matrix
    get <- function() x
    ## set the value of the inverse
    setinv <- function(inverse) inv <<- inverse
    ## get the value of the inverse
    getinv <- function() inv
    ## create list to store these
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## this assignment assumes that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## if the inverse has already been calculated (and the matrix has not changed), 
    ## then the cachesolve should retrieve the inverse from the cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if not, calculate the inverse and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
