## A CacheMatrix is an object that allow us
## to store a matrix as well as its inverse. 

## We create a CacheMatrix from a matrix by calling makeCacheMatrix.
## makeCacheMatrix constructs a list of four functions that allow
## us to get/set the matrix as well as its inverse

## We pass in a CacheMatrix to cacheSolve to compute the inverse
## of a matrix. cacheSolve will calculate the inverse on
## its first invocation and use the cached value on subsequent
## calls.

## makeCacheMatrix creates a CacheMatrix from a matrix x.
## The CacheMatrix is implemented as a list of four functions:
##   1. set(y) - takes in a new matrix and resets its inverse to null
##   2. get() - returns the current matrix
##   3. setinverse(inverse) - sets the inverse value of the matrix
##   4. getinverse() - gets the inverse which may be null if it
##                     has not been calculated.

## inputs: x is a regular R programming language matrix
## returns: list(set, get, setinverse, getinverse) as described above.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
       x <<- y
       inv <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse of a matrix stored in a CacheMatrix.
## This function will use the stored value of the inverse
## if it has already been calculated and will calculate it
## otherwise.

## inputs: x is a CacheMatrix
## returns: the inverse of the matrix returned by x$get()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
       return(inv)
    }
    
    mat = x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
