## These two functions makeCacheMatrix and cacheSolve cache the inverse
## of a matrix. This especially for large matrices costly operation can
## therefore be avoided.

## makeCacheMatrix creates a special matrix object, which can cache the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize inverse as NULL
        inv <- NULL

        ## test if matrix is invertible
        ## otherwise print
        if (nrow(x) != ncol(x)) {
                # if the matrix is not square end function
                message("the entered matrix is no square matrix")
                return(NULL)
        }

        ## function to initialize the matrix
        set <- function(y) {
                # initialize the matrix x with y
                x <<- y
                inv <<- NULL
        }
        ## function to get the matrix
        get <- function() x

        ## function to set the inverse
        setInv <- function(solve) inv <<- solve

        ## function to get the inverse
        getInv <- function() inv

        ## return value matrix object actually consists of a number of lists
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve returns the inverse of the matrix x.
## It is taken from the cache if it was already calculated.
## Otherwise the inverse is calculated and stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ##  Get inverse for x
        inv <- x$getInv()
        ## If the returned inverse != NULL, the inverse was
        ## already calculated and is returned.
        if (!is.null(inv)) {
                message("getting data from cache")
                return(inv)
        }
        ## This code is only executed if the inverse is not
        ## already known.
        ## The matrix data is used to calculated it with
        ## the built-in function solve and stored in the
        ## cache.
        data <- x$get()
        inv <- solve(data,...)
        x$setInv(inv)

        ## The Inverse is returned
        inv
}
