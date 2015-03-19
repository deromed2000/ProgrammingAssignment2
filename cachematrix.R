## We create here two functions that the first creates a matrix and is able to cache
## its inverse after first calculation for later use

## This function caches a matrix and returns a list of functions
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse matrix
##4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        set <- function(y) {
                x <<- y
                matrixInv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) matrixInv <<- solve
        getsolve <- function() matrixInv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calculate the inverse of a matrix if it does not exist yet
##in memory, if else it returns the matrix cached


cacheSolve <- function(x, ...) {
        matrixInv <- x$getsolve()
        if(!is.null(matrixInv)) {
                message("getting cached data")
                return(matrixInv)
        }
        data <- x$get()
        matrixInv <- solve(data, ...)
        x$setsolve(matrixInv)
        matrixInv
}

