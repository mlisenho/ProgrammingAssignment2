## Author:  Michelle L. Isenhour
##   Date:  12-23-2019
## Course:  R Programming
##   Note:  Assignment #2

## The pair of functions below cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a list 
## containing functions which does the following:
##   1. sets the value of a matrix
##   2. gets the value of a matrix
##   3. sets the inverse of a matrix
##   4. gets the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The following function calculates the inverse of the 
## matrix created with the above function. However, it
## first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it computes
## the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
