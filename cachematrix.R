## The following is submission to Coursera's R Programming Assignment #2
## offered by Johns Hopkins University
## Below are implementation of two functions
## makeCacheMatrix: This function creates a special "matrix" object that can cache
##      its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned
##      by makeCacheMatrix above. If the inverse has already been calculated (and
##      the matrix has not changed), then the cachesolve should retrieve the 
##      inverse from the cache.

## makeCacheMatrix stores a matrix and its inverse. It also has set and get
## functions.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will return inverse of a given makeCacheMatrix object
## it first checks if it's inverse has been calculated, if not it
## uses base function solve. cacheSolve only works for invertible matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
