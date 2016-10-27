## OVERALL DESCRIPTION
## makeCacheMatrix sets up a cache for a matrix.
## CacheSolve solves for the inverse of a square invertible matrix.


## The makeCacheMatrix function returns a list of four functions, 
## set - to set the matrix
## get - to get the matrix
## setmatrix to set the inverse matrix
## getmatrix to get the inverse matrix
## This list of functions is used as inpute for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve uses the getmatrix function in makeCachematrix to get the
## last iteration of the inverse matrix and checks if it is null.  If not
## it gets the cached data. Solve provides the inverse of the matrix 

cacheSolve <- function(x, ...) { m <- x$getmatrix()
if(!is.null(m)) {
        message("getting cached data")
        return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setmatrix(m)
m
} 
## Returns a matrix that is the inverse of 'x'

## Test Input Data Below:
##
## Matrices<- matrix(2:6,nrow=2, ncol=2)
## test<- makeCacheMatrix(Matrices)
## cacheSolve(test)