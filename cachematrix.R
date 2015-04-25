## Put comments here that give an overall description of what your
## functions do

## This function creates special matrix (actualy list) who stores 
## matrix itself and functions to get and set cached inverted value

makeCacheMatrix <- function(x = matrix()) {
        invert <- NULL
        set <- function(y) {
                x <<- y
                invert <<- NULL
        }
        get <- function() x
        setInvert <- function(invertM) invert <<- invertM
        getInvert <- function() invert
        list(set = set, get = get,
             setInvert = setInvert,
             getInvert = getInvert)

}


## This function computes inverted matrix.
## Before computing it cheks for cached (already computed) value
## and returns it if it's there.
## It uses a 'special' matrix made by makeCacheMatrix
##
## How to use
## c = rbind(c(1, -1/4), c(-1/4, 1))   ## 1. make matrix
## specMat <- makeCacheMatrix(c)       ## 2. make 'special' matrix
## cacheSolve(specMat)                 ## 3. compute inverted matrix and cach it value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invert <- x$getInvert()
        if(!is.null(invert)) {
                message("getting cached data")
                return(invert)
        }
        data <- x$get()
        invert <- solve(data, ...)
        x$setInvert(invert)
        invert
}
