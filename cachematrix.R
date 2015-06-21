## Subroutine   : cacheMatrix.R
## Date         : June 18, 2015

## This subroutine will make a matrix object and checks whether the object's
## inverse exists in the cache. If it already exists then the subroutine will 
## get and return the inverse. If not then it will calculate the inverse using
## the 'solve' function and stores the inverse in the cache. The subroutine 
## consists of two parts: makeCacheMatrix and cacheSolve, which are described 
## below

## makeCacheMatrix function creates a special matrix object that does the 
## following:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse
##      4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix returned by makeCacheMatrix 
## or retrieves the inverse from the cache if already previously calculated
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
