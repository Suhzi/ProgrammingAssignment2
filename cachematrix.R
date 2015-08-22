## The two functions in this file are makeCacheMatrix and cacheSolve.
## The first function will be used to create a special "matrix" that can cache its inverse and, 
## the second function will be used to compute the inverse of the matrix returned from the frst function.
## It is assumed that the matrix given is invertible, so it will always be possible to compute its inverse.


## This first function, makeCacheMatrix creates a special "matrix" which is able to cache its inverse. 
## This function will return the special "matrix".
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## This sets the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## This gets the value of the matrix
        get <- function() x
        ## The value of the inverse of the matrix is set
        setInverse <- function(solve) inv <<- solve
        ## This returns the value of the inverse of the matrix
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This second function, cacheSolve will compute the inverse of the matrix returned from the makeCacheMatrix function. 
## It will check whether the inverse of the matrix has already been calculated;
## if the inverse has been calculated, this function will retrieve the inverse from the cache.
## This function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()        
        ## Check if this inverse has already been computed
        if(!is.null(inv)){
                ##If the inverse is not null, retrieve the inverse from the cache and return it, 
                ##because the inverse has already been computed. 
                message("retrieving cached data") 
                return(inv)
        }
        ## otherwise,
        ## Compute the inverse if it is null
        data <- x$get()
        inv  <- solve(data, ...)
        x$setInverse(inv) 
        ##The inverse of 'x' is returned 
        inv     
} 


