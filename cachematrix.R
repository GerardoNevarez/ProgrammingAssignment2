## This functions help cache the result of obtaiing the inverse of a matrix. Is
## expected thay any matix received as parameter is a square matrix than can be
## reversed.

## Based on the MakeVector and cacheMean functions in the instructions for
## the programming assignment 2 (Coursera course "R Programming", by Roger D.
## Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD; John Hopkins Bloomberg School
## of Public Health)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## Variable to store the cached result
    cache <- NULL
    changed <- FALSE
    
    ## function to set a new value for the matrix (deletes the cached result)
    set <- function(y) {
        x <<- y
        # Sets change flag
        changed <<- TRUE
    }
    
    ## Retrieves the changed flag
    isChanged <- function(){
        changed
    }
    
    ## Resets change flag
    resetChanged <- function(){
        changed <<- FALSE
    }
    
    
    ## Retrieves the matrix to be "inversed"
    get <- function(){
        x
    }
    
    ## Sets the cache to the given input (called from cacheSolve)
    setCachedInverse <- function(inverse){
        cache <<- inverse
    }
    
    ## Retrieves the cached result
    getCachedInverse <- function(){
        cache
    }
    
    list(set = set, get = get,
         setCachedInverse = setCachedInverse,
         getCachedInverse = getCachedInverse,
         isChanged = isChanged,
         resetChanged = resetChanged)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

## In order to verify that the original matrix has not changed, we maintain a
## logical/boolean flag to determine if the matrix data has been changed through
## set() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## X is the result of a call to makeCacheMatrix()
    
    # Get cached inverse
    inverse <- x$getCachedInverse()
    
    # if there is actually a value cached... 
    if(!is.null(inverse)) {
        # And the original matrix has NOT changed...
        if(!x$isChanged()){
            # return the cached inverse
            message("getting cached data")
            return(inverse)
        } else {
            # ELSE
            # resets/erases the cached inverse
            x$setCachedInverse(NULL)
            # and sends a messageadvising of the data change
            message("matrix changed since previous cacheSolve() invocation!")
        }
    }
    # Gets the matrix to be inversed/solved
    data <- x$get()
    # Obtains the inverse by making a call to solve()
    inverse <- solve(data, ...)
    # stores/cached the result
    x$setCachedInverse(inverse)
    # resets the "data changed" flag
    x$resetChanged()
    # returns the inverse as function result
    inverse
}
