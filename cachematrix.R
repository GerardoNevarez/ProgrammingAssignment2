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
    
    ## function to set a new value for the matrix (deletes the cached result)
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    ## Retrieves the matrix to be "inversed"
    get <- function() x
    
    ## Sets the cache to the given input (called from cacheSolve)
    setCachedInverse <- function(inverse) cache <<- inverse
    
    ## Retrieves the cached result
    getCachedInverse <- function() cache
    
    ## Determines if the matrix from where the inverse was calculated has
    ## changed. 
    hasChanged <- function hasChanged(){
        ##If the is no cache the response is FALSE since the is nothing to
        ## compare to.
        if (NULL == cache){
            return TRUE
        }
       
        ## TODO Finish implementation
        TRUE
    }
    list(set = set, get = get,
         setmean = setCachedInverse,
         getmean = getCachedInverse,
         hasChanged = hasChanged)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache.

## In order to verify that the original matrix has not changed, the cached
## matrix is multiplied by the original, and the result is compared to an
## identity matrix of the appropiate size

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## X is the result of a call to makeCacheMatrix()

}
