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
        #force convoluted logic to detect change!!!
        #Used to be abl to detect if the matrix changed, and send a message.
        #cache <<- NULL
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
         getCachedInverse = getCachedInverse)

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
    
    ## Determines if the matrix from where the inverse was calculated has
    ## changed. 
    # Parameters:
    # delta : value to determine if a substraction result is aproximately zero
    hasNotChanged <- function (delta = 0.001){
        ##If the is no cache the response is FALSE since the is nothing to
        ## compare to.
        if (is.null(x$getCachedInverse())){
            return(FALSE)
        }
        
        # Multiply matrixes - should be "equal" to the identity matrix
        product <- x$getCachedInverse() %*% x$get()
        # Obtain "difference" from the identity matrix (obtained using diag() )
        difference <- product - diag(nrow=nrow(x$get()))
        # Holds the result, after comparing all the cells are close enough to zero
        notChanged <- TRUE
        # Applies the anonymous function to every matrix cell value. Equivalent
        # to a "for" loop in both dimensions, compares the delta with the cell
        # value, and determines if any of the values are bigger than the delta
        # (thus, considered a non-zero value)
        apply(difference, c(1,2), function(c){
            notChanged <<- notChanged & (c < delta)
            }
        )
        notChanged
    }
    
    inverse <- x$getCachedInverse()
    if(!is.null(inverse)) {
        if(hasNotChanged()){
            message("getting cached data")
            return(inverse)
        } else {
            message("matrix changed since previous cacheSolve() invocation!")
        }
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setCachedInverse(inverse)
    inverse
}
