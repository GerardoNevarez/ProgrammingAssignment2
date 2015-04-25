## This functions help cache the result of obtaiing the inverse of a matrix. Is
## expected thay any matix received as parameter is a square matrix than can be
## reversed. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

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
