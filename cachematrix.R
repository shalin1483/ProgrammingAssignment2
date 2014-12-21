## Create a cache marix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- matrix(runif(16), nrow=4, ncol=4)
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the matrix being cached.
##  M <- cacheMatrix$get()  # Returns the matrix being cached.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

#create a cache matrix for given matrix.
makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        ## Set functions is used to update matrix.
        ## Inverse will be reset when new matrix is set.
        setMatrix <- function(y) {
                x <<- y
                InverseMatrix <<- NULL
        }
        ## Get saved matrix
        getMatrix <- function() x
        ## Set inverse matrix
        setInverse <- function(inverse) InverseMatrix <<- inverse
        ## get inverse matrix
        getInverse <- getInverse <- function() InverseMatrix
        ##return list of functions
        list(set = setMatrix,
             get = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If matrix is found in the cache it will be returned.

cacheSolve <- function(x, ...) {
        ## check cache
        cacheInverse <- x$getInverse()
        if(!is.null(cacheInverse)) {
                message("Available in cache")
                return (cacheInverse);
        }
        ## cache empty compute and save inc cache
        matrix <- x$get()
        Inv <- solve(matrix, ...)
        x$setInverse(Inv)
        Inv
}
