## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
##          returned by makeCacheMatrix above. If the inverse has already been calculated 
##          (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.
##
## example usage: 
## mymatrix <- makeCacheMatrix(matrix(c(2,5,1,3), nrow=2, ncol=2, byrow=TRUE))

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## use this function to change the matrix
    set <- function(y) {
        message("reset matrix")
        x <<- y      # stores given matrix (y) in (global) the matrix (x)
        i <<- NULL   # reset (global) the inverted matrix (i)
    }
    get <- function() x  # return the (global) stored matrix
    setinverse <- function(inverse) i <<- inverse  # stores the inverse to the global inverted matrix (i)
    getinverse <- function() i   # get the (global) stored inverted matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function will return a matrix that is the inverse of 'x'.
##
## Therefore it will first lookup if the matrix already has an inverse calculated and cached.
## If yes, the function returns the cached result, otherwise calculates it, and store it to the cache.
##
## example usage (you need to call makeCacheMatrix(mymatrix) before!!): 
## cacheSolve(mymatrix)
## 

cacheSolve <- function(x, ...) {
        ## check if there is already an inverse of the given matrix (x)
        i <- x$getinverse()
        if(!is.null(i)) {
            # inverse found so return the stored (cached) inverse
            message("getting cached data")
            return(i)
        }
        ## calculate the inverse
        data <- x$get()   # get the matrix
        i <- solve(data, ...)  # calculate the inverse
        x$setinverse(i)  # store (local) result to cache
        i  # return the result (inverted matrix)
}
