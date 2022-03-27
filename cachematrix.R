## Caching the inverse of a matrix
## It takes time and resources to repeatedly computing the inverse of a matrix.
## The functions below create an object that will store a matrix and caches the 
## inverse.

## This function creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      sets <- function(y) {
              x <<- y
              inv <<- NULL
      }
      gets <- function() x
      setInverse <- function(inverse) inv <<- inverse
      getInverse <- function() inv
      list(sets = sets,gets = gets,setInverse = setInverse,getInverse = getInverse)
}


## The function below computes the inverse of the matrix created above by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of x...
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Retrieving the cached data!!!")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
