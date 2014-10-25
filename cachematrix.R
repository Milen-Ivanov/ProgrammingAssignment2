## This R function is able to cache the inverse of a matrix.  
## Provided that the contents of the matrix are not changing,
## the inverse of the matrix can be looked up in the cache when needed,
## rather than being recomputed again.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                  s <- NULL
                  set <- function(y) {                        ## set the value of the matrix
                      x <<- y
                      s <<- NULL
                  }
                  get <- function() x                         ## get the value of the matrix       
                  setinverse <- function(solve) s <<- solve   ## set the value of the inverse
                  getinverse <- function() s                  ## get the value of the inverse
                  list(set = set, get = get,
                      setinverse = setinverse,
                      getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
                s <- x$getinverse()
                if(!is.null(s)) {
                        message("getting cached data")
                        return(s)
                }
                data <- x$get()
                s <- solve(data, ...)     ## Computing the inverse of a square matrix 
                x$setinverse(s)           ## Seting the inverse value in the cache     
                s      
}
