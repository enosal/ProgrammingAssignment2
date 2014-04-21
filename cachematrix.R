## SUMMARY. The following functions solve the inverse of a given matrix,
## store the cached inverse, and retrieve the cached inverse. If the inverse
## of a matrix has not yet been solved for, the function will solve for the
## inverse. If the inverse has already been solved for, the solved inverse
## is saved so that it can be retrieved in the future instead of recomputed.

## makeCacheMatrix takes the argument of a matrix and returns a list with 4
## items: setting the value of a matrix, getting the value of a matrix, 
## setting the value of the solved matrix, getting the value of the solved
## matrix. 



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve solves for the inverse of a matrix using the vector that
## makeCacheMatrix outputs. First, cacheSolve checks to see if the inverse
## has already been solved and retrieves the solved inverse.If the inverse 
## has not yet been solved for, the function solves for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}