## This function computes the inverse of the special "matrix" created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## Examples verified with 
##  cMatrix <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
##  cMatrix$get()
##  cacheSolve(cMatrix)


