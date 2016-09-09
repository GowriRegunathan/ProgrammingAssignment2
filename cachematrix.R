#makeCacheMatrix: This function creates a special "matrix" object that can cache
#its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) minv <<- inverse
  getInverse <- function() minv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
#has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  minv <- x$getInverse()
  if (!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  minv <- solve(mat, ...)
  x$setInverse(minv)
  minv
}
