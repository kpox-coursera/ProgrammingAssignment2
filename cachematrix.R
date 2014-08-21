
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse
## 
## cacheSolve: computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve retrieves the inverse from the cache


## returns a matrix object, which is really a list of functions that
## form a closure on the object. The functions allow setting the matrix, fetching it,
## and setting or fetching its inverse (from cache, so to say)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## fetches the inverse of a matrix 'x' from using the getinverse function from
## the matrix returned by makeCacheMatrix. If this fails, generates a new inverse for the matrix,
## caches it using the setinverse of the matric, and returns the cached value
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
