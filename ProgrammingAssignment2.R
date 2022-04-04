## Caching the Inverse of a Matrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversematrix <- function(x) im <<- solve(x)
  getinversematrix <- function() im
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
} 

## This function computes the inverse of the above or retrive from the cache if alreadz calculated
cacheSolve <- function(x, ...) {
  im <- x$getinversematrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  im 
}

