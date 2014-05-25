## These R functions can be used to cache potentially time-consuming Matrix inversion 
## computations  


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matris
## set the value of the inverse matrix
## get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the
## above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  
  im
}
