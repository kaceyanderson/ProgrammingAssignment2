## # In this assignment we are asked to write an R function that is able 
# to cache potentially time-consuming computations.  We were introduced to the
# <<-- operator which can be used to assign a value to an object in an environment 
# that is different from the current environment.  Two functions were 
# introduced in the assignment that will be used here to create a special 
# object that store's a numeric vector and cache's its mean.  The first function 
# "makeCacheMatrix" is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


## The following describes the code for makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  

}


## The second function cacheSolve returns the inverse of the matrix created 
# by makeCachematrix. First it will check to see if the inverse has already been calculated.
# If previously calculated it will retrieve the inverse from the cache. 
# If not already calculated, it will compute the inverse through the setinverse function.
# In this example we are asked to assum the matrix is always invertible.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
  message("getting cached data.")
  return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
