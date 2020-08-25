## Put comments here that give an overall description of what your
## functions do

# makeChacheMatrix aims to create the inverse of a matrix and then cacheSolve 
# would compute the inverse of that matrix if necessary.
# If the inverse of that exact matrix had been calculated, cacheSolve will
# return that value from cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initiates the inverse of matrix
  m <- NULL
  
  # define the get function to the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # define the get function to the matrix
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setinverse(m)
  # return the cached matrix if possible
  m
}
