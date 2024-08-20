## Put comments here that give an overall description of what your
## functions do

# 1. This function creates a special “matrix” object that can cache its inverse.

## Write a short comment describing this function

# 2. This function, makeCacheMatrix creates a special “matrix”, 
# which is a list containing a function to:
  
# set the elements of the matrix
# get the elements of the matrix
# set the elements of the matrix inverse
# get the elements of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

# 3. This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It calculates the inverse of the matrix and sets it in the cache via the setinverse function.

cacheinverse <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
