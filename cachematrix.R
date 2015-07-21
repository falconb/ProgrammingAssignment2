## Put comments here that give an overall description of what your
## functions do

# The functions makeCacheMatrix and cacheSolve help speed up inverting a matrix by
# caching the results and returning the cached results, whenever available,
# instead of calculating each time

## Write a short comment describing this function

# makeCacheMatrix takes a matrix (x) as an input and a list of 4 functions as output -
# set - sets the matrix
# get - gets the matrix
# setInverse - sets the inverse of the matrix
# getInverse - gets the (cached) inverse of the matrix
# The function caches the result of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL
  set <- function(y_mat) {
    x <<- y_mat
    mat_inv <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inv) {
    mat_inv <<- inv
  }
  getInverse <- function(){
    mat_inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve calculates the inverse of the matrix
# it checks to see if the inverse already exists in the cache, if yes - 
#     - return inverse from the cache
# if the inverse does not exist in the cache -
#     - Calculate inverse
#     - Set the inverse in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getInverse()
  if(!is.null(mat_inv)){
    message("Getting Cached Data")
    return(mat_inv)
  }
  mat <- x$get()
  mat_inv <- solve(mat)
  x$setInverse(mat_inv)
  mat_inv
}
