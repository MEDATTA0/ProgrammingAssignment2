## Put comments here that give an overall description of what your
## functions do

## cachematrix.R
## Functions to create a matrix object that caches its inverse,
## and a function to retrieve the cached inverse (computing it only when needed).

## ---------------------------------------------------------------------
## makeCacheMatrix
## Creates a special "matrix" object (really a list of four functions)
## that stores a matrix and caches its inverse.
##
## Methods:
##   set(y)       - store a new matrix (clears any cached inverse)
##   get()        - retrieve the stored matrix
##   setInverse(inv) - store a computed inverse in the cache
##   getInverse() - retrieve the cached inverse (NULL if not yet cached)
## ---------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL  # cached inverse; NULL means "not yet computed"

  ## Store a new matrix and clear the stale cache
  set <- function(y) {
    x   <<- y     # update the matrix in the enclosing environment
    inv <<- NULL  # invalidate the cached inverse
  }

  ## Return the stored matrix
  get <- function() x

  ## Store a computed inverse in the cache
  setInverse <- function(inverse) inv <<- inverse

  ## Return the cached inverse (NULL if cache is empty)
  getInverse <- function() inv

  ## Return the four functions as a named list
  list(
    set        = set,
    get        = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## ---------------------------------------------------------------------
## cacheSolve
## Computes (or retrieves from cache) the inverse of the special matrix
## created by makeCacheMatrix().
##
## Logic:
##   1. Ask the cache object for a previously stored inverse.
##   2. If one exists, return it immediately with a message.
##   3. Otherwise compute the inverse via solve(), store it in the
##      cache, and return it.
##
## Arguments:
##   x   - a special "matrix" object created by makeCacheMatrix()
##   ... - additional arguments forwarded to solve()
## ---------------------------------------------------------------------
cacheSolve <- function(x, ...) {

  inv <- x$getInverse()

  ## Cache hit: inverse was already computed
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }

  ## Cache miss: compute, store, and return the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
