## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse.

## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

## makeCacheMatrix create a matrix that can have its inverse stored on cache by the next function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve will store the inverse of the indicated matrix on cache. If the function was already been applied to the matrix,
## than the function will get the inverse matrix on cached data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv   
}

## Testing the funcions

my_matrix <- matrix(rnorm(25),5,5)
my_matrix_cached <- makeCacheMatrix(my_matrix)
cacheSolve(my_matrix_cached)

## [,1]       [,2]       [,3]        [,4]       [,5]
## [1,]  0.243659392 -0.4429717 -0.4985223 -1.65642020  0.4645730
## [2,]  0.033708219  0.6971282  1.0428226  1.14785339 -1.4920296
## [3,] -0.604838774  0.6280577  0.2140196  1.47902504  0.7852962
## [4,] -0.260374914 -0.1351449  0.3420485 -0.04331824 -1.4472900
## [5,] -0.004439894 -0.1412987  1.0594317  0.28799940 -1.0416161

cacheSolve(my_matrix_cached)

## getting cached data

## [,1]       [,2]       [,3]        [,4]       [,5]
## [1,]  0.243659392 -0.4429717 -0.4985223 -1.65642020  0.4645730
## [2,]  0.033708219  0.6971282  1.0428226  1.14785339 -1.4920296
## [3,] -0.604838774  0.6280577  0.2140196  1.47902504  0.7852962
## [4,] -0.260374914 -0.1351449  0.3420485 -0.04331824 -1.4472900
## [5,] -0.004439894 -0.1412987  1.0594317  0.28799940 -1.0416161
