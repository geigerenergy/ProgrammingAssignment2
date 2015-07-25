## makeCacheMatrix creates a special matrix based on a standard matrix
## that was fed to the function. It includes generating the utility functions
## (getter and setter) that will be used to obtain the inverse if present.
## cacheSolve gets the matrix and tries to read the inverse. If it was computed
## it is obtained otherwise, it will compute the inverse and save it to the 
## special matrix

## gets matrix, defines variables and getter and setter functions

makeCacheMatrix <- function(matrix = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    matrix <<- y
    inverse_matrix <<- NULL
  }
  get <- function() matrix
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## reads the inverse, if null is returned, copute inverse and save to matrix

cacheSolve <- function(matrix, ...) {
  inverse_matrix <- matrix$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached matrix data")
    return(inverse_matrix)
  }
  data <- matrix$get()
  inverse_matrix <- solve(data, ...)
  matrix$setinverse(inverse_matrix)
  inverse_matrix
}
