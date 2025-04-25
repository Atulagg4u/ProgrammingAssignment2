# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the cached inverse
  
  # Setter function to update the matrix and clear the cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter function to return the matrix
  get <- function() x
  
  # Setter function for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function for the inverse
  getInverse <- function() inv
  
  # Return a list of functions to interact with the matrix and its inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Function to compute or retrieve the cached inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, compute the inverse, cache it, and return it
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
