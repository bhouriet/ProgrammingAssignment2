## makeCacheMatrix and cacheSolve are a pair of functions that
## work together and cache (store) the inverse of a matrix

# Function makeCacheMatrix -> This function creates a 
# special "matrix" object that can cache its inverse
# -> We assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}

# Function cacheSolve -> This function computes the inverse 
# of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has
# not changed), then the cachesolve should retrieve inv. from cache
# To calculate the inverse -> we use the function "solve" from R

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
 
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv      
  
  ## Return a matrix 'inv' that is the inverse of 'x'
}
