## The sources contained in this file are used to
## create a matrix object that could be used to
## cache its inverse, to avoid computations if the 
## inverse has already been calculated

## makeCacheMatrix # Creates a special matrix object able to cache
## its inverse
## 
## @param x a square invertible matrix; we suppose the matrix to be always 
##          invertible
## @return a list containing functions used to: 1)set the matrix 2)get the matrix
##         (3) set the inverse  (4) get the inverse

makeCacheMatrix <- function(m = matrix()) {
  ##If the matrix is square
  if(nrow(m)==ncol(m)) {
  inv <- NULL ##Stored cached value of the inverse of the matrix
  
  #Set function to assign the values of the matrix
  set <- function(y) {
    if(nrow(m)==ncol(m)) {
      m <<- y
      inv <<- NULL
    }
    else
      stop("Provided matrix is not square as per requirements")
  }
  
  #Get function to retrieve the value of the matrix
  get <- function() m
  
  #Function to set the cached value of the matrix's inverse
  setinv <- function(inverse) inv <<- inverse 
  
  #Function to retrieve the cached value of the matrix's inverse
  getinv <- function() inv
  
  #Functions exposed through the object
  list( set = set, get = get, 
        setinv = setinv, getinv = getinv)
  }
  else 
    stop("Provided matrix is not square as per requirements")
}


## Function that retrieves a matrix that is the inverse of m, possibly using the cached
## computed inverse of the matrix
##
## @param m a cachable matrix object (output of makeCacheMatrix() )
## @return a matrix that is the inverse of m

cacheSolve <- function(m, ...) {
  
  inv <- m$getinv()
  
  # Does the object that has been passed contain a cached result already?
  if (is.null(inv)){
    # If not, calculate the inverse
    retrieved <- m$get()
    inv <- solve(retrieved, ...)
    
    # Store the value of the inverse in the original matrix object through the setinv function
    m$setinv(inv)
  }
  
  # return the cached result 
  return(inv)
}
