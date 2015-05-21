## Put comments here that give an overall description of what your
## functions do

# This makeCacheMatrix function creates a inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
# Variable m stores the Inverse Matrix of x here
  
  m <- NULL
# set function used to set the matrix x
  
  set <- function(y) {
 # <<- assign x to different environment variable y 
    x <<- y
    m <<- NULL
  }
# get function used to get the matrix x
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
# list returns values of matrix x and InverseMatrix m 
# using these set,get,setinv and getinv functions
  list(set = set, get = get, setinv = setinv,getinv = getinv)
}

#This  cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
# Return a matrix that is the inverse of 'x'
# Here the function gets the inverse matrix from cache
  
  m <- x$getinv()
# First checks if matrix is not null then skips the calculation 
# Gets the inverse matrix from cache.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
# Get the data from matrix and stores in a data variable
  data <- x$get()
#This solve function calculates the inverse matrix.
  m <- solve(data, ...)
# Set the inverse matrix in the cache through the setinv() function
  x$setinv(m)
  m
}


# Test Case

# matrix1  <- matrix(runif(16,1,20), 4, 4)
# inv.mat <- makeCacheMatrix(matrix1)
# cacheSolve(inv.mat)
# cacheSolve(inv.mat)
# Getting cached data

