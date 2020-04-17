## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. 
# Therefor a pair of functions is created that cache the inverse of a matrix. 

## Write a short comment describing this function
# This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.
# Input should be a matrix 
# Assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  InvMat <- NULL
  #setting the value of the Matrix
  setMat <- function(y) {
    x <<- y
    InvMat <<- NULL
  }

  #get the value of the Matrix
  getMat <- function() x                              
  setInv <- function(inverse) InvMat <<- inverse   	# set the value of invertible matrix 
  getInv <- function() InvMat                      	# get the value of invertible matrix 
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
# The function 'cacheSolve' computes the inverse of the matrix obtained from 'makeCacheMatrix(matrix)'.
# If the computed inverse matrix is empty, then it gets the original matrix and then set the invertible matrix by the use of solve function.
# If the inverse matrix from 'makeCacheMatrix(matrix)' is not emtpy (i.e., has some value) then it returns a message
# "Getting Cached Invertible Matrix" and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMat <- x$getInv()		 # get the value of the invertible matrix from the above makeCacheMatrix function	
  if(!is.null(InvMat)) {                       
    message("Getting Cached Invertible Matrix")   
    return(InvMat)               # Return the invertible matrix
  }

  # if value of the invertible matrix is NULL then  
  MatrixData <- x$getMat()                     
  InvMat <- solve(MatrixData, ...)         # solve(X) returns inverse of a matrix X     
  x$setInv(InvMat)                         
  return(InvMat)        
}
