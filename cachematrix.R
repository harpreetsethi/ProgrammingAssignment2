## This file contains two functions makeCacheMatrix & cacheSolve
## makeCacheMatrix - is always invoked first when setting the matrix variable
## cacheSolve - is called to compute the inverse of a matrix
# Restrictions - function is set up to accept & compute inverse of an invertible square matrix only
# please remember a matrix such as matrix (c(1,1,1,1), nrow=2, ncol=2) is singular and not invertible

## Usage
# var_x<-makeCacheMatrix (matrix (c(1,2,3,4), nrow=2, ncol=2))
# cacheSolve (var_x)

makeCacheMatrix <- function(x = matrix()) {
  matrix_var<-NULL #initialize the inverse matrix variable
  
  #define the set function including setting the inverse matrix variable to null
  set <-function (y){
    x<<-y
    matrix_var<<-NULL
  }
  
  #returns the matrix, already set in set function
  get <-function () x
  
  #sets the inverse matrix variable for future reference
  setinverse <- function (inverse) matrix_var <<- inverse
  
  #returns the inverse matrix variable
  getinverse <- function () matrix_var
  
  #call the functions via the list command
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #call the get inverse function
  matinv <- x$getinverse()
  
  #check if the inverse of the matrix has been already formulated
  # If yes then return the stored value, otherwise the control by passes the if condition
  if (!is.null(matinv)){
    message ("getting cached matrix inverse")
    return (matinv)
  }
  message ("cached matrix inverse not found")
  
  #get the matrix data already set when the makeCacheMatrix was invoked
  matdata <- x$get()
  
  #call the solve function with input and other default parameters
  message ("computing matrix inverse")
  matinv <-solve(matdata,...)
  
  #call the set inverse function to set the value for future references
  x$setinverse(matinv)
  
  #print the inverse matrix
  matinv
}
