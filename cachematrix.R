## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrix_var<-NULL
  set <-function (y){
    x<<-y
    matrix_var<<-NULL
  }
  get <-function () x
  setinverse <- function (inverse) matrix_var <<- inverse
  getinverse <- function () matrix_var
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  matinv <- x$getinverse()
  if (!is.null(matinv)){
    message ("getting cached matrix inverse")
    return (matinv)
  }
  matdata <- x$get()
  matinv <-solve(matdata,...)
  x$setinverse(matinv)
  matinv
}
