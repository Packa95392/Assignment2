## Put comments here that give an overall description of what your
## functions do
## Overall we store a matrix and then retrieve this matrix for a total of two functions

## Write a short comment describing this function
##the makeCacheMatrix function creates the matrix via the set, get, setInverse, and getInverse arguments

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ##inverse is set as NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function()x ##calling the x matrix
  setInverse <- function(inverse)inv<<- inverse
  getInverse <-function(){  ##calling the inverse of x matrix
    inver<-ginv(x)
    inver%*%x}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) { ##function used to retrieve cache data
  inv<-x$getInverse()
  if(!is.null(inv)){  ##is the inverse null?
    message("getting cached data")
    return(inv)  ##returning inverse
  }
  mat<-x$get()
 inv<-solve(mat)%*%mat ##calculates inverse matrix x values
  x$setInverse(inv)
  inv ##requesting R to give inverse of matrix x
}


source("MakeCacheMatrix.R")
mm<-makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2))
> mm$get()
[,1] [,2]
[1,]    1    3
[2,]    2    4
> mm$getInverse()
[,1]          [,2]
[1,]    1 -2.664535e-15
[2,]    0  1.000000e+00
> cacheSolve(mm)
getting cached data
[,1]          [,2]
[1,]    1 -2.664535e-15
[2,]    0  1.000000e+00