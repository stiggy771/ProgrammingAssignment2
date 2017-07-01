## object i is assigned a null placeholder for future adding future matrix inverse data
## the value of the matrix is set 
##get the value of matrtix fed into x
##the value of inverse matrix is set
## get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function()x 
  setinverse <- function(inverse)i <<- inverse
  getinverse <- function(){i}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


##function below reurns the inverse matrix.
## the function checks if the value of inverse matrix is already fed into it or computed
## if yes, then the message "getting cached data" is shown and the stored vaule is displayed sans computation
## if no, then the function moves forward with the solve command and this value is stored in
## the cache by the setinverse function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i  ## Return a matrix that is the inverse of 'x'
}

##trial run

##x <- matrix(c(2,3,2,2),2,2)        
##m <- makeCacheMatrix(x)
##m$get()
##     [,1] [,2]
##[1,]    2    2
##[2,]    3    2
## no value in cache
##cacheSolve(m)
##[,1] [,2]
##[1,] -1.0    1
##[2,]  1.5   -1
## to see if inverse is again calculated
##cacheSolve(m)
##
##getting cached data
##[,1] [,2]
##[1,] -1.0    1
##[2,]  1.5   -1

