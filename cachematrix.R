## makeCacheMatrix function creates a special "matrix" object that
##  can cache its inverse.
## cacheSolve function computes the inverse of the special "matrix"
##  returned by makeCacheMatrix. If the inverse has already been
##  calculated (and the matrix has not changed), then the cachesolve
##  should retrieve the inverse from the cache.


## The makeCacheMatrix function creates a list containing a function to:
##   1.set the value of the matrix
##   2.get the value of the matrix
##   3.set the value of the inverse of the matrix
##   4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     invm <- NULL
     set <- function(y){
          x <<-y
          invm <<- NULL
     }
     get <-function()x
     setinvers <- function(invers) invm <<-invers
     getinvers <- function() invm
     list(set = set, get = get,
          setinvers = setinvers,
          getinvers = getinvers)
}


## The following function calculates the inverse of the matrix  
##   It first checks if the inverse matrix has already been calculated. 
##   If so, it gets the inverse matrix from the cache and skips the computation. 
##   Otherwise, it calculates it, and sets the value of the inverse matrix in the 
##   cache via the setinvers function.
##   This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     invm <- x$getinvers()
     if(!is.null(invm)) {
          message("getting cached data")
          return(invm)
     }
     data <- x$get()
     invm <- solve(data)
     x$setinvers(invm)
     invm
}
