## makeCacheMatrix creates a list containing a function that does the following:
## 1) Set the value of the matrix
## 2) Get the value of the matrix
## 3) Set the value of the inverse of the matrix
## 4) Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Assuming that the matrix is always invertible, this function returns the inverse of the matrix.
##It first checks if the inverse has already been computer (if so, it gets the results and skips computation).
##It then computers the inverse and sets the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
