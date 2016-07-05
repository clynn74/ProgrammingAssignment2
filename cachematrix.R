## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) 
    { x <<- y    
      inv <<- NULL     
    }
    
    get <- function() x
    
    setINV <- function(inverse) inv <<- inverse;

    getINV <- function() inv
    
    list(set = set, get = get,
         setINV = setINV,
         getINV = getINV)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Calculate the inverse of 'x'
  inv <- NULL ##x$getINV()
  print(x)
  if (!is.null(inv)) {
    message("use cache")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setINV(inv)
  return(inv)
}

