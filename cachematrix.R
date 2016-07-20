## Put comments here that give an overall description of what your
## functions do

## This function allows precalculated matrix inverse to be stored in memory - and not be recalculated repeatedly.
## This improves run time performance. 


makeCacheMatrix <- function(x = matrix()) {

## populate get, set, getinv, setinv - and make them a list. 

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


## This function first determines if the inverse matrix has already been calculated and returns the result if it has.
## If not calculated - it will perform the calculation. 

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

