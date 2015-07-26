## This functions have been created to accelerate the process of calculating the
## inverse of a matrix in certain contexts.

## The first function creates a list of functions associated to a given matrix.
## Specifically the set and setInv functions save data in the cache.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y = matrix()) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function decides wether to take the inverse of a matrix from the value
## saved in the cache or to calculate/recalculate it

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
  
}
