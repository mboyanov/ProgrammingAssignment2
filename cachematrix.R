## This R script allows caching of already computed matrice inverses.
## This is achieved through a special data structure created via the makeCacheMatrix function.
## The computed inverses are then available through the cacheSolve function.
## Example Usage:
## exampleMatrix <- makeCacheMatrix(matrix(c(1,2,3,4),3,3))
## cacheSolve(exampleMatrix)

## Defines a special data structure to allow the caching of computed inverses. The inverses are then available through the getInverse function.
## If any changes to the matrix data occurs, then the cache is reset. 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    inverse <<-NULL
    x<<-y
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Returns the inverse of objects created via the makeCacheMatrix function.
## If a cached inverse value is available, then it is immediately returned.
## Otherwise, the inverse is computed, cached and returned.
cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if (!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv<- solve(data,...)
    x$setInverse(inv)
    inv
}

