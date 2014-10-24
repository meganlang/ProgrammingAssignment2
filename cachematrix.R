## Create functions to cache the inverse of a matrix.

## First function: Create a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x #This will return the original matrix
  setInv <- function(solve) m<<- solve
  getInv <- function() m 
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Compute the inverse of the matrix created above. If the inverse has already been 
##calculated, then get the inverse from the cached value.

cacheSolve <- function(x) {
  m <- x$getInv()
  if(!is.null(m)){ #If m is not null, it means that the inverse has already been
    #computed and cached, so retrieve rather than re-calcuating.
    message("getting cached data")
    return(m)
  }
  data <- x$get() #From the first function, grab the original matrix.
  m <- solve(data) #Computes the inverse.
  x$setInv(m) 
  m
}
