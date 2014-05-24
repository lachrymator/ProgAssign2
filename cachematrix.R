## makeCacheMatrix(x) returns a list of functions to cache a matrix and its inverse
## Does the following 4 things
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix (N.B external code required to actually calculate inverse)
## 4. get the value of the inverse matrix
## 
## Example usage:
## cache <- makeCacheMatrix(x)
## cache$get()              #returns x
## cache$setinv(solve(x))   #set the inverse of x using the solve() function
## cache$getinv()           #returns the inverse set by the above command


makeCacheMatrix <- function(x = matrix()) {
    
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solved) inv <<- solved
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve(cache) uses the cache list created by the above makeCacheMatrix() to identify whether
## the inverse of a matrix has already been cached
## If it has, then return the cached value
## If it hasn't, then calculate the inverse and store it within the list for future use
##
## Sample usage:
## cache <- makeCacheMatrix(x)  # creates the function list as above
## cacheSolve(cache)            # calculates the inverse for the first time, and store it in cache
## cacheSolve(cache)            # second iteration returns the cached inverted matrix
## 
## ASSUMPTION: Original x input matrix is a square invertible matrix



cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  #Here we are using the solve() function to actually calculate the inverse
  x$setinv(inv)
  inv
}

