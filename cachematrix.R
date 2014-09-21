## These functions cache the inverse of a matrix and retrieve it if it has been calculated before

## Ceates a special matrix, which can perform the following functions
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## set the value of the inverse of matrix
  setinv <- function(inv) m_inv <<- inv
  ## get the value of the inverse of matrix
  getinv <- function() m_inv
  list(set = set, get = get, setinv = setinv,       getinv = getinv)
}


## Calculates the inverse of the special "matrix". If the inverse has already been calculated, retrieves the inverse from the cache and skips the computation.
## If inverse is NULL it calculates the inverse and stores in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #retrieve stored value of inverse of x (m_inv)
  m_inv <- x$getinv()
  
  #If m_inv is not NULL then it has been already calculated. Retrieve and return cached value. 
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  ## If m_inv is null then the inverse of x is calculated...
  data <- x$get()
  m_inv <- solve(data, ...)
  ## ...and cached
  x$setinv(m_inv)
  ##print the inverse
  m_inv
}
