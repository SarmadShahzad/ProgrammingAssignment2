# Computing the inverse of a matrix is usually very expensive.
# To avoid computing the inverse repeatedly we cache its inverse 
# to be used later on. The following functions can be used to cache the inverse:

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  set <- function (y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function (solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. If the inverse 
# has already been computed, it gets the inverse without further compution.
# If its not computed, it computes it and saves the value in the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
