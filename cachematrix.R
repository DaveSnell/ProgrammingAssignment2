## These functions allow the cache of a matrix inverse
## and the subsequent retrieval of that inverse without the 
## overhead of recalculation of the inverse.
## NOTE: the input matrix is assumed to be non-singular
##       i.e., it has an inverse

## The makeCacheMatrix function caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  # caches matrix and inverse
  inv <- NULL # initialize local object to null
  set <- function(y) {
    x <<- y # replace x, inv in calling environment
    inv <<- NULL
  }
  # the following three functions do not need braces ({}) because they are each one statement long
  get = function() x
  setinverse = function(inverse) inv <<- inverse # set inv of calling environment to function argument
  getinverse = function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse) # form list of get and set attributes
}


## The cacheSolve function calls makeCacheMatrix and returns the inverse of an input matrix x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # returns inverse of matrix (if not singular)
  # patterned after makeVector and cachemean
  # TODO: add check for singularity
  # sample usage:
  #   m <- matrix(c(1,8,3,4,5,6,7,8,9),3,3) # 3 by 3 matrix
  #   m_Inverse <- cacheSolve(makeCacheMatrix(m))
  #   m %*% m_Inverse # this should show identity matrix (within precision allowances)
  
  inv = x$getinverse()
  if (!is.null(inv)){ # if inv already exists, return it without any further processing
    message("getting cached data")
    return(inv)
  } else { # calculate the matrix inverse
    data = x$get()
    inv = solve(data, ...)
    x$setinverse(inv)
    return(inv)
  }        
}
