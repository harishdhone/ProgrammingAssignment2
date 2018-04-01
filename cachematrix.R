
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatirx <- function(x = matrix()){
  i <- matrix() #setting the default value of the inverse matrix to null
  set <- function(y){ #this function resets the value of the matrix
    x <<- y
    i <<- matrix()
  }
  get <- function() x #this functions returns the input matrix
  setinverse <- function(inverse) i <<- inverse #this function sets the inverse matrix i to the inverse of the input
  getinverse <- function() i #this function returns inverse matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x,...){
  i <- x$getinverse()
  if(!any(is.na(i))){   #this condition returns the cached matrix if the inverse has already been calculated for the input
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
