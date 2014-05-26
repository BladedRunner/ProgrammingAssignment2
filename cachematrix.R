### makeCacheMatrix allows you to make a matrix that is a list containing functions allowing you to set the value of
## the matrix, get the value of the matrix, set the value of the inverse and get the value of the inverse


makeCacheMatrix <- function(x = matrix()) 
{
  
    
  i <- NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates the mean of the matrix made with makeCacheMatrix. It  checks whether an inverse has been calculated. 
## If calculated, it gets the inverse from the cache and skips computation. If not, it calculates the inverse and the data and sets 
## the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) 
{
  
  i <- x$getinverse()
  
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
