## Put comments here that give an overall description of what your
## functions do

## A function to create and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(i) inverse <<- i
  
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


##A function to compute and retrieve inverse of a matrix if already exist

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
