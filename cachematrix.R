## function 1 creates the special list of the matrix that can set,get,setinverse & getinverse of the matrix x
## function 2 will check the cache x$getinverse() , is its not null, it will display it, if its null it will compute it and 
## gets the inverse of the matrix by calling the solve() function 


## creates a special "matrix", which is a list containing 4 functions
## Set , get , setinverse & getinverse 

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


# Computes the inverse of the Matrix we created by the above function. 
# If the inverse has been calculated and the matrix havent changed,
# then the cachesolve will retrieve the inverse from the cache and dislay the message "getting cached data"

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
