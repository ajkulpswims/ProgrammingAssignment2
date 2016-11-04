# The two functions below create a special object from a matrix and then either solves for the inverse of that matrix and caches it,
# or retrieves the cached inverse and reports it

# The makeCacheMatrix function takes in a square invertible matrix and uses it to create a special object that initializes the variable 
# that will store our inverse, sets the values of the matrix and inverse, allows the cacheSolve functions 
# to get the values of the matrix and inverse

makeCacheMatrix <- function(x){
  i <- NULL
  set <- function(y){
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

# The cacheSolve function checks to see if the inverse of the matrix has already been calculated. If the inverse has already
# been solved then the function retrieves it from the cache and reports it. If  the inverse hasn't been solved, the function
# solves for the inverse and caches the result.
          
cacheSolve <- function(x){
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
