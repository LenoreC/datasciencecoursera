## These functions were created for the programming 
#assignment 2 of "r programming" course in coursera, this function was created
#to display my understanding of function caching the two functions will create a special matrix
#that can cache its inverse, while the second function computes the inverse of this special matrix
#The second function will return a message of "grabbing cached data -- brb!" and the cached inverse, if 
#this has already been calculated
## 

## makeCacheMatrix is the first function which creates the special "matrix" object as described above.

makeCacheMatrix <- function(x = matrix()) { 
  inversem <- NULL                         
  set <- function(y) {                      
    x <<- y                                
    inversem <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inversem <<- inverse 
  getInverse <- function() inversem
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve is the second function which computes the inverse of the above matrix "x"

cacheSolve <- function(x, ...) {
  inversem <- x$getInverse()
  if(!is.null(inversem)) {
    message("grabbing cached data -- brb!")
    return(inversem)
  }
  data <- x$get()
  inversem <- solve(data, ...)
  x$setInverse(inversem)
  inversem
}

