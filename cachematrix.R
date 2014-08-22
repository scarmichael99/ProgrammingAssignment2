#################################################################
###  Cachematrix.R                                            ###
###  Computes the inverse of a matrix and caches the results  ###
#################################################################


## Creates an object that caches data about a matrix (including its inverse)

makeCacheMatrix <- function(x = matrix()) {
  
     # assigns NULL to the variable i locally
     i <- NULL
  
     # function that takes a matrix (y), caches its value in the calling environment
     # (as x), and resets i to NULL in the calling environment
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
  
     # function that returns the matrix value cached above (takes no arguments)
     get <- function() x
  
     # function that caches the value of a matrix's inverse
     setinverse <- function(inverse) i <<- inverse
  
     # function that returns the cached inverse (takes no arguments)
     getinverse <- function() i
  
     # combines the above functions into a single list object and returns that list
     list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes and returns the inverse of a matrix 
## after first checking for a cached solution

cacheSolve <- function(x, ...) {
  
    # checks cached value of matrix's inverse 
    i <- x$getinverse()
  
    # if cached value exists (i.e. isn't NULL), returns cached value 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
  
    # otherwise takes the matrix,
    data <- x$get()
    # computes its inverse,
    i <- solve(data, ...)
    # caches the inverse,
    x$setinverse(i)
    # and returns the inverse
    i
}
