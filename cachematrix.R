## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) { ## defining the argument with default mode of "matrix"
        invmax <- NULL                             ## initialize invmax as NULL for it to hold value of matrix inverse 
        set <- function(y) {                    ## define the set function to assign new 
          x <<- y                               ## value of matrix in parent environment
          invmax <<- NULL                          ## if there is a new matrix, reset invmax to NULL
        }
        get <- function() x   ## returns value of the matrix argument
        
        setinverse <- function(inverse) invmax <<- inverse  ## assigns value of invmax in parent environment
        getinverse <- function() invmax                     ## gets the value of invmax where called
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)  ## you need this in order to refer 
        ## to the functions with the $ operator
    
  }    
  
  ## Write a short comment describing this function
  ## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above and sets the inverse in the cache via the setinverse.
  ## It checks to see if the inverse has already been calculated (and the matrix has not changed),
  ## If it has been calculated it gets the inverse from the cache and skip the computation.
  ## cacheSolve will retrieve the inverse from the cache
  cacheSolve <- function(x, ...) {
              ## Return a matrix that is the inverse of 'x'
        invmax <- x$getinverse()
        if(!is.null(invmax)) {
          message("getting cached data")
          return(invmax)
        }
        data <- x$get()
        invmax <- solve(data, ...)
        x$setinverse(invmax)
        invmax
 
  
}
