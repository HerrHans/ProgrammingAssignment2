## The functions makeCacheMatrix and cacheSolve compute the inverse of an invertible matrix and store the
## result in the cache.

## The function makeCacheMatrix takes an invertible matrix as argument and returns a list of functions.
## If the matrix is changed, it sets the cached inverse to NULL.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                     # sets the cached inverse to NULL
                                              
  set <- function(y) {                            # defines a function changing the original matrix and setting the cached inverse to NULL
    x <<- y
    inv <<- NULL  
  }                         
  get <- function() x                             # defines a function returning the original matrix
  setinverse <- function(inverse) inv <<- inverse # defines a function caching the inverse of the original matrix
  getinverse <- function() inv                    # defines a function returning the inverse of the original matrix
  
  list(set = set, get = get,                      # returns a list containing the above of functions.
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The function cacheSolve computes the inverse of the original matrix passed to makeCacheMatrix. It takes
## the special matrix object (i.e. the list of functions returned by makeCacheMatrix) as argument. It then
## stores the result in the cache. If the special matrix object is changed by makeCacheMatrix, solveCache 
## will recompute the inverse. If not, it will print out the cached result.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                  # checks wehtether the inverse has been cached.
  if(!is.null(inv)) {                    # if so, the cached inverses is returned. 
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()                        # gets the original matrix,
  inv <- solve(data, ...)                # computes its inverse
  x$setinverse(inv)                      # and caches its inverse.
  
  inv                                    # returns the inverse                             
}


## EXAMPLE:
## >my_special_matrix_object <- makeCacheMatrix(matrix(c(1:4), ncol = 2, nrow = 2))
## > cacheSolve(my_special_matrix_object)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(my_special_matrix_object)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > 
