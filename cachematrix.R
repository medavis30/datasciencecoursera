# These two functions will create a storage place for a square matrix
# to cache its inverse to be used to call the cached data without
# computing the inverse repeatedly once already calculated.

# The first function, makeCacheMatrix creates a special matrix object
# that can cache its inverse. This function will set the value of the
# matrix, get the value of the matrix, then set the value of the inverse
# and get the value of the inverse. The solve function will be used to
# calculate the inverse for the matrix and cache it.

makeCacheMatrix <- function(x = matrix()) {
    
    # Creating an empty storage place to store the cached inverse
    w <- NULL
    
    # Storing values into a matrix and emptying out any old values of
    # the cached storage place
    set <- function(y) {
        x <<- y
        w <<- NULL
    }
    
    # Returns the matrix
    get <- function(){
        x
    }
    
    # Calculates the inverse of the set matrix and stores it in the
    # cached storage place
    setinverse <- function(solve){
        w <<- solve
    }
    
    # Returns the inverse matrix
    getinverse <- function(){
        w
    }
    
    # Returns a list with each function called in makeCacheMatrix function
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of the special matrix objected
# created with the makeCacheMatrix function. It will call getinverse
# to see if the inverse has already been calculated, and will grab the
# inverse from the cache if it was already calculated. If the inverse
# hasn't been calculated, or the matrix has changed, then the following
# function will calculate the inverse and set the value of its inverse
# in the cache.

cacheSolve <- function(x, ...) {
   
    # Calls the the cached value from makeCacheMatrix
    w <- x$getinverse()
    
    # Checks to see if there is already stored inverse matrix, outputs
    # a message stating what function is doing, and returns the matrix
    if(!is.null(w)) {
        message("getting cached data")
        return(w)
    }
    
    # If matrix has changed, or inverse hasn't been calculated, grab the
    # matrix
    data <- x$get()
    
    # Calculates and stores its inverse
    w <- solve(data, ...)
    
    # Calls the stored inverse of the matrix
    x$setinverse(w)
    
    # Returns the inverse matrix
    w
}