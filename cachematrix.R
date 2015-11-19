## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

# This function creates a special "matrix" object that can cache its inverse
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    #Initially set to NULL
    inv = NULL
    
    #Set function
    #sets the matrix but not the inverse
    set = function(y){
    x <<- y
    inv <<- NULL
    }
    
    #get function
    #gets the matrix but not the inverse
    get = function() x
    
    # Manually set the inverse
    setinverse = function(inverse) inv <<- inverse
    
    # Get the inverse
    getinverse = function() inv
    #put into list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinverse()
    
    # If it is not empty
    if(!is.null(inv)) {
        # Simply return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }
    
    # If it is empty
    # Get the matrix itself
    data = x$get()
    
    # Find the inverse
    inv = solve(data, ...)
    
    #Cache the result
    x$setinverse(inv)
    
    #return result
    inv
}

#x <- matrix(1:4, nrow=2, ncol=2)
#m <- makeCacheMatrix(x)
#s <- cacheSolve(m)
#print(s)

#s2 <- cacheSolve(m)
## This should display a "Getting cached matrix" message
#print(s2)

