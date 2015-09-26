#  The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix returns a list of attributes
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    # Holds the cached value or NULL if nothing is cached
    inv <- NULL
    # store the matrix
    set <- function(y) {
        x <<- y
        # flush the cache
        inv <<- NULL
    }
    # returns the stored matrix
    get <- function() x
    
    # Cache the given inverse
    setinverse <- function(inverse) inv <<- inverse
    
    # Get the cached value
    getinverse <- function() inv
    
    # returns the list of following attributes
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. Checks if inverse is already calculated. If not, calcutaes it and sets the value in the cache.

cacheSolve <- function(x, ...) {
    
    # get the cached value
    inv <- x$getinverse()
    
    # return the cached value if exists
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    # calculate the inverse and store in cache
    inv <- solve(data)
    x$setinverse(inv)
    
    # return the inverse
    inv
}
