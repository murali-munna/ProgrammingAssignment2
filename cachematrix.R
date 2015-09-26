#  The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix returns a list of attributes
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of the matrix. Checks if inverse is already calculated. If not, calcutaes it and sets the value in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
