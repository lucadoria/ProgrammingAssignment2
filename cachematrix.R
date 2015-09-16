#The code calculates the inverse of a matrix and caches the
# result for future use if the inversion was already done.

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

# The following function returns the inverse of the matrix. 
# First, it checks if the matrix was inverted already. 
# If so, it does not invert.
# Otherwise, it inverts and caches the value.

cacheSolve <- function(x, ...) {
    Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data.")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data)
    x$setinverse(Inv)
    return(Inv)
}

# Test
A = rbind(c(1, 2), c(-5, 1))
B = makeCacheMatrix(A)
B$get()

# No cache in the first run
# cacheSolve(B)

# Get the matrix from the cache
# cacheSolve(B)
