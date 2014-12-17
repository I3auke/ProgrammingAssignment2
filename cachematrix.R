## makeCacheMatrix is a function that constructs 4 functions in a list:
## 'set' assigns the input matrix as a value to an object outside the current environment
## 'get' retreives the value of an object that was assigned earlier
## 'setinv' assigns the inverted matrix to an object outside the current environment
## 'getinv' retreives the value of the inverted matrix that was assigned earlier

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}

## cacheSolve checks if a value for the inverted matrix was already calculated and assigned
## if so, it gets the value from the assigned object and returns it
## if not, it calls the functions that are necessary to: get the matrix-object, 
## calculate the inverted matrix and assign the value of the inverted matrix.
## After that, it returns the inverted matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
