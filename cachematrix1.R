## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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
x<-matrix(c(2,3,2,2),2,2)
solve(x)
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
makeVector(2)
a<-c(1,2,3)
1

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
cachemean(a)


crazy <- function() {
    x <<- 3.14                   # variable x in the containing environment (global in this case) is updated to be 3.14
    print(x)                        # since no local variable 'x' exists within function 'crazy' R searches the containing environments
{ print(x);                     # this is to demonstrate the function, not a code block, is the smallest environment in R
  x <- 42; print(x)         # local variable 'x' is declared (created) and assigned the value 42; overrides the variable 'x' in
    }                                  # the containing environment
print(x)                       # since local variable 'x' now exists within the function there is no need to search the containing
}                                   # environment (global in this case)

foo<-function(){
    a<-10
    return(a)
}

###https://class.coursera.org/rprog-016/forum/thread?thread_id=140

a
