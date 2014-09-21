## The following functions create a special object that stores a numeric 
##  matrix and caches its inverse


## makeCacheMatrix() creates a special "matrix", 
##  which is really a list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv

    list(set = set, get = get, setinv=setinv, getinv=getinv)

}


## cacheSolve() calculates the inverse of the special "matrix" created 
##  with the above function
## It first checks to see if the inverse has already been calculated. 
##  If so, it gets the inverse from the cache and skips the computation. 
##  Otherwise, it calculates the inverse of the data and sets the value 
##  of the inverse in the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv()
    if (!is.null(x_inv)) {
        message('getting cached data')
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setinv(x_inv)

    x_inv
}
