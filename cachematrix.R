## There are 2 functions in this script: one to create a special object that store a matrix and cache the inverse 
## and the other that calculate the inverse using the other function  

## Creation of special objects that contains list of functions that set/get the value of the matrix 
## and set/get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Verify if the inverted matrix is already in the cache, in this case return the value, otherwise make
## the inverted matrix using the function solve and set the inverted matrix in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        print("cache")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
