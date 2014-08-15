## These two functions solve for and cache the inverse of a matrix.

## This first function creates a list of functions that can 1) set the matrix, 2) retreive the matrix, 
## 3) set the inverse of the, matrix and 4) retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function takes the matrix stored in the list from makeCahceMatrix and returns its inverse.
## If the inverse matrix has already been solved for, the function returns it via the getinverse function.
## If the inverse has not been solved for, the function 1) solves for it, 2) chaches it in the 
## setinverse function 3) and returns it.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}