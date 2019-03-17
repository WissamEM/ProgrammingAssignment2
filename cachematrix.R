## makeCacheMatrix() creates an R object that stores a matrix and its inverse
# it returns a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
        Inv <-NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve() requires an argument that is returned by makeCacheMatrix() 
# in order to retrieve the inverse from the cached value that is 
# stored in the mmakeCacheMatrix() object's environment.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
}