## The purpose of these functions is to create a matrix and calculate its inverse
## (therefor the matrix must be square and not singular)
## besides the functions try to get the value of the inverse from the cache, when
## available in order to save time in unneeded calculations.


## This function creates a matrix and stores it in the cache
## every time this function is called the inverse is set to NULL
## because the values of the matrix might have been changed
## and therefor the inverse too.
## The functions has subfunctions which set and get the matrix
## and the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)

}


## This function looks for the value of the inverse in the cache
## if there's no value (NULL) then it calculates the inverse,
## stores it in the cache and returns it.
cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data_matrix <- x$get()
    if (!det(data_matrix)){
        print("Error: singular matrix")
        return()
    }
    inv <- solve(data_matrix, ...)
    x$setinverse(inv)
    inv
    
}
