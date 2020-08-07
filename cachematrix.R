# This is a pair of functions that cache the inverse of a regular matrix

# This function creates a special matrix that can cache its inverse

makecachematrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
        x <<- y 
        inv <<- NULL
    }
    get <- function() {x}
    setInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# The next function give us the inverse of the matrix created by the last funtion,
# when the inverse has already been calculated, and the matrix it is the same, 
# then it should retrieve the inverse from the cache.

cachesolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting catched data ")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
