## The makeCacheMatrix function define a list of functions to set on the parent
# environment to provide the function cacheSolve with objetct needed  

## The cacheSolve function takes the object from the makeCacheMatrix, cheks on the 
# cache memory to see if the calculation is allready done and assigned to an 
# object (m), and if is not the case, proceed to do the calculation and return the
# result.

## makeCacheMatrix
# Set a list of functions and objects to the parent environment. 
# functions, set, get, setinverse, getinverse
# objects, x (a matrix), and m a null object.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## cacheSolve
# cheks if m in the provided object x is different than NULL, and return a value
# if its exist. otherwise store the matrix x as data, and apply the solve function
# to obtain the inverse, then set it on the list, as setinverse, and return the value.
# For the next time, a inverse data is set as m, and when the function getinverse is 
# applied, that inverse matrix is returned, from the cache memory.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
