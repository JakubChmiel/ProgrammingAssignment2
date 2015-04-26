## The first function, makeCacheMatrix creates list containing a function to set the value of matrix, get the value of matrix, set the value of conversed matrix and get the value of inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
    list(set = set, get = get,
        setInvMatrix = setInvMatrix,
        getInvMatrix = getInvMatrix)
}


## The second function, cacheSolve calculates the inverse matrix of the special list created with the above function. However, it first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix in the cache via the setInvMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m
}