#makeCacheMatrix: creates "matrix" object that can cache inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the value of the inverse
        setinv <- function(solve) i <<- solve
        
        #get the value of the inverse
        getinv <- function() i
        
        #list results
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

#cacheSolve: computes inverse of "matrix". If inverse was already calculated (and not changed), then retrieve inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()

        #check if inverse is set and return if so
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #if inverse is not set, calculate and return inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}