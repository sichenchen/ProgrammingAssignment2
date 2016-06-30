##this function shows how we can use cache to get inverse

makeCacheMatrix <- function(x = matrix()) {
  ##set a matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##get the matrix
        get <- function() x
        setsolve <- function(solve) m <<- solve
        ##get the inverse
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
cacheSolve <- function(x, ...){
        m <- x$getsolve()
        if(!is.null(m)) {
          ##we can use the cache directly and skipthe computation
                message("getting cached data")
                return(m)
        }
        ##calculate the inverse of the data and sets the value of the inversein the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
