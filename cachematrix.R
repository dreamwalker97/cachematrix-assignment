makeCacheMatrix <- function(x = matrix()){
    invrs <- NULL
    set <- function(y){
        x <<- y 
        invrs <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) invrs <<- solve         
    getsolve <- function() invrs
    list( set = set,
          get = get ,
          setsolve = setsolve ,
          getsolve = getsolve
    )
    
}
cacheSolve <- function(x,...){
    invrs <- x$getsolve()
    if (!is.null(invrs)){
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data,...)
    x$setsolve(invrs)
    invrs
}