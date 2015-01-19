## These functions are meant to work together to create a
## matrix inverse stored in cache for easy retrival.

## First function: creates a unique environment to house
## the variables (functions) for the second function.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    set_solve <- function(solve) s <<- solve
    get_solve <- function() s
    list(set = set, get = get, 
         set_solve = set_solve, 
         get_solve = get_solve)

}


## Second function: retrieves the inverse of matrix 'x' from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$get_solve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$set_solve(s)
    s
}
