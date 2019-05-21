## These two functions create a special object that stores a matrix and caches its inverse

## This function creates a special "vector", which is a list containing four functions, which:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m<<- solve
        getinverse <- function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function calculates the inverse of the matrix, but it first checks to
## see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse
## of rhe matrix and sets the value of the inverse in the cache via the setinverse
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m<- solve(data,...)
        x$setinverse(m)
        m
}
