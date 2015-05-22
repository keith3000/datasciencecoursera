## makeCacheMatrix and cacheSolve - function description
## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve is a function that computes the inverse of the special matrix returnd by makeCacheMatrix 
##      if the inverse has already been calculated and the matrix has not changed, then the
##      cacheSolve will retrieve the inverse from the cache 

## MakeCacheMatrix creates a special matrix that can cache its inverse for later use
## takes matrix name, solves for matrix inverse and stores it in higher environment
## MakeCacheMatrix creates a list containing functions to 
## 1. set the value of a matrix, 2. get the value of a matrix, 
## 3. set the value of the inverse, 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        ## make cache to store inverse of matrix        
        m <- NULL
        set <- function(y) {
                x <<- y
                M <<- NULL
        }
        get <- function() x
        ## make list of functions to set and retrieve matrix inverse         
                setinverse <- function(solve) m <<- inverse
                getinverse <- function () m
                list(set = set, get = get, 
                        setinverse = setinverse,
                        getinverse = getinverse)
}


## cacheSolve is a function that calculates the inverse of the special matrix created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and 
## sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(lis.null(m)) {
                message(" getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
