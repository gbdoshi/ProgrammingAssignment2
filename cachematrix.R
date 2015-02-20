## 'makeCacheMatrix' function creates a special vector 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Set inverse to NULL
        inv <- NULL
        ## Function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
    
        ## Function to get the matrix
        get <- function() x
    
        ## Function to set inverse
        setinverse <- function(inverse) inv <<- inverse
    
        ## Function to get the inverse
        getinverse <- function() inv
    
        ## form a list of all of these four function
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}

## 'cacheSolve' function calculates inverse of the matrix created in 'makeCacheMatrix' 
## It first checks if inverse has already been calculated, if so return the cached value and avoid computation
## if not, then calculate and cache the value and then return the inverse
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        ## Check if the inverse was already calculated and cached, if so return the cached value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## We are here since inverse wasn't calculated earlier and hence is null
        ## so calculate and set the value
        data <- x$get()
        inv <- solve(data, ...)
        
        ## cache the inverse value to avoid future calculation
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of 'x'
        inv
}
