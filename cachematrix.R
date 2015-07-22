## Caches inverse of a matrix and returns inverse
## from cache if it exists, else compute inverse
## and store in cache

## Function to create functions to set and get
## matrix and its inverse to / from cache

makeCacheMatrix <- function(x = matrix()) {
    
        # initialize inverse to null
        inverse_matrix <- NULL
        
        # set matrix to incoming value and 
        # inverse to null in cache (different environment)
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        
        # get matrix from cache
        get <- function() x
        
        # set inverse in cache 
        setinverse <- function (inv_matrix) {
            inverse_matrix <<- inv_matrix
        }
        
        # get inverse from cache
        getinverse <- function () inverse_matrix
        
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
    }
    
    
    ## Solves the inverse of a matrix by looking up 
    ## cache and returns cache value if it exists
    ## else if computes inverse and adds to cache
    ## Return a matrix that is the inverse of 'x'

    cacheSolve <- function(x, ...) {
            
            # check in cache    
            inverse_matrix <- x$getinverse()
            
            # if cache is not empty, retrieve from cache
            if (!is.null(inverse_matrix)){
                message("getting from cache")
                return(inverse_matrix)
            }
            
            #if cache is emty, get matrix
            data <- x$get()
            
            # compute inverse
            inverse_matrix <- solve(data)
            
            # set matrix in cache
            x$setinverse(inverse_matrix)
            
            # return inverse 
            inverse_matrix
}