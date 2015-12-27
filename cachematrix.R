## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly
## 

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
			m <- NULL
			
			#set the value of the matrix
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
			
			#get the value of the matrix			
            get <- function() x
			
			#set the inverse of the matrix
            setsolve <- function(solve) m <<- solve
			
			#get the inverse of the matrix			
            getsolve <- function() m
			
			#list of functions
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.		
cacheSolve <- function(x, ...) {
			## call the getsolve function       
			m<-x$getsolve			
			
			## if the cache is not null, return the cache
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
			
			## else, calculate the inverse....
            data <- x$get
            m <- solve(data, ...)
			
			## then put it in the cache
            x$setsolve(m)
            m
}
