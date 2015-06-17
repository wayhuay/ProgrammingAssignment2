

# MakeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
# This function assumes that the matrix is always invertible.


makeCacheMatrix <- function(x = matrix()) {
        # store the cached value
        # initialize to NULL
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}





# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It first checks to see if the inverse has already been calculated. 
# If so (and the matrix has not changed), cachesolve should retrieve the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data)
        
        x$setinverse(inv)
        
        inv
}


# Test Scenario 1
# > x = rbind(c(1, 3), c(2, 4))
# > m = makeCacheMatrix(x)
# > m$get()

# Results of Test Scenario 1
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4


# Test Scenario 2
# > cacheSolve(m)

# Results of Test Scenario 2
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



