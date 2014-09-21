## makeCacheMatrix creates a special matrix that allows for storing values, in this case, storing its inverse. 
## It requires cacheSolve to be called to calculate the inverse and store it in a matrix created by makeCacheMatrix. 
## 

## makeCacheMatrix, similar to makeVector, creates a matrix with some extra information, which allows  
## cacheSolve to work. It creates variables and functions that cacheSolve can call on and manipulate so that it can 
## store answers. I don't understand, however, how set works, or why the '<<-' needs to be invoked. My best guess 
## is that it is useful if other functions than cacheSolve would need to call on the stored values. 

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                  x <<- y
                  inv <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## cacheSolve solves the inverse of the invertible matrix it is provided, and stores the answer in the special matrix
## that it is given. It needs to be given a matrix that has already been run through makeCacheMatrix, which is 
## something like a matrix with metadata. The first time cacheSolve is solved for a given CacheMatrix, it stores the
## solution, so if it is called on again, it grabs the stored value rather than recalculating it. 
## The functions getinverse, get, and setinverse are all defined within makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         
                  inv <- x$getinverse()
                  if(!is.null(inv)) {
                           message("getting cached data")
                           return(inv)
                  }
                  data <- x$get()
                  inv <- solve(data, ...)
                  x$setinverse(inv)
                  inv
         }         
         
         
         

