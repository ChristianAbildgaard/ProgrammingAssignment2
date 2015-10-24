## The functions first creates a special matrix, the next
## function calculates the inverse of the original matrix if
## this hasn't already been done

## This function creates a special matrix which is really a 
## list containing a function to 1. set the value of the vector
## 2. get the value of the vector 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the original matrix,
## if this hasn't already been done, using the function solve

cacheSolve <- function(x, ...) {
        I <- x$getinverse()
        if (!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        O_Mat <- x$get()
        I <- solve(O_Mat, ...)
        x$setinverse(I)
        I
}
