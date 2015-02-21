## These two functions can cache the inverse of a matrix if it has already been calculated.
## If the inverse has not been calculated, it will calculate the inverse and store it.


## This function can set and get the value of the matrix, and set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
			i <- NULL
			set <- function(y) {
				x <<- y
				i <<- NULL
			}
			get <- function() x
			setinverse <- function(inverse) i <<- inverse
			getinverse <- function() i
			list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function first checks if the inverse has been calculated, if so, it gets the value. 
## If not, it caluculates the inverse and store it to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       		i <- x$getinverse()
			if(!is.null(i)) {
				message("getting cached inverse matrix")
				return(i)
			}
			matr <- x$get()
			i <- solve(matr)
			x$setinverse(i)
			i
}
