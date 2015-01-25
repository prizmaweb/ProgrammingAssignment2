##  The make cache matrix is a function that creates   a wrapper around an r  matrix variable.
##  It caches the inverse value within the scope of this function, and methods to get and set the inverse are provided

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	prevdata <- x

        set <- function(y) {
                x <<- y
                prevdata <<- y
                inv <<- NULL
        }
        
        get <- function() x
        getprev <- function() prevdata
        setinverse <- function(inverse) inv<<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse, 
             getprev= getprev)

}


## This function checks if an inverse value was calculated . It calls the getinverse function to get the inv variable. If this is not null, it returns 
## that value. If the inverse variable is null, it implies the inverse was never calculated and thus calls 
## the solve function to calculate the inverse and store it in inv using the setinverse method. It also returns the inverse

cacheSolve <- function(x, ...) {

        ## get the  current matrix  and the original value that was stored
        data <- x$get()
	prevdata <- x$getprev()	

	## check first that the matrix data was not altered before returning the cached inverse
	## all elements of original  and current matrix match

	if(all(data == prevdata))  {
		## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
			if(!is.null(inv)) {
				message("getting cached data")
				return(inv)
			}
	}
	## data modified- so this sets the current cached inverse to null
	else
		x$set(data)

	## now calculate inverse 
	inv<- solve(data)
	x$setinverse(inv)
        inv
}
