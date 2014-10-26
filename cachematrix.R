
# MakeCacheMatrix is a constructor function that allow to store a matrix and cache its inverse 
# the returning value is a list

makeCacheMatrix <- function(x = matrix()) {
		im <- NULL
		
		# setting the value of the matrix
		set <- function(y) {
		x <<- y 
		im <<- NULL
		}
		
		# defining get function that get the value of the matrix
		get <- function() x 
		
		# defining setinverse function that set the value of the inverse matrix
		setinverse <- function(solve) im <<- solve
		
		# defining getinverse function that get the value of the inverse matrix
		getinverse <- function() im
		
		# returning the list of functions
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

# cacheSolve takes as input the object created by MakeCacheMatrix and
# returns the inverse of the matrix.
# If the inverse matrix is already present in the cache it does not perform
# the inverse of the matrix and retrieve the value from cache.
# If the inverse matrix is not in the cache it computes it and store in cache,
# then it returns the inverse matrix value to console.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		# Retrieve the value of the matrix from cache
		im <- x$getinverse() 
		
		if(!is.null(im)) { # if the matrix is already computed do not perform computation and get the cached one
			message("getting cached data")
			return(im) # return the value of inverse matrix already computed 
		}
		# get the value of the raw matrix and store it to data variable
		data <- x$get() 
		
		# perform inverse matrix computation through solve command
		im <- solve(data, ...) 
		
		# Store the value of im in the cache
		x$setinverse(im) 
		
		# return the value of the inverse matrix
		im 
}
