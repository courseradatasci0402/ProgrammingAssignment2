## The makeCache Matrix and cacheSolve functions are used to create a special object that stores a matrix and caches its inverse. 


## This functions creates a special "matrix" object which can contain a matrix and cache the inverse of the stored matrix.

makeCacheMatrix <- function(x = matrix()) {
	
	##sets the cached variable to NULL for the intialised object
	inv <- NULL
	
	##calling the set function stores a matrix in the matrix object. The <<- assignment operator binds the object that y is pointing to the x variable in the parent environment(s)
	set <- function(y) {
		x <<- y
		inv <<- NULL
		
	}
	
	##the get function returns the stored matrix
	get <- function() x
	
	##caches the inverse matrix
	setinverse <- function(inververse) inv <<- inververse
	
	##returns the cached inverse matrix
	getinverse <- function() inv
	
	##return of main function. returns data as a list
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
	
}


## This function retrieves the inv variable from an object created using makeCacheMatrix and checks if the inv contains a cache matrix. If it does, the inverse matrix is returned else the inverse is calculated, cached using x$setinverse and returned.

cacheSolve <- function(x, ...) {
        
	##retrieves the inv var from an makeCacheMatrix object
	inv <- x$getinverse()
	
	##checks to see if the inv variable contains a cached inverse
	if(!is.null(inv)){
		##prints message to console
		message("getting cache data")
		
		##returns the cached inverse
		return(inv)
	}
	
	##if an inverse matrix is not cached, the non-inverse matrix is retrieved from a makeCacheMatrix object
	data <- x$get()
	
	##calculated in the matrix inverse
	inv <- solve(data, ...)
	
	##caches the inverse matrix in the makeCacheMatrix object
	x$setinverse(inv)
	
	##returns the inverse matrix
	inv
}
