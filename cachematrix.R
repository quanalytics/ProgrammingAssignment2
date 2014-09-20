## Put comments here that give an overall description of what your
## functions do

## 	Below are two functions that are used to create a special object 
## 	that stores a matrix and caches it, then calculates its inverse 
##	from the cached values. If the inverse is already calculated, the
##	second function gets the inverse from the cache and skips the
##	computation.

## Write a short comment describing this function

## 	makeCacheMatrix.R: This function creates a special "matrix" object, 
## 	such that its inverse can be created from the cached values.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
        set <- function(y) {
	        x <<- y
		s <<- NULL
	}
	get <- function() x

## m$setsolve: invokes the matrix "solve" function

	setsolve <- function(solve) s <<- solve

## m$getsolve: computes the inverse and caches it	

	getsolve <- function() s
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)


}


## Write a short comment describing this function

## 	cacheSolve.R: This function computes the inverse of the 
##	special "matrix" returned by makeCacheMatrix above. If the 
##	inverse has already been calculated (and the matrix has not 
##	changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

    s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)
	s
}

## 
## Example:
##
## > m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
## > a <- makeCacheMatrix(m)	# create cached matrix
## > a$get()			# show matrix m
##      [,1] [,2]
## [1,]    0    2
## [2,]    1    0
## > a$getsolve()		# invalid before cacheSolve
## NULL
## > cacheSolve(a)		# return inverted (uncached) matrix
##      [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
## > cacheSolve(a)		# return inverted (cached) matrix
## getting cached data
##      [,1] [,2]
## [1,]  0.0    1
## [2,]  0.5    0
