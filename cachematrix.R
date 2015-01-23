## Programming Assignment 2 : Caching the Inverse of a Matrix
## Compute the inverse of the matrix.
## If the contents of the matrix are not changing, it may make
## sense to cache the inverse matrix so that when we need it again, it
## can be looked up in the cache rather than recomputed. In this
## Programming Assignment using the scoping rules of the R language and 
## how they can be manipulated to preserve state inside of an R object.

## This function creates a special "vector", which is
## really a list containing a function to
## 1.  set the value of the vector
## 2.  get the value of the vector
## 3.  set the value of the inverse
## 4.  get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) m <<- inverse
	getInverse <- function() m
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

## This function calculates the inverse matrix of the special "vector"
## created with the above function 'makeCacheMatrix'. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the `setInverse` function.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m		
}
