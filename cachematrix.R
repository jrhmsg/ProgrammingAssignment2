### ------------------------------------------------------------------------------#
#   Programming Assignment #2 - Caching the Inverse of a Matrix
#   
#   Goal: to create a pair of functions to cache expensive calculations (matrix inverse)
#      * List <- makeCacheMatrix(Matrix) - stores the matrix and the result of its inverse
#      * Matrix <- cacheSolver(List) - returns the inverse of the wrapped matrix / caching the result
#
### ------------------------------------------------------------------------------#



### ------------------------------------------------------------------------------#
#  makeCacheMatrix(Matrix)  -- wraps a matrix as a list/ capable of caching its inverse
#
#  Returns an "object"-like creation (constructor of a list of functions and 2 private vars)
#  *  get()/ set() - the matrix contents
#  *  get_inverse()/ set_inverse() - the inverse matrix
#
### ------------------------------------------------------------------------------#

makeCacheMatrix <- function(x = matrix()) {

 	inv_matrix <- NULL

	### Function to set the matrix (and clear the inverse)
    	set <- function(y) {
            	x <<- y
            	inv_matrix <<- NULL
    	}

	### Return the matrix
    	get <- function() x

	### Set the inverse  (Note: the inverse is NOT calculated but simply assigned
    	set_inverse <- function(inverse)  inv_matrix <<- inverse

	### Get the inverse  (Note: the invers is NOT calculated but simply returned
    	get_inverse <- function()  inv_matrix

	### Return the "wrapped" matrix
    	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


### ------------------------------------------------------------------------------#
#  cacheSolver() -- returns inverse of the "wrapped" matrix, caching the result in the object
#
#  Accessor-like function for the matrix
#  * Checks the matrix for a cached copy of the inverse
#      * If it exists, return the value and PRINT message that it's cached
#      * Else, calculate the inverse, set the cache copy on the matrix, and return it
#
### ------------------------------------------------------------------------------#

cacheSolve <- function(x, ...) {

	### Check to see if the inverse has been stored
	inverse <- x$get_inverse()

	### If it has, return it (and print a diagnostic message)
	if(!is.null(inverse)) {
        	message("getting cached data")
        	return(inverse)
	}


	### Else, get the matrix, find its inverse, store it back into the matrix list and return the inverseit
	data <- x$get()
	inverse <- solve(data, ...)
	x$set_inverse(inverse)
	inverse
}


###  Simple test function to demonstrate these two functions

my_test_matrix <- function() {
	
	print("Created a wrapped matrix - Identity Matrix")
	m1 <- matrix(c(1,0,0,0,1,0,0,0,1), 3, 3)
	cm <- makeCacheMatrix(m1)
	print(cm$get())
	
	print("Finding the inverse")
	print(cacheSolve(cm))
	
	print("Finding the inverse again")
	print(cacheSolve(cm))
	
	writeLines("\n\n")
	print("Created a wrapped matrix - Interesting Matrix")
	m2 <- matrix(c(1,1,1,3,4,3,3,3,4), 3, 3)
	cm <- makeCacheMatrix(m2)
	print(cm$get())
	
	print("Finding the inverse")
	print(cacheSolve(cm))
	
	print("Finding the inverse again")
	print(cacheSolve(cm))
}
