#####  
## Sample Code provided by class on caching
####


### ------------------------------------------------------------------------------#
#
# Returns an "object"-like creation (constructor of a list of functions and 2 private vars)
#  *  get()/set() - the vector contents
#  *  getmean()/setmean() - the mean value
#
### ------------------------------------------------------------------------------#

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


### ------------------------------------------------------------------------------#
# Accessor-like function for the vector
#  * Checks the vector for a cached copy of the mean
#      * If it exists, return the value and PRINT message that it's cached
#      * Else, calculate the mean, set the cache copy on the vector, and return it
#
### ------------------------------------------------------------------------------#

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
