## Put comments here that give an overall description of what your
## functions do

# example call:
# cm <- makeCacheMatrix(m)
# cacheSolve(cm)

##
# the set function set's the original matrix and clears the inverse
#   every time set() is called, the cache gets cleared
#
# the original matrix and is returned in the get() function
#
# the setinverse() function set's the inverse
#
# the inverse is returned in the getinverse() function
#
# returns a list containing the original and a cache to store the inverse
#
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL

        # set the original matrix and clear the cache
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        # get the original matrix
        get <- function() x

        # get the inverse
        setinverse <- function(inverse) i <<- inverse

        # set the inverse
        getinverse <- function() i

        # return a list containing all helper getters and setters
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##
# if the inverse was already cached, it returns the cached value
# else calculate the inverse and store it in the cache
# 
# x must be the type of the return value of the makeCacheMatrix function
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        # retrieve the cache
        i <- x$getinverse()

        # check if the cache was already filled
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        # get the original matrix
        data <- x$get()

        # calculate the inverse
        i <- solve(data)

        # store the inverse in the cache
        x$setinverse(i)

        # return the inverse
        i
}
