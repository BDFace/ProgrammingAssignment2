#returns a list of functions to set and get matrix and set and get matrix inverse
makeCacheMatrix <- function(x = matrix()) {
	
		# m to store the inverse
        m <- NULL
        
        # get and set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        # get and set the inverse
        setmatrix <- function(solve) m <<- solve
        
        getmatrix <- function() m
        
        # return the matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


# compute the inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()

		# find the inverse in the cache and if it's there return it
        if(!is.null(m)) {
                message("getting cached imatrix")
                return(m)
        }
        
        # if no inverse present calculate it
        matrix <- x$get()
        m <- solve(matrix, ...)
        
        # and cache it
        x$setmatrix(m)
        
        # and return it
        m
}