

# This function creates a special "matrix" object that can cache its inverse.
# check if the matrix was already seen by the function , hence the check if inverse already computed
# if inverse already computed return the inverse
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
    
	set <- function(y) 
		{
            x <<- y
            m <<- NULL
        }
            
	get <- function() 
		x
            
	setinverse <- function(solve) 
		m <<- solve
		
    getinverse <- function() 
		m
            
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )

}



# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then the
# `cachesolve` will retrieve the inverse from the cache.

# return the inverse of the matrix
# also cache the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	m <- x$getinverse()
		if(!is.null(m)) 
		{
            message("getting cached inverse")
            return(m)
        }
            
	data <- x$get()
    m <- solve(data, ...)
    
	x$setinverse(m)
    
	m		
}