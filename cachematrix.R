

# This function creates a special "matrix" object that can cache its inverse.
# check if the matrix was already seen by the function , hence the check if inverse already computed
# if inverse already computed return the inverse
makeCacheMatrix <- function(x = matrix()) {
	
	#create matrix to store inverse , initialize to <0 x 0 matrix>	
	inverse <- matrix(0,0,0)
    	
	set <- function(y) 
		{
			#new assignment , save matrix and initialise inverse
            x <<- y
            inverse <<- matrix(0,0,0)
        }
            
	get <- function()
		{
			#return matrix
			x
        }    
	setinverse <- function(solve)
		{
			#find inverse
			inverse <<- solve
		}
		
    getinverse <- function()
		{
			#return inverse
			inverse
        }	
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )

}



# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), then the
# `cachesolve` will retrieve the inverse from the cache.

# return the inverse of the matrix
# also cache the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	#get the inverse 
	inverse <- x$getinverse()	
	
	#check if inverse has been calculated and cached
		if( ! (nrow(inverse)==0 & ncol(inverse)==0) )
		{
            #yes it has been calculated and cached, return the inverse
			message("returning cached inverse")
            return(inverse)
        }
    
	#As inverse has not been calculated
	#Get the matrix , so that inverse can be calculated
	data <- x$get()
	
	#calculate the inverse
    inverse <- solve(data, ...)
    
	#set the inverse
	x$setinverse(inverse)
    
	#And return inverse
	inverse		
}