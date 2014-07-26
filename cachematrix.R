## The functions below allows a user to create a matrix and store its cached inverse, by using the makeCacheMatrix function
##setting and changing the cache matrix can be done through the cacheSolve function

#the mackeCacheMatrix function creates a special vector with which we aim to 
#store and calculate a matrix, we will cache the matrix when created this is
#to help with speed
makeCacheMatrix <- function(x = numeric()) {
	#initially there is no caches version and as such the cache is null
	cache <- NULL

	#sets the matrix which we will use i.e. x
      setCacheMatrix <- function(values) {
		x <<- values
		#due to the setting of a new matrix we will reset the cache to null
		cache  <<- NULL
	}

	#returns a cache matrix
	getCacheMatrix <- function(){
		x
	}

	#caches the inversion of the matrix
	setCacheInverse <- function(inverse){
		cache   <<- inverse
	}

	#gets the cached inverse
	getCacheInverse <- function(){
		cache
	}

	#list out the information
	list(setCacheMatrix = setCacheMatrix, 
		getCacheMatrix = getCacheMatrix,
		setCacheInverse = setCacheInverse ,
		getCacheInverse = getCacheInverse )
}

#the cacheSolve function tries to solve and store the special matrix cache 
#stated above
cacheSolve <- function(cacheInfo, ...) {
	#gets the cached inverse
	inverse <- cacheInfo$getCacheInverse()
	
	#if the cache inverse is null, then it has not been cached yet
	#if it is not null then we can use the cached version
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	#get the cached matrix
	data <- cacheInfo$getCacheMatrix()

	#using the solve function to find the inverse
	inverse <- solve(data)

	#sets the new cache inverse
	cacheInfo$setCacheInverse(inverse)

	#return the inverse
	inverse
}