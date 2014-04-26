## Below R function is able to cache potentially time-consuming computations.
## If the contents of a matrix are not changing, then cache the value of so that 
## when we need it again, it can be looked up in the cache rather than recomputed.

## The makeCacheMatrix function inturn has five functions(i.e., closure-functions written by 
## functions), which are used to set and display the value of matrix, compute and 
## display the mean of matrix and to get the inverse of matrix passed.

makeCacheMatrix <- function(x = matrix()) 
{
		cachedMatrix<-NULL
		i=0

		# First Function "set()"
      	# "<<-" is the same as "<-" except it 
      	# looks a level up to the parent environment, 
      	# in this case makeCacheMatrix(). Since "cachedMatrix" is 
      	# defined within makeCacheMatrix() NOT in set()
		# so it must look outside of itself.

		set<-function(y)
		{
			x <<- y
			cachedMatrix <<- NULL
		}

		# Second Function "get()"
		get<-function()
		{
			x
		}

		# Third Function "setmean()"
		setmean <- function(mean) 
		{
      		cachedMatrix <<- mean
      	}

		# Fourth Function "getmean()"
		getmean <- function() 
		{
      		cachedMatrix
      	}
		
		# Fifth Function "getInverse()"
		getInverse <- function ()
		{	
			if(i==0)
			{
				print(cachedMatrix) #if calling for the first time
				i<<-i+1
			}
			else
			solve(x) #compute the inverse of the matrix
		}

		# Return
		list(set = set, get = get,
      	setmean = setmean,
      	getmean = getmean, getInverse = getInverse)
}


## CacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve will retrieve the inverse from the cache.
	
cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
	 m <- x$getmean()
	 if(!is.null(m)) 
	 {
       	message("getting cached data")## If the same matrix is passed again
            return(m)
       }
       data <- x$get()
       m <- solve(data, ...) ##used to compute the inverse of matrix
       x$setmean(m)
       m
}
