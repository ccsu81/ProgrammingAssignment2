## This function take in as argument a matrix and return a list in which
## the objects are functions and cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
## 1. Set the value of the Matrix
	set <- function(y){
			x <<- y
			m <<- NULL
	}
## 2. Get the value of the vector
	get <- function() x
## 3. Set the value of the Inverse
	setinverse <- function(solve) m <<- solve
## 4. Get the value of the Inverse
	getinverse <- function() m
## Finally, the function return a list of objects which are fuctions.
	list(set = set, get = get, setinverse = setinverse, 
		getinverse = getinverse)
}

## This function computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix' above. If the inverse has already
## been calculated (and the matrix has not changed), then 'cacheSolve'
## should retrieve the inverse from the cache

## This function retrives the inverse from x first. If inverse exist, 
## then the function prints out the inverse. If not, then matrix is
## stored in 'data' and the inverse is calculated

cacheSolve <- function(x, ...) {
## Retrives 'm' from the global environment 
## ('m' is stored globally by the function "makeCacheMatrix" with the operator '<<-')
	m <- x$getinverse()
## If the inverse already exist, then this function ends and prints the inverse
	if(!is.null(m)){
			message("getting cached data")
			return(m)
	}
## If the inverse does not exist, the matrix is stored in 'data'
	data <- x$get()
## and using 'solve', the inverse is stored in 'm'
	m <- solve(data)
## using function setinverse, 
	x$setinverse(m)
## Return a matrix that is the inverse of 'x'
	m
}