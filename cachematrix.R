# **************************************
# Two functions.  One returns a list object with four elements, each a function.
# The second references the functions in the object created by the first.  A
# variable, "m", is superset, and if that variable (the inverse of a matrix) is
# already calculated and stored, the calculation is not repeated.
# **************************************


# **************************************
# Name:		makeCacheMatrix
# Purpose:	Returns a list object w/ 4 elements.
#		... These elements can be referenced by another function,
#		... and a calculation avoided if it's already been performed
# Author:	mac
# Date:		11/21/2014
# **************************************

makeCacheMatrix <- function(x = matrix()) { 	# Function passed a matrix, and will 
						# ... return a list
        m <- NULL                           	# “m” initialized to NULL upon 
						# ... calling makeCacheMatrix
        set <- function(y) {			# “set”, the first item in the 
						# ... list returned, will be a 
						# ... function that can be called 
						# ... externally
                x <<- y                     	# x is super-set to y if the 
						# ... “set” function is called
                m <<- NULL                      # m is super-set to NULL if the 
						# ... “set” function is called
        }
        get <- function() x			# “get” is the second item in the
						# ... list returned, and returns 
						# ... x w/o any calculations
        setinv <- function(inv) {m <<- inv} 	# “setinv”, the third list item, 
						# ... is a function that is 
						# ... passed a variable “inv”, 
						# ... and super-sets “m” to that
        getinv <- function() m              	# “getinv” is the fourth list 
						# ... item, another function that 
						# ... just returns the variable 
						# ... “m”
        list(set = set,                     # here’s what the whole function returns, the list of four elements
             get = get,           
             setinv = setinv,
             getinv = getinv)
}
 

# **************************************
# Name:		cacheSolve
# Purpose:	References the object created by "makeCacheMatrix",
#		... determines if the inverse has been calculated,
#		... and doesn't perform the calc unless necessary
# Author:	mac
# Date:		11/21/2014
# **************************************

 cacheSolve <- function(x, ...) {	# cacheSolve will work with the list already created by makeCacheMatrix
        m <- x$getinv()			# the “getinv” function is called, and the result is assigned to “m”
        if(!is.null(m)) {		# if that value is not null, then it will be returned.  No further calculation
                message("getting cached data")	# this message will let you know that yes, there was a cached variable, and here it is.
                return(m)		# that cached value is returned, and exit function 
        }
        data <- x$get()			# now, if “m” was null, then more is required.  The “get” function is called, which returns the original matrix.
        m <- solve(data, ...)		# “m” is assigned the inverse of the matrix
        x$setinv(m)			# the “setinv” function is called, and “m” is super-set to the inverse of the original matrix
        m				# here m (the inverse of the matrix) is returned, exit function
}