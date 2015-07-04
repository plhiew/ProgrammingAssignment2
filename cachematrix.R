## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix() expects an matrix as input and creates a special "vector" containing a function to:
#         - set the value of the matrix
#         - get the value of the matrix
#         - set the value of the inverse matrix
#         - get the value of the inverse matrix
# these functions make use of the concept of lexical scoping rule to cache the result of a
# lengthy and complex calculation of the inverse of the input matrix as well as the input matrix itself

makeCacheMatrix <- function(x = matrix()) {
    Inv_mat <- NULL                                 # Cache for inverse matrix is set to NULL
    set <- function(y) {                            # set function updates x with the new matrix for inversion and set cache of inverse matrix to NULL
        x <<- y
        Inv_mat <<- NULL
    }
    get <- function() x                             # get function returns the matrix for inversion
    set_inverse <- function(mat) Inv_mat <<- mat    # set_inverse sets the cache for inverse matrix to a new value
    get_inverse <- function() Inv_mat               # get_inverse returns the cached value of the inverse matrix 
    list(set_matrix = set, get_matrix = get, set_inverse_matrix = set_inverse, get_inverse_matrix = get_inverse)
}



## Write a short comment describing this function

# cacheSolve() expects a vector object produced by the makeCacheMatrix() function 
# It first checks to see if the inverse matrix has already been calculated. If so, it gets the inverse matrix 
# from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the matrix stored in the vector object
# by calling the solve() function (that expects a square invertible matrix) and sets the value of the inverse matrix in the cache 
# via the set_inverse_matrix() function.

cacheSolve <- function(x, ...) {
    Imat <- x$get_inverse_matrix()             # gets the cache value of the inverse matrix within the vector object argument

    if(!is.null(Imat)) {                       # If the cache is not NULL, the inverse matrix has already been calculated
        message("getting cached data")
        return(Imat)                           # return the cached value of the inverse matrix
    }                                          # otherwise
    data <- x$get_matrix()                     # get the value of the matrix stored in the vector object argument
    Imat <- solve(data)                        # and call solve() function to calculate the inverse matrix. Note that solve() expects a square invertible matrix
    x$set_inverse_matrix(Imat)                 # update the cache using the set_inverse_matrix() function
    Imat                                       # return a matrix that is the inverse of 'x'
}
