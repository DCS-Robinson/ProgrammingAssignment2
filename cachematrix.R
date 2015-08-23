## These two functions cooperate to cache the inverse of a square invertible matrix

## Takes the matrix to be inverted and returns an object that stores the matrix
## as well as it's inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        inverse_mat <- NULL
        
        set <- function(y) {
                x <<- y
                
                inverse_mat <<- NULL
        }
        
        get <- function() x
        
        set_inverse_mat <- function(calculated_inverse) inverse_mat <<- calculated_inverse
        
        get_inverse_mat <- function() inverse_mat
        
        list(set = set, get = get, set_inverse_mat = set_inverse_mat, get_inverse_mat = get_inverse_mat)
        
}


## Takes the matrix cache returns the inverse matrix. It calculates and stores
## the invere only if it does not already exist in the cache. Otherwise, it simply
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse_mat <- x$get_inverse_mat()
        
        if(!is.null(inverse_mat)){
                print("retrieving inverse matrix")
                return(inverse_mat)
        }
        argument_matrix <- x$get()
        inverse_mat <- solve(argument_matrix)
        
        x$set_inverse_mat(inverse_mat)
        
        inverse_mat
}
