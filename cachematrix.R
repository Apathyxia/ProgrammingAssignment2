# Functions created here are heavily inspired by the given functions in the assignment.

# Function to cache an inversion of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    # Declares a variable that can be utilised for matrix calculations.
    m <- NULL
    
    # Assigns a new value to the initial matrix; different environment than the current environment.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Declares the matrix to be inverted.
    get <- function() x
    
    # solve is a generic function to solve matrix equations.
    # See ?solve
    set_matrix <- function(solve) m <<- solve

    # Returns the inverse of a matrix from the cache..    
    get_matrix <- function() m
    
    # Creates a list of before created values to store.
    list(set = set, get = get,
         set_matrix = set_matrix,
         get_matrix = get_matrix)
        
}

# Function to compute the inverse of a matrix returned by makeCacheMatrix function. 
# If inversion has already been calculated, it is returned from the cache.
# x is the matrix from makeCacheMatrix function.
# Utilises function from makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    
    # Check for the existence of an inverted matrix.
    m <- x$get_matrix()
    
    if(!is.null(m)) {
        message("Getting chached data... \nAn inversed matrix was found!")
        return(m)
    }
    
    # Gets the initial matrix.
    data <- x$get()
    
    # Calculates the inverse of the matrix x.
    m <- solve(data, ...)
    
    # Stores the inverse matrix of x.
    x$set_matrix(m)
    
    # Function return. 
    m
    
}
