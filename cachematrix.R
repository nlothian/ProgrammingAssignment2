# 
# Coursera "Programming in R" Assignment 2
#
# A pair of functions that shows how to cache the inverse of a matrix. These
# functions should be used together like this:
#   wrappedMatrix <- makeCacheMatrix(matrix(1:4,2,2))
#   cacheSolve(wrappedMatrix)
#
# Method and variable naming style based on
#  Google's R Style Guide, available from
#  https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml
#
###############################################################################

# Build and return a list object wrapping the matrix passed in with
# functions to get and reset it, as well as to cache its inverse
# 
# Args:
#   x: A square, invertible matrix
#
# Returns: 
#   A list wrapping the matrix, which can cache the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
    
    # Define the Validate function used below
    Validate <- function(input) {        
        # Do some basic checks on the validity of the arguments.
        # We don't want to do a full "solve" or iterate over all
        # the elements for performance reasons here, but
        # we can check that input is a square matrix
        if (is.null(input)) {
            stop("Argument must not be null")
        } else if (class(input) != "matrix") {
            stop("Argument must be a matrix")
        } else if (nrow(input) != ncol(input)) {
            stop("Argument must be a square matrix")
        }
            
    }         

    # Perform validation using the validate function defined above
    Validate(x)
    
    # initilize the inverse to NULL
    inverse <- NULL
    
    # Set the the matrix  
    Set <- function(input) {
        # Reset "x" (the source matrix) and "inverse" (the inverted matrix) 
        # to NULL. We do this here in case validate fails
        x <<- NULL
        inverse <<- NULL                
        
        # validate the input
        Validate(input)
        
        # here we use the superassignment operator (<<-) to set 
        # the value of "x" and "inverse" in the parent scope
        x <<- input
        
        # need to reset inverse to NULL
        inverse <<- NULL
    }
    
    # return the source matrix
    Get <- function() {
        x
    }
    
    # Allow the inverse matrix to be cached
    SetInverse <- function(invInput) {
        # use superassigment to set "inverse" in the parent scope
        inverse <<- invInput
    }
    
    # return the invrted matrix
    GetInverse <- function() {        
        inverse
    }
   
    
    # Build the list object and return it
    return (list(
            Set = Set, Get = Get, 
            SetInverse = SetInverse, GetInverse = GetInverse))
}


# Takes a wrapped matrix object in the form given by makeCacheMatrix and return 
# its inverse, either by retirving the previously calculated inverse from the 
# cache, or calculating and caching it.
# 
# Args:
#   x: An object wrapping a matrix, in the form returned by the makeCacheMatrix
#       function
#
# Returns:
#   The inverse matrix
cacheSolve <- function(x, ...) {
    # check that the argument x is the type we are expecting
    if (class(x) != "list") {        
        stop("Argument x must be a list")
    } else {
        # Check that the list contains the methods we need
        
        # Note that we don't check for the x$Set method. We don't actually
        # need it in this function, so it is technically possibly to imagine
        # circumstances where it makes sense to allow this    
        
        if (class(x$Get) != "function") {
            stop("Argument does not contain a Get function. The argument should be created using the makeCacheMatrix function")
        }
        
        if (class(x$GetInverse) != "function") {
            stop("Argument does not contain a GetInverse function. The argument should be created using the makeCacheMatrix function")
        }
        
        if (class(x$SetInverse) != "function") {
            stop("Argument does not contain a SetInverse function. The argument should be created using the makeCacheMatrix function")
        }        
    }
    
    # Check if the matrix already has the inverse cached
    if(is.null(x$GetInverse())) {
        # GetInverse() returned null, so solve the matrix
        # and cache the solution
        inverse = tryCatch({
                inputMatrix = x$Get()
                
                if (nrow(inputMatrix) == 1  && ncol(inputMatrix) == 1
                    && is.na(inputMatrix[1,1])) {
                        
                    # This is a special case.
                    #
                    # Inversion of the default argument supplied as part of the function 
                    # definition fails on some versions of R. 
                    #
                    # Eg, on my computer:
                    #
                    # > solve(matrix())
                    # Error in solve.default(matrix()) : 
                    #    system is computationally singular: reciprocal condition number = 0
                    #
                    # It seems important to handle this. 
                    # 
                    # See https://class.coursera.org/rprog-014/forum/thread?thread_id=569
                    #
                    return (matrix())                        
                    
                } else {
                    return (solve(inputMatrix))
                }
            }, error = function(err) {
                message("Could not invert matrix. Error is:")
                stop(err)                
            }
        ) 
        
        x$SetInverse(inverse)                     
    } else {
        # The inverse was already cached
        message("Cached data found")   
    }

    # At this point the inverse is always cached, so return it
    return(x$GetInverse())
}
