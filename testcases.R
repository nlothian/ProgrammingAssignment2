source("cachematrix.R")

input <- matrix(1:4,2,2)
wrappedMatrix <- makeCacheMatrix(input)
inverted <- cacheSolve(wrappedMatrix)
solvedInput <- solve(input)


if (!isTRUE(all.equal(inverted, solvedInput))) {
    stop("Basic test failed")
} else {
    message("Passed: Basic Test")
}

input <- matrix(2:5,2,2)
wrappedMatrix$Set(input)
if (is.null(wrappedMatrix$GetInverse)) {    
    stop("Failure: resetting matrix did not reset inverse", call. = FALSE)
} else {    
    message("Passed: Resetting matrix resets inverse")
}

defaultWrappedMatrix = makeCacheMatrix();
cacheSolve(defaultWrappedMatrix)
message("Passed: Default argument")


input <- matrix(1:6,2,3)
tryCatch({
    wrappedMatrix$Set(input)
}, error = function(err) { 
    print(err)
    message("Passed: detected non-square matrix")
}) 

input <- c(1:2)
tryCatch({
    wrappedMatrix$Set(input)
}, error = function(err) {        
    print(err)
    message("Passed: detected non-matrix argument")
}) 

tryCatch({
    cacheSolve("A")
}, error = function(err) {        
    print(err)
    message("Passed: detected non-list argument to cacheSolve")
}) 

tryCatch({
    cacheSolve(list("A", "B"))
}, error = function(err) {            
    print(err)
    message("Passed: detected invalid list argument to cacheSolve")
}) 



