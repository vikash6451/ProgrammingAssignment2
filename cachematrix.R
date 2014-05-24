## The following two functions generally help to create a matrix vector,
## with access to modify it,then to get the Solve(inverted format) of
## the matrix stored inside the vector(if there is),otherwise to calculate
## that Solve and store it inside the vector.

## This makeCacheMatrix has two variables and four functions 
## x is a matrix intaked from outside,and s should be seen as
## a place to store the Solve

makeCacheMatrix <- function(x = matrix()) {  ##intake a matrix x
        s<-NULL                         ##initial s as NULL
        set<-function(y){             
                x<<-y                      ##"<<-"operator indicates we assign y to 
                ## that "x" which was intaked by the upper
                ##function and passed to the set() function
                ##instead of a new "x" initialized in set()
                
                s<<-NULL               ## likewise,this s is exactly that "s" initialized
                ## in the beginning of upper function
        }
        get<-function(){
                x                             ## get() intakes the "x" from the upper function
        }
        setSolve<-function(Solve){
                s<<-Solve               ## setSolve() intakes a "Solve" outside the upper
                ## function,and assign it to that "s" in upper
                ## function
        }
        getSolve<-function(){
                s
        }
        list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
        ## store the four functions as elements of a list which will be
        ## returned if we call the makeCacheMatrix function
}


## this cacheSolve intakes a matrix x,and firstly checks if
## there were already a Solve stored.If so,it will directly
## return that Solve with a message for notification,otherwise 
## it will do a new calculation of this matrix before return
## the Solve,and put it into storage at the same time.

cacheSolve <- function(x, ...) {
        s <- x$getSolve()               ##  intake the "s" from an existing matrix
        if(!is.null(s)) {                     ## CHECK process 
                message("getting cached solveData")
                return(s)
        }
        solveData <- x$get()          ## if there isn't one in storage,do calculation
        s <- solve(solveData, ...)   ## solve() basically does the inverse job
        x$setSolve(s)                    ## put result of calculation in storage
        s
        
}