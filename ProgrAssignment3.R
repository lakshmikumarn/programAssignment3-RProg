
loadDS <- function() {
  
  outcome <- NULL
  hsptl<- NULL
  uStList <- NULL
  dis <- NULL
  
  init <- function() {
    outcome <<- read.csv("~/coursera/datascience/ProgAssignment3-data/outcome-of-care-measurescopy.csv", colClasses = "character")
    hsptl <<- read.csv("~/coursera/datascience/ProgAssignment3-data/hospital-data.csv", colClasses = "character")
    uStList <<- unique(trimws(outcome[,7],which="both"))
    print(cat("State list initialized - ", uStList))
    dis <<- c("heart attack", "heart failure", "pneumonia")
  }
  
  drawHist <- function(){
    outcome[,11] <- as.numeric(outcome[,11])
    hist(outcome[,11])
  }
  
  isValidState <- function(st) {
    if(is.null(st) | is.na(st)) stop("State value is Null/NA")
    trimws(st, which="both") %in% uStList # checks if the value is an element; match returns the location of the value
  }
  
  isValidOutcome <- function(oc){
    if(is.null(oc) | is.na(oc)) stop("Outcome value is Null/NA")
    trimws(oc, which="both") %in% dis # checks if the outcome for the disease is valid
  }
  
  ## Function that retuurns the lowest of applicable lower bound of the 30 day mortality rate
  best <- function(st, outcm) {
    if(! isValidState(st)){
      stop("Invalid State or no hospital in the state")
    }
    if(! isValidOutcome(outcm)){
      stop("Invalid outcome")
    }
    
    pos <- NULL # position of column (lower bound for a given type of situation) in the data frame
    
    switch(outcm,
           "heart attack" = pos <- 13,
           "heart failure" = pos <- 19,
           "pneumonia" = pos <- 25
          )
    #coln <- colnames(outcome)
    #print(coln)
    #print(coln[pos])
    
    # ** Trim down the outcome data based on the given state
    # ** Extract only data frame under virtual column name 'TRUE'. NOTE the use of single quote
    vOutcome <- split(outcome, outcome$State == st)
    vOutcome <- vOutcome$'TRUE'
    #print(vOutcome)
    
    # ** Objective - Fetch first minimum value index, find the value and use the value to check across
    # the data frame to fetch appropriate hospital names
    # ** as.numeric changes non-numeric (here "non available' ) to NA. Warning message is fine.
    # which.min discards missing and NAN values
   
    min <- which.min(as.numeric(vOutcome[[pos]])) # slice the column in which min to be found
    minVal <- vOutcome[min,pos]
    print("Min ..")
    print(min)
    print(minVal)
    
    # Objective: Fetch logical array of true and false to subset the valid rows
    # ** double sqr bracket to select a column of data frame as the name is a big string
    
    validvals <- vOutcome[[pos]] == minVal 
    #print(validvals[min])
    
    hlist <- vOutcome[validvals,2] # column 2 is hospital name
    sort(hlist)
    print("Best hospital(s) -")
    print(hlist)
    
  }
  
  rankHospital <- function(st,outcm,num){
    
    if(! isValidState(st)){
      stop("Invalid State or no hospital in the state")
    }
    if(! isValidOutcome(outcm)){
      stop("Invalid outcome")
    }
    vOutcome <- split(outcome, outcome$State == st)
    vOutcome <- vOutcome$'TRUE'
    
    pos <- NULL # position of column (lower bound for a given type of situation) in the data frame
    
    switch(outcm,
           "heart attack" = pos <- 11,
           "heart failure" = pos <- 17,
           "pneumonia" = pos <- 23
    )
    
    ##*** VERY IMPORTANT - not converting sorted column to be numeric for numeric sorting leads to erroneous results
    ordRowList <- order(as.numeric(vOutcome[[pos]]),vOutcome[[2]], na.last = NA)
    #print(ordRowList)
    #print(head(vOutcome[ordRowList,pos]))
    
    rnk <- ordRowList[as.numeric(num)] # fetch the rownum of the ordered list given the position passed through num
    
    switch(num,
           "best"  = rnk <- ordRowList[1],
           "worst" = rnk <- ordRowList[length(ordRowList)]
           )

    #print("Rank..")
    #print(rnk)
    
    hlist <- vOutcome[as.numeric(rnk),2] # column 2 is hospital name
    print("Ranked hospital(s) -")
    print(hlist)
    
  }
  
  rankAll <- function(outcm,num){

    if(! isValidOutcome(outcm)){
      stop("Invalid outcome")
    }
    
    pos <- NULL 
    switch(outcm,
           "heart attack" = pos <- 11,
           "heart failure" = pos <- 17,
           "pneumonia" = pos <- 23
    )
    
    grpBySt <- split(outcome,outcome$State)
    results <- list(length(grpBySt)) #initialize a list to store results for each state
    rnk <- NULL
    
    for (i in 1:length(grpBySt)) {
      byStateMatrix <- grpBySt[[i]] # extract each group as a subset data frame
      
      #order the subset by outcome and by hospital name
      ordRowList <- order(as.numeric(byStateMatrix[[pos]]),byStateMatrix[[2]], na.last = NA)
      rnk <- ordRowList[as.numeric(num)] # Default case
      
      switch(num,
             "best"  = rnk <- ordRowList[1],
             "worst" = rnk <- ordRowList[length(ordRowList)]
      )
      #print(rnk)
      results[i] <- list(byStateMatrix[rnk,c(2,pos)])
      names(results)[i] <- names(grpBySt)[i]
    }
    
    invisible(results)
    
  }
  
  # make the function(closure) object accessible to environment outside the scope of the super function
  list(init=init, best=best, drawHist = drawHist,rankHospital=rankHospital, rankAll=rankAll)
  
}






