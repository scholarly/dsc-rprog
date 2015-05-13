outcomes = c(11,17,23)
names(outcomes) = c("heart attack", "heart failure","pneumonia")

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  ss = subset(data,State==state)
  if(nrow(ss)==0){
    stop("invalid state")
  }
  var = outcomes[outcome]
  if(is.na(var)){
    stop("invalid outcome")
  }
  
  outv = as.numeric(ss[,var])
  sort(ss[outv==min(outv,na.rm=TRUE),"Hospital.Name"])
  ## Return hospital name in that state with lowest 30-day death rate
}
