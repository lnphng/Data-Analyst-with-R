## Read outcome data
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data   <- as.data.frame(cbind(data[, 2],   # hospital
                              data[, 7],   # state
                              data[, 11],  # heart attack
                              data[, 17],  # heart failure
                              data[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
## Check that state and outcome are valid
if (!state %in% data[, "state"]) {
  stop('invalid state')
}
else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
  stop('invalid outcome')
}
else if (is.numeric(rank)) {
  selectState <- which(data[, "state"] == state)
  calledState <- data[selectState, ]   # extracting dataframe for the called state
  calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
  calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
  output <- calledState[, "hospital"][rank]
}
else if (!is.numeric(rank)){
  if (rank == "best") {
    output <- best(state, outcome)
  }
  else if (rank == "worst") {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE) ]
    output <- calledState[, "hospital"][1]
  }
  else {
    stop('invalid rank')
  }
}
return(output)
}
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% data[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[!is.na(calledState[, eval(outcome)])]
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE) ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% data[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[!is.na(data[selectState, ])]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE) ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% data[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE) ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% data[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[!is.na(calledState[, eval(outcome)])]
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE) ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% fd[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE), ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data   <- as.data.frame(cbind(data[, 2],   # hospital
                                data[, 7],   # state
                                data[, 11],  # heart attack
                                data[, 17],  # heart failure
                                data[, 23]), # pneumonia
                          stringsAsFactors = FALSE)
  colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% data[, "state"]) {
    stop('invalid state')
  }
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  else if (is.numeric(rank)) {
    selectState <- which(data[, "state"] == state)
    calledState <- data[selectState, ]   # extracting dataframe for the called state
    calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
    calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"]), ]
    output <- calledState[, "hospital"][rank]
  }
  else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    }
    else if (rank == "worst") {
      selectState <- which(data[, "state"] == state)
      calledState <- data[selectState, ]
      calledState[, eval(outcome)] <- as.numeric(calledState[, eval(outcome)])
      calledState <- calledState[order(calledState[, eval(outcome)], calledState[, "hospital"], decreasing = TRUE), ]
      output <- calledState[, "hospital"][1]
    }
    else {
      stop('invalid rank')
    }
  }
  return(output)
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
reldata2 <- data  %>%
  select(starts_with("Hospital"), -contains("Readmission"), State) %>%
  mutate_at(vars(contains("Mortality")), as.numeric)%>%
  group_by(`Hospital Name`,State)
rankall<- function(outcome, num= "best"){
  outcome <- regex(outcome, ignore_case = T)
  `%notin%` <- Negate(`%in%`)
  if (TRUE %notin% str_detect(names(reldata2), outcome)){
    stop("invalid outcome")
  }
  ranking <- reldata2 %>%
    select(hospital= `Hospital Name`, state= State, Rate= contains(outcome)) %>%
    arrange(Rate, hospital) %>%
    group_by(state)%>%
    arrange(state) %>%
    mutate(rank= row_number())
  if (num== "worst"){
    worst= summarize(ranking%>%drop_na(), rank= max(rank))
    return(inner_join(ranking, worst, by= c("state", "rank"))
           %>% select(hospital, state))
  } else if (num== "best"){
    num= min(ranking$rank)
  } else {
    num= num
  }
  ranking <- ranking %>%
    filter(rank== num)%>%
    select(hospital, state)
  statenames <- unique(reldata2$State)
  subs<- statenames[statenames %notin% ranking$state]
  missing= NULL
  for (i in seq_along(subs)){
    hospital <-  "<NA>"
    state<-  subs[i]
    missing= bind_rows(missing, tibble(hospital,state))
  }
  bind_rows(ranking,missing) %>%
    arrange(state)
}
head(rankall("heart attack", 20), 10)
rankAll <- function(outcome, num = "best") {
  dataAll <- data.frame(hospital = character(), state = character())
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that outcome and num are valid
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    dataAll <- "invalid outcome"
  }
  else {
    keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    outcomeKey <- keys[outcome]
    ## For each state, find the hospital of the given rank
    dataPerState <- split(data, data$State)
    for (stat in names(dataPerState)) {
      dataOurState <- dataPerState[[stat]]
      dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
      good <- complete.cases(dataOutcome)
      dataOutcome <- dataOutcome[good]
      dataOurState <- dataOurState[good,]
      dataOurState <- dataOurState[ order(dataOutcome, dataOurState["Hospital.Name"]), ]
      if (num == "best") {
        numState <- c(1)
      } else {
        if (num == "worst") {
          numState <- length(dataOutcome)
        } else {
          numState <- num
        }
      }
      dataPart <- data.frame(hospital = dataOurState[numState, "Hospital.Name"], state = stat, row.names = stat)
      dataAll <- rbind(dataAll, dataPart)
    }
  }
  head(rankall("heart attack", 20), 10)
  rankAll <- function(outcome, num = "best") {
    dataAll <- data.frame(hospital = character(), state = character())
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that outcome and num are valid
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      dataAll <- "invalid outcome"
    }
    else {
      keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      outcomeKey <- keys[outcome]
      ## For each state, find the hospital of the given rank
      dataPerState <- split(data, data$State)
      for (stat in names(dataPerState)) {
        dataOurState <- dataPerState[[stat]]
        dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
        good <- complete.cases(dataOutcome)
        dataOutcome <- dataOutcome[good]
        dataOurState <- dataOurState[good,]
        dataOurState <- dataOurState[ order(dataOutcome, dataOurState["Hospital.Name"]), ]
        if (num == "best") {
          numState <- c(1)
        } else {
          if (num == "worst") {
            numState <- length(dataOutcome)
          } else {
            numState <- num
          }
        }
        dataPart <- data.frame(hospital = dataOurState[numState, "Hospital.Name"], state = stat, row.names = stat)
        dataAll <- rbind(dataAll, dataPart)
      }
    }
    dataAll
  }
  head(rankall("heart attack", 20), 10)
  head(rankall("heart attack", 20), 10)
  rankAll <- function(outcome, num = "best") {
    dataAll <- data.frame(hospital = character(), state = character())
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that outcome and num are valid
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
      dataAll <- "invalid outcome"
    }
    else {
      keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
      outcomeKey <- keys[outcome]
      ## For each state, find the hospital of the given rank
      dataPerState <- split(data, data$State)
      for (stat in names(dataPerState)) {
        dataOurState <- dataPerState[[stat]]
        dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
        good <- complete.cases(dataOutcome)
        dataOutcome <- dataOutcome[good]
        dataOurState <- dataOurState[good,]
        dataOurState <- dataOurState[ order(dataOutcome, dataOurState["Hospital.Name"]), ]
        if (num == "best") {
          numState <- c(1)
        } else {
          if (num == "worst") {
            numState <- length(dataOutcome)
          } else {
            numState <- num
          }
        }
        dataPart <- data.frame(hospital = dataOurState[numState, "Hospital.Name"], state = stat, row.names = stat)
        dataAll <- rbind(dataAll, dataPart)
      }
    }
    ## Return a data frame with the hospital names and the (abbreviated) state name
    dataAll
  }
  head(rankall("heart attack", 20), 10)
  rankall <- function(outcome, num = "best") {
    ## Read the outcome data
    dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states = unique(dat[, 7])
    switch(outcome, `heart attack` = {
      col = 11
    }, `heart failure` = {
      col = 17
    }, pneumonia = {
      col = 23
    }, stop("invalid outcome"))
    ## Return hospital name in that state with the given rank 30-day death rate
    dat[, col] = as.numeric(dat[, col])
    dat = dat[, c(2, 7, col)]  # leave only name, state, and death rate
    dat = na.omit(dat)
    # head(dat) Hospital.Name State 1 SOUTHEAST ALABAMA MEDICAL CENTER AL 2
    # MARSHALL MEDICAL CENTER SOUTH AL 3 ELIZA COFFEE MEMORIAL HOSPITAL AL 7 ST
    # VINCENT'S EAST AL 8 DEKALB REGIONAL MEDICAL CENTER AL 9 SHELBY BAPTIST
    # MEDICAL CENTER AL
    # Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack 1 14.3 2 18.5 3
    # 18.1 7 17.7 8 18.0 9 15.9
    rank_in_state <- function(state) {
      df = dat[dat[, 2] == state, ]
      nhospital = nrow(df)
      switch(num, best = {
        num = 1
      }, worst = {
        num = nhospital
      })
      if (num > nhospital) {
        result = NA
      }
      o = order(df[, 3], df[, 1])
      result = df[o, ][num, 1]
      c(result, state)
    }
    output = do.call(rbind, lapply(states, rank_in_state))
    output = output[order(output[, 2]), ]
    rownames(output) = output[, 2]
    colnames(output) = c("hospital", "state")
    data.frame(output)
  }
  head(rankall("heart attack", 20), 10)
  tail(rankall("pneumonia", "worst"), 3)
  tail(rankall("heart failure"), 10)
  tail(rankall("heart failure"), 10)
  r <- rankall("heart attack", 4)
  as.character(subset(r, state == "HI")$hospital)
  r <- rankall("pneumonia", "worst")
  as.character(subset(r, state == "NJ")$hospital)
  r <- rankall("heart failure", 10)
  as.character(subset(r, state == "NV")$hospital)
  