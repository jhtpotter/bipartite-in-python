#setwd("~/R/bipartite")
setwd("~/Python/Code/bipartite-in-python")
#setwd("C:/Users/Josh/Google Drive/PhD/R Work/Bipartite")
exampleDF <- read.csv("Example_OTUs.csv")
rownames(exampleDF) <- exampleDF[,1]
exampleDF<-exampleDF[,-1]

#web<-exampleDF#[,1]
#colSums(web)
#cempty <- which(colSums(web)==0)
#cempty
#web>0
#sum(web>0)

#colSumVec <- colSums(web)
#colSumVec
#order(colSumVec)
#min(colSumVec)
#minCol <- which(colSumVec == min(colSumVec))
#minCol
#minRow <- which(rowSumVec == min(rowSumVec))
#print(minCol,minRow)

#rowSumVec <- rowSums(df)
#colSumVec <- colSums(df)
#rowSeq <- order(rowSums(df))
#colSeq <- order(colSums(df))
#minCol <- which(colSumVec == min(colSumVec))
#minRow <- which(rowSumVec == min(rowSumVec))

#extOTUDict <- as.list(c(0,0,0,0))
#extOTUDict
#rownames(web)
#rownames(web)[3]
#names(extOTUDict) <- colnames(web)[1:4]
#extOTUDict <- c(extOTUDict, "NEWBAT"=1)
#extOTUDict$"NEWBAT"
#"NEWBAT" %in% names(extOTUDict)


pause <- function(){
  line <- readline()
}


eval_df <- function(df, taxaDict){
  print("Running function eval_df()")
#  pause()
  predators <- colnames(df)
  cat("Predators: ", predators)
  preyOTUs <- rownames(df)
  cat("Prey OTUs: ", preyOTUs)
#  pause()
  for (i in 1:ncol(df)){
    colValues <- numeric()
    predator <- predators[i]
    cat("Current predator: ", predator)
#    pause()
    preyGained <- 0
    preyLost <- 0
    if (!(predator %in% names(taxaDict))){
      print("ERROR PREDATOR NOT FOUND IN INTERACTION DICTIONARY")
      # INSERT DIE STATEMENT
      next()
    }
    preyBreadth <- length(taxaDict[[predator]]) - 1
    cat("Current dietary breadth, according to interaction dictionary: ", preyBreadth)
    for (j in 1:nrow(df)){
      preyOTU <- rownames(df)[j]
      cat("Current OTU: ", preyOTU)
      interactionVal <- df[j, i]
      cat("Interaction in df: ", interactionVal)
#      pause()
      if (interactionVal == 1){
        colValues <- append(colValues, interactionVal)
        if (length(which(taxaDict[[predator]]==preyOTU)) == 0){
          print("Adding prey/OTU to interaction dictionary")
          taxaDict[[predator]] <- append(taxaDict[[predator]],preyOTU)
          preyGained <- preyGained + 1
        } else if (length(which(taxaDict[[predator]]==preyOTU)) == 1){
          print("OTU already in interaction dictionary, moving on")
          next()
        } else {
          print("ERROR OTU PRESENT IN DICTIONARY MORE THAN ONCE")
          # INSERT DIE STATEMENT
          next()
        }
      } else if (interactionVal == 0){
        colValues <- append(colValues, interactionVal)
        if (length(which(taxaDict[[predator]]==preyOTU)) == 0){
          print("OTU not present in interaction dictionary, so moving on")
          next()
        } else if (length(which(taxaDict[[predator]]==preyOTU)) == 1){
          print("Removing prey/OTU from interaction dictionary")
          taxaDict[[predator]] <- taxaDict[[predator]][taxaDict[[predator]] != preyOTU]
          preyLost <- preyLost + 1
        } else {
          print("ERROR OTU PRESENT IN DICTIONARY MORE THAN ONCE")
          # INSERT DIE STATEMENT
          next()
        }
      } else {
        print("ERROR NON-BINARY INTERACTION VALUE FOUND")
        # INSERT DIE STATEMENT
        next()
      }
    }
    colValSum <- sum(colValues)
    cat("New dietary breadth: ", colValSum)
    if (colValSum != length(taxaDict[[predator]]) - 1){
      print("ERROR MISMATCH BETWEEN COLUMN SUM AND DICTIONARY")
      # INSERT DIE STATEMENT
      next()
    }
    if (colValSum != preyBreadth + preyGained - preyLost){
      print("ERROR MISMATCH BETWEEN COLUMN SUM AND OTU GAIN/LOSS COUNT")
      # INSERT DIE STATEMENT
      next()
    }
    taxaDict[[predator]][1] <- colValSum
  }
#  pause()
  return(taxaDict)
}


empty <- function(df, count=FALSE){
  print("Running function empty()")
#  pause()
  numCols <- ncol(df)
  numrows <- nrow(df)
  numEmptyCols <- 0
  numEmptyRows <- 0
  emptyColIdxs <- which(colSums(df)==0)
  numEmptyCols <- length(emptyColIdxs)
  print(emptyColIdxs)
  emptyRowIdxs <- which(rowSums(df)==0)
  numEmptyRows <- length(emptyRowIdxs)
  print(emptyRowIdxs)
  if (numEmptyRows == 0){
    nonemptyRowIdxs <- 1:nrow(df)
  } else {
    nonemptyRowIdxs <- (1:nrow(df))[-emptyRowIdxs]
  }
  if (numEmptyCols == 0){
    nonemptyColIdxs <- 1:ncol(df)
  } else {
    nonemptyColIdxs <- (1:ncol(df))[-emptyColIdxs]
  }
  df <- df[nonemptyRowIdxs,nonemptyColIdxs]
  emptyNumbers <- c(numEmptyRows, numEmptyCols)
  emptyOut <- list(df, emptyNumbers)
  names(emptyOut) <- c("emptiedDF","emptyInfo")
  return(emptyOut)
}


extinction <- function(df, participant, method, interactionDict, switchDict, extOTUDict){
  print("Running function extinction")
#  pause()
  numCols <-  ncol(df)
  numrows <- nrow(df)
  rowSumVec <- rowSums(df)
  colSumVec <- colSums(df)
  if (participant == "both" && method == "random"){
    # Randomly pick a participant:
    partIdx <- sample(1:2, 1)
    participant <- participants[partIdx]
    print("Participant is now:", participant)
    pause()
  }
  if (method == "random"){
    rowExtin <- sample(1:numrows, 1)
    colExtin <- sample(1:numCols, 1)
    if (participant == "lower"){
      for (i in 1:numCols){
        cat("Randomly making OTU ", rowExtin, " extinct")
        df[rowExtin,i] <- 0
      }
    }
    if (participant == "higher"){
      for (j in 1:numrows){
        cat("Randomly making predator ", colExtin, " extinct")
        df[j,colExtin] <- 0
      }
    }
    pause()
  } else if (method == "abundance"){
    rowSumVec <- numeric()
    colSumVec <- numeric()
    print("Randomly shuffling dataframe")
    df <- df[sample(1:nrow(df),nrow(df)),sample(1:ncol(df),ncol(df))]
#    pause()
    rowSeq <- order(rowSums(df))
    colSeq <- order(colSums(df))
    extOTUIdx <- rowSeq[1]
    extPredIdx <- colSeq[1]
    rowSumVec <- rowSums(df)
    colSumVec <- colSums(df)
    minRow <- which(rowSumVec == min(rowSumVec))
    minCol <- which(colSumVec == min(colSumVec))
    if ((extOTUIdx != minRow) || (extPredIdx != minCol)){
      print("ERROR FOUND DIFFERENT LEAST-ABUNDANT OTU OR PREDATOR BY DIFFERENT METHODS")
      # INSERT DIE STATEMENT
      next()
    }
    cat(minCol,minRow,"\n")
    if (participant == "lower"){
      extOTU <- rownames(df)[extOTUIdx]
      cat("Making ", extOTU, " extinct\n")
      if (extOTU %in% names(extOTUDict)){
        print("ERROR, OTU ALREADY FOUND IN EXTINCTION DICTIONARY")
        # INSERT DIE STATEMENT
        next()
      }
      else {
        print("Adding OTU to extinction dictionary")
        extOTUDict[[extOTU]] <- 1
      }
      for (i in 1:ncol(df)){
        currentPredator <- colnames(df)[i]
        print(df[,i])
        nonPreyIdxs <- which(df[,i]==0)
        cat("Indexes of OTUs for which interaction is 0: ",nonPreyIdxs, "\n")
        validNewPrey <- numeric()
        if (length(nonPreyIdxs) > 0){
          for (k in 1:length(nonPreyIdxs)){
            cat("Non prey index: ",nonPreyIdxs[k],"\n")
            cat("Motu: ",rownames(df)[nonPreyIdxs[k]], "\n")
            if (!(rownames(df)[nonPreyIdxs[k]] %in% names(extOTUDict))){
              validNewPrey <- append(validNewPrey, nonPreyIdxs[k])
            } else {
              cat("OTU ", rownames(df)[nonPreyIdxs[k]], " already extinct\n")
            }
          }
        }
        cat("Potential new prey species: ", validNewPrey, "\n")
        if (switchDict[[currentPredator]] == "switcher" && length(validNewPrey) > 0){
          print("Predator is a switcher")
          print("Randomly selecting a new OTU/prey")
          newPrey <- validNewPrey[sample(1:length(validNewPrey),1)]
          cat("New prey: ", rownames(df)[newPrey], " at index: ", newPrey, "\n")
#          pause()
          if (df[newPrey, i] == 0){
            print("Changing 0 to 1 in df")
            df[newPrey, i] <- 1
          } else {
            print("ERROR POSITIVE INTERACTION ALREADY FOUND FOR THIS OTU")
            # INSERT DIE STATEMENT
            next()
          }
        }
      }
      # this line actually sets the value at the row corresponding to the extincted prey species to 0
      print("Making prey extinct")
      df[extOTUIdx,] <- 0
#      pause()
    } else if (participant == "higher"){
      extPred <- colnames(df)[extPredIdx]
      df[,extPredIdx] <- 0
#      for (j in 1:nrow(df)){
#        df[j,extPredIdx] <- 0
#      }
    } else if (participant == "both"){
      if (min(rowSumVec) < min(colSumVec)){
        df[extOTUIdx,] <- 0
      } else if (min(colSumVec) < min(rowSumVec)){
        df[,extPredIdx] <- 0
      } else {
        if (sample(1:2,1) == 1){
          df[extOTUIdx,] <- 0
        } else {
          df[,extPredIdx] <- 0
        }
      }
    }
  } else if (method == "degree"){
    if (participant == "lower"){
      topRows <- which(rowSums(df) == max(rowSums(df)))
      if (length(topRows) > 1){
        rowExtin <- sample(topRows, 1)
      } else {
        rowExtin <- topRows[1]
      }
      df[rowExtin,] <- 0
    } else if (participant == "higher"){
      topCols <- which(colSums(df) == max(colSums(df)))
      if (length(topCols) > 1){
        colExtin <- sample(topCols, 1)
      } else {
        colExtin <- topCols[1]
      }
      df[,colExtin] <- 0
    }
  }
  return(df)
}


second_extinction <- function(df, participant, method){
  library(stringr)
  interactionDict <- list()
  switchDict <- list()
  extOTUDict <- list()
  predatorVec <- colnames(df)
  numPreds <- length(predatorVec)
  for (i in 1:numPreds){
    predator <- predatorVec[i]
    cat("Predator: ", predator, "\n")
#    pause()
    if ((predator %in% names(interactionDict)) || (predator %in% names(switchDict))){
      print("ERROR PREDATOR ALREADY IN DICTIONARY")
      pause()
      # INSERT DIE STATEMENT
      next()
    } else {
      print("Initialising interaction dictionary")
      interactionDict[[predator]] <- as.character("0")
      if (sample(1:2,1)==1){
        print("Randomly making predator a non-switcher")
        switchDict[[predator]] <- "non_switcher"
      } else {
        print("Randomly making predator a switcher")
        switchDict[[predator]] <- "switcher"
      }
#      pause()
    }
    for (j in 1:nrow(df)){
      preyOTU <- rownames(df)[j]
      cat("Prey: ", preyOTU, "\n")
      interactionVal <- df[j,i]
      cat("Binary interaction: ", interactionVal, "\n")
      if (interactionVal == 1){
        if (!(predator %in% names(interactionDict))){
          print("ERROR PREDATOR NOT IN INTERACTION DICTIONARY")
          pause()
          # INSERT DIE STATEMENT
          next()
        } else {
          cat("Current interaction dictionary contains: ", interactionDict[[predator]], "\n")
          if (sum(str_count(interactionDict[[predator]], preyOTU)) != 0){
            print("ERROR PREY ALREADY FOUND UNDER PREDATOR LOOKUP")
            pause()
            # INSERT DIE STATEMENT
            next()
          } else {
            interactionDict[[predator]] <- append(interactionDict[[predator]], preyOTU)
            interactionDict[[predator]][1] <- as.character(as.integer(interactionDict[[predator]][1]) + 1)
          }
        }
      } else if (interactionVal == 0){
        next()
      } else {
        print("ERROR NON BINARY INTERACTION VALUE FOUND")
        # INSERT DIE STATEMENT
        next()
      }
    }
  }
  print(interactionDict)
  participants = c("lower", "higher", "both")
  extinMethods = c("random", "abundance", "degree")
  while (!(participant %in% participants)){
    # GET INPUT
    participant <- "lower"
  }
  while (!(method %in% extinMethods)){
    # GET INPUT
    method <- "abundance"
  }
  one_second_extinction <- function(osDF, osParticipant, osMethod, currentDict){
    dead <- matrix(nrow=0,ncol=3)
    colnames(dead) <- c("no", "ext_lower", "ext_higher")
    repz = TRUE
    i = 1
    while (repz){
      print(i)
      currentDF <- extinction(df=osDF, participant=osParticipant, method=osMethod, interactionDict=currentDict, switchDict=switchDict, extOTUDict=extOTUDict)
      currentDict <- eval_df(df=currentDF, taxaDict=currentDict)
      emptyOutList <- empty(df=currentDF, count=TRUE)
      str(emptyOutList)
      print(emptyOutList)
      osDF <- emptyOutList$emptiedDF
      emptyOutput <- emptyOutList$emptyInfo
      if (length(emptyOutput)!=2){
        print("ERROR OUTPUT FROM FUNCTION EMPTY() NOT OF CORRECT SIZE")
        # INSERT DIE STATEMENT
        next()
      }
      deadrow = c(i, emptyOutput)
      dead <- rbind(dead,deadrow)
      numCols <- ncol(osDF)
      numRows <- nrow(osDF)
      if (osParticipant == "lower" && numRows < 2){
        break()
      }
      if (osParticipant == "higher" && numCols < 2){
        break()
      }
      if (osParticipant == "both" && (numRows < 2 || numCols < 2)){
        break()
      }
      if (numRows <= 1 || numCols <= 1){
        break()
      }
      i <- i + 1
    }
    dead2 <- dead
    deadRows <- nrow(dead)+1
    dead2 <- rbind(dead2, c(deadRows, numRows, numCols))
    return(dead2)
  }
  seOutput <- one_second_extinction(osDF=df, osParticipant=participant, osMethod=method, currentDict=interactionDict)
  return(seOutput)
}

#library(stringr)
#exampleDF[1,1] <- 50
#newList <- second_extinction(df = web)
#web[1,1]==1
#newList$BAT0001 <- c(newList$BAT0001, "PREY1")
#prey <- "PREY1"
#prey
#str(newList$BAT0001)
#sum(str_count(newList$BAT0001, prey))
#newList
#predator <- "BAT0001"
#append(newList[[predator]], "YOLO")
#which(newList[[predator]] == "OTU_42")
#taxaDict[[predator]] <- taxaDict[[predator]][taxaDict[[predator]] != preyOTU]
#newList[[predator]][newList[[predator]] != "OTU_42"]

second_extinction(df=exampleDF, participant="lower", method="abundance")


