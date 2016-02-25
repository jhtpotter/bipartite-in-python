#setwd("~/R/bipartite")
setwd("~/Python/Code/bipartite-in-python")
#setwd("C:/Users/Josh/Google Drive/PhD/R Work/Bipartite")
exampleDF <- read.csv("Example_OTUs.csv")
rownames(exampleDF) <- exampleDF[,1]
exampleDF<-exampleDF[,-1]

web<-exampleDF#[,1]
colSums(web)
cempty <- which(colSums(web)==0)
cempty
web>0
sum(web>0)

colSumVec <- colSums(web)
colSumVec
order(colSumVec)
min(colSumVec)
minCol <- which(colSumVec == min(colSumVec))
minCol
minRow <- which(rowSumVec == min(rowSumVec))
print(minCol,minRow)

rowSumVec <- rowSums(df)
colSumVec <- colSums(df)
rowSeq <- order(rowSums(df))
colSeq <- order(colSums(df))
minCol <- which(colSumVec == min(colSumVec))
minRow <- which(rowSumVec == min(rowSumVec))

extOTUDict <- as.list(c(0,0,0,0))
extOTUDict
rownames(web)
rownames(web)[3]
names(extOTUDict) <- colnames(web)[1:4]
extOTUDict <- c(extOTUDict, "NEWBAT"=1)
extOTUDict$"NEWBAT"
"NEWBAT" %in% names(extOTUDict)





eval_df <- function(df, taxaDict){
  predators <- colnames(df)
  print(paste0("Predators: ", predators))
  preyOTUs <- rownames(df)
  print(paste0("Prey OTUs: ", preyOTUs))
  for (i in 1:ncol(df)){
    colValues <- numeric()
    predator <- predators[i]
    preyGained <- 0
    preyLost <- 0
    if (!(predator %in% names(taxaDict))){
      # INSERT DIE STATEMENT
      next()
    }
    preyBreadth <- length(taxaDict[[predator]]) - 1
    for (j in 1:nrow(df)){
      preyOTU <- rownames(df)[j]
      interactionVal <- df[j, i]
      if (interactionVal == 1){
        colValues <- append(colValues, interactionVal)
        if (length(which(taxaDict[[predator]]==preyOTU)) == 0){
          taxaDict[[predator]] <- append(taxaDict[[predator]],preyOTU)
          preyGained <- preyGained + 1
        } else if (length(which(taxaDict[[predator]]==preyOTU)) == 1){
          next()
        } else {
          # INSERT DIE STATEMENT
          next()
        }
      } else if (interactionVal == 0){
        colValues <- append(colValues, interactionVal)
        if (length(which(taxaDict[[predator]]==preyOTU)) == 0){
          next()
        } else if (length(which(taxaDict[[predator]]==preyOTU)) == 1){
          taxaDict[[predator]] <- taxaDict[[predator]][taxaDict[[predator]] != preyOTU]
          preyLost <- preyLost + 1
        } else {
          # INSERT DIE STATEMENT
          next()
        }
      } else {
        # INSERT DIE STATEMENT
        next()
      }
    }
    colValSum <- sum(colValues)
    if (colValSum != length(taxaDict[[predator]]) - 1){
      # INSERT DIE STATEMENT
      next()
    }
    if (colValSum != preyBreadth + preyGained - preyLost){
      # INSERT DIE STATEMENT
      next()
    }
    taxaDict[[predator]][1] <- colValSum
  }
  return(taxaDict)
}





empty <- function(df, count=FALSE){
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
    nonEmptyColIdxs <- 1:ncol(df)
  } else {
    nonEmptyColIdxs <- (1:ncol(df))[-emptyColIdxs]
  }
  df <- df[,!emptycolIDS]
  df <- df[!emptyrowIDs,]
  for (i in 1:numrows){
    row <- df[i,]
  }
  emptyOut <- list(df,c(numEmptyRows,numEmptyCols))
  names(emptyOut) <- c(emptiedDF,emptyInfo)
  return(emptyOut)
}





extinction <- function(df, participant, method, interactionDict, switchDict, extOTUdict){
  numCols <-  ncol(df)
  numrows <- nrow(df)
  rowSumVec <- rowSums(df)
  colSumVec <- colSums(df)
  if (participant == "both" && method == "random"){
    # Randomly pick a participant:
    partIdx <- sample(1:2, 1)
    participant <- participants[partIdx]
    print("Participant is now:", participant)
  }
  if (method == "random"){
    rowExtin <- sample(1:numrows, 1)
    colExtin <- sample(1:numCols, 1)
    if (participant == "lower"){
      for (i in 1:numCols){
        df[rowExtin,i] <- 0
      }
    }
    if (participant == "higher"){
      for (j in 1:numrows){
        df[j,colExtin] <- 0
      }
    }
  } else if (method == "abundance"){
    rowSumVec <- numeric()
    colSumVec <- numeric()
    df <- df[sample(1:nrow(df),nrow(df)),]
    df <- df[,sample(1:ncol(df),ncol(df))]
    rowSeq <- order(rowSums(df))
    colSeq <- order(colSums(df))
    extOTUIdx <- rowSeq[1]
    extPredIdx <- colSeq[1]
    rowSumVec <- rowSums(df)
    colSumVec <- colSums(df)
    minRow <- which(rowSumVec == min(rowSumVec))
    minCol <- which(colSumVec == min(colSumVec))
    if ((extOTUIdx != minRow) || (extPredIdx != minCol)){
      # INSERT DIE STATEMENT
      next()
    }
    print(minCol,minRow)
    if (participant == "lower"){
      extOTU <- rownames(df)[extOTUIdx]
      if (extOTU %in% names(extOTUdict)){
        # INSERT DIE STATEMENT
        next()
      }
      else {
        extOTUdict[[extOTU]] <- 1
      }
      for (i in 1:ncol(df)){
        currentPredator <- colnames(df)[i]
        nonPreyIdxs <- which(df[,i]==0 && !(rownames(df)[i] %in% extOTUdict))
        print(nonPreyIdxs)
        if (switchDict$currentPredator == "switcher" && length(nonPreyIdxs) > 0){
          newPrey <- nonPreyIdxs[sample(1:length(nonPreyIdxs),1)]
          if (df$newPrey[i] == 0){
            df$newPrey[i] <- 1
          } else {
            # INSERT DIE STATEMENT
            next()
          }
        }
        # this line actually sets the value at the row corresponding to the extincted prey species to 0
        #df[extOTUIdx,i] <- 0
      }
      df[extOTUIdx,] <- 0
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
    print(paste0("Predator: ",predator))
    if ((predator %in% names(interactionDict)) || (predator %in% names(switchDict))){
      print("ERROR PREDATOR ALREADY IN DICTIONARY")
      # INSERT DIE STATEMENT
      next()
    } else {
      print("Initialising interaction dictionary")
      interactionDict[[predator]] <- as.character("0")
#      interactionDict <- c(interactionDict, predator=0)
      if (sample(1:2,1)==1){
        switchDict[[predator]] <- "non_switcher"
      } else {
        switchDict[[predator]] <- "switcher"
      }
    }
    for (j in 1:nrow(df)){
      preyOTU <- rownames(df)[j]
      print(paste0("Prey: ",preyOTU))
      interactionVal <- df[j,i]
      print(paste0("Binary interaction: ",interactionVal))
      if (interactionVal == 1){
        if (!(predator %in% names(interactionDict))){
          print("ERROR PREDATOR NOT IN INTERACTION DICTIONARY")
          # INSERT DIE STATEMENT
          next()
        } else {
          print(paste0("Current interaction dictionary contains: ",interactionDict[[predator]]))
          print(sum(str_count(interactionDict[[predator]], preyOTU)))
          if (sum(str_count(interactionDict[[predator]], preyOTU)) != 0){
            print("ERROR PREY ALREADY FOUND UNDER PREDATOR LOOKUP")
            # INSERT DIE STATEMENT
            next()
          } else {
            interactionDict[[predator]] <- c(interactionDict[[predator]], preyOTU)
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
  return(interactionDict)
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
    colnames(dead) <- c("")
    repz = TRUE
    i = 1
    while (repz){
      print(i)
      currentDF <- extinction(df=osDF, participant=osParticipant, method=osMethod, interactionDict=currentDict, switchDict=switchDict, extOTUdict=extOTUDict)
      currentDict <- eval_df(df=currentDF, taxaDict=currentDict)
      emptyOutList <- empty(df=currentDF, count=TRUE)
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
    deadRows <- nrow(dead)
    dead2 <- rbind(dead2, c(deadRows, numCols, numRows))
    return(dead2)
  }
  seOutput <- one_second_extinction(osDF=df, osParticipant=participant, osMethod=method, currentDict=interactionDict)
  return(seOutput)
}

library(stringr)
exampleDF[1,1] <- 50
newList <- second_extinction(df = web)
web[1,1]==1
newList$BAT0001 <- c(newList$BAT0001, "PREY1")
prey <- "PREY1"
prey
str(newList$BAT0001)
sum(str_count(newList$BAT0001, prey))
newList
predator <- "BAT0001"
append(newList[[predator]], "YOLO")
which(newList[[predator]] == "OTU_42")
#taxaDict[[predator]] <- taxaDict[[predator]][taxaDict[[predator]] != preyOTU]
newList[[predator]][newList[[predator]] != "OTU_42"]
