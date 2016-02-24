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
  print("predators:", predators)
  preyOTUs <- rownames(df)
  print("prey OTUs:", preyOTUs)
  for (i in 1:ncol(df)){
    colValues <- numeric()
    predator <- predators[i]
    preyGained <- 0
    preyLost <- 0
    if (!(predator %in% names(taxaDict))){
      # INSERT DIE STATEMENT
      next()
    }
    preyBreadth <- length(taxaDict$predator) - 1
    for (j in 1:nrow(df)){
      preyOTU <- preyOTU[j]
      interactionVal <- df[j, i]
      if (interactionVal == 1){
        append(colValues, interactionVal)
        if (length(which(taxadict$predator==preyOTU)) == 0){
          append(taxadict$predator,preyOTU)
          preyGained <- preyGained + 1
        } else if (length(which(taxadict$predator==preyOTU)) == 1){
          next()
        } else {
          # INSERT DIE STATEMENT
          next()
        }
      } else if (interactionVal == 0){
        append(colValues, interactionVal)
        if (length(which(taxadict$predator==preyOTU)) == 0){
          next()
        } else if (length(which(taxadict$predator==preyOTU)) == 1){
          taxadict$predator <- taxadict$predator[taxadict$predator != preyOTU]
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
    if (colValSum != length(taxadict$predator) - 1){
      # INSERT DIE STATEMENT
      next()
    }
    if (colValSum != preyBreadth + preyGained - preyLost){
      # INSERT DIE STATEMENT
      next()
    }
    taxadict$predator[1] <- colValSum
  }
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
        extOTUdict <- c(extOTUdict, extOTU=1)
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
}






