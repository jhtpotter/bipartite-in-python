


eval_df <- function(df, taxadict){
  predators = colnames(df)
  print("predators:", predators)
  preyOTUs = rownames(df)
  print("prey OTUs:", preyOTUs)
  for (i in 1:ncol(df)){
    colvalues <- numeric()
    predator <- predators[i]
    preygained <- 0
    preylost <- 0
    if (!(predator %in% names(taxadict))){
      # INSERT DIE STATEMENT
      next()
    }
    preybreadth <- length(taxadict$predator) - 1
    for (j in 1:nrow(df)){
      preyOTU <- preyOTU[j]
      interactionval <- df[j, i]
      if (interactionval == 1){
        append(colvalues, interactionval)
        if (length(which(taxadict$predator==preyOTU)) == 0){
          append(taxadict$predator,preyOTU)
          preygained <- preygained + 1
        } else if (length(which(taxadict$predator==preyOTU)) == 1){
          next()
        } else {
          # INSERT DIE STATEMENT
          next()
        }
      } else if (interactionval == 0){
        append(colvalues, interactionval)
        if (length(which(taxadict$predator==preyOTU)) == 0){
          next()
        } else if (length(which(taxadict$predator==preyOTU)) == 1){
          taxadict$predator <- taxadict$predator[taxadict$predator != preyOTU]
          preylost <- preylost + 1
        } else {
          # INSERT DIE STATEMENT
          next()
        }
      } else {
        # INSERT DIE STATEMENT
        next()
      }
    }
    colvalsum <- sum(colvalues)
    if (colvalsum != length(taxadict$predator) - 1){
      # INSERT DIE STATEMENT
      next()
    }
    if (colvalsum != preybreadth + preygained - preylost){
      # INSERT DIE STATEMENT
      next()
    }
    taxadict$predator[1] = colvalsum
  }
}

empty <- function(df, count=FALSE){
  numcols = ncol(df)
  numrows = nrow(df)
  numemptycols = 0
  numemptyrows = 0
  emptycolIDs = which(colSums(df)==0)
  print(emptycolIDs)
  emptyrowIDs = which(rowSums(df)==0)
  print(emptyrowIDs)
  df <- df[,!emptycolIDS]
  df <- df[!emptyrowIDs,]
  for (i in 1:numrows){
    row = df[i,]
  }
}

extinction <- function(df, participant, method, interactionDict, switchDict, extOTUdict){
  numcols = ncol(df)
  numrows = nrow(df)
  rowsumVec = rowSums(df)
  colsumVec = colSums(df)
  if (participant == "both" && method == "random"){
    # Randomly pick a participant:
    partIdx = sample(1:2, 1)
    participant = participants[partIdx]
    print("Participant is now:", participant)
  }
  if (method == "random"){
    rowextin = sample(1:numrows)
    colextin = sample(1:numcols)
    if (participant == "lower"){
      for (i in 1:numcols){
        df[rowextin,i] = 0
      }
    }
    if (participant == "higher"){
      for (j in 1:numrows){
        df[j,colextin] = 0
      }
    }
  }
  else if (method == "abundance"){
    rowsumVec = numeric()
    colsumVec = numeric()
    df = df[sample(1:nrow(df),nrow(df)),]
    df = df[,sample(1:ncol(df),ncol(df))]
    rowsumVec = rowSums(df)
    colsumVec = colSums(df)
    minCol = which(colsumVec == min(colsumVec))
    minRow = which(rowsumVec == min(rowsumVec))
    print(minCol,minRow)
    if (participant == "lower"){
      extOTU = rownames(df[minRow,])
      if (extOTU %in% names(extOTUdict)){
        # INSERT DIE STATEMENT
        next()
      }
      else {
        extOTUdict <- c(extOTUdict, extOTU=1)
      }
      for (i in 1:ncol(df)){
        currentPredator = colnames(df[,i])
        nonpreyidxs = which(df[,i]==0 && !(rownames(df) %in% extOTUdict))
        print(nonpreyidxs)
        if (switchDict$currentPredator == "swticher" && length(nonpreyidxs) > 0){
          newprey = nonpreyidxs[sample(1:length(nonpreyidxs),1)]
        }
      }
    }
  }
  
}

