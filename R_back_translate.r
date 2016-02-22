


eval_df <- function(df, taxadict){
  predators = colnames(df)
  preyOTUs = rownames(df)
  for (i in 1:ncol(df)){
    colvalues <- numeric()
    predator <- predators[i]
    preygained <- 0
    preylost <- 0
    if (! predator %in% names(taxadict)){
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
          next()
        }
      } else {
        next()
      }
    }
    colvalsum <- sum(colvalues)
    if (colvalsum != length(taxadict$predator) - 1){
      next()
    }
    if (colvalsum != preybreadth + preygained - preylost){
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
  emptyrowIDs = which(rowSums(df)==0)
  df <- df[,!emptycolIDS]
  df <- df[!emptyrowIDs,]
  for (i in 1:numrows){
    row = df[i,]
  }
}