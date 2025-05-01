function(trapData=tl, recent=TRUE, mammals=allMammals,
                                  stat=stdStat, nBoxes=nBox, lineNames=NULL)
{
  if (is.null(lineNames)) lineNames <- unique(trapData$line)

  # Subset trapData to the current month if currMth is non-NULL
  if (!is.null(currMth))
  {
    trapData <- trapData[mthInt==levels(mthInt)[currMth],]
    mthInt <- mthInt[mthInt==levels(mthInt)[currMth]]

    # Recompute nBox (i.e., number of boxes with functioning traps) for the month
    cM1 <- trapData[!(trapData$status=='Trap gone' | regexpr('miss', trapData$notes, ignore.case=TRUE)>0),]
    nBoxes <- with(cM1[-grep(' trap2', cM1$code),], table(line, gsub('([[:upper:]]+)([[:digit:]]+)', '\\2', code)))
    nBoxes <- apply(nBoxes, 1, function(x) length(x[x!=0]))
  }

  # Tabluate the catches (species.caught) by line, lump some species, and get total
  tt <- trapData[,c('line','species.caught','status')]
  tt[tt$species.caught=='None' & tt$status=='Sprung', 'species.caught'] <- 'Sprung'
  catch <- table(tt$line, tt$species.caught)

  catch <- catch[, match(unique(c(stat, mammals)), colnames(catch))]
  catch <- cbind(catch[, 1:length(stat)],
                 Other=apply(catch[, -(1:length(stat))], 1, sum, na.rm=TRUE))
  catch <- cbind(catch, Total=apply(catch[, -1], 1, sum, na.rm=TRUE))

  # Add in the number of traps per line (irrespective of whether some are missing if entire year)
  catch <- merge(nBoxes, catch, by='row.names', all=TRUE)
  colnames(catch)[2:3] <- c('nTrap','Sprung')

  rownames(catch) <- catch$Row.names
  catch <- catch[,-1]
  # Replace missing values with 0 (necessary as missing values are put if the pest wasn't trapped in the period)
  catch[is.na(catch)] <- 0

  # Ensure all lines are represented (necessary in some months lines are missed)
  catch <- catch[lineNames,]
  rownames(catch) <- lineNames

  # Add total line
  catch <- rbind(catch, apply(catch, 2, sum, na.rm=TRUE))
  rownames(catch)[nrow(catch)] <- 'TOTAL'

  # Add numbers caught per trap per month (x 100)
  nMth <- length(unique(mthInt))
  catch <- cbind(catch, round(apply(catch[,-1], 2, function(x) 100*x/(catch[,1]*nMth)), 1))

  catch[nrow(catch), ncol(catch)] <- round(100*catch[nrow(catch), 'Total']/
                                             (sum(catch[-nrow(catch), 'nTrap'][!is.na(catch[,'Total'])], na.rm=TRUE)*nMth), 1)

  # Put lines in alphabetical order
  catch <- catch[order(rownames(catch)),]

  return(catch)
}