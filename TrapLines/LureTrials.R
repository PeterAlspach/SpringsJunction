# ---
#   title: "Maruia Trap-lines: Monthly Summary"
# author: "Peter Alspach"
# date: "`r format(Sys.time(), '%d %B, %Y')`"
# output:
#   pdf_document:
#   # html_document:
#   # html_notebook:
#   theme: spacelab
# toc: yes
# toc_depth: 5
# # toc_float: yes
# # code_folding: hide
# ---
  

# ```{r setup, include=FALSE}
# knitr::opts_chunk$set(echo=FALSE, cache=FALSE, error=TRUE)
# library(knitr)
# library(kableExtra)
# library(jpeg)
library(TraplineTools)
# 
setwd('~/GitHub/SpringsJunction/TrapLines') # set appropriate working directory
# ```
# ## Introduction
# 
# ```{r readData}
# Read the data
tl <- getTrapData(dstOffset=TRUE)
yr <- tl$yr
tl <- tl$tl
# ```
# 
# ```{r getReady}
# Set the cut-off dates for the monthly intervals
lastDate <- sort(tl$Date.Checked, decreasing=TRUE)[1]
beginAt <- 8 # Good options are 10/11 and 22/23
obsMth <- setCutDates(beginAt, yr, tl$jDay)
mth <- max(as.numeric(obsMth), na.rm=TRUE)

# Set the standard trap status of interest
stdStat <- c('Sprung and Empty','Rat','Stoat','Weasel') # standard trap status
allMammals <- c('Rat','Stoat','Weasel','Cat','Ferret','Mouse','Rabbit','Hedgehog','Hare','Possum')

# Next compute the number of traps per line (irrespective of whether some are missing)
nTrap <- table(tl$Line, gsub('([[:upper:]]+)([[:digit:]]+)', '\\2', tl$Tag.No))
nTrap <- apply(nTrap, 1, function(x) length(x[x!=0]))
# ```

scent1 <- grep(paste('[[:digit:]]', c('Fish|','Blood|','Meat|','Flax|','Chicken|','Rabbit'), collapse=''), tl$Record.Comment)
scent1Trt <- tl[scent1, c('Tag.No','Record.Comment')]
scent1Trt$Rep <- substring(scent1Trt$Record.Comment, 1, 1)
scent1Trt$Trt <- substring(scent1Trt$Record.Comment, 3)
scent1Data <- tl[(tl$Tag.No %in% tl[scent1, 'Tag.No']) & (tl$Date.Checked > tl[scent1, 'Date.Checked'][1]),]
scent1Data <- scent1Data[, c('Tag.No','Trap.1','Record.Comment','User.ID','jDay')]
scent1Data <- scent1Data[!duplicated(apply(scent1Data, 1, paste, collapse=':')),]
scent1Data <- merge(scent1Data, scent1Trt[, -2], by='Tag.No')
with(scent1Data[scent1Data$Trap.1!='Still Set',], table(Trt, Trap.1, jDay))

comp1 <- tl[(tl$Tag.No %in% c(paste0('LD0', 1:6), paste0('LD', 56:60))) & (tl$Date.Checked > tl[scent1, 'Date.Checked'][1]),]
comp1 <- comp1[, c('Tag.No','Trap.1','Record.Comment','User.ID','jDay')]
comp1 <- comp1[!duplicated(apply(comp1, 1, paste, collapse=':')),]
with(comp1[comp1$Trap.1!='Still Set',], table(Trap.1, jDay))

# scent1Data <- reshape(scent1Data, v.names=names(scent1Data)[2:4], idvar='Tag.No', timevar='jDay', direction='wide')

scent2 <- grep('Fat salmon|^Rabbit', tl$Record.Comment)
scent2Trt <- tl[scent2, c('Tag.No','Record.Comment')]
scent2Data <- tl[(tl$Tag.No %in% tl[scent2, 'Tag.No']) & (tl$Date.Checked > tl[scent2, 'Date.Checked'][1]),]
scent2Data <- scent2Data[, c('Tag.No','Trap.1','Record.Comment','User.ID','jDay')]
scent2Data <- scent2Data[!duplicated(apply(scent2Data, 1, paste, collapse=':')),]
scent2Data[scent2Data$Trap.1!='Still Set',]
scent2Data <- merge(scent2Data, scent2Trt, by='Tag.No')
names(scent2Data)[ncol(scent2Data)] <- 'Treatment'
with(scent2Data[scent2Data$Trap.1!='Still Set',], table(Treatment, Trap.1, jDay))
# scent2Data <- reshape(scent2Data, v.names=names(scent2Data)[2:4], idvar='Tag.No', timevar='jDay', direction='wide')

NvR <- grep('New lure|Recharged lure', tl$Record.Comment)
NvRTrt <- tl[NvR, c('Tag.No','Record.Comment')]
NvRData <- tl[(tl$Tag.No %in% tl[NvR, 'Tag.No']) & (tl$Date.Checked > tl[NvR, 'Date.Checked'][1]),]
NvRData <- NvRData[, c('Tag.No','Trap.1','Record.Comment','User.ID','jDay')]
NvRData <- NvRData[!duplicated(apply(NvRData, 1, paste, collapse=':')),]
NvRData <- merge(NvRData, NvRTrt, by='Tag.No')
names(NvRData)[ncol(NvRData)] <- 'Treatment'
NvRData[grep('New', NvRData$Treatment), 'Treatment'] <- 'New Lure'
NvRData[grep('Recharged', NvRData$Treatment), 'Treatment'] <- 'Recharged Lure'
NvRData[grep('tual', NvRData$Record.Comment.x), 'Trap.1'] <- 'Still Set' # remove catches from traps 31, 77 and 78
with(NvRData[NvRData$Trap.1!='Still Set',], table(Treatment, Trap.1, jDay))
# NvRData <- reshape(NvRData, v.names=names(NvRData)[2:4], idvar='Tag.No', timevar='jDay', direction='wide')

