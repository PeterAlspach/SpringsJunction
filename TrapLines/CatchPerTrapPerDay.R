# This is a trial bit of code to look at catch per trap per day.  There is not intention to develop it as the
# work to date indicates that it doesn't make a substantial amount of difference to the overall monthly summary

library(TraplineTools)
setwd('~/GitHub/SpringsJunction/TrapLines') # set appropriate working directory
options(stringsAsFactors = FALSE)

tl <- getTrapData(dstOffset=TRUE)$tl
tlPY <- getTrapData('TrapCatches2019.csv', dstOffset=TRUE)$tl
tl <- tl[order(tl$Line, tl$Tag.No, tl$Date.Checked),] # not necessary but safer to do so
tlPY <- tlPY[order(tlPY$Line, tlPY$Tag.No, tlPY$Date.Checked),] # not necessary but safer to do so



ttPY <- tlPY[cumsum(rle(tlPY$Tag.No)$lengths),]
tt <- rbind(ttPY, tl)
tt <- tt[order(tt$Line, tt$Tag.No, tt$Date.Checked),]

TT <- tapply(as.Date(tt$Date.Checked), tt$Tag.No, diff)
tl$dyInt <- as.numeric(unlist(TT))
tl$catch <- apply(tl[,c('Trap.1','Trap.2')], 1, function(x) length(x[x %in% allMammals]))
tl$cpd <- tl$catch/tl$dyInt

ttAgg <- cbind(aggregate(tl$cpd, tl[,c('jDay','Line')], sum), n=aggregate(tl$cpd, tl[,c('jDay','Line')], length)[,3],
               obsMth=as.character(aggregate(obsMth, tl[,c('jDay','Line')], unique)[,3]))
ttRLE <- rle(ttAgg[,'obsMth'])$lengths
ttAgg <- cbind(ttAgg, ctd=ttAgg[,'x']/ttAgg[,'n'], nChks=rep(ttRLE, ttRLE))
TTAgg <- aggregate(ttAgg[,c('x','n')], list(ttAgg[,'obsMth']), sum)
mthInt <- (1:nlevels(obsMth))[match(TTAgg[,'Group.1'], levels(obsMth))]
TTAgg <- TTAgg[order(mthInt),]
plot(30*TTAgg[,'x']/528,
     cex=TTAgg[,'n']/528, pch=16,
     ylim=c(0, 30*max(TTAgg[,'x'])/528),
     ylab='Catch per trap per day (x30)', xlab='Motnhly interval')
lines(30*TTAgg[,'x']/528)

# Essentially the same pattern as what is currently being done