library(TraplineTools)
setwd('~/GitHub/SpringsJunction/TrapLines') # set appropriate working directory


## Introduction

# Read the data

tl <- read.csv('SH2019-2024.csv')

# Get appropriate month and year category
# First take away 10 days from the date
ttDate <- as.POSIXlt(tl$date)
ttDate <- as.character(ttDate-10*(24*60*60))

tl$year <- as.numeric(substring(ttDate, 1, 4))
tl$month <- as.numeric(substring(ttDate, 6, 7))

# Get rat and mustelid numbers
tl$rat <- as.numeric(tl$species.caught=='Rat')
tl$mustelid <- as.numeric(tl$species.caught %in% c('Stoat','Weasel'))
tl$total <- as.numeric(tl$species.caught %in% c('Rat','Stoat','Weasel'))

# Define zones (10 trap-box groupings)
tt <- tl[substring(tl$code, 1, 3)=='SHY', 'code']
tl[substring(tl$code, 1, 3)=='SHY', 'code'] <- paste0('SH', substring(tt, 4, nchar(tt)))

tl[nchar(tl$code)==3, 'code'] <- paste0('SH0', substring(tl[nchar(tl$code)==3, 'code'], 3, 3))
tl[nchar(tl$code)==9, 'code'] <- paste0('SH0', substring(tl[nchar(tl$code)==9, 'code'], 3, 3), ' trap2')
tl$zone <- floor(as.numeric(substring(tl$code, 3, 4))/10 + 0.95)

# Remove second trap records in doubles
tl <- tl[nchar(tl$code)==4,]

# Produce summary tables (annual catch per trap)
zoneN <- c(10,10,10,9,10,9,10,10,5)
ttSum <- array(NA, c(3,9,6), list(c('rat','mustelid','total'), 1:9, 2019:2024))
ttSum[1,,] <- tapply(tl$rat, tl[,c('zone','year')], sum)/zoneN
ttSum[2,,] <- tapply(tl$mustelid, tl[,c('zone','year')], sum)/zoneN
ttSum[3,,] <- tapply(tl$total, tl[,c('zone','year')], sum)/zoneN

ttSum

# Finally produce some plots to display this summary data
par(mar=c(3,3,0.5,2), las=1, mgp=c(2.2, 0.8, 0), xpd=TRUE)

# Rats
ttMean <- apply(ttSum[1,,], 1, mean)
plot(1:9, ttMean, xlab='Zone', ylab='Annual catch per trap', type='l', lwd=2, xlim=c(1,10),
     ylim=c(min(ttSum[1,,]), max(ttSum[1,,])), xaxt='n')
axis(1, 1:9, c('Hot\nSprings', 2:8, 'Marble\nHill'))
text(9.1, ttMean[9], 'All years', adj=0)

for (i in 1:6)
{
  lines(1:9, ttSum[1,,i], col=heat.colors(7, alpha=1)[i], lwd=1.5)
  text(9.1, ttSum[1,9,i], dimnames(ttSum)[[3]][i], col=heat.colors(7, alpha=1)[i], adj=0)
}

# Mustelids
ttMean <- apply(ttSum[2,,], 1, mean)
plot(1:9, ttMean, xlab='Zone', ylab='Annual catch per trap', type='l', lwd=2, xlim=c(1,10),
     ylim=c(min(ttSum[2,,]), max(ttSum[2,,])), xaxt='n')
axis(1, 1:9, c('Hot\nSprings', 2:8, 'Marble\nHill'))
text(9.1, ttMean[9], 'All years', adj=0)

for (i in 1:6)
{
  lines(1:9, ttSum[2,,i], col=heat.colors(7, alpha=1)[i], lwd=1.5)
  text(9.1, ttSum[2,9,i], dimnames(ttSum)[[3]][i], col=heat.colors(7, alpha=1)[i], adj=0)
}

# Total (rats and mustelids combined)
ttMean <- apply(ttSum[3,,], 1, mean)
plot(1:9, ttMean, xlab='Zone', ylab='Annual catch per trap', type='l', lwd=2, xlim=c(1,10),
     ylim=c(min(ttSum[3,,]), max(ttSum[3,,])), xaxt='n')
axis(1, 1:9, c('Hot\nSprings', 2:8, 'Marble\nHill'))
text(9.1, ttMean[9], 'All years', adj=0)

for (i in 1:6)
{
  lines(1:9, ttSum[3,,i], col=heat.colors(7, alpha=1)[i], lwd=1.5)
  text(9.1, ttSum[3,9,i], dimnames(ttSum)[[3]][i], col=heat.colors(7, alpha=1)[i], adj=0)
}


