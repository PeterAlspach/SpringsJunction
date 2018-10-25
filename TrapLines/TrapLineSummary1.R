# Set the working directory and read the data
setwd('~/GitHub/SpringsJunction/TrapLines')
tl <- read.csv('traplines.csv', header=TRUE, sep=',', stringsAsFactors=FALSE)

# Rename traps with only a single digit 'd' to '0d'
relNo <- tl[grep('[[:upper:]][[:digit:]]{1}$', tl$Tag.No), 'Tag.No']
relNo <- paste0(substring(relNo, 1, nchar(relNo)-1), '0', substring(relNo, nchar(relNo)))
tl[grep('[[:upper:]][[:digit:]]{1}$', tl$Tag.No), 'Tag.No'] <- relNo

# Remove time from date stamp
tl$Date.Checked <- substring(tl$Date.Checked, 1, 10)
tl$jDay <- julian.Date(as.Date(tl$Date.Checked), origin=as.Date('2017-12-31'))

# Line name and code
ttTab <- table(tl$Line, gsub('([[:upper:]]+)([[:digit:]]+)', '\\1', tl$Tag.No))
apply(ttTab, 1, function(x) dimnames(ttTab)[[2]][x!=0])

# Proportion of traps with pest by time
cutDays <- paste('2018', c(paste0('0', 1:9), 10:12), '15', sep='-') # cut at the middle of the month 
cutDays <- julian.Date(as.Date(cutDays), origin=as.Date('2017-12-31'))
obsMth <- cut(tl$jDay, c(0, cutDays, 365))
levels(obsMth) <- c('Jan', 'J-F', 'F-M', 'M-A','A-M', 'M-J', 'J-J',
                    'J-A', 'A-S', 'S-O', 'O-N', 'N-D', 'Dec')
table(gsub('([[:upper:]]+)([[:digit:]]+)', '\\1', tl$Tag.No), obsMth)

ttTots <- table(tl$Line, obsMth)
ttRat <- tapply(tl$Trap.1, list(tl$Line, obsMth), function(x) length(x[x=='Rat']))
ttMust <- tapply(tl$Trap.1, list(tl$Line, obsMth), 
                 function(x) length(x[x %in% c('Stoat','Weasel','Ferret')]))
ttPest <- tapply(tl$Trap.1, list(tl$Line, obsMth),
                 function(x) length(x[x %in% c('Rat','Mouse','Cat','Stoat','Weasel','Ferret')]))

round(100*ttRat/ttTots, 1)
round(100*ttMust/ttTots, 1)
round(100*ttPest/ttTots, 1)


all.equal(dimnames(ttTots), dimnames(ttRat))
tl[1,]
table(tl$Trap.1) # total trap catches (excluding double catch)
table(tl$Trap.2) # catches on the second trap
dim(table(tl$Tag.No))
