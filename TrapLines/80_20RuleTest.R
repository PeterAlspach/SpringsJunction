# This code will eventually check the 80:20 rule
library(TraplineTools)

tl <- getTrapData()$tl # current year's data
tlPY <- getTrapData('TrapCatches2019.csv')$tl # previous year's data
tl <- rbind(tlPY, tl) # combine the two years 

tl$Trap.No <- paste(tl$Tag.No, tl$Trap.No, sep=':') # combine Tag and Trap numbers
ttTab <- as.matrix(table(tl$Trap.No, tl$Trap.1)) # tabulate catches
# Get total catch of selected animals
catch <- apply(ttTab[, c('Cat','Ferret','Mouse','Possum','Rabbit','Rat','Stoat','Weasel')], 1, sum, na.rm=TRUE)
catch <- catch[order(catch, decreasing = TRUE)] # sort from highest to lowest
catch <- catch[catch!=0] # remove traps with no catches over the period as these generally weren't working
cCatch <-  cumsum(catch) # cumulate

# plot(cCatch, type='l')
rule <- cbind(10*(1:9), round(100*cCatch[round((1:9)*length(cCatch)/10)]/max(cCatch), 0))
(rule <- cbind(rule, 100-rule[,2]))
# That is, the best 20% caught 30% of the animals and the worst 20% caught only 9%

# What could one expect by chance if all traps have an equal probability of catching an animal
pC <- mean(catch)/15 # probability of catch during 15 month period
ttRes <- array(dim=c(9,2,1000))
for (i in 1:dim(ttRes)[3])
{
  ttCatch <- rbinom(length(catch), 15, pC)
  ttCatch <- ttCatch[order(ttCatch, decreasing=TRUE)]
  ttCatch <- cumsum(ttCatch)
  ttRule <- round(100*ttCatch[round((1:9)*length(ttCatch)/10)]/max(ttCatch), 0)
  ttRes[,,i] <- cbind(ttRule, 100-ttRule)
}
c(min(ttRes[2,1,]), max(ttRes[2,1,]), min(ttRes[8,1,]), max(ttRes[8,1,]))
# That is, based on chance alone one would expect the best 20% to catch somewhere between 27 and 29% of
# the animals and the worst 20% between 11 and 13%