rainfall <- read.csv('Rainfall_depth.csv', skip=1)
rainfall$date <- as.Date(substring(rainfall[,1], 1, 8), format='%d/%m/%y')
names(rainfall) <- c('dateTime','rain')
head(rainfall)
rainfall <- rainfall[nrow(rainfall):1,]
rainfall$time <- substring(rainfall[,1], 10)

tt <- rle(rainfall$time)$lengths
rainfall$toChk <- rep(tt, tt)
rainfall[rainfall$toChk>2,]

aggRain <- aggregate(rainfall$rain, list(rainfall$date), sum)
aggChkd <- aggregate(rainfall[rainfall$toChk <= 2,'rain'], 
                     list(rainfall[rainfall$toChk <= 2,'date']), sum)
with(aggRain, barplot(x))
with(aggChkd, barplot(x))
