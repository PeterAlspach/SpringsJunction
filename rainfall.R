rainfall <- read.csv('Rainfall_depth.csv', skip=1, stringsAsFactors=FALSE)
rainfall$date <- as.Date(substring(rainfall[,1], 1, 8), format='%d/%m/%y')
names(rainfall) <- c('dateTime','rain')
rainfall <- rainfall[nrow(rainfall):1,]
rainfall$time <- substring(rainfall[,1], 10)

tt <- rle(rainfall$dateTime)$lengths
rainfall$toChk <- rep(tt, tt)
rainfall[rainfall$toChk>2,]

aggRain <- aggregate(rainfall$rain, list(rainfall$date), sum)
aggChkd <- aggregate(rainfall[rainfall$toChk <= 2,'rain'], 
                     list(rainfall[rainfall$toChk <= 2,'date']), sum)
with(aggRain, barplot(x, ylab='Daily rainfall (mm)', space=0))
axis(2, c(0, max(aggRain[,2])), c('',''), tck=0)
mthBrks <- cumsum(rle(substring(aggRain[,1], 1, 7))$lengths)
axis(1, mthBrks[-length(mthBrks)], rep('', length(mthBrks)-1), pos=0) 
with(aggChkd, barplot(x))
text((mthBrks+c(0, mthBrks[-length(mthBrks)]))/2, par()$usr[3], 1:4)
