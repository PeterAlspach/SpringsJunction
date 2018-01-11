rainfall <- read.csv('Rainfall_depth.csv', skip=1, stringsAsFactors=FALSE)
rainfall$date <- as.Date(substring(rainfall[,1], 1, 8), format='%d/%m/%y')
names(rainfall) <- c('dateTime','rain')
rainfall <- rainfall[nrow(rainfall):1,]
rainfall$time <- substring(rainfall[,1], 10)

tt <- rle(as.character(rainfall$dateTime))$lengths
rainfall$toChk <- rep(tt, tt)

aggRain <- aggregate(rainfall$rain, list(rainfall$date), sum)
aggChkd <- aggregate(rainfall[rainfall$toChk <= 1,'rain'], 
                     list(rainfall[rainfall$toChk <= 1,'date']), sum)
with(aggRain, barplot(x, ylab='Daily rainfall (mm)', space=0))
axis(2, c(0, max(aggRain[,2])), c('',''), tck=0)
mthBrks <- cumsum(rle(substring(aggRain[,1], 1, 7))$lengths)
axis(1, mthBrks[-length(mthBrks)], rep('', length(mthBrks)-1), pos=0) 
text((mthBrks+c(0, mthBrks[-length(mthBrks)]))/2,
     par()$usr[3]-diff(par()$usr[3:4])/20,
     month.name[as.numeric(substring(mthRain[,1], 6))],
     xpd=TRUE, cex=0.9)
text((mthBrks+c(0, mthBrks[-length(mthBrks)]))/2,
     par()$usr[4], round(mthRain[,2]), xpd=TRUE, cex=0.9)
segments(mthBrks, 0, mthBrks, par()$usr[4], lty='dotted')


library('rvest')

#Specifying the url for desired website to be scrapped
# url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'
url <- "http://data.wcrc.govt.nz/cgi-bin/HydWebServer.cgi/points/details?point=597"
url1 <- "http://data.wcrc.govt.nz/cgi-bin/HydWebServer.cgi/points/details?point=597&input"
#Reading the HTML code from the website
webpage <- read_html(url)
webpage1 <- read_html(url1)
all.equal(webpage, webpage1)
webpage1[[2]]
rank_data_html <- html_nodes(webpage,'#sdt')

#Converting the ranking data to text
html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)
html_form(webpage)
names(webpage)
html_node(webpage, 'sdt')
is.character(url) <- TRUE
s <- html_session(url)
s %>% follow_link('ínput')
