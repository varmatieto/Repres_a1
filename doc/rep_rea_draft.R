
## Reading data
AMD <- read.table("~/GitHub/RepData_PeerAssessment1/data/activity.csv",
                      header = TRUE,
                      sep = ",",
                      dec = ".",
                      stringsAsFactors = FALSE)


str(AMD)                   # 17586 obs. in three var
## Parse time data
AMD$date <- as.Date(AMD$date, "%Y-%m-%d")
# AMD$interval <- as.factor(AMD$interval)
str(AMD)

length(unique(AMD$date )) #61 days 
length(unique(AMD$interval )) #288 interval in a days 

24*60/288 # a interval each 5 minutes

max(AMD$date)- min(AMD$date)  #Time difference of 60 days

head (AMD)

AMD[1:23,]

mean(is.na(AMD$steps[AMD$date=="2012-10-02"]))

#######question one ############################################################

library (plyr)

AMDday<-ddply(AMD, .(date), summarise,
             d_na=mean(is.na(steps)),
             dtot= sum(steps, na.rm = T),
             dmean=mean(steps, na.rm = T))

str(AMDday)

table(AMDday$d_na) # 8 days with NA
summary(AMDday$dmean)


library (ggplot2)

ggplot(data=AMDday, aes(x=date, y= dtot, fill= "#FF0000")) + 
    geom_bar(stat="identity",show_guide = F ) +
    ggtitle("total steps by day") + 
    xlab("Date") + ylab( "n. steps")
ggsave(file="plot/plot1.jpeg", dpi=72)

mean (AMDday$dtot) # 9354
median (AMDday$dtot) # 10395

AMDday$date[AMDday$dtot==max(AMDday$dtot)]

#######question two ############################################################

AMDint<-ddply(AMD, .(interval), summarise,
            i_na=mean(is.na(steps)),
            itot= sum(steps, na.rm = T),
            imean=mean(steps, na.rm = T))

str(AMDint)

table(AMDint$i_na) # 8 days with NA
summary(AMDint$imean)
summary(AMDint$interval)


    
head(AMDint)

plot(imean ~ interval, AMDint, xaxt = "n", type = "l")


ggplot(data=AMDint, aes(x=interval, y= imean)) +
    geom_line(size=1.3,color= "#FF00CC") +
    ggtitle("mean steps per interval ") + 
    xlab("interval") + ylab( "n. steps")
ggsave(file="plot/plot2.jpeg", dpi=72)


AMDint$interval[AMDint$imean==max(AMDint$imean)]

#######question three ############################################################

sum(is.na(AMD$steps)) # 2304

sum(!complete.cases(AMD$steps)) # 2304

288*8

NAdays<- unique (AMD$date[is.na(AMD$steps)] ) # in 8 days
nNAdays<-length( unique (AMD$interval[is.na(AMD$steps)] ) )# in all intervals 

AMDna <- AMD
AMDna$steps[!complete.cases(AMD$steps) ] <- 1
AMDna$steps[complete.cases(AMD$steps) ] <- 0

ggplot(data=AMDna, aes(x=date, y= steps, color= "#FF0000")) + 
    geom_point(stat="identity",show_guide = F, size = 3 ) +
    ggtitle("where are NA ") + 
    xlab("Date") + ylab( "NA is 1")
ggsave(file="plot/plot3.jpeg", dpi=72)

AMDna <- AMD[!complete.cases(AMD$steps), ]
str(AMDna)

unique (AMDna$date)
summary (AMDna$steps)

AMD[AMD$date=="2012-10-01",][1:10 ,3] 
AMD$interval[AMD$date=="2012-10-01"][1]

str(NAdays)
str(AMDf)

AMDf <- AMD
for (i in NAdays) {
    
    for (j in 1:288){

    AMDf$steps[AMD$date==i][j]<-AMDint$imean[j]
    }
    
}

AMDf <- AMD
for (i in NAdays) {
    
    
        
        AMDf$steps[AMD$date==i]<-AMDint$imean
 
    
}

sum(is.na(AMDf$steps)) # 2304

sum(!complete.cases(AMDf$steps)) # 2304



AMDfday<-ddply(AMDf, .(date), summarise,
              d_na=mean(is.na(steps)),
              dtot= sum(steps, na.rm = F),
              dmean=mean(steps, na.rm = F))

summary(AMDfday$d_na)
str(AMDfday)
AMDfday$d_na[AMDfday$date%in%NAdays]<-1

table(AMDfday$d_na) # 8 days with NA
summary(AMDfday$dmean)


ggplot(data=AMDfday, aes(x=date, y= dtot, fill= factor(d_na))) + 
    geom_bar(stat="identity", show_guide = F ) +
    ggtitle("total steps by day (no NA)") + 
    xlab("Date") + ylab( "n. steps")

ggsave(file="plot/plot31.jpeg", dpi=72)


mean (AMDfday$dtot)
median (AMDfday$dtot)


#######question four ############################################################


daysinweek<-weekdays(unique(AMD$date)[1:7])
wdays<- daysinweek[1:5]
edays<- daysinweek[6:7]

whatday<- 0

str(AMDf)
str(AMD)

AMDfwe<- cbind (AMDf, whatday)
str(AMDfwe)

isworking <- weekdays(AMDfwe$date)%in%wdays

AMDfwe$whatday[isworking]<- 1


# AMDfwe$whatday<-as.factor(AMDfwe$whatday)

AMDfw <- subset(AMDfwe, whatday ==  1 )
AMDfe <- subset(AMDfwe, whatday ==  0 )

weekdays(unique(AMDfw$date)[1:7])

AMDfwint<-ddply(AMDfw, .(interval), summarise,
             flag=1,
            imean=mean(steps, na.rm = T))

str(AMDfwint)

summary(AMDfwint$imean)
summary(AMDfwint$interval)

head(AMDfwint)

AMDfeint<-ddply(AMDfe, .(interval), summarise,
                flag=0,
                imean=mean(steps, na.rm = T))

str(AMDfeint)

summary(AMDfeint$imean)
summary(AMDfeint$interval)

head(AMDfeint)

AMDfweint<-rbind(AMDfeint, AMDfwint)

str(AMDfweint)

AMDfweint$flag <- as.factor (AMDfweint$flag)
str(AMDfweint)

levels(AMDfweint$flag) <- c("weekday" ,"weekend" )

plot(imean ~ interval, AMDfweint, xaxt = "n", type = "l")


ggplot(data=AMDfweint, aes(x=interval, y= imean, color=flag)) +
    geom_line(size=1.3, show_guide = F  ) +
    facet_grid(flag ~ .) +
    ggtitle("mean steps per interval ") + 
    xlab("interval") + ylab( "n. steps")


ggsave(file="plot/plot41.jpeg", dpi=72)

ggplot(data=AMDfweint, aes(x=interval, y= imean, color=flag)) +
    geom_line(size=1.3, show_guide = F  ) +
    facet_wrap(~ flag , ncol=1) +
    ggtitle("mean steps per interval ") + 
    xlab("interval") + ylab( "n. steps")


ggsave(file="plot/plot42.jpeg", dpi=72)

