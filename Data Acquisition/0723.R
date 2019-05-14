library(ggplot2)
library(Hmisc)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(grid)
library(scales)

### Hourly usage per person
event0723a = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/events 0723.csv", header=TRUE)
myevent0723a = event0723a[,-c(1,3,5,8,9,15:16)]
myevent0723a$ParticipantID = as.character(myevent0723a$ParticipantID)
myevent0723a = myevent0723a[which(myevent0723a$ParticipantID!="UU-FEAP-VHNA-SXPV-MXJS-ID"&
                                  myevent0723a$ParticipantID!="UU-ROSR-QDTY-HZGI-UHRJ-ID"),]

myevent0723a$Applications="NULL"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.facebook.katana"|
                                 myevent0723a$AppPackage=="com.facebook.lite")] <- "Facebook"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.facebook.orca")] <-"FB Messenger"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.google.android.youtube")] <-"YouTube"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.instagram.android")] <-"Instagram"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.reddit.frontpage")] <-"Reddit"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.whatsapp")] <-"WhatsApp"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.twitter.android")] <-"Twitter"
myevent0723a$Applications[which(myevent0723a$AppPackage=="jp.naver.line.android")] <-"LINE"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.tumblr")] <-"Tumblr"
myevent0723a$Applications[which(myevent0723a$AppPackage=="com.yahoo.mobile.client.android.flickr")] <-"Flickr"
myevent0723a = myevent0723a[which(myevent0723a$Applications!="NULL"),]
#head(myevent0723a)
#describe(myevent0723a)

names(myevent0723a)[1] = "ParticipantID"
myevent0723a[,"Date"] = format(ymd_hms(myevent0723a$TimestampHumanReadableWithTZ),
                              tz = "Europe/London", "%m/%d/%Y %H:%M")
myevent0723a[,"Hour"] = hour(myevent0723a$TimestampHumanReadableWithTZ)
myevent0723a[,"Minute"] = minute(myevent0723a$TimestampHumanReadableWithTZ)
myevent0723a$UserID = "NULL"
for(i in 1:length(unique(myevent0723a$ParticipantID))){
  myevent0723a$UserID[which(myevent0723a$ParticipantID==paste0(unique(myevent0723a$ParticipantID)[i]))] <- substr(paste0(unique(myevent0723a$ParticipantID)[i]),4,7)
}

#names(myevent0723a)
myevent0723a = myevent0723a[which(myevent0723a$ScreenOn=="true"),]

day0723a = myevent0723a[,-c(2,4,10)]
#str(day0723a)
day0723a$DurationMilliSec = as.numeric(as.character(day0723a$DurationMilliSec))
day0723a = day0723a[which(day0723a$DurationMilliSec>1000),]
day0723a$DurationSec = day0723a$DurationMilliSec*0.001
day0723a$DurationMin = day0723a$DurationSec/60

#########
### Visualisatoin
userlist=list(); usage=list(); customtext=c(); barhourplots=list()
for(i in 1:length(unique(day0723a$UserID))){
  userlist[[i]] = day0723a[which(day0723a$UserID == paste0(unique(day0723a$UserID))[i]),]
  usage[[i]] = summarise(group_by(userlist[[i]], Applications, UserID), Total = sum(DurationMin))
  customtext[i] = round(sum(userlist[[i]][,"DurationMin"]),2)
  usage[[i]][,"ActiveMin"] = customtext[i]
  userlist[[i]]$Time = as.POSIXct(userlist[[i]]$Date, format="%m/%d/%Y %H:%M")
  temp = group_by(userlist[[i]], UserID, Applications, Hour) %>%
    summarise(Usage = sum(DurationMin))
  temp = data.frame(temp)
  temp$Hour = factor(temp$Hour, levels=c("21","22","23","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
  t1 = data.frame(summarise(group_by(temp,Hour), Usage=sum(Usage)))
  t1$UserID = paste(userlist[[i]]$UserID[1])
  t1$Applications = "ID Activity"
  t2 = rbind(t1,temp)
  t2$Applications = factor(t2$Applications, levels=c("Facebook","FB Messenger","YouTube",
                                                     "Instagram","Reddit","WhatsApp","Twitter",
                                                     "LINE","Tumblr","Flickr","ID Activity"))
  cols = c("#892e82","#0f0fbc","#298b0d","#fad304","#ff6600","slategray4","aquamarine")
  
  barhourplots[[i]] = 
    ggplot(t2, aes(x=Hour, y=Usage)) +
    geom_bar(aes(fill=Applications), stat="identity", position = "stack") +
    #coord_flip() +
    labs(y="Minutes", title="Hourly Usage over 24 hour period\nbetween 22/07 21:00 to 23/07 21:00",
         subtitle = paste(userlist[[i]]$ParticipantID[1])) +
    scale_x_discrete(limits=c("21","22","23","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")) +
    scale_fill_manual(values = cols) +
    theme(text = element_text(size=30, face="bold"),
          plot.title = element_text(hjust=0),
          plot.subtitle = element_text(colour = "lightcoral"),
          legend.key.height = unit(1.2, "cm")) 
}  
barhourplots
# TWO people are gone ###
#########
