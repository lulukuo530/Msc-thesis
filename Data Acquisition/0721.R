event0721 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/events 0721.csv", header=TRUE)
myevent0721 = event0721[,-c(1,3,5,8,9,15:16)]
myevent0721$ParticipantID = as.character(myevent0721$ParticipantID)
myevent0721 = myevent0721[which(myevent0721$ParticipantID!="UU-FEAP-VHNA-SXPV-MXJS-ID"&
                                  myevent0721$ParticipantID!="UU-ROSR-QDTY-HZGI-UHRJ-ID"),]

myevent0721$Applications="NULL"
myevent0721$Applications[which(myevent0721$AppPackage=="com.facebook.katana"|
                                 myevent0721$AppPackage=="com.facebook.lite")] <- "Facebook"
myevent0721$Applications[which(myevent0721$AppPackage=="com.facebook.orca")] <-"FB Messenger"
myevent0721$Applications[which(myevent0721$AppPackage=="com.google.android.youtube")] <-"YouTube"
myevent0721$Applications[which(myevent0721$AppPackage=="com.instagram.android")] <-"Instagram"
myevent0721$Applications[which(myevent0721$AppPackage=="com.reddit.frontpage")] <-"Reddit"
myevent0721$Applications[which(myevent0721$AppPackage=="com.whatsapp")] <-"WhatsApp"
myevent0721$Applications[which(myevent0721$AppPackage=="com.twitter.android")] <-"Twitter"
myevent0721$Applications[which(myevent0721$AppPackage=="jp.naver.line.android")] <-"LINE"
myevent0721$Applications[which(myevent0721$AppPackage=="com.tumblr")] <-"Tumblr"
myevent0721$Applications[which(myevent0721$AppPackage=="com.yahoo.mobile.client.android.flickr")] <-"Flickr"
myevent0721 = myevent0721[which(myevent0721$Applications!="NULL"),]
#head(myevent0721)
#describe(myevent0721)

library(ggplot2)
library(Hmisc)
library(lubridate)
names(myevent0721)[1] = "ParticipantID"
myevent0721[,"Date"] = format(ymd_hms(myevent0721$TimestampHumanReadableWithTZ),
                              tz = "Europe/London", "%m/%d/%Y %H:%M")
myevent0721[,"Hour"] = hour(myevent0721$TimestampHumanReadableWithTZ)
myevent0721[,"Minute"] = minute(myevent0721$TimestampHumanReadableWithTZ)
myevent0721$UserID = "NULL"
for(i in 1:length(unique(myevent0721$ParticipantID))){
  myevent0721$UserID[which(myevent0721$ParticipantID==paste0(unique(myevent0721$ParticipantID)[i]))] <- substr(paste0(unique(myevent0721$ParticipantID)[i]),4,7)
}

#names(myevent0721)
myevent0721 = myevent0721[which(myevent0721$ScreenOn=="true"),]

day0721 = myevent0721[,-c(2,4,10)]
str(day0721)
day0721$DurationMilliSec = as.character(day0721$DurationMilliSec)
day0721$DurationMilliSec = as.numeric(day0721$DurationMilliSec)
day0721 = day0721[which(day0721$DurationMilliSec>1000),]
day0721$DurationSec = day0721$DurationMilliSec*0.001
day0721$DurationMin = day0721$DurationSec/60
#describe(day0721)
#head(day0721)

#########
### Visualisatoin
### Peer Comparison ###
# How often they check the ten apps we monitor
# with screen on time
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(grid)

dur = subset(day0721, select=c("UserID","Applications","DurationMin"))
dur$DurationMin = as.numeric(format(round(dur$DurationMin, 2), nsmall=2))
hm = matrix(0,length(unique(dur$UserID))*length(unique(dur$Applications)),3)
colnames(hm) = c("UserID","Applications","DurationMin")
hm[,"UserID"] = rep(unique(dur$UserID),each=7)
hm[,"Applications"] = rep(unique(dur$Applications))

user=list()
out = c()
for(i in 1:length(unique(dur$UserID))){
  for(j in 1:length(unique(dur$Applications))){
    user[[i]] = dur[which(dur$UserID==paste0(unique(dur$UserID)[i])),]
    out = c(out, sum(user[[i]][which(user[[i]]$Applications==paste0(unique(dur$Applications)[j])),"DurationMin"]))
    user[[i]][,"ActiveMin"] = sum(user[[i]]$DurationMin)
  }
} 

#out
hm[,3] = out
hm = data.frame(hm)
hm$DurationMin = as.numeric(as.character(hm$DurationMin))

Active=matrix(0,length(unique(dur$UserID)),3)
for(i in 1:length(unique(dur$UserID))){
  Active[i,1] = unique(user[[i]][,"UserID"])
  Active[i,2] = "Total Active Time"
  Active[i,3] = user[[i]]$ActiveMin[1]
}
colnames(Active) = c("UserID", "Applications", "DurationMin")
Active = data.frame(Active)
Active[,3] = as.numeric(as.character(Active[,3]))
hm = rbind(hm,Active)

#getPalette = colorRampPalette(brewer.pal(5, "Dark2"))
#colourCount = length(unique(day0720$UserID))
cols = c("cadetblue","cornflowerblue","cyan","darkblue",
         "darkolivegreen1","darkgreen","palegreen","lawngreen",
         "lightpink","orangered1","grey10","palevioletred",
         "peachpuff2","tan3","yellow1","grey50",
         "lightsteelblue3","lightseagreen","mistyrose3","slateblue1",
         "darkmagenta","violet")

ggplot(hm, aes(x=UserID, y=DurationMin, fill=UserID)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_blank(), text = element_text(size=25,face="bold"),
        axis.title.x=element_text(size=30),axis.title.y=element_text(size=30)) +
  labs(x = "UserID", y = "Minutes", title = "Usage Comparison per Apps over 24 hour period\nbetween 9pm on 20/07 to 9pm on 21/07") + 
  facet_grid(Applications ~ ., scales = "free") +
  theme(strip.text = element_text(colour="firebrick3"), 
        strip.text.y = element_text(angle = 0),
        strip.switch.pad.grid = unit(10,"cm"),
        panel.spacing = unit(0.2, "cm"),
        legend.text = element_text(size=30), 
        legend.title = element_text(size=30),
        legend.background = element_rect(fill="grey90")) +
  guides(fill=guide_legend(keyheight=1.2, default.unit="cm")) +
  scale_fill_manual(values = cols) +  
  theme(plot.margin = unit(c(.3,.3,.3,.3),"cm"), plot.title = element_text(hjust = 0))

##############
# Personal usage according to Time
pp = subset(day0721, select = c("UserID","Date","Applications","DurationMin"))
# combine total ID active minutes
temp = group_by(day0721, Date, UserID) %>%
  summarise(DurationMin = sum(DurationMin))
temp = data.frame(temp)
temp$DurationMin = round(temp$DurationMin,2)
temp$Applications = "ID Activity"
newcol = c("UserID","Date","Applications","DurationMin")
temp = temp[, c(newcol, setdiff(names(temp), newcol))]
pp = rbind(pp,temp)

userlist=list(); usage=list(); customtext=c()
for(i in 1:length(unique(pp$UserID))){
  userlist[[i]] = pp[which(pp$UserID == paste0(unique(pp$UserID))[i]),c("UserID","Date","Applications","DurationMin")]
  usage[[i]] = summarise(group_by(userlist[[i]], Applications, UserID), Total = sum(DurationMin))
  customtext[i] = round(sum(userlist[[i]][,"DurationMin"]),2)
  usage[[i]][,"ActiveMin"] = customtext[i]
  userlist[[i]]$Time = as.POSIXct(userlist[[i]]$Date, format="%m/%d/%Y %H:%M")
}
tail(userlist[[5]])
head(usage[[1]])

library(scales)
time1 = strptime("07/20/2017 21:00", format = "%m/%d/%Y %H:%M")
time2 = strptime("07/21/2017 21:00", format = "%m/%d/%Y %H:%M")
xlimit = as.POSIXct(c(time1, time2), tz="Europe/London")

timeplots = list()
for (i in 1:length(userlist)){
  userlist[[i]]$Applications = factor(userlist[[i]]$Applications, levels=c("Facebook","FB Messenger","YouTube",
                                                                             "Instagram","Reddit","WhatsApp","Twitter",
                                                                             "LINE","Tumblr","Flickr","ID Activity"))
  timeplots[[i]] = 
    ggplot(userlist[[i]], aes(x=Time, y=DurationMin, colour=Applications)) +
    geom_line(size=1.3) + geom_point(size=1.7, shape=20) +
    labs(y="Minutes", title="Applications Usage Comparison through time\nover 24 hour period between 20/07 21:00 to 21/07 21:00",
         subtitle=paste(unique(day0721$ParticipantID)[i])) +
    scale_x_datetime(breaks = date_breaks("3 hour"),
                     date_minor_breaks = "1 hour",
                     minor_breaks = "30 min",
                     labels=date_format("%d/%m\n%H:%M"),
                     limits = xlimit) +
    theme(text = element_text(size=30, face="bold"),
          plot.subtitle = element_text(size=25, face="bold", colour="darkgreen"),
          axis.text.x = element_text(angle=0),
          strip.text = element_text(colour="firebrick3",size=35), 
          strip.text.y = element_text(angle = 0),
          strip.switch.pad.grid = unit(10,"cm"),
          panel.spacing = unit(.5, "cm"),
          legend.position = "none") +
    facet_grid(Applications ~ ., scales = "free", margins = unit(c(0,.3,0,.3),"cm")) +
    theme(plot.margin = unit(c(.3,.3,0,.3),"cm"), plot.title = element_text(hjust = .2))
}
timeplots

######################
t = userlist[[1]][which(userlist[[1]]$Applications!="ID Activity"),]
time11 = strptime("07/20/2017 21:00", format = "%m/%d/%Y %H:%M")
time12 = strptime("07/21/2017 02:00", format = "%m/%d/%Y %H:%M")
xlimit1 = as.POSIXct(c(time11, time12), tz="Europe/London")


ggplot(userlist[[1]], aes(x=Time, y=DurationMin, colour=Applications)) +
  geom_point(aes(size=Applications)) +
  scale_x_datetime(breaks = date_breaks("3 hour"),
                   date_minor_breaks = "1 hour")






