event0718 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/events 0718.csv", header=TRUE)
myevent0718 = event0718[,-c(1,3,5,8,9,15:17)]
myevent0718$ParticipantID = as.character(myevent0718$ParticipantID)
myevent0718 = myevent0718[which(myevent0718$ParticipantID!="UU-FEAP-VHNA-SXPV-MXJS-ID"&
                                  myevent0718$ParticipantID!="UU-ROSR-QDTY-HZGI-UHRJ-ID"),]

myevent0718$Applications="NULL"
myevent0718$Applications[which(myevent0718$AppPackage=="com.facebook.katana"|
                                 myevent0718$AppPackage=="com.facebook.lite")] <- "Facebook"
myevent0718$Applications[which(myevent0718$AppPackage=="com.facebook.orca")] <-"FB Messenger"
myevent0718$Applications[which(myevent0718$AppPackage=="com.google.android.youtube")] <-"YouTube"
myevent0718$Applications[which(myevent0718$AppPackage=="com.instagram.android")] <-"Instagram"
myevent0718$Applications[which(myevent0718$AppPackage=="com.reddit.frontpage")] <-"Reddit"
myevent0718$Applications[which(myevent0718$AppPackage=="com.whatsapp")] <-"WhatsApp"
myevent0718$Applications[which(myevent0718$AppPackage=="com.twitter.android")] <-"Twitter"
myevent0718$Applications[which(myevent0718$AppPackage=="jp.naver.line.android")] <-"LINE"
myevent0718$Applications[which(myevent0718$AppPackage=="com.tumblr")] <-"Tumblr"
myevent0718$Applications[which(myevent0718$AppPackage=="com.yahoo.mobile.client.android.flickr")] <-"Flickr"
myevent0718 = myevent0718[which(myevent0718$Applications!="NULL"),]
#head(myevent0718)
#describe(myevent0718)

#library(ggplot2)
#library(Hmisc)
#library(lubridate)
names(myevent0718)[1] = "UserID"
myevent0718[,"Date"] = format(ymd_hms(myevent0718$TimestampHumanReadableWithTZ),
                              tz = "Europe/London", "%m/%d/%Y %H:%M")
myevent0718[,"Hour"] = hour(myevent0718$TimestampHumanReadableWithTZ)
myevent0718[,"Minute"] = minute(myevent0718$TimestampHumanReadableWithTZ)
d = mdy_hm(myevent0718$Date)
one = myevent0718[mday(d) == 17 & hour(d) >= 21,]
two = myevent0718[mday(d) == 18 & hour(d) <= 21,]
#1918
day0718 = rbind(one, two)

head(day0718)
names(day0718)
day0718 = day0718[,-c(2,4)]
str(day0718)
day0718$DurationMilliSec = as.character(day0718$DurationMilliSec)
day0718$DurationMilliSec = as.numeric(day0718$DurationMilliSec)
day0718 = day0718[which(day0718$DurationMilliSec>1000),]
day0718$DurationSec = day0718$DurationMilliSec*0.001
day0718$DurationMin = day0718$DurationSec/60
#describe(day0718)
#head(day0718)

#########
### Visualisatoin
### Peer Comparison ###
# How often they check the ten apps we monitor
library(RColorBrewer)
library(reshape2)
library(dplyr)
userfreq = data.frame(table(day0718$UserID))
colnames(userfreq) = c("UserID","Freq")
colourCount = length(unique(day0718$UserID))
getPalette = colorRampPalette(brewer.pal(9, "Set3"))

ggplot(userfreq, aes(x=reorder(UserID,-Freq), y=Freq, fill=UserID)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x=element_blank(),text = element_text(size=20)) + 
  labs(x="User", y="Frequency", title="Total Frequency Applications are Checked over 24 hour period\nbetween 17/07 9pm to 18/07 9pm") +
  scale_fill_manual(values = getPalette(colourCount))

# Duration comparison
#Total usage for each user
temp=c()
for(i in 1:length(unique(heatmap$UserID))){
  temp = c(temp, sum(heatmap[which(heatmap$UserID==unique(heatmap$UserID)[i]),"DurationMin"]))
}
usage = matrix(c(paste0(unique(day0718$UserID)),temp),length(unique(day0718$UserID)),2)
colnames(usage) = c("UserID","TotalDurationMin")
usage = data.frame(usage)
usage$TotalDurationMin = as.numeric(as.character(usage$TotalDurationMin))
#add colours
getPalette = colorRampPalette(brewer.pal(5, "Dark2"))
colourCount = length(unique(usage$UserID))

ggplot(usage, aes(x=reorder(UserID,-TotalDurationMin), y=TotalDurationMin, fill=UserID)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_blank(), text = element_text(size=20)) +
  labs(x = "UserID", y = "Total Usage (min)", title = "Application Usage Comparison among Peers\nover 24 hour period between 9pm 17/07 to 9pm on 18/07") +
  scale_fill_manual(values = getPalette(colourCount))

# Usage comparison among peers
ggplot(heatmap, aes(x=UserID, y=DurationMin, fill=UserID)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_blank(), text = element_text(size=20)) +
  labs(x = "UserID", y = "Minutes", title = "Application Usage Comparison among Peers\nover 24 hour period between 9pm 17/07 to 9pm on 18/07") + 
  facet_grid(Applications ~ ., scales = "free") +
  theme(strip.text.y = element_text(angle = 0)) +
  scale_fill_manual(values = getPalette(colourCount))

# How long they spent on the ten apps we monitor / per app
dur = subset(day0718, select=c("UserID","Applications","DurationMin"))
dur$DurationMin = as.numeric(format(round(dur$DurationMin, 2), nsmall=2))
heatmap = matrix(0,length(unique(dur$UserID))*length(unique(dur$Applications)),3)
colnames(heatmap) = c("UserID","Applications","DurationMin")
heatmap[,"UserID"] = rep(unique(dur$UserID),each=7)
heatmap[,"Applications"] = rep(unique(dur$Applications))
user=list()
out = c()
for(i in 1:length(unique(dur$UserID))){
  for(j in 1:length(unique(dur$Applications))){
    user[[i]] = dur[which(dur$UserID==paste0(unique(dur$UserID)[i])),]
    out = c(out, sum(user[[i]][which(user[[i]]$Applications==paste0(unique(dur$Applications)[j])),"DurationMin"]))
    }
} 
#out
heatmap[,3] = out
heatmap = data.frame(heatmap)
heatmap$DurationMin = as.numeric(as.character(heatmap$DurationMin))
hm.palette <- colorRampPalette(brewer.pal(5, 'OrRd'), space='Lab')

ggplot(heatmap, aes(x=reorder(Applications,-DurationMin), y=UserID)) + 
  geom_tile(aes(fill=DurationMin),colour="white") +
  scale_fill_gradient(low="cadetblue1", high="red") +
  #scale_fill_gradientn(colours = c("blue","green","yellow","red"), values = rescale(heatmap$DurationMin)) +
  geom_text(aes(label = heatmap$DurationMin)) +
  labs(x="Applications", y="User ID", title = "Application Usage Comparison among Peers\nover 24 hour period between 9pm 17/07 to 9pm on 18/07", fill="Minutes") +
  theme(text = element_text(size=20), axis.text.x=element_text(face="bold", angle=330),
        axis.text.y = element_text(face="bold"))

################## ==================== #####################

