library(ggplot2)
library(Hmisc)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(grid)
library(scales)

### Hourly usage per person
event0726 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/events 0723.csv", header=TRUE)
myevent0726 = event0726[,-c(1,3,5,8,9,15:16)]
myevent0726$ParticipantID = as.character(myevent0726$ParticipantID)
myevent0726 = myevent0726[which(myevent0726$ParticipantID!="UU-FEAP-VHNA-SXPV-MXJS-ID"&
                                  myevent0726$ParticipantID!="UU-ROSR-QDTY-HZGI-UHRJ-ID"),]

myevent0726$Applications="NULL"
myevent0726$Applications[which(myevent0726$AppPackage=="com.facebook.katana"|
                                 myevent0726$AppPackage=="com.facebook.lite")] <- "Facebook"
myevent0726$Applications[which(myevent0726$AppPackage=="com.facebook.orca")] <-"FB Messenger"
myevent0726$Applications[which(myevent0726$AppPackage=="com.google.android.youtube")] <-"YouTube"
myevent0726$Applications[which(myevent0726$AppPackage=="com.instagram.android")] <-"Instagram"
myevent0726$Applications[which(myevent0726$AppPackage=="com.reddit.frontpage")] <-"Reddit"
myevent0726$Applications[which(myevent0726$AppPackage=="com.whatsapp")] <-"WhatsApp"
myevent0726$Applications[which(myevent0726$AppPackage=="com.twitter.android")] <-"Twitter"
myevent0726$Applications[which(myevent0726$AppPackage=="jp.naver.line.android")] <-"LINE"
myevent0726$Applications[which(myevent0726$AppPackage=="com.tumblr")] <-"Tumblr"
myevent0726$Applications[which(myevent0726$AppPackage=="com.yahoo.mobile.client.android.flickr")] <-"Flickr"
myevent0726 = myevent0726[which(myevent0726$Applications!="NULL"),]
#head(myevent0726)
#describe(myevent0726)

names(myevent0726)[1] = "ParticipantID"
myevent0726[,"Date"] = format(ymd_hms(myevent0726$TimestampHumanReadableWithTZ),
                              tz = "Europe/London", "%m/%d/%Y %H:%M")
myevent0726[,"Hour"] = hour(myevent0726$TimestampHumanReadableWithTZ)
myevent0726[,"Minute"] = minute(myevent0726$TimestampHumanReadableWithTZ)
myevent0726$UserID = "NULL"
for(i in 1:length(unique(myevent0726$ParticipantID))){
  myevent0726$UserID[which(myevent0726$ParticipantID==paste0(unique(myevent0726$ParticipantID)[i]))] <- substr(paste0(unique(myevent0726$ParticipantID)[i]),4,7)
}

#names(myevent0726)
myevent0726 = myevent0726[which(myevent0726$ScreenOn=="true"),]

day0726 = myevent0726[,-c(2,4,10)]
#str(day0726)
day0726$DurationMilliSec = as.numeric(as.character(day0726$DurationMilliSec))
day0726 = day0726[which(day0726$DurationMilliSec>1000),]
day0726$DurationSec = day0726$DurationMilliSec*0.001
day0726$DurationMin = day0726$DurationSec/60

### Add gender/race/age information
day0726$Gender="Prefer not\nto say"
day0726$Gender[which(day0726$UserID=="FYIV"|day0726$UserID=="IRHV"|day0726$UserID=="QPDZ"|
                       day0726$UserID=="ZLCZ"|day0726$UserID=="CWEV"|day0726$UserID=="KEQH"|
                       day0726$UserID=="DUGS"|day0726$UserID=="IRHV"|day0726$UserID=="VRLE"|
                       day0726$UserID=="WEZF")] <- "Male"
day0726$Gender[which(day0726$UserID=="KYVV"|day0726$UserID=="YOGB"|day0726$UserID=="NRSP"|
                       day0726$UserID=="JLFU"|day0726$UserID=="QISZ"|day0726$UserID=="HNMA"|
                       day0726$UserID=="TGQN"|day0726$UserID=="IUZU"|day0726$UserID=="XRNC"|
                       day0726$UserID=="JSSR")] <- "Female"
day0726$Ethnicity="English"
day0726$Ethnicity[which(day0726$UserID=="FYIV"|day0726$UserID=="JLFU"|day0726$UserID=="QISZ"|
                          day0726$UserID=="HNMA"|day0726$UserID=="IRHV"|day0726$UserID=="QPDZ"|
                          day0726$UserID=="KYVV"|day0726$UserID=="YOGB"|day0726$UserID=="NRSP"|
                          day0726$UserID=="ZLCZ"|day0726$UserID=="TGQN"|day0726$UserID=="IUZU")] <-"Taiwanese"
day0726$Age="Prefer not\nto say"
day0726$Age[which(day0726$UserID=="FYIV"|day0726$UserID=="JLFU"|day0726$UserID=="YOGB"|
                    day0726$UserID=="TGQN"|day0726$UserID=="DUGS"|day0726$UserID=="UKFG")] <- "18-25"
day0726$Age[which(day0726$UserID=="QISZ"|day0726$UserID=="IRHV"|day0726$UserID=="QPDZ"|
                    day0726$UserID=="KYVV"|day0726$UserID=="ZLCZ"|day0726$UserID=="IUZU"|
                    day0726$UserID=="CWEV"|day0726$UserID=="VRLE"|day0726$UserID=="JSSR")] <- "26-35"
day0726$Age[which(day0726$UserID=="HNMA"|day0726$UserID=="NRSP")] <- "36-50"
day0726$Age[which(day0726$UserID=="KEQH"|day0726$UserID=="XRNC")] <- "51-65"
day0726$Age[which(day0726$UserID=="WEZF")] <- "65+"

day0726$GEgroup = "NULL"
day0726$GEgroup[which(day0726$Gender=="Female" & day0726$Ethnicity=="Taiwanese")] <- "F/T"
day0726$GEgroup[which(day0726$Gender=="Male" & day0726$Ethnicity=="Taiwanese")] <- "M/T"
day0726$GEgroup[which(day0726$Gender=="Female" & day0726$Ethnicity=="English")] <- "F/E"
day0726$GEgroup[which(day0726$Gender=="Male" & day0726$Ethnicity=="English")] <- "M/E"

##################
##### Visualisation
###Heatmap: Hour v.s. Race/Gender/Age
#Only FB usage counted
FB = day0726[which(day0726$Applications=="Facebook"&day0726$GEgroup!="NULL"),]
FB_gender = data.frame(summarise(group_by(FB, Hour, Gender), Counts=length(Hour)))
colnames(FB_gender)[2] = "group"
FB_ethnicity = data.frame(summarise(group_by(FB, Hour, Ethnicity), Counts=length(Hour)))
colnames(FB_ethnicity)[2] = "group"
FB_age = data.frame(summarise(group_by(FB, Hour, Age), Counts=length(Hour)))
colnames(FB_age)[2] = "group"
FB_counts = rbind(FB_gender, FB_ethnicity, FB_age)
FB_counts$Hour = factor(FB_counts$Hour, levels=c("21","22","23","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))
FB_counts$group = factor(FB_counts$group, levels=c("Female","Male","Taiwanese","English","18-25","26-35","36-50","51-65","65+"))

ggplot(FB_counts, aes(x=group, y=Hour, fill=Counts)) +
  geom_tile(colour="palegreen", size=1) +
  scale_fill_gradient(low="yellow", high="red") +
  labs(x="", title = "Group Comparison of Total Frequency over\n24 hour period between 25/07 21:00 to 26/07 21:00",
       subtitle = "Only Facebook usage counted") +
  geom_text(aes(label=Counts)) +
  scale_x_discrete(limits=c("Female","Male","Taiwanese","English","18-25","26-35","36-50","51-65","65+")) +
  theme(text=element_text(size=30, face="bold"),
        plot.subtitle = element_text(colour="#56B4E9"),
        axis.text.x = element_text(angle=330),
        plot.margin = unit(c(.3,.3,.1,.3), "cm"))

### Daily comparison
#another file
##########
