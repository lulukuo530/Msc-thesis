library(ggplot2)
library(Hmisc)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(grid)
library(scales)

### Hourly usage per person
event0725 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0723.csv", header=TRUE)
myevent0725 = event0725[,-c(1,3,5,8,9,15:16)]
myevent0725$ParticipantID = as.character(myevent0725$ParticipantID)
myevent0725 = myevent0725[which(myevent0725$ParticipantID!="UU-FEAP-VHNA-SXPV-MXJS-ID"&
                                    myevent0725$ParticipantID!="UU-ROSR-QDTY-HZGI-UHRJ-ID"),]

myevent0725$Applications="NULL"
myevent0725$Applications[which(myevent0725$AppPackage=="com.facebook.katana"|
                                  myevent0725$AppPackage=="com.facebook.lite")] <- "Facebook"
myevent0725$Applications[which(myevent0725$AppPackage=="com.facebook.orca")] <-"FB Messenger"
myevent0725$Applications[which(myevent0725$AppPackage=="com.google.android.youtube")] <-"YouTube"
myevent0725$Applications[which(myevent0725$AppPackage=="com.instagram.android")] <-"Instagram"
myevent0725$Applications[which(myevent0725$AppPackage=="com.reddit.frontpage")] <-"Reddit"
myevent0725$Applications[which(myevent0725$AppPackage=="com.whatsapp")] <-"WhatsApp"
myevent0725$Applications[which(myevent0725$AppPackage=="com.twitter.android")] <-"Twitter"
myevent0725$Applications[which(myevent0725$AppPackage=="jp.naver.line.android")] <-"LINE"
myevent0725$Applications[which(myevent0725$AppPackage=="com.tumblr")] <-"Tumblr"
myevent0725$Applications[which(myevent0725$AppPackage=="com.yahoo.mobile.client.android.flickr")] <-"Flickr"
myevent0725 = myevent0725[which(myevent0725$Applications!="NULL"),]
#head(myevent0725)
#describe(myevent0725)

names(myevent0725)[1] = "ParticipantID"
myevent0725[,"Date"] = format(ymd_hms(myevent0725$TimestampHumanReadableWithTZ),
                               tz = "Europe/London", "%m/%d/%Y %H:%M")
myevent0725[,"Hour"] = hour(myevent0725$TimestampHumanReadableWithTZ)
myevent0725[,"Minute"] = minute(myevent0725$TimestampHumanReadableWithTZ)
myevent0725$UserID = "NULL"
for(i in 1:length(unique(myevent0725$ParticipantID))){
  myevent0725$UserID[which(myevent0725$ParticipantID==paste0(unique(myevent0725$ParticipantID)[i]))] <- substr(paste0(unique(myevent0725$ParticipantID)[i]),4,7)
}

#names(myevent0725)
myevent0725 = myevent0725[which(myevent0725$ScreenOn=="true"),]

day0725 = myevent0725[,-c(2,4,10)]
#str(day0725)
day0725$DurationMilliSec = as.numeric(as.character(day0725$DurationMilliSec))
day0725 = day0725[which(day0725$DurationMilliSec>1000),]
day0725$DurationSec = day0725$DurationMilliSec*0.001
day0725$DurationMin = day0725$DurationSec/60

#########
### Visualisatoin
# Freq vs Total duration
dayuser=list(); freqplots=list()
for(i in 1:length(unique(day0725$UserID))){
  dayuser[[i]] = day0725[which(day0725$UserID==paste(unique(day0725$UserID)[i])),]
  dayuser[[i]]$Date = as.POSIXct(dayuser[[i]]$Date, format = "%m/%d/%Y %H:%M")
  t1 = data.frame(summarise(group_by(dayuser[[i]], Applications), Counts = length(Applications)))
  t1$group = "Frequency\n(# per day)"
  t2 = data.frame(summarise(group_by(dayuser[[i]], Applications), Counts = sum(DurationMin)))
  t2$group = "Total Usage\n(Minutes)"
  tt = rbind(t1,t2)
  cols = c("steelblue3","gold")
  
  freqplots[[i]] = 
    ggplot(tt, aes(x=Applications, y=Counts, fill=group)) +
    geom_bar(stat="identity", position="dodge") +
    geom_text(aes(label = sprintf("%2.2f", Counts)), 
              position = position_dodge(width = 1), hjust=.8, size=8, 
              fontface="bold", colour="hotpink") +     
    theme(text=element_text(size=30, face="bold"),
          plot.title = element_text(hjust=.5),
          plot.subtitle = element_text(colour = "lightcoral"),
          legend.key.height = unit(2, "cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill="grey90")) +
    scale_fill_manual(values=cols) +
    labs(y="Values",  title="Frequency and Total Usage over 24 hour period\nbetween 24/07 21:00 to 25/07 21:00",
         subtitle = paste(dayuser[[i]]$ParticipantID[1])) +
    guides(fill = guide_legend(reverse=T)) +
    coord_flip()
}
freqplots

###UU-IRHV-EQMT-UUPB-CTOI-ID
###UU-UKFG-VGYK-YCHJ-ZFJH-ID
library(reshape2)
count_app = summarise(group_by(day0725, Applications, UserID), Counts = length(Applications))
ggplot(count_app, aes(x=reorder(Applications,-Counts), y=UserID, fill = Counts)) +  
  geom_tile(colour="palegreen", size=1) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x="Applications", y="UserID", 
       title = "Application Frequency Comparison over 24 hour period\nbetween 24/07 21:00 to 25/07 21:00") +
  theme(text = element_text(size=30, face="bold"),
        axis.text.x=element_text(angle=330),
        plot.title = element_text(hjust=.5),
        legend.key.height = unit(1,"cm")) +
  geom_text(aes(label = count_app$Counts)) 

##############
freqapp=list()
for(i in 1:length(unique(day0725$UserID))){
  freqapp[[i]] = 
  ggplot(dayuser[[i]], aes(x=Date, y=DurationMin, colour=Applications, width=500)) + 
  geom_bar(stat = "identity", fill="grey50") +
  facet_grid(Applications~.) +
  scale_x_datetime(breaks=date_breaks("2 hour"), 
                   date_minor_breaks = "1 hour",
                   minor_breaks = "30 min",
                   labels = date_format("%d/%m\n%H:%M")) +
  theme(text=element_text(size=30, face="bold"),
        legend.position = "blank",
        strip.text.y = element_text(angle=0, colour="firebrick4")) +
  labs(x="Time", y="Minutes", title="Frequency Usage over 24 hour period\nbetween 24/07 21:00 to 25/07 21:00",
       subtitle = paste(dayuser[[i]]$ParticipantID[1])) +
  theme(text = element_text(size=30, face="bold"),
        plot.title = element_text(hjust=0),
        plot.subtitle = element_text(colour = "lightcoral"),
        legend.key.height = unit(1.2, "cm")) 
}
freqapp

##############
# gender or age group or ethnic group?
day0725$Gender="Prefer not\nto say"
day0725$Gender[which(day0725$UserID=="FYIV"|day0725$UserID=="IRHV"|day0725$UserID=="QPDZ"|
                     day0725$UserID=="ZLCZ"|day0725$UserID=="CWEV"|day0725$UserID=="KEQH"|
                     day0725$UserID=="DUGS"|day0725$UserID=="IRHV"|day0725$UserID=="VRLE"|
                     day0725$UserID=="WEZF")] <- "Male"
day0725$Gender[which(day0725$UserID=="KYVV"|day0725$UserID=="YOGB"|day0725$UserID=="NRSP"|
                     day0725$UserID=="JLFU"|day0725$UserID=="QISZ"|day0725$UserID=="HNMA"|
                     day0725$UserID=="TGQN"|day0725$UserID=="IUZU"|day0725$UserID=="XRNC"|
                     day0725$UserID=="JSSR")] <- "Female"
day0725$Ethnicity="English"
day0725$Ethnicity[which(day0725$UserID=="FYIV"|day0725$UserID=="JLFU"|day0725$UserID=="QISZ"|
                        day0725$UserID=="HNMA"|day0725$UserID=="IRHV"|day0725$UserID=="QPDZ"|
                        day0725$UserID=="KYVV"|day0725$UserID=="YOGB"|day0725$UserID=="NRSP"|
                        day0725$UserID=="ZLCZ"|day0725$UserID=="TGQN"|day0725$UserID=="IUZU")] <-"Taiwanese"
day0725$Age="Prefer not\nto say"
day0725$Age[which(day0725$UserID=="FYIV"|day0725$UserID=="JLFU"|day0725$UserID=="YOGB"|
                  day0725$UserID=="TGQN"|day0725$UserID=="DUGS"|day0725$UserID=="UKFG")] <- "18-25"
day0725$Age[which(day0725$UserID=="QISZ"|day0725$UserID=="IRHV"|day0725$UserID=="QPDZ"|
                  day0725$UserID=="KYVV"|day0725$UserID=="ZLCZ"|day0725$UserID=="IUZU"|
                  day0725$UserID=="CWEV"|day0725$UserID=="VRLE"|day0725$UserID=="JSSR")] <- "26-35"
day0725$Age[which(day0725$UserID=="HNMA"|day0725$UserID=="NRSP")] <- "36-50"
day0725$Age[which(day0725$UserID=="KEQH"|day0725$UserID=="XRNC")] <- "51-65"
day0725$Age[which(day0725$UserID=="WEZF")] <- "65+"
##############

###UU-IRHV-EQMT-UUPB-CTOI-ID
###UU-UKFG-VGYK-YCHJ-ZFJH-ID
day0725$hmgroup = "NULL"
day0725$hmgroup[which(day0725$Gender=="Female" & day0725$Ethnicity=="Taiwanese")] <- "F/T"
day0725$hmgroup[which(day0725$Gender=="Male" & day0725$Ethnicity=="Taiwanese")] <- "M/T"
day0725$hmgroup[which(day0725$Gender=="Female" & day0725$Ethnicity=="English")] <- "F/E"
day0725$hmgroup[which(day0725$Gender=="Male" & day0725$Ethnicity=="English")] <- "M/E"
draw = day0725[which(day0725$hmgroup!="NULL"),]

head(draw)
draw1 = data.frame(summarise(group_by(draw, Hour, hmgroup), Total = sum(DurationMin)))
draw1$Hour = factor(draw1$Hour, levels=c("21","22","23","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20"))

ggplot(draw1, aes(x=reorder(hmgroup, -Total), y=Hour, fill=Total)) +
  geom_tile(colour="palegreen", size=1) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x="Groups", y="Hour", fill="Total Usage\n(Minutes)",
       title = "Group Comparison of Total Usage over 24 hour period\nbetween 24/07 21:00 to 25/07 21:00") +
  theme(text = element_text(size=30, face="bold"),
        plot.title = element_text(hjust=0),
        legend.key.height = unit(1,"cm"))+
  scale_x_discrete(labels=c("F/T"="Female\nTaiwanese", "M/T"="Male\nTaiwanese", "F/E"="Female\nEnglish", "M/E"="Male\nEnglish")) +
  geom_text(aes(label = round(draw1$Total,2))) 
  #+ guides(col = guide_legend(ncol = 8, byrow = TRUE))

    
  

scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

