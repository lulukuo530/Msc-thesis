day0717 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0717.csv", header=TRUE)
day0718 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0718.csv", header=TRUE)
day0719 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0719.csv", header=TRUE)
day0720 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0720.csv", header=TRUE)
day0721 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0721.csv", header=TRUE)
day0722 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0722.csv", header=TRUE)
day0723 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0723.csv", header=TRUE)
day0724 = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/experiment data/day0724.csv", header=TRUE)
day0717$daygroup = "17/07"
day0718$daygroup = "18/07"
day0719$daygroup = "19/07"
day0720$daygroup = "20/07"
day0721$daygroup = "21/07"
day0722$daygroup = "22/07"
day0723$daygroup = "23/07"
day0724$daygroup = "24/07"

library(ggplot2)
library(Hmisc)
library(lubridate)
library(RColorBrewer)
library(reshape2)
library(dplyr)
library(grid)
library(scales)
#head(as.POSIXct(try$Date, format="%m/%d/%Y %H:%M"))
### Application in sequential order for 8 days
day = rbind(day0717, day0718, day0719, day0720, day0721, day0722, day0723, day0724)
day$Time = paste(day$Hour, day$Minute, sep=":")
day$Time = as.POSIXct(factor(day$Time), format="%H:%M")
day = day[which(day$DurationMilliSec>1000),]

dayuser=list(); dailyplots=list()
cols = c("#892e82","#0f0fbc","#298b0d","#fad304","#ff6600","slategray4","aquamarine")
for(i in 1:length(unique(day$UserID))){
  dayuser[[i]] = day[which(day$UserID==unique(day$UserID)[i]),]

  g = ggplot(dayuser[[i]], aes(x=Time, y=DurationMin, group=length(unique(day$Applications)))) + 
  geom_line(aes(colour=Applications), size=1.5) +
  #geom_point(aes(colour=Applications), size=3, shape=18) +
  labs(y="Minutes", title="Patterns of Application usage\nover 8 day period between 17/07 to 24/07",
       subtitle = paste(unique(day$ParticipantID))[i]) +
  scale_color_manual(values = cols) +
  scale_x_datetime(breaks=date_breaks("3 hour"), 
                   date_minor_breaks = "1 hour",
                   minor_breaks = "30 min",
                   labels = date_format("%H:%M")) +
  theme(text = element_text(size=30, face = "bold"),
        axis.text.y = element_blank(),    # every y panel ranges the same
        plot.title = element_text(hjust=.5),
        plot.subtitle = element_text(hjust=.5, colour="lightcoral"),
        legend.position = "bottom",
        plot.margin = unit(c(.1,.1,.1,.1),"cm"),
        strip.text.y = element_text(angle=0, colour="firebrick4"),
        legend.background = element_rect(fill="grey80"),
        legend.text = element_text(face="italic", size=25, hjust=.5),
        legend.title = element_text(face="italic"),
        panel.background = element_rect(fill="grey75")) 
  
  dailyplots[[i]] = 
  g + facet_grid(daygroup ~ .) 
#, labeller = labeller(group=labels)
#+ guides(colour=guide_legend(oevrride.aes = list(size=5))) 
#}
}
dailyplots        

t = dayuser[[1]][which(dayuser[[1]]$daygroup=="17/07"),]





#######
### Daily usage comparison
usagedata = data.frame(summarise(group_by(day, daygroup, UserID), Usage = sum(DurationMin)))
usagedata$group = "NULL"
usagedata$group[which(usagedata$UserID=="KEQH"|usagedata$UserID=="IRHV"|usagedata$UserID=="UKFG"|usagedata$UserID=="VRLE"|
                 usagedata$UserID=="KYVV"|usagedata$UserID=="HNMA"|usagedata$UserID=="QISZ"|usagedata$UserID=="DUGS")
                 ] <- "1"
usagedata$group[which(usagedata$UserID=="ZLCZ"|usagedata$UserID=="YOGB"|usagedata$UserID=="JLFU"|usagedata$UserID=="IUZU"|
                 usagedata$UserID=="KWAK"|usagedata$UserID=="XRNC"|usagedata$UserID=="TGQN")] <- "2"
usagedata$group[which(usagedata$UserID=="SICY"|usagedata$UserID=="WEZF"|usagedata$UserID=="FYIV"|usagedata$UserID=="CWEV"|
                usagedata$UserID=="JSSR"|usagedata$UserID=="NRSP"|usagedata$UserID=="QPDZ")] <- "3"

cols = c("cadetblue","cornflowerblue","cyan","darkblue",
         "darkolivegreen1","darkgreen","palegreen","skyblue1",
         "lightpink","orangered1","grey10","palevioletred",
         "peachpuff2","tan3","yellow1","grey50",
         "lightsteelblue3","lightseagreen","mistyrose3","slateblue1",
         "darkmagenta","violet")

ggplot(usagedata, aes(x=daygroup, y=Usage, colour=UserID, group=UserID)) +
  geom_point(size=5, shape=18) + geom_line(size=1.3) +
  scale_colour_manual(values=cols) +
  facet_grid(group ~ ., scales = "free", margins = unit(c(0,.3,0,.3),"cm")) +
  scale_y_continuous(breaks = c(0,200,400,600)) +
  theme(strip.text = element_blank(),
        plot.title = element_text(hjust=.5),
        legend.position = "bottom",
        text = element_text(size=30, face="bold"),
        legend.text = element_text(size=20),
        plot.margin = unit(c(.1,.1,.1,.1), "cm"),
        panel.spacing = unit(.3, "cm"),
        legend.background = element_rect(fill="grey90")) +
  labs(x="Date", y="Minutes", title="Daily total usage comparison\nover 8 day period between 17/07 to 24/07")
  
###############################



