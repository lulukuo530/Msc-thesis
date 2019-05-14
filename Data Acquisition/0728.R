temp = rbind(d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27,d28)
mydata=temp
### Heatmap Date v.s. Parts of Day
nn = data.frame(summarise(group_by(mydata, mday, partsofday), Usage=sum(DurationMin), Freq=length(mday)))
nn$Ave = round(nn$Usage/nn$Freq,2)
nn$partsofday = factor(nn$partsofday, levels=c("Morning","Daytime","Evening"))

ggplot(nn, aes(x=mday, y=partsofday)) +
  geom_tile(aes(fill=Ave), colour="palegreen", size=1) +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(breaks = c(17:28)) +
  scale_y_discrete(labels=c("Morning\n00:00-11:59", "Daytime\n12:00-17:59", "Evening\n18:00-23:59")) +
  theme(text = element_text(size=30, face="bold"),
        plot.title = element_text(hjust=.5),
        legend.title = element_text(size=25)) +
  geom_text(aes(label=Ave), size=8) +
  labs(x="Date (July)", y="Parts of Day", fill="Average\nUsage\n(minutes\nper time)",
       title="Average Usage by Date and Parts of Time\nover 12 day period between 17/07 and 28/07")

##### Same day pattern comparison
temp = rbind(d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27,d28,d29)
mydata = temp
mydata$partsofday = "NULL"
mydata$partsofday[which(mydata$Hour==0|mydata$Hour==1|mydata$Hour==2|mydata$Hour==3|mydata$Hour==4|mydata$Hour==5|
                          mydata$Hour==6|mydata$Hour==7|mydata$Hour==8|mydata$Hour==9|mydata$Hour==10|mydata$Hour==11)] <- "Morning"
mydata$partsofday[which(mydata$Hour==12|mydata$Hour==13|mydata$Hour==14|mydata$Hour==15|mydata$Hour==16|mydata$Hour==17)] <- "Daytime"
mydata$partsofday[which(mydata$Hour==18|mydata$Hour==19|mydata$Hour==20|mydata$Hour==21|mydata$Hour==22|mydata$Hour==23)] <- "Evening"
mydata$weekday = "NULL"
mydata$weekday[which(mydata$mday=="17"|mydata$mday=="18"|mydata$mday=="19"|mydata$mday=="20"|mydata$mday=="21"|
                       mydata$mday=="24"|mydata$mday=="25"|mydata$mday=="26"|mydata$mday=="27")] <- "Weekdays"
mydata$weekday[which(mydata$mday=="22"|mydata$mday=="23")] <- "Weekend"

mydata$dof = "NULL"
mydata$dof[which(mydata$mday==17|mydata$mday==24)] <- "Mon"
mydata$dof[which(mydata$mday==18|mydata$mday==25)] <- "Tue"
mydata$dof[which(mydata$mday==19|mydata$mday==26)] <- "Wed"
mydata$dof[which(mydata$mday==20|mydata$mday==27)] <- "Thu"
mydata$dof[which(mydata$mday==21|mydata$mday==28)] <- "Fri"
mydata$dof[which(mydata$mday==22|mydata$mday==29)] <- "Sat"
mydata$dof[which(mydata$mday==23|mydata$mday==30)] <- "Sun"
dofm = mydata[which(mydata$dof!="Sat" & mydata$dof!="Sun"),]

userlist=list();   ppplots=list()
for(i in 1:length(unique(dofm$ParticipantID))){
  userlist[[i]] = dofm[which(dofm$ParticipantID == paste0(unique(dofm$ParticipantID))[i]),]
  ###
  pp = data.frame(summarise(group_by(userlist[[i]], mday, dof, Hour), Usage = sum(DurationMin), Freq = length(dof)))
  pp$Ave = round(pp$Usage/pp$Freq,4)
  pp$dof = factor(pp$dof, levels=c("Mon","Tue","Wed","Thu","Fri"))
  
  ppplots[[i]] = 
  ggplot(pp, aes(x=Hour, y=Ave, colour=factor(mday))) +
    geom_line(size=1.5) + geom_point(size=5, shape=18) +
    facet_grid(dof ~ .) +
    scale_x_continuous(breaks = c(0:23)) +
    theme(text = element_text(size=30, face="bold"),
          plot.title = element_text(hjust=.5),
          plot.subtitle = element_text(colour="lightcoral"),
          strip.text.y = element_text(angle=0, colour="orchid3"),
          legend.key.height = unit(1,"cm")) +
    scale_colour_brewer(palette = "Paired") +
    labs(y="Average Usage (minutes per time)", colour="Date\n(July)",
         title="Average Usage Comparison by Weekdays and Hour\nover 10 day period between 17/07 and 28/07",
         subtitle=paste(userlist[[i]]$ParticipantID[1]))
}


### Afternoon pie chart
pieplots=list()
for(j in 1:22){
  t = data.frame(summarise(group_by(userlist[[j]], mday, partsofday, Applications),Usage=sum(DurationMin)))
  t$partsofday = factor(t$partsofday, levels=c("Morning","Daytime","Evening"))
  t = t[which(t$mday==28|t$mday==27|t$mday==26|t$mday==25|t$mday==24),]
  x1 = sum(t[which(t$mday==28 & t$partsofday=="Morning"),"Usage"])
  y1 = sum(t[which(t$mday==28 & t$partsofday=="Daytime"),"Usage"])
  z1 = sum(t[which(t$mday==28 & t$partsofday=="Evening"),"Usage"])
  x2 = sum(t[which(t$mday==27 & t$partsofday=="Morning"),"Usage"])
  y2 = sum(t[which(t$mday==27 & t$partsofday=="Daytime"),"Usage"])
  z2 = sum(t[which(t$mday==27 & t$partsofday=="Evening"),"Usage"])
  x3 = sum(t[which(t$mday==26 & t$partsofday=="Morning"),"Usage"])
  y3 = sum(t[which(t$mday==26 & t$partsofday=="Daytime"),"Usage"])
  z3 = sum(t[which(t$mday==26 & t$partsofday=="Evening"),"Usage"])
  x4 = sum(t[which(t$mday==25 & t$partsofday=="Morning"),"Usage"])
  y4 = sum(t[which(t$mday==25 & t$partsofday=="Daytime"),"Usage"])
  z4 = sum(t[which(t$mday==25 & t$partsofday=="Evening"),"Usage"])
  x5 = sum(t[which(t$mday==24 & t$partsofday=="Morning"),"Usage"])
  y5 = sum(t[which(t$mday==24 & t$partsofday=="Daytime"),"Usage"])
  z5 = sum(t[which(t$mday==24 & t$partsofday=="Evening"),"Usage"])
  
  t$percent = "NULL"
  for(i in 1:nrow(t)){
    if(t[i,"partsofday"]=="Morning" & t[i,"mday"]==28) {t[i,"percent"]=t[i,"Usage"]/x1}
    if(t[i,"partsofday"]=="Daytime" & t[i,"mday"]==28) {t[i,"percent"]=t[i,"Usage"]/y1}
    if(t[i,"partsofday"]=="Evening" & t[i,"mday"]==28) {t[i,"percent"]=t[i,"Usage"]/z1}
    if(t[i,"partsofday"]=="Morning" & t[i,"mday"]==27) {t[i,"percent"]=t[i,"Usage"]/x2}
    if(t[i,"partsofday"]=="Daytime" & t[i,"mday"]==27) {t[i,"percent"]=t[i,"Usage"]/y2}
    if(t[i,"partsofday"]=="Evening" & t[i,"mday"]==27) {t[i,"percent"]=t[i,"Usage"]/z2}
    if(t[i,"partsofday"]=="Morning" & t[i,"mday"]==26) {t[i,"percent"]=t[i,"Usage"]/x3}
    if(t[i,"partsofday"]=="Daytime" & t[i,"mday"]==26) {t[i,"percent"]=t[i,"Usage"]/y3}
    if(t[i,"partsofday"]=="Evening" & t[i,"mday"]==26) {t[i,"percent"]=t[i,"Usage"]/z3}
    if(t[i,"partsofday"]=="Morning" & t[i,"mday"]==25) {t[i,"percent"]=t[i,"Usage"]/x4}
    if(t[i,"partsofday"]=="Daytime" & t[i,"mday"]==25) {t[i,"percent"]=t[i,"Usage"]/y4}
    if(t[i,"partsofday"]=="Evening" & t[i,"mday"]==25) {t[i,"percent"]=t[i,"Usage"]/z4}
    if(t[i,"partsofday"]=="Morning" & t[i,"mday"]==24) {t[i,"percent"]=t[i,"Usage"]/x5}
    if(t[i,"partsofday"]=="Daytime" & t[i,"mday"]==24) {t[i,"percent"]=t[i,"Usage"]/y5}
    if(t[i,"partsofday"]=="Evening" & t[i,"mday"]==24) {t[i,"percent"]=t[i,"Usage"]/z5}
  }
  t$percent = as.numeric(t$percent)
  
  for(i in 1:nrow(t)){
    if(t[i,"mday"]==28) {t[i,"mday"]="28/07"}
    if(t[i,"mday"]==27) {t[i,"mday"]="27/07"}
    if(t[i,"mday"]==26) {t[i,"mday"]="26/07"}
    if(t[i,"mday"]==25) {t[i,"mday"]="25/07"}
    if(t[i,"mday"]==24) {t[i,"mday"]="24/07"}
  }
  
  pieplots[[j]] = 
  ggplot(t, aes(x="", y=percent, fill=Applications)) +
    geom_bar(width=5, stat="identity") +
    coord_polar("y") +
    facet_wrap(mday ~ partsofday, ncol=3) +
    #facet_grid(partsofday ~ mday) +
    theme_void() +
    theme(text=element_text(size=30, face="bold"),
          strip.text.x = element_text(colour="palegreen4"),
          axis.title = element_blank(),
          panel.background = element_rect(fill = "beige"),
          legend.background = element_rect(fill="grey90"),
          legend.key.height = unit(1,"cm"),
          plot.margin = unit(c(.5,.5,.5,.5), "cm"),
          strip.background = element_rect(colour = "black", fill = "white"),
          plot.title = element_text(hjust=.5),
          plot.subtitle = element_text(size=25, colour="snow4", hjust=.5)) + 
    scale_fill_manual(values=c("lightgreen","lightpink","lightskyblue2","lightcoral",
                               "lightgoldenrod","mediumpurple1","mistyrose1")) +
    labs(title="Application Usage Percentage\nby Parts of Day\nover 5 day period\nbetween 24/07 and 28/07",
         subtitle=paste(userlist[[j]]$ParticipantID[1]))
}
###########

FB = mydata[which(mydata$Applications=="Facebook"&mydata$GEgroup!="NULL"),]
FB1 = data.frame(summarise(group_by(FB, Hour, weekday), Counts=length(weekday)))
for(i in 1:nrow(FB1)){
  if(FB1[i,"weekday"]=="Weekdays") {FB1[i,"Countss"] = FB1[i,"Counts"]/10}
  if(FB1[i,"weekday"]=="Weekend") {FB1[i,"Countss"] = FB1[i,"Counts"]/2}
}

ggplot(FB1, aes(x=weekday, y=Hour, fill=Countss)) +
  geom_tile(colour="palegreen", size=1) +
  scale_fill_gradient(low="yellow", high="red") +
  labs(x="",title = "Total Frequency by Day of Week and Hour\nover 12 day period between 17/07 and 28/07",
       subtitle = "Only Facebook usage counted", fill="Daily Frequency\n(# times per day)") +
  geom_text(aes(label=Countss)) +
  theme(text=element_text(size=30, face="bold"),
        plot.title = element_text(hjust=.5),
        plot.subtitle = element_text(colour="#56B4E9",hjust=.5),
        plot.margin = unit(c(.1,.1,.1,.1), "cm"),
        legend.position = "bottom",
        legend.key.width = unit(1.5,"cm"),
        legend.margin = margin(.1,.1,.1,.1,"cm"),
        axis.text.y = element_text(size=25)) +
  scale_y_continuous(breaks = c(0:23))
#################

