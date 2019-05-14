### In day of week file
# 
temp = rbind(d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27)
head(temp)
ctemp1 = data.frame(summarise(group_by(temp, mday, Gender), Counts = length(mday)))
ctemp1$group="Freq"
ctemp2 = data.frame(summarise(group_by(temp, mday, Gender), Counts = sum(DurationMin)))
ctemp2$group="Usage"
ctemp3 = data.frame(mday=ctemp1$mday, Gender=ctemp1$Gender, Counts=ctemp2$Counts/ctemp1$Counts, group="Ave")
ctemp3$Counts = round(ctemp3$Counts,2)
ctemp3 = ctemp3[which(ctemp3$Gender!="Prefer not\nto say"),]

ctemp = rbind(ctemp1,ctemp2)
ctemp = ctemp[which(ctemp$Gender!="Prefer not\nto say"),]

ggplot(ctemp, aes(x=mday, y=Counts, colour=Gender))  +
  geom_point(size=5, shape=18) +
  geom_line(aes(linetype=group, colour=Gender), size=1.2) +
  #geom_text(data=ctemp3[which(ctemp3$Gender=="Female"),], 
  #          aes(x=mday,y=3000, label=Counts), size=10, fontface="bold") +
  #geom_text(data=ctemp3[which(ctemp3$Gender=="Female"),], 
  #          aes(x=25,y=2800, label="(min per usage)"), size=10, fontface="bold") +
  #geom_text(data=ctemp3[which(ctemp3$Gender=="Male"),], 
  #          aes(x=mday,y=200, label=Counts), size=10, fontface="bold") +
  #geom_text(data=ctemp3[which(ctemp3$Gender=="Male"),], 
  #          aes(x=25,y=0, label="(min per usage)"), size=10, fontface="bold") +
  labs(x="Date (July)", y="Values", linetype="Group",
       title="Gender Comparison by Frequency and Total Usage\nover 11 day period between 17/07 and 27/07") +
  scale_x_continuous(breaks = c(17:27)) +
  scale_y_continuous(limits = c(0,2500), breaks = c(0,500,1000,1500,2000,2500)) +
  scale_colour_manual(values=c("#E69F00", "#56B4E9")) +
  scale_linetype(labels=c("Frequency (# per day)","Total Usage (Minutes)")) +
  theme(text=element_text(size=30, face="bold"),
        plot.title = element_text(hjust=.5),
        legend.key.width = unit(1.3,"cm"),
        legend.key.height = unit(1,"cm"),
        legend.position = "bottom",
        legend.text = element_text(colour="gray40"),
        legend.title = element_text(colour="gray40"),
        legend.background = element_rect(colour="black"),
        axis.line = element_line(colour="black"),
        panel.grid.minor = element_line(size=2),
        panel.background = element_rect(fill="azure2"))+
  guides(col=guide_legend(ncol=1, byrow=TRUE),
         linetype=guide_legend(ncol=1, byrow=TRUE))

######## USAGE/FREQUENCY AND AGE GROUP
# daily routine for different age groups
# According to Wikipedia
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

pd = data.frame(summarise(group_by(mydata, weekday,partsofday, Age), Usage = sum(DurationMin)))
pd$partsofday = factor(pd$partsofday, levels=c("Morning","Daytime","Evening"))
# calculate the usage per person 
pd$AveUse="NULL"
for(i in 1:nrow(pd)){
  if(pd[i,"Age"]=="18-25") {pd[i,"AveUse"] = pd[i,"Usage"]/6}
  if(pd[i,"Age"]=="26-35") {pd[i,"AveUse"] = pd[i,"Usage"]/9}
  if(pd[i,"Age"]=="36-50") {pd[i,"AveUse"] = pd[i,"Usage"]/2}
  if(pd[i,"Age"]=="51-65") {pd[i,"AveUse"] = pd[i,"Usage"]/2}
  if(pd[i,"Age"]=="65+") {pd[i,"AveUse"] = pd[i,"Usage"]/1}
  if(pd[i,"Age"]=="Prefer not\nto say") {pd[i,"AveUse"] = pd[i,"Usage"]/2}
}
pd$AveUse = round(as.numeric(as.character(pd$AveUse)), 4)

pd$WkAve = "NULL"
for(i in 1:nrow(pd)){
  if(pd[i,"weekday"]=="Weekdays") {pd[i,"WkAve"]=pd[i,"AveUse"]/9}
  if(pd[i,"weekday"]=="Weekend") {pd[i,"WkAve"]=pd[i,"AveUse"]/2}
}
pd$WkAve = round(as.numeric(as.character(pd$WkAve)), 4)

ggplot(pd, aes(x=Age, y=WkAve, fill=partsofday)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(labels=c("Morning\n00:00-11:59", "Daytime\n12:00-17:59", "Evening\n18:00-23:59"),
                    values=c("lightgreen","lightpink","lightskyblue2")) +
  labs(x="Age Group", y="Usage (Minutes/per person)", fill="Parts of Day",
       title="Daily Average Usage by Age Group\nover 11 day period between 17/07 and 27/07") +
  scale_y_continuous(limits=c(0,100), breaks=c(0,20,40,60,80,100)) +
  theme_bw() +
  theme(text = element_text(size=30, face="bold"),
        strip.text.y = element_text(angle=0, colour="orchid3"),
        legend.key.height = unit(2,"cm"),
        legend.background = element_rect(fill="ivory1"),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(size=1),
        panel.grid.minor = element_line(size=1)) +
  facet_grid(weekday ~ ., scales="free") 
  
#################
gp = data.frame(summarise(group_by(mydata, partsofday, Gender, mday), Usage=sum(DurationMin)))
gp = gp[which(gp$Gender!="Prefer not\nto say"),]
gp$Ave = "NULL"
for(i in 1:nrow(gp)){
  if(gp[i,"Gender"]=="Female") {gp[i,"Ave"]=gp[i,"Usage"]/10}
  if(gp[i,"Gender"]=="Male") {gp[i,"Ave"]=gp[i,"Usage"]/9}
}
gp$Ave = round(as.numeric(as.character(gp$Ave)),4)
gp$partsofday = factor(gp$partsofday, levels=c("Morning","Daytime","Evening"))

ggplot(gp, aes(x=mday, y=Ave, colour=Gender)) +
  geom_point(size=5, shape=18) +
  geom_line(size=1.2) +
  facet_grid(partsofday ~ .) +
  scale_x_continuous(breaks = c(17:27)) +
  theme(text=element_text(size=25, face="bold"),
        strip.text.y = element_text(angle=0, colour="firebrick4"),
        legend.position = "bottom",
        legend.title = element_text(size=20),
        legend.text = element_text(size=20),
        legend.margin = margin(.1,.1,.1,.1,"cm"),
        plot.margin = unit(c(.1,0,.1,0),"cm")) +
  labs(x="Date (July)", y="partsofday", title="Daily Average Usage by Gender\nover 11 day period between 17/07 and 27/07") 
  #geom_vline(xintercept = c(12,18), size=1, linetype=2, colour="blue")
# No particular pattern

#################
### Heatmap mday v.s. groups
head(temp)
FB = mydata[which(mydata$Applications=="Facebook"&mydata$GEgroup!="NULL"),]
FB_gender = data.frame(summarise(group_by(FB, mday, Gender), Counts=length(mday)))
colnames(FB_gender)[2] = "group"
FB_ethnicity = data.frame(summarise(group_by(FB, mday, Ethnicity), Counts=length(mday)))
colnames(FB_ethnicity)[2] = "group"
FB_age = data.frame(summarise(group_by(FB, mday, Age), Counts=length(mday)))
colnames(FB_age)[2] = "group"
FB_counts = rbind(FB_gender, FB_ethnicity, FB_age)
FB_counts$group = factor(FB_counts$group, levels=c("Female","Male","Taiwanese","English","18-25","26-35","36-50","51-65","65+"))

ggplot(FB_counts, aes(x=group, y=mday, fill=Counts)) +
  geom_tile(colour="palegreen", size=1) +
  scale_fill_gradient(low="yellow", high="red") +
  labs(x="", y="Date (July)",title = "Group Comparison of Total Frequency over\n24 hour period between 25/07 21:00 to 26/07 21:00",
       subtitle = "Only Facebook usage counted") +
  geom_text(aes(label=Counts)) +
  scale_y_continuous(breaks = c(17:27)) +
  scale_x_discrete(limits=c("Female","Male","Taiwanese","English","18-25","26-35","36-50","51-65","65+")) +
  theme(text=element_text(size=30, face="bold"),
        plot.subtitle = element_text(colour="#56B4E9"),
        axis.text.x = element_text(angle=330),
        plot.margin = unit(c(.3,.3,.1,.3), "cm"))
#################

  
  
