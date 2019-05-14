
#fb = temp[which(temp$Applications=="Facebook"),]
ss = data.frame(summarise(group_by(temp, UserID), Usage = sum(DurationMin)))
ss$real = (ss$Usage-mean(ss$Usage))/sd(ss$Usage)   # calculate normality
ss$group = ifelse(ss$real>0, "above", "below")
ss = ss[order(ss$real),]   # sort data
ss$UserID = factor(ss$UserID, levels=ss$UserID)

ggplot(ss, aes(x=UserID, y=real)) +
  geom_bar(aes(fill=group), stat="identity", width=.5) +
  scale_fill_manual(name = "Usage (Minutes)",
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) +
  coord_flip() + theme_bw() +
  labs(y="Normalised value", title="Diverging Bars of Total Usage\nover 13 day period between 17/07 and 29/07",
       subtitle="Normalised Total Usage") +
  theme(text=element_text(size=30, face = "bold"),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5, colour="steelblue"),
        legend.key.height = unit(1,"cm"),
        legend.background = element_rect(fill="grey90"),
        legend.position = "bottom") +
  scale_y_continuous(limits = c(-3,3), breaks = c(-3:3)) +
  annotate("text", x=c(15:22), y=c(0.25,0.8,1.06,1.09,1.18,1.48,1.95,2.78)+.1, 
           label=paste(round(ss$real,2)[15:22]), size=8, fontface="bold", col="darkslategrey") +
  annotate("text", x=c(14:1), y=c(-0.37,-0.45,-0.62,-0.69,-0.7,-0.71,-0.79,-0.81,-0.96,-1.02,-1.04,-1.16,-1.2,-1.28)-.1, 
           label=paste(round(ss$real,2)[14:1]), size=8, fontface="bold", col="darkslategrey")

##############################
### Self comparison by Usage and Freq over 13 days
ff = data.frame(summarise(group_by(temp, ParticipantID, Applications, mday), Freq=length(UserID), Usage=sum(DurationMin)))
ff$ave = ff$Usage/ff$Freq
usersf=list(); scatterplots=list()
for(i in 1:22){
  usersf[[i]] = ff[which(ff$ParticipantID==paste(unique(ff$ParticipantID)[i])),]
  scatterplots[[i]] = 
    ggplot(usersf[[i]], aes(x=Usage, y=Freq)) +
    geom_point(aes(colour=Applications, size=ave),alpha=.7) +
    scale_size_continuous(range = c(3,20), name="Average Usage\n(Minutes/per time)") +
    labs(x="Total Usage (Minutes)",y="Frequency (# per day)",subtitle=paste(unique(ff$ParticipantID)[i]),
         title="Frequency and Total Usage by Applications\nover 13 day period between 17/07 and 29/07") +
    theme_bw() +
    theme(text = element_text(size=30, face="bold"),
          plot.title = element_text(hjust = .5),
          plot.subtitle = element_text(hjust = .5, colour="steelblue"),
          #legend.position = "bottom",
          legend.key.height = unit(1,"cm"),
          legend.background = element_rect(colour="darkslategrey"),
          legend.text = element_text(size=25),
          legend.title = element_text(size=25),
          panel.grid = element_line(size=2),
          panel.grid.minor = element_line(size=1)) +
    scale_colour_manual(values=c("lightgreen","lightsteelblue3","lightskyblue2","lightcoral",
                                 "lightgoldenrod","mediumpurple1","mistyrose1")) +
    guides(colour=guide_legend(override.aes = list(size=8, alpha=1)))
}
scatterplots




i=2
ggplot(usersf[[i]], aes(x=Usage, y=Freq)) +
  geom_point(aes(colour=Applications, size=ave),alpha=.7) +
  scale_size_continuous(range = c(5,20), name="Average Usage\n(Minutes/per time)") +
  labs(x="Total Usage (Minutes)",y="Frequency (# per day)",subtitle=paste(unique(ff$ParticipantID)[i]),
       title="Frequency and Total Usage by Applications\nover 13 day period between 17/07 and 29/07") +
  theme_bw() +
  theme(text = element_text(size=30, face="bold"),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5, colour="steelblue"),
        #legend.position = "bottom",
        legend.key.height = unit(1,"cm"),
        legend.background = element_rect(colour="darkslategrey"),
        legend.text = element_text(size=25),
        legend.title = element_text(size=25),
        panel.grid = element_line(size=2),
        panel.grid.minor = element_line(size=1)) +
  scale_colour_manual(values=c("lightgreen","lightsteelblue3","lightskyblue2","lightcoral",
                               "lightgoldenrod","mediumpurple1","mistyrose1")) +
  guides(colour=guide_legend(override.aes = list(size=8, alpha=1)))
