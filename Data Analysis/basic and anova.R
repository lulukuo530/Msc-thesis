### Daily data compiled and given label, file name 'temp'
temp = temp[which(temp$DurationMilliSec>1000),]
temp$Gender="Prefer not\nto say"
temp$Gender[which(temp$UserID=="FYIV"|temp$UserID=="IRHV"|temp$UserID=="QPDZ"|
                    temp$UserID=="ZLCZ"|temp$UserID=="CWEV"|temp$UserID=="KEQH"|
                    temp$UserID=="DUGS"|temp$UserID=="VRLE"|temp$UserID=="WEZF")] <- "Male"
temp$Gender[which(temp$UserID=="KYVV"|temp$UserID=="YOGB"|temp$UserID=="NRSP"|
                    temp$UserID=="JLFU"|temp$UserID=="QISZ"|temp$UserID=="HNMA"|
                    temp$UserID=="TGQN"|temp$UserID=="IUZU"|temp$UserID=="XRNC"|
                    temp$UserID=="JSSR")] <- "Female"
temp$Ethnicity="English"
temp$Ethnicity[which(temp$UserID=="FYIV"|temp$UserID=="JLFU"|temp$UserID=="QISZ"|
                       temp$UserID=="HNMA"|temp$UserID=="IRHV"|temp$UserID=="QPDZ"|
                       temp$UserID=="KYVV"|temp$UserID=="YOGB"|temp$UserID=="NRSP"|
                       temp$UserID=="ZLCZ"|temp$UserID=="TGQN"|temp$UserID=="IUZU")] <-"Taiwanese"
temp$Age="Prefer not\nto say"
temp$Age[which(temp$UserID=="FYIV"|temp$UserID=="JLFU"|temp$UserID=="YOGB"|
                 temp$UserID=="TGQN"|temp$UserID=="DUGS"|temp$UserID=="UKFG")] <- "18-25"
temp$Age[which(temp$UserID=="QISZ"|temp$UserID=="IRHV"|temp$UserID=="QPDZ"|
                 temp$UserID=="KYVV"|temp$UserID=="ZLCZ"|temp$UserID=="IUZU"|
                 temp$UserID=="CWEV"|temp$UserID=="VRLE"|temp$UserID=="JSSR")] <- "26-35"
temp$Age[which(temp$UserID=="HNMA"|temp$UserID=="NRSP")] <- "36-50"
temp$Age[which(temp$UserID=="KEQH"|temp$UserID=="XRNC")] <- "51-65"
temp$Age[which(temp$UserID=="WEZF")] <- "65+"
### 
dd1 = subset(temp, select=c("DurationMin","Gender","Ethnicity","Age"))
dd1 = dd1[which(dd1$Gender!="Prefer not\nto say"|dd1$Age!="Prefer not\nto say"),]
summary(dd1)
##### Hypothesis Testing
###Examine outliers
par(mfrow=c(1,3))
boxplot(DurationMin~Gender, data=dd1)
boxplot(DurationMin~Ethnicity, data=dd1)
boxplot(DurationMin~Age, data=dd1)
#### No outliers but severe skewness
boxplot(log(DurationMin)~Gender, data=dd1)
dp = dd1[sample(1:nrow(dd1), 100, replace=F),]
stripchart(log(DurationMin)~Gender, data=dp, vertical = TRUE, method = "jitter",
            pch = 21, col = c("maroon", "blue", "green4"), bg = "bisque", add = TRUE)
boxplot(log(DurationMin)~Ethnicity, data=dd1)
stripchart(log(DurationMin)~Ethnicity, data=dp, vertical = TRUE, method = "jitter",
           pch = 21, col = c("maroon", "blue", "green4"), bg = "bisque", add = TRUE)
boxplot(log(DurationMin)~Age, data=dd1)
stripchart(log(DurationMin)~Age, data=dp, vertical = TRUE, method = "jitter",
           pch = 21, col = c(1:6), bg = "bisque", add = TRUE)
# After log transformation it seems better. Several outliers but not many
dd1$logMin = log(dd1$DurationMin)

###Test normality
library(ggplot2)
p1 = ggplot(dd1, aes(sample=log(DurationMin), col=Gender, shape=Gender))
p1 + geom_qq() + 
  scale_color_brewer(palette="Dark2") +
  labs(title="Q-Q plot by Gender") + 
  theme_classic() +
  theme(plot.title = element_text(hjust=.5), text = element_text(size=15))
p2 = ggplot(dd1, aes(sample=log(DurationMin), col=Ethnicity, shape=Ethnicity))
p2 + geom_qq() + 
  scale_color_brewer(palette="Dark2") +
  labs(title="Q-Q plot by Ethnicity") + 
  theme_classic() +
  theme(plot.title = element_text(hjust=.5), text = element_text(size=15)) +
  scale_x_continuous(limits=c(-5,5)) +
  scale_y_continuous(limits=c(-5,5)) 
p3 = ggplot(dd1, aes(sample=log(DurationMin), col=Age))
p3 + geom_qq() + 
  scale_color_brewer(palette="Dark2") +
  labs(title="Q-Q plot by Age Group") + 
  theme_classic() +
  theme(plot.title = element_text(hjust=.5), text = element_text(size=15))

###Test homogeneity
library(car)
leveneTest(logMin~Gender, data=dd1)
leveneTest(logMin~Ethnicity, data=dd1)
leveneTest(logMin~Age, data=dd1)

ff = dd1[which(dd1$Gender=="Female"),]
mm = dd1[which(dd1$Gender=="Male"),]
female = ff[sample(1:nrow(ff), size=7330, replace=F),]
male = mm[sample(1:nrow(mm), size=7330, replace=F),]
g = rbind(female, mm)
table(g$Gender)
leveneTest(logMin~Gender, data=g)

table(dd1$Ethnicity)
ee = dd1[which(dd1$Ethnicity=="English"),]
tt = dd1[which(dd1$Ethnicity=="Taiwanese"),]
english = ee[sample(1:nrow(ee), size=2649, replace=F),]
taiwan = tt[sample(1:nrow(tt), size=2650, replace=F),]
e = rbind(english, taiwan)
table(e$Ethnicity)
leveneTest(logMin~Ethnicity, data=e)

table(dd1$Age1)
one = dd1[which(dd1$Age1=="1"),]
two = dd1[which(dd1$Age1=="2"),]
three = dd1[which(dd1$Age1=="3"),]
four = dd1[which(dd1$Age1=="4"),]
five = dd1[which(dd1$Age1=="5"),]
one = one[sample(1:nrow(one), size=80, replace=F),]
two = two[sample(1:nrow(two), size=80, replace=F),]
three = three[sample(1:nrow(three), size=80, replace=F),]
four = four[sample(1:nrow(four), size=80, replace=F),]
agea = rbind(one,two,three,four,five)
table(agea$Age)
leveneTest(logMin~Age, data=agea)

###Descriptive Stat
library(psych)
describeBy(g, g$Gender)
describeBy(e,e$Ethnicity)
describeBy(agea,agea$Age)

###ANOVA
#g, e, agea
a1 = aov(logMin~Gender, data=g)
a2 = aov(logMin~Ethnicity, data=e)
a3 = aov(logMin~Age, data=agea)
summary(a1)
summary(a2)
summary(a3)
# Ethnicity and Age are significant

###Tukey test to see further difference between groups
ta1 = TukeyHSD(a2)
ta1$Ethnicity
par(oma=c(2,2,2,2))
plot(ta1, col="brown")
ta2 = TukeyHSD(a3)
ta2
plot(ta2, las=1, col="brown")

##### VISUALISE RESULTS #####

###############
##### Emotions
emotions = read.csv("/users/macintosh/Desktop/BU/MScADA/3_Dissertation/Experiment/Feedback/emotions feedback.csv", header=TRUE)
emotions = emotions[,c(2,5,6)]
head(emotions)
library(lubridate)
emotions$hour = hour(emotions$FeedbackTimestampHumanReadableWithTZ)
emotions$mday = mday(emotions$FeedbackTimestampHumanReadableWithTZ)
head(emotions)
nrow(emotions)
nrow(temp)
try1 = emotions[,c(1,2,4,5)]
try2 = temp[,c(1,10,14,15)]
names(try2)[2] = "hour"
try1$time = paste(try1$mday, try1$hour, sep="/")
try2$time = paste(try2$mday, try2$hour, sep="/")
try1 = try1[try1$time %in% try2$time,]
try2 = try2[try2$time %in% try1$time,]
library(dplyr)
try2 = data.frame(summarise(group_by(try2, ParticipantID, time), Usage=sum(DurationMin)))

head(try1)
head(try2)
nrow(try1)+nrow(try2)
try1 = subset(try1, select=c("ParticipantID","FeedbackText","time"))
try = merge(try1, try2, by=c("ParticipantID","time"))
head(try)
### Test ANOVA assumptions
dev.off()
boxplot(log(Usage)~FeedbackText, data=try, xlab="FeedbackText", main="Boxplot of Data after log transformation")
stripchart(log(Usage)~FeedbackText, data=try, vertical = TRUE, method = "jitter",
           pch = 21, col = c("lightpink2","mediumspringgreen","lightskyblue","plum2",
                             "indianred3","forestgreen","dodgerblue2","palevioletred4"), bg = "bisque", add = TRUE)

ggplot(try, aes(sample=log(Usage), col=FeedbackText)) +
  geom_qq() +
  scale_colour_manual(values=c("lightpink2","mediumspringgreen","lightskyblue","plum2",
                               "indianred3","forestgreen","dodgerblue2","palevioletred4"),
                      labels=c("Boredom","Pensiveness","Distraction","Apprehension",
                               "Acceptance","Serenity","Interest","Annoyance")) +
  labs(title="Q-Q plot by Emotions", col="Emotions") + 
  theme_classic() +
  theme(plot.title = element_text(hjust=.5), text = element_text(size=15)) +
  scale_x_continuous(limits=c(-4,4)) 
  
leveneTest(log(Usage)~FeedbackText, data=try)
try$logUsage = log(try$Usage)

nrow(try)
try$FeedbackText = as.factor(try$FeedbackText)
trya = aov(logUsage~FeedbackText, data=try)
summary(trya)

describeBy(try, try$FeedbackText)
### Draw emotions and usage over time
try$mday = substr(try$time,1,2)
try$hour = substr(try$time,4,5)
try$hour = as.numeric(as.character(try$hour))
trymood = try[which(try$hour>=8 & try$hour<=23),]
trymood$group[which(trymood$mday>="20" & trymood$mday<="24")] <- "A"
trymood$group[which(trymood$mday>="25" & trymood$mday<="29")] <- "B"
head(trymood)

#library(ggplot2)
trymood$mday = paste(trymood$mday,07,sep="/")
ggplot(trymood[which(trymood$group=="A"),], aes(x=hour, y=Usage, col=FeedbackText)) +
  geom_point(size=2.5) +
  facet_grid(mday~.) +
  scale_x_continuous(breaks=c(8:23)) +
  scale_colour_manual(values=c("lightpink2","mediumspringgreen","lightskyblue","plum2",
                               "indianred3","forestgreen","dodgerblue2","palevioletred4"),
                      labels=c("Boredom","Pensiveness","Distraction","Apprehension",
                               "Acceptance","Serenity","Interest","Annoyance")) +
  labs(x="Hour", y="Total Hourly Usage (Min)", col="Emotions",
       title="Total Hourly Usage From 8am to 11pm\nby Emotions between 20/07 and 24/07") +
  theme(plot.title=element_text(hjust=.5), 
        text=element_text(size=15))

ggplot(trymood[which(trymood$group=="B"),], aes(x=hour, y=Usage, col=FeedbackText)) +
  geom_point(size=2.5) +
  facet_grid(mday~.) +
  scale_x_continuous(breaks=c(8:23)) +
  scale_colour_manual(values=c("lightpink2","mediumspringgreen","lightskyblue","plum2",
                               "indianred3","forestgreen","dodgerblue2","palevioletred4"),
                      labels=c("Boredom","Pensiveness","Distraction","Apprehension",
                               "Acceptance","Serenity","Interest","Annoyance")) +
  labs(x="Hour", y="Total Hourly Usage (Min)", col="Emotions",
       title="Total Hourly Usage From 8am to 11pm\nby Emotions between 25/07 and 29/07") +
  theme(plot.title=element_text(hjust=.5), 
        text=element_text(size=15))

library(dplyr)
ggplot(trymood[which(trymood$group=="B"),], aes(x=hour, y=mday, fill=Usage)) +
  geom_tile() +
  facet_grid(.~FeedbackText)



