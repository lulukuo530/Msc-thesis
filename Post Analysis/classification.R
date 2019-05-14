classifydata = mydata[which(mydata$Gender!="Prefer not\nto say" & mydata$pol=="Pos"),]
head(classifydata)
classifydata = subset(classifydata, select=c("Type.of.Comparison","Type.of.Graph","Gender","Ethnicity","Age"))
head(classifydata)
str(classifydata)
levels(classifydata$Type.of.Graph)[2] = "bar chart"

classifydata$Gender1 = ifelse(classifydata$Gender=="Male",1,0)
classifydata$Ethnicity1 = ifelse(classifydata$Ethnicity=="English",1,0)
classifydata$Age1[which(classifydata$Age=="18-25")] <- 1
classifydata$Age1[which(classifydata$Age=="26-35")] <- 2
classifydata$Age1[which(classifydata$Age=="36-50")] <- 3
classifydata$Age1[which(classifydata$Age=="51-65")] <- 4
classifydata$Age1[which(classifydata$Age=="65+")] <- 5
###
classifydata$Compare[which(classifydata$Type.of.Comparison=="personal")] <- 1
classifydata$Compare[which(classifydata$Type.of.Comparison=="peer")] <- 2
classifydata$Compare[which(classifydata$Type.of.Comparison=="group")] <- 3
classifydata$Graph[which(classifydata$Type.of.Graph=="bar chart")] <- 1
classifydata$Graph[which(classifydata$Type.of.Graph=="pie chart")] <- 2
classifydata$Graph[which(classifydata$Type.of.Graph=="line chart")] <- 3
classifydata$Graph[which(classifydata$Type.of.Graph=="heat map")] <- 4
classifydata$Graph[which(classifydata$Type.of.Graph=="scatter plot")] <- 5
###
names(classifydata)[1] = "Compare"
names(classifydata)[2] = "Graph"
newdata = subset(classifydata, select = c("Graph","Compare","Gender1","Ethnicity1","Age1"))
str(newdata)

### fit multinom model
library(nnet)
options(contrast=c("contr.treatment","contr.poly"))
fit=multinom(Graph~Gender1+Ethnicity1+Age1,data=newdata)
summary(fit)
library(caret)
train_control=trainControl(method="cv", number=5)
model1=train(Graph~Gender1+Ethnicity1+Age1,data=newdata,
                   trControl=train_control, method="multinom")
prmodel1=predict(model1,newdata)
c1=confusionMatrix(prmodel1, newdata$Graph) 
c1  #Accuracy : 0.4331

### SVM
library(e1071)
model = svm(Graph~Gender1+Ethnicity1+Age1,data=newdata)
x = subset(newdata, select=-Graph)
x = subset(newdata, select=-Compare)
y = newdata$Graph
pred_result = predict(model, x, decision.values = T)
d = table(pred_result, y)
sum(diag(d))/sum(d)   #Accuracy = 0.4488
mean(pred_result==y)

### Rpart
library(rpart)
m1 = rpart(Graph~Gender+Ethnicity+Age,control=rpart.control(minsplit = 50,cp=0.005),data=classifydata)
plot(m1)
text(m1, pretty = F)

tr.control<-trainControl(method="cv",number=5)
att2<- train(Graph~Gender1+Ethnicity1+Age1, data=newdata, 
                   method="rpart",trControl=tr.control,tuneGrid=expand.grid(.cp=c(0.01,0.005,0.001)))
att2  #Accuracy : 0.3932


### LDA
library(MASS)
a.lda = lda(Graph~Gender+Ethnicity+Age,data=classifydata)
plot(a.lda, dimen=3, col="blue")

lda.pred = predict(a.lda, classifydata)
table(lda.pred$class, classifydata$Graph)
mean(lda.pred$class==classifydata$Graph)   #Accuracy = 0.4449
