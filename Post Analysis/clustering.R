clustertemp = temp[which(temp$LocationAccuracy!="N/A"&temp$LocationAltitude!="N/A"&temp$LocationLatitude!="N/A"&temp$LocationLongitude!="N/A"),]
summary(clustertemp)
clustertemp$Gender[which(clustertemp$Gender=="Male")] <- 1
clustertemp$Gender[which(clustertemp$Gender=="Female")] <- 2
clustertemp$Gender = as.numeric(as.character(clustertemp$Gender))
clustertemp = clustertemp[which(clustertemp$Gender!="NA"),]
clustertemp$Ethnicity[which(clustertemp$Ethnicity=="English")] <- 1
clustertemp$Ethnicity[which(clustertemp$Ethnicity=="Taiwanese")] <- 2
clustertemp$Ethnicity = as.numeric(as.character(clustertemp$Ethnicity))
clustertemp$Age[which(clustertemp$Age=="18-25")] <- 1
clustertemp$Age[which(clustertemp$Age=="26-35")] <- 2
clustertemp$Age[which(clustertemp$Age=="36-50")] <- 3
clustertemp$Age[which(clustertemp$Age=="51-65")] <- 4
clustertemp$Age[which(clustertemp$Age=="65+")] <- 5
clustertemp$Age = as.numeric(as.character(clustertemp$Age))
str(clustertemp)

##### Clustering
### K-means
data2 = clustertemp[,c("DurationSec","DurationMin","LocationAccuracy","LocationAltitude","LocationLatitude","LocationLongitude",
                       "Gender","Ethnicity","Age")]
data2$LocationAccuracy = as.numeric(as.character(data2$LocationAccuracy))
data2$LocationAltitude = as.numeric(as.character(data2$LocationAltitude))
data2$LocationLatitude = as.numeric(as.character(data2$LocationLatitude))
data2$LocationLongitude = as.numeric(as.character(data2$LocationLongitude))
str(data2)
### Examine correlation
library(corrplot)
cor(data2[,1:6])
corrplot(cor(data2[,1:6]),"pie","lower")
### Because the correlation between variables are not high enough so no PCA
### cluster
data2$Group = "NULL"
for(i in 1:nrow(data2)){
  if(data2[i,8]=="English"&data2[i,9]=="18-25") {data2[i,10]="A"}
  if(data2[i,8]=="Taiwanese"&data2[i,9]=="18-25") {data2[i,10]="B"}
  if(data2[i,8]=="English"&data2[i,9]=="26-35") {data2[i,10]="C"}
  if(data2[i,8]=="Taiwanese"&data2[i,9]=="26-35") {data2[i,10]="D"}
  if(data2[i,8]=="English"&data2[i,9]=="36-50") {data2[i,10]="E"}
  if(data2[i,8]=="Taiwanese"&data2[i,9]=="36-50") {data2[i,10]="F"}
  if(data2[i,8]=="English"&data2[i,9]=="51-65") {data2[i,10]="G"}
  if(data2[i,8]=="Taiwanese"&data2[i,9]=="51-65") {data2[i,10]="H"}
  if(data2[i,8]=="English"&data2[i,9]=="65+") {data2[i,10]="I"}
  if(data2[i,8]=="Taiwanese"&data2[i,9]=="65+") {data2[i,10]="J"}
}
table(data2$Group)
data2$Groups = NULL
str(data2)
kmeans.result = kmeans(data2[,1:6], 6)
names(kmeans.result)
table(data2$Group)

plot(data2[c("DurationMin","LocationLatitude")], col = kmeans.result$cluster, pch=16,
     main="K-means clustering (k=5)")
points(kmeans.result$centers[,c("DurationMin","LocationLatitude")], col = 1:5, pch = 8, cex=5)

# Visualize
library("factoextra")
fviz_cluster(kmeans.result, data = data2[,1:6], ellipse.type = "norm", geom = "point")+
  theme_minimal()

### Hierarchical
data2 = data2[which(data2$Gender!="Prefer not\nto say"),]
dsample = data2[sample(1:nrow(data2), size=50, replace=F),]
#dsample
names(dsample)
dsample$Group = "NULL"
for(i in 1:nrow(dsample)){
  if(dsample[i,8]=="English"&dsample[i,9]=="18-25") {dsample[i,10]="A"}
  if(dsample[i,8]=="Taiwanese"&dsample[i,9]=="18-25") {dsample[i,10]="B"}
  if(dsample[i,8]=="English"&dsample[i,9]=="26-35") {dsample[i,10]="C"}
  if(dsample[i,8]=="Taiwanese"&dsample[i,9]=="26-35") {dsample[i,10]="D"}
  if(dsample[i,8]=="English"&dsample[i,9]=="36-50") {dsample[i,10]="E"}
  if(dsample[i,8]=="Taiwanese"&dsample[i,9]=="36-50") {dsample[i,10]="F"}
  if(dsample[i,8]=="English"&dsample[i,9]=="51-65") {dsample[i,10]="G"}
  if(dsample[i,8]=="Taiwanese"&dsample[i,9]=="51-65") {dsample[i,10]="H"}
  if(dsample[i,8]=="English"&dsample[i,9]=="65+") {dsample[i,10]="I"}
  if(dsample[i,8]=="Taiwanese"&dsample[i,9]=="65+") {dsample[i,10]="J"}
}

names(dsample)
dsample$Group = NULL
dsample$DurationMin = round(dsample$DurationMin,4)
hc = hclust(dist(dsample[,1:6]), method="ave")
plot(hc, hang=-1, labels=dsample$DurationMin, main="Cluster Dendrogram (k=2)")
rect.hclust(hc, k=2, border=2:6)
table(cutree(hc, k=5))
#data2
hc1 = hclust(dist(data2[,1:6]), method="ave")
summary(hc1)
table(cutree(hc1, k=6))
### Validation
my_data <- scale(data2)
head(my_data)
my_data = my_data[1:100,]
# Compute clValid
library("clValid")
intern = list()
cs = list()
for(i in 1:10){
  cs[[i]] = data2[sample(1:nrow(data2), size=600, replace=F),]
  intern[[i]] = clValid(cs[[i]][,1:6], nClust = c(2,5), 
                        clMethods = c("hierarchical","kmeans"),
                        validation = "internal")
  summary(intern[[i]])
}
par(mfrow=c(1,3))
plot(intern[[8]])

cs = data2[sample(1:nrow(data2), size=600, replace=F),]

intern1 = clValid(cs[,1:6], nClust = c(2,5), 
                  clMethods = c("hierarchical","kmeans"),
                  validation = "internal")
summary(intern1)
plot(intern)
