library(cluster)
library(factoextra)
library(caret)
library(tidyverse)
set.seed(123)
file<-read.csv(file.choose())

str(file)

##PCA
##kmeans

file<-scale(file)

pca1 <- prcomp(file, scale = TRUE)
fviz_eig(pca1)

#compute standard deviation of each principal component
std_dev <- pca1$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


fviz_pca_var(pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)




# Results for Variables
res.var <- get_pca_var(pca1)
res.var$coord          # Coordinates
res.var$contrib       # Contributions to the PCs
res.var$cos2           # Quality of representation 

## Based on contribution levels to explain variance, I decided to use 
##Store Location, Freshness, Product Quality, Convenient Store Layout, 
##Cleaness, Choice Variety, Healthness, I considermyself a healthyperson,I eat healthy

file2<-file[,c(7,2,8,6,11,3,4,17,16)]

d <- dist(file2, method = "euclidean")

fit <- hclust(d, method="ward.D")
str(fit)
plot(fit) # display dendogram

groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")

## 4 looks reasonable

wss <- (nrow(file2)-1)*sum(apply(file2,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(file2, 
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## seems 4 should be the shortcut

fviz_nbclust(file2, kmeans, method = "wss")

gap_stat <- clusGap(file2, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

function1<-function(k){
  kmeans(file2, centers=k, nstart=10)
  fviz_cluster(kmeans(file2, centers=k, nstart=10), data =file2)
}
function1(3)
function1(4)
function1(5)
function1(6)

## i would choose 4

# get cluster means 

fit1 <- kmeans(file2, 4)

aggregate(file2,by=list(fit1$cluster),FUN=mean)

mydata <- data.frame(file2, fit1$cluster)
clusplot(file2, fit1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

## business insights: seems like 57 & 58 are outliers, other business insighst can be extracted from aggregate function above:
## Group.1 A.StoreLocation A.Freshness A.ProductQuality A.ConvenientStoreLayout A.Cleanliness A.ChoiceVariety A.Healthiness
#1       1      -0.3121517  -0.2433390      -0.12596663             -0.41711881    -0.4603617   -1.782232e-01    -0.1994094
#2       2       0.5148649   0.5293559       0.51142450              0.65134707     0.6290383    4.770341e-01     0.6476817
#3       3      -3.3217091  -4.0618892      -4.14430202             -2.58292804    -3.2225322   -3.467801e+00    -3.0709047
#4       4       0.2283675   0.1548521      -0.08817664              0.05104601     0.3281124   -2.422354e-16    -0.4060700
#L.Iconsidermyselfahealthyperson L.Ieathealthy
#1                       0.2300590    0.23189440
#2                       0.4347406    0.48412130
#3                       0.3972826   -0.01298609
#4                      -1.6458850   -1.68819123


## it seems like lifetyles won't impact much (no big varainces in terms of averages) while product quality,
## choice vareity and freshness are what customers care most

cars.pam<-pam(file2,4)
plot(cars.pam)




### Use attributes only

file3<-file[,1:12]

pca2 <- prcomp(file3, scale = TRUE)
fviz_eig(pca2)

#compute standard deviation of each principal component
std_dev <- pca2$sdev
pr_var <- std_dev^2
prop_varex <- pr_var/sum(pr_var)
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")


fviz_pca_var(pca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

file4<-file3[,c(2,8,11,3,4,5)]

d <- dist(file4, method = "euclidean")

fit <- hclust(d, method="ward.D")
str(fit)
plot(fit) # display dendogram

groups <- cutree(fit, k=4)
rect.hclust(fit, k=4, border="red")

wss <- (nrow(file4)-1)*sum(apply(file4,2,var))
for (i in 1:15) wss[i] <- sum(kmeans(file4, 
                                     centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## seems 4 should be the shortcut

fviz_nbclust(file4, kmeans, method = "wss")

gap_stat <- clusGap(file4, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

function1<-function(k){
  kmeans(file4, centers=k, nstart=10)
  fviz_cluster(kmeans(file4, centers=k, nstart=10), data =file4)
}
function1(3)
function1(4)
function1(5)
function1(6)

## i would choose 4

# get cluster means 

fit1 <- kmeans(file4, 4)

aggregate(file4,by=list(fit1$cluster),FUN=mean)

#Group.1 A.Freshness A.ProductQuality A.Cleanliness A.ChoiceVariety A.Healthiness A.OrganicAlternatives
#1       1   0.4305228        0.4282865    0.66292091       0.3866538     0.8375195             0.9186118
#2       2   0.3930860        0.3012702    0.09667597       0.4088058    -0.1046899            -0.8779209
#3       3  -0.5448035       -0.4176788   -0.51560515      -0.5787088    -0.4701864             0.2176833
#4       4  -4.0618892       -4.1443020   -3.22253219      -3.4678010    -3.0709047            -1.1783649

mydata <- data.frame(file4, fit1$cluster)
clusplot(file4, fit1$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

pam1 <- pam(file4,4)
plot(pam1)

wss[4]

file<-read.csv(file.choose())

aggregate(file,by=list(fit1$cluster),FUN=mean)

data1<-data.frame(file,fit1$cluster)
write.csv(data1, "DOS2final.csv")
