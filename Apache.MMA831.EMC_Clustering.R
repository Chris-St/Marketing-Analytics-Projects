
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("ggpubr","dmm","catools","rocr")

# Marketing Analytics Project
library(readxl)
library(ggplot2)
library(dplyr)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("dmm")
library(dmm)

#1 Logistic Regression Model

banking <- read_excel(file.choose(),2)

head(banking)

# glimpse 
glimpse(banking)

###############Model after removing the unknowns #################

###Emilie: feature engineering
banking$youngAge <- ifelse(banking$age <=30, 1,0)
banking$retiredAge <- ifelse(banking$age >=66,1,0)
banking$groupedLoans <- ifelse(banking$housing == "yes" | banking$loan == "yes", 1,0)
banking$taxSeason <- ifelse(banking$month == "mar" | banking$month == "apr", 1,0)
banking$groupedPdays_notcontacted <- ifelse(banking$pdays == 0, 1,0)
banking$groupedPdays_within1week <- ifelse(banking$pdays != 0 & banking$pdays <= 7, 1,0)
banking$groupedPdays_1to2weeks <- ifelse(banking$pdays >7 & banking$pdays <= 14, 1,0)
banking$groupedPdays_2to3weeks <- ifelse(banking$pdays >14 & banking$pdays <= 21, 1,0)
banking$groupedPdays_3to4weeks <- ifelse(banking$pdays >21 & banking$pdays <= 28, 1,0)
banking$groupedEducation_basic <- ifelse(banking$education == "illiterate" | banking$education == "basic.4y" | banking$education == "basic.6y" | banking$education == "basic.9y",1,0)
banking$groupedEducation_higher <- ifelse(banking$education == "university.degree" | banking$education == "professional.course",1,0)
str(banking)

demographic <- c("age","job","marital","education")
bases <- c("default","housing","loan","contact", "month","day_of_week","duration","campaign","pdays","previous","poutcome","has_unknown","y")
social <- c("emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")

#########################Clustering ########################

banking_bases <- banking[bases]
banking_bases <- banking_bases[which(banking_bases$has_unknown == "N"),]
str(banking_bases)
banking_bases$has_unknown <- NULL
banking_bases$default <- NULL

#hclust
d <- dist(banking_bases, method = "euclidean")
fit <- hclust(d, method="ward.D")
summary(fit)
str(fit)
plot(fit)

library(factoextra)
fviz_nbclust(banking_bases, kmeans, method = "silhouette")

groups3 <- cutree(fit, k=3)
plot(fit)
rect.hclust(fit, k=3, border="red")
library(cluster)
plot(silhouette(groups3,d), col=1:2, border=NA)

groups4 <- cutree(fit, k=4)
plot(fit)
rect.hclust(fit, k=4, border="red")
plot(silhouette(groups4,d), col=1:2, border=NA)


#k-means

wss <- (nrow(banking_bases)-1)*sum(apply(banking_bases,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(banking_bases, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

set.seed(123) # set seed to make sure the random assignment starts at the same point
seg.k5 <- kmeans(banking_bases, centers=5)

# inspect it
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

seg.summ(banking_bases, seg.k5$cluster)

clusplot(banking_bases, seg.k5$cluster, color=TRUE, shade=TRUE, 
         labels=5, lines=0, main="K-means cluster plot")

plot(silhouette(seg.k5$cluster,d), col=1:2, border=NA)

banking_known <- banking[which(banking$has_unknown == "N"),]
df_hclust <- data.frame(banking_known, groups3,groups4,seg.k5$cluster)

write.csv(df_hclust,"Final HClusters_Final.csv")


########### Adding the Demographics to see if the results improve ##############


banking_demo <- banking[c(bases,demographic)]
banking_demo <- banking_demo[which(banking_demo$has_unknown == "N"),]
str(banking_demo)
banking_demo$has_unknown <- NULL
banking_demo$default <- NULL

d.demo <- dist(banking_demo, method = "euclidean")
fit_demo <- hclust(d.demo, method="ward.D")
summary(fit_demo)
str(fit_demo)


groups4_demo <- cutree(fit_demo, k=4)
plot(fit_demo)
rect.hclust(fit_demo, k=4, border="red")
library(cluster)
plot(silhouette(groups4_demo,d), col=1:2, border=NA)









