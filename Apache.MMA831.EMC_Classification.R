
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

banking <- read_excel(file.choose())

head(banking)

# glimpse 
glimpse(banking)

# age consider making age a factor? 
range(banking$age)
str(banking$age)

df <- banking %>%
  group_by(age) %>%
  summarise(counts = n())

ggplot(df, aes(x = age, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

qqnorm(banking$age)
skewness(banking$age)

# job 
banking$job <- as.factor(banking$job)
str(banking$job)
sort(table(banking$job)) # 330 unknown 

# marital 
banking$marital <- as.factor(banking$marital)
str(banking$marital)
sort(table(banking$marital)) # only 80 unknown
ggplot(banking, aes(marital)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


# education D
banking$education <- as.factor(banking$education)
str(banking$education)
sort(table(banking$education)) # 18 illiterate people 
ggplot(banking, aes(education)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# default DROP
banking$default <- as.factor(banking$default)
str(banking$default)
sort(table(banking$default))
ggplot(banking, aes(default)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


# housing
banking$housing <- as.factor(banking$housing)
str(banking$housing)
sort(table(banking$housing))
ggplot(banking, aes(housing)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# loan 
banking$loan <- as.factor(banking$loan)
sort(table(banking$loan))
ggplot(banking, aes(loan)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# contact
banking$contact <- as.factor(banking$contact)
sort(table(banking$contact))
ggplot(banking, aes(contact)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# year 2010 is very few people 
str(banking$year)
range(banking$year)
banking$year <- as.factor(banking$year)
sort(table(banking$year))
ggplot(banking, aes(year)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# month ONLY 10 months and the main months are April to May 
str(banking$month)
banking$month <- as.factor(banking$month)
sort(table(banking$month)) 
ggplot(banking, aes(month)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


# day_of_week
str(banking$day_of_week)
banking$day_of_week <- as.factor(banking$day_of_week)
sort(table(banking$day_of_week))
ggplot(banking, aes(day_of_week)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

# duration
str(banking$duration)
df <- banking %>%
  group_by(duration) %>%
  summarise(counts = n())

ggplot(df, aes(x = duration, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()

qqnorm(banking$duration)
skewness(banking$duration) # slightly skewed 3.267
range(banking$duration)
count(banking[banking$duration == 0,]) # ONLY 4 zeros? remove..?
# need to remove the zeros to log transform this variable 

# campaign 
str(banking$campaign)
range(banking$campaign)

df <- banking %>%
  group_by(campaign) %>%
  summarise(counts = n())

ggplot(df, aes(x = campaign, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()

skewness(banking$campaign) # slightly skewed as well
qqnorm(banking$campaign)
qqnorm(log(banking$campaign))
skewness(log(banking$campaign)) # this works well if we want to take the log to deal with skewness

# pdays 
str(banking$pdays)
range(banking$pdays)

df <- banking %>%
  group_by(pdays) %>%
  summarise(counts = n())

ggplot(df, aes(x = pdays, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()

# obviously crazy distribution here
# let's look at it as a factor
banking$pdays <- as.factor(banking$pdays)
sort(table(banking$pdays))

# convert it back now
banking$pdays <- unfactor(banking$pdays)
str(banking$pdays)

# previous 
str(banking$previous)
sort(table(banking$previous))
ggplot(banking, aes(previous)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
skewness(banking$previous) # slightly skewed 

# poutcome
str(banking$poutcome)
sort(table(banking$poutcome))
banking$poutcome <- as.factor(banking$poutcome)

# emp.var.rate ## not too sure about this variable 
str(banking$emp.var.rate)
range(banking$emp.var.rate)
ggplot(banking, aes(emp.var.rate)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()
skewness(banking$emp.var.rate)

# cons.price.idx
range(banking$cons.price.idx)
sort(table(banking$cons.price.idx))
skewness(banking$cons.price.idx)
ggplot(banking, aes(cons.price.idx)) +
  geom_density(fill = "blue") +
  theme_pubclean()
skewness(banking$cons.price.idx) # no skew

# cons.conf.idx 
range(banking$cons.conf.idx)
ggplot(banking, aes(cons.conf.idx)) +
  geom_density(fill = "blue") +
  theme_pubclean()
skewness(banking$cons.conf.idx) # no skew

# euribor3m ### seems like all the data is around 1 or around 5 and none in the middle 
str(banking$euribor3m)
range(banking$euribor3m)
ggplot(banking, aes(euribor3m)) +
  geom_density(fill = "blue") +
  theme_pubclean()
skewness(banking$euribor3m) 

# nr.employed
str(banking$nr.employed)
summary(banking$nr.employed)
range(banking$nr.employed)
table(banking$nr.employed)
ggplot(banking, aes(nr.employed)) +
  geom_density(fill = "blue") +
  theme_pubclean()
skewness(banking$nr.employed) # no skew 

# Y VARIABLE
table(banking$y) # very skewed
df <- banking %>%
  group_by(y) %>%
  summarise(counts = n())

ggplot(df, aes(x = y, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()

# change to factor
str(banking$y)
banking$y <- as.factor(banking$y) # skewness is evident, may need to upsample to increase "YES" responses

# percentage of yes
sales <- (table(banking$y)[2])/(table(banking$y)[1] + table(banking$y)[2])
sales # 11% 

# num unknowns
summary(banking$num_unknowns)
table(banking$num_unknowns) # almost all zeros 
 # slightly skewed to the right 

banking$num_unknowns <- as.factor(banking$num_unknowns)
table(banking$num_unknowns) # clearly a factor with 5 categories 
# may be worth dropping the 5th category with only two entries

ggplot(banking, aes(num_unknowns)) +
  geom_density(fill = "blue") +
  theme_pubclean()
skewness(banking$num_unknowns)


# has unknown 
summary(banking$has_unknown)
banking$has_unknown <- as.factor(banking$has_unknown)
sort(table(banking$has_unknown))

df <- banking %>%
  group_by(has_unknown) %>%
  summarise(counts = n())

ggplot(df, aes(x = has_unknown, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()

# no contact 
summary(banking$no_contact) # skewed to No heavily 
banking$no_contact <- as.factor(banking$no_contact)

df <- banking %>%
  group_by(no_contact) %>%
  summarise(counts = n())

ggplot(df, aes(x = no_contact, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  theme_pubclean()


###### DATA IS CLEAN BUT WE HAVE TO MAKE DECISIONS ON REMOVING OUTLIERS ###### 
# creating a nice csv version
#write.csv(banking, "Banking_Clean.csv")

#############Running Model with no Feature Engineering & With Unknowns#####################

# split 
library(caTools)
set.seed(123)
split <- sample.split(banking$y, SplitRatio = 0.2)
test_bank = subset(banking , split == TRUE)
train_bank = subset(banking , split == FALSE)

rm(split)

# logit 
thresh = 0.11
log_model <- glm(as.factor(y) ~., data=test_bank, family="binomial")
summary(log_model)
predict <- predict(log_model, test_bank, type="response")
predict <- ifelse(predict < thresh,0,1)
accuracy  <- sum(diag(table(test_bank$y, predict))/nrow(test_bank))
accuracy
table(test_bank$y)
table(predict)

####ROC Curve
log_ROC_prediction <- prediction(predict,test_bank$y)
log_ROC <- performance(log_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(log_ROC_val) #Plot ROC curve

####AUC (area under curve)
auc.tmp.log <- performance(log_ROC_prediction,"auc") #Create AUC data
log_auc_val <- as.numeric(auc.tmp.log@y.values) #Calculate AUC
log_auc_val

predict <- ifelse(predict == 0,"no","yes")
levels(predict) <- levels(test_bank$y)
table(predict,test_bank$y)
sensitivity <- 818/(818+110)
specificity <- 6263/(6263+1047)

log_auc_val
accuracy
sensitivity
specificity

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



# logit MOdel Emilie Feature Engineering

set.seed(123)
split <- sample.split(banking$y, SplitRatio = 0.2)

str(banking)

banking_nounk <- banking
test_bank = subset(banking , split == TRUE)
train_bank = subset(banking , split == FALSE)

rm(split)

thresh = 0.11
log_model <- glm(as.factor(y) ~., data=test_bank, family="binomial")
summary(log_model)
predict <- predict(log_model, test_bank, type="response")
predict <- ifelse(predict < thresh,0,1)
accuracy  <- sum(diag(table(test_bank$y, predict))/nrow(test_bank))
accuracy
table(test_bank$y)
table(predict)

####ROC Curve
log_ROC_prediction <- prediction(predict,test_bank$y)
log_ROC <- performance(log_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(log_ROC_val) #Plot ROC curve

####AUC (area under curve)
auc.tmp.log <- performance(log_ROC_prediction,"auc") #Create AUC data
log_auc_val <- as.numeric(auc.tmp.log@y.values) #Calculate AUC
log_auc_val

predict <- ifelse(predict == 0,"no","yes")
levels(predict) <- levels(test_bank$y)
table(predict,test_bank$y)
sensitivity <- 820/(820+108)
specificity <- 6267/(6267+1043)

log_auc_val
accuracy
sensitivity
specificity



