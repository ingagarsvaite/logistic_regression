#clearing environment
#rm(list = ls())


#libraries
#install.packages(c("tidyverse","ggplot2","psych","rstatix","readxl"))
library("tidyverse")
library("ggplot2")
#install.packages("kernlab")
library(kernlab)
#install.packages("ggcorrplot")
library(ggcorrplot)
library(psych)
#install.packages("ltm")
library(ltm)
library(rstatix)
library(readxl)
library(dplyr)

# Install readr explicitly
install.packages("readr")

# You might also consider updating the entire tidyverse to ensure compatibility
install.packages("tidyverse")

install.packages("timechange")



#reading data
data <- read.csv("df_final_mortality.csv")
head(data)

#dataset info
dim(data)
summary(data)

#no missing values
colSums(is.na(data))

# Replace NANs in types of coffee with zeros, if the person didn't drink any coffee at all
data$TotalCoffeeIntake <- ifelse(is.na(data$TotalCoffeeIntake) & data$Coffee == 0, 0, data$TotalCoffeeIntake)
data$CaffeinatedStatus <- ifelse(is.na(data$CaffeinatedStatus) & data$Coffee == 0, 0, data$CaffeinatedStatus)
data$SugaryStatus <- ifelse(is.na(data$SugaryStatus) & data$Coffee == 0, 0, data$SugaryStatus)
data$FattyStatus <- ifelse(is.na(data$FattyStatus) & data$Coffee == 0, 0, data$FattyStatus)
data$MilkContainingStatus <- ifelse(is.na(data$MilkContainingStatus) & data$Coffee == 0, 0, data$MilkContainingStatus)


#creating the categorical variables according to the TABLE 1 in Journal of periodontology
data <- data%>% 
  rename(Age_num = RIDAGEYR,
         Sex = RIAGENDR,
         Education = DMDEDUC,
         Race = RIDRETH1,
         MaritalStatus = DMDMARTL,
         PovertyIncome = INDFMPIR) %>%
  mutate(Sex = as.factor(Sex),
         Education = as.factor(Education),
         Race = as.factor(Race),
         MaritalStatus = as.factor(MaritalStatus),
         PAD = as.factor(PAD),
         Coffee = as.factor(Coffee),
         CaffeinatedStatus = as.factor(CaffeinatedStatus),
         SugaryStatus = as.factor(SugaryStatus),
         FattyStatus = as.factor(FattyStatus),
         MilkContainingStatus = as.factor(MilkContainingStatus)) %>%
  mutate(Sex = plyr::revalue(Sex, c("1" = "Male","2" = "Female")),
         Education = case_when(Education == "1" ~ "Less Than High School",
                               Education == "2" ~ "High School",
                               Education == "3" ~ "Some college or above",
                               Education == "7" | Education == "9" ~ "Incomplete"), #patients REFUSED to give into about the education or they DON'T KNOW
         Education = as.factor(Education),
         MaritalStatus = case_when(MaritalStatus == "1" ~ "Married",
                                   MaritalStatus == "5" ~ "Never Married",
                                   MaritalStatus =="2" | MaritalStatus == "3" | MaritalStatus == "4" | MaritalStatus == "6" ~ "Unmarried but have or had a partner"),
         MaritalStatus = as.factor(MaritalStatus),
         Race = case_when(Race == "1" ~ "Mexican American",
                          Race == "4" ~ "Non-Hispanic Black",
                          Race == "3" ~ "Non-Hispanic White",
                          Race == "2" | Race == "5" ~ "Other"),
         Race = as.factor(Race),
         Age_fct = case_when(Age_num < 65 ~ "Age < 65 years",
                             Age_num >= 65 ~ "Age >= 65 years"),
         Age_fct = as.factor(Age_fct)) %>%
  filter(LEXLABPI < 1.40 & LEXRABPI <1.40) #filtering according to the first paragraph in the Results section in the "Secondary prevention..." article





summary(data)
data <- na.omit(data)

summary(data)
##################################################################################################
#-----2-------Logistic regression 
##################################################################################################


model_full = glm(PAD ~  TotalCoffeeIntake + CaffeinatedStatus + SugaryStatus + FattyStatus + MilkContainingStatus,family = "binomial",data=data,weights = )
summary(model_full)
model_step = stats::step(model_full, direction = "both")
summary(model_step)

#####################################################

# Multiple logistic regression

# Trying logistic regression
# Firstly lets see if caffeine intake is linked with mortality
# dependent variable is mortality. Independant variables: TotalCoffeeIntake + CaffeinatedStatus + SugaryStatus, Age, Sex, Education,Age_fct
#PAD: Disease

#Coffee: Patient consumed coffee (1) or not (0)

#CaffeinatedStatus: Caffeinated Coffee(1) | Decaffeited Coffee(0)

#SugaryStatus: Sweetened Coffee(1) | Unsweetened Coffee(0)

#FattyStatus: Coffee with Fat(1) | FatFree Coffee(0)

#MilkContainingStatus: Coffee with milk (1) | Coffee without milk(0)
# mortality: 0-alive, 1- dead
head(data)

View(data)

# models with TotalCoffeeIntake, Age and Sex. 

model1 <- glm(mortstat ~ TotalCoffeeIntake + Age_num + Sex, family = binomial(), data = data)
summary(model1)

model2 <- glm(mortstat ~ TotalCoffeeIntake + Age_num, family = binomial(), data = data)
summary(model2)

model3 <- glm(mortstat ~ TotalCoffeeIntake + Sex, family = binomial(), data = data)
summary(model3)

model4 <- glm(mortstat ~ TotalCoffeeIntake, family = binomial(), data = data)
summary(model4)

# model whether coffee was consumed or not

model5 <- glm(mortstat ~ Coffee, family = binomial(), data = data)
summary(model5)

model6 <- glm(mortstat ~ Coffee + Age_num + Sex, family = binomial(), data = data)
summary(model6)

model7 <- glm(mortstat ~ Coffee + Age_num, family = binomial(), data = data)
summary(model7)

model8 <- glm(mortstat ~ Coffee + Sex, family = binomial(), data = data)
summary(model8)

# Different coffee types

model9 <- glm(mortstat ~ CaffeinatedStatus, family = binomial(), data = data)
summary(model9)

model10 <- glm(mortstat ~ CaffeinatedStatus + Age_num + Sex, family = binomial(), data = data)
summary(model10)

model11 <- glm(mortstat ~ CaffeinatedStatus + Age_num, family = binomial(), data = data)
summary(model11)

model12 <- glm(mortstat ~ CaffeinatedStatus + Sex, family = binomial(), data = data)
summary(model12)

######################################

model13 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus, family = binomial(), data = data)
summary(model13)

model14 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + Age_num, family = binomial(), data = data)
summary(model14)

model15 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + Age_num + Sex, family = binomial(), data = data)
summary(model15)

model16 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + Sex, family = binomial(), data = data)
summary(model16)

model16 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + TotalCoffeeIntake, family = binomial(), data = data)
summary(model16)

model17 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + TotalCoffeeIntake + Age_num, family = binomial(), data = data)
summary(model17)

model18 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + TotalCoffeeIntake + Age_num + Sex, family = binomial(), data = data)
summary(model18)

model19 <- glm(mortstat ~ CaffeinatedStatus + FattyStatus + TotalCoffeeIntake + Sex, family = binomial(), data = data)
summary(model19)

# Age factor

model20 <- glm(mortstat ~ Age_fct, family = binomial(), data = data)
summary(model20)

#############################
model21 <- glm(mortstat ~ Age_fct + CaffeinatedStatus + FattyStatus + TotalCoffeeIntake + Sex, family = binomial(), data = data)
summary(model21)

model22 <- glm(mortstat ~ Age_fct + CaffeinatedStatus + FattyStatus + TotalCoffeeIntake, family = binomial(), data = data)
summary(model22)

model23 <- glm(mortstat ~ Age_fct + CaffeinatedStatus + FattyStatus, family = binomial(), data = data)
summary(model23)
############################################
model24 <- glm(mortstat ~ Age_fct + Coffee, family = binomial(), data = data)
summary(model24)

model25 <- glm(mortstat ~ Age_fct + CaffeinatedStatus, family = binomial(), data = data)
summary(model25)