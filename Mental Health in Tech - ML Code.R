library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(grid)
library(cowplot)

##load data
data2016 <- read.csv("~/Documents/SPRING 2022 - CONN/STA 336 - Statistical Machine Learning/mental-heath-in-tech-2016_20161114.csv")
dim(data2016)
names(data2016)

#mental.heath.in.tech.2016 <- read.csv("~/Downloads/mental-heath-in-tech-2016_20161114.csv", na.strings=c(""," ","NA"))
sub16 <- data2016[-c(11:12,4,17:36,38,40,44:45,47:50,52,54:55,60:63)]

colnames(sub16)<-c("self-employed","no_employees","tech_company","benefits","care_options","wellness_program", 
                   "seek_help","anonyimity","leave","coworkers", "supervisor","mental_vs_physical","obs_neg_consequence",
                   "physhealth_interview","mentalhealth_interview","hurt_career",
                   "views","willing_share","family_history", "current_diagnosis","treatment","age","gender","country","state")
sub16<-subset(sub16,sub16$`self-employed`==0)

###fixing NA values
colSums(is.na(sub16))

##fix gender
sub16$gender <- tolower(sub16$gender)

#checking unique
unique(sub16$gender)

# defining lists on the basis of unique values present in the dataset
male <- c("male","male ","m","man","cis male","male.","male (cis)","sex is male","malr","i'm a man why didn't you make this a drop down question. you should of asked sex? and i would of answered yes please. seriously how much text can this take? ",
          "mail","m|","male/genderqueer","cisdude","cis man")
female <- c("female", "f","i identify as female.","female ","cisgender female", "fem", "female-bodied; no feelings about gender",
            "female (props for making this a freeform field, though)"," female", "cis-woman","afab",
            "female assigned at birth ","woman","fm","cis female ","female or multi-gender femme","genderqueer woman")
lgbtqia <- c("non-binary","bigender","non-binary","transitioned, m2f","genderfluid (born female)",
             "other/transfeminine","female/woman","androgynous","male 9:1 female, roughly","nb masculine",
             "genderfluid","enby","mtf","queer", "agender","fluid","nonbinary" ,"male (trans, ftm)","genderflux demi-girl","transgender woman")
other <- c("n/a","other","none of your business","genderqueer","human","dude","unicorn","NA",NA)

# replace the values in the data set so that the categories are consistent
sub16 <- sub16 %>% mutate(gender = replace(gender, which(gender %in% male), 'Male'),
                         gender = replace(gender, which(gender %in% female), 'Female'),
                         gender = replace(gender, which(gender %in% lgbtqia), 'LGBTQIA+'),
                         gender = replace(gender, which(gender %in% other), 'Other'))

# checking if all values are covered and category is consistent                                                
unique(sub16$gender)
sub16$gender <- as.factor(sub16$gender)

####just US subset
us.16<-subset(sub16,country=="United States of America")

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)
us.16$regions <- sapply(us.16$state, 
                        function(x) names(region.list)[grep(x,region.list)])
dim(us.16)
sapply(us.16, class)
#-------------------data transformation-----------------------------------------#
#remove self-employed
us.16 <- us.16[-1]
names(us.16)

#set aside age, gender, country, state, regions for now
us.16.model <- us.16[-c(21,22,23,24,25)]
names(us.16.model)
str(us.16.model)

#no_employees: ordinal categorical --> group into 2 groups: 0-500; 500-1000+
unique(us.16.model$no_employees)
one_to_500 <- c("1-5", "6-25", "26-100", "100-500")
five00_to_1000 <- c("500-1000", "More than 1000")
us.16.model <- us.16.model %>% mutate(no_employees = replace(no_employees, which(no_employees %in% one_to_500), '1-500'),
                                      no_employees = replace(no_employees, which(no_employees %in% five00_to_1000), '500-1000+'))
unique(us.16.model$no_employees)

#benefits
table(us.16.model$benefits)
us.16.model$benefits[us.16.model$benefits == "Not eligible for coverage / N/A"] <- "No"

#leave
table(us.16.model$leave)
easy <- c("Somewhat easy", "Very easy")
difficult <- c("Somewhat difficult", "Very difficult")
leave_other <- c("I don't know", "Neither easy nor difficult")
us.16.model <- us.16.model %>% mutate(leave = replace(leave, which(leave %in% easy), 'Easy'),
                                      leave = replace(leave, which(leave %in% difficult), 'Difficult'),
                                      leave = replace(leave, which(leave %in% leave_other), 'Other'))

#hurt_career
table(us.16.model$hurt_career)
hurt_yes <- c("Yes, I think it would", "Yes, it has")
hurt_no <- c("No, I don't think it would", "No, it has not")
us.16.model <- us.16.model %>% mutate(hurt_career = replace(hurt_career, which(hurt_career %in% hurt_yes), 'Yes'),
                                      hurt_career = replace(hurt_career, which(hurt_career %in% hurt_no), 'No'))

#views
table(us.16.model$views)
views_yes <- c("Yes, I think they would", "Yes, they do")
views_no <- c("No, I don't think they would", "No, they do not")
us.16.model <- us.16.model %>% mutate(views = replace(views, which(views %in% views_yes), 'Yes'),
                                      views = replace(views, which(views %in% views_no), 'No'))

#willing_share
table(us.16.model$willing_share)
share_yes <- c("Somewhat open", "Very open")
share_no <- c("Not open at all", "Somewhat not open")
share_other <- c("Not applicable to me (I do not have a mental illness)", "Neutral")
us.16.model <- us.16.model %>% mutate(willing_share = replace(willing_share, which(willing_share %in% share_yes), 'Yes'),
                                      willing_share = replace(willing_share, which(willing_share %in% share_no), 'No'),
                                      willing_share = replace(willing_share, which(willing_share %in% share_other), 'Other'))

#care_options
table(us.16.model$care_options)
care_other <- c("I am not sure", "N/A")
us.16.model <- us.16.model %>% mutate(care_options = replace(care_options, which(care_options %in% care_other), 'Other'))


#convert int/char to factor
to_convert_vars <- names(us.16.model)
us.16.model[,to_convert_vars] <- lapply(us.16.model[,to_convert_vars], factor)
str(us.16.model)
names(us.16.model)
table(us.16.model$current_diagnosis, us.16.model$treatment)

#Save csv dataset for modeling
#write.csv(us.16.model, file = "~/Documents/MentalHealth2016_Model.csv")

#------------------------------------------------------------------------------#
## DATASET FOR VISUALIZATION ##
#Reassign unencoded variables to dataset
us.16.eda <- us.16.model
us.16.eda$age <- us.16$age
us.16.eda$gender <- us.16$gender
us.16.eda$country <- us.16$country
us.16.eda$state <- us.16$state
us.16.eda$regions <- us.16$regions
names(us.16.eda)

#Add back position and remote as variables for data visualization
eda16 <- data2016[-c(11:12,4,17:36,38,40,44:45,47:50,52,54:55,60:61)]
colnames(eda16)<-c("self-employed","no_employees","tech_company","benefits","care_options","wellness_program", 
                   "seek_help","anonyimity","leave","coworkers", "supervisor","mental_vs_physical","obs_neg_consequence",
                   "physhealth_interview","mentalhealth_interview","hurt_career",
                   "views","willing_share","family_history", "current_diagnosis","treatment","age","gender","country","state", "position", "remote")
names(eda16)
eda16<-subset(eda16,eda16$`self-employed`==0)
##fix gender
eda16$gender <- tolower(eda16$gender)

# defining lists on the basis of unique values present in the dataset
male <- c("male","male ","m","man","cis male","male.","male (cis)","sex is male","malr","i'm a man why didn't you make this a drop down question. you should of asked sex? and i would of answered yes please. seriously how much text can this take? ",
          "mail","m|","male/genderqueer","cisdude","cis man")
female <- c("female", "f","i identify as female.","female ","cisgender female", "fem", "female-bodied; no feelings about gender",
            "female (props for making this a freeform field, though)"," female", "cis-woman","afab",
            "female assigned at birth ","woman","fm","cis female ","female or multi-gender femme","genderqueer woman")
lgbtqia <- c("non-binary","bigender","non-binary","transitioned, m2f","genderfluid (born female)",
             "other/transfeminine","female/woman","androgynous","male 9:1 female, roughly","nb masculine",
             "genderfluid","enby","mtf","queer", "agender","fluid","nonbinary" ,"male (trans, ftm)","genderflux demi-girl","transgender woman")
other <- c("n/a","other","none of your business","genderqueer","human","dude","unicorn","NA",NA)

# replace the values in the data set so that the categories are consistent
eda16 <- eda16 %>% mutate(gender = replace(gender, which(gender %in% male), 'Male'),
                          gender = replace(gender, which(gender %in% female), 'Female'),
                          gender = replace(gender, which(gender %in% lgbtqia), 'LGBTQIA+'),
                          gender = replace(gender, which(gender %in% other), 'Other'))
eda16$gender <- as.factor(eda16$gender)
unique(eda16$gender)

####just US subset
eda.16.us <- subset(eda16, country == "United States of America")

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)
eda.16.us$regions <- sapply(eda.16.us$state, 
                            function(x) names(region.list)[grep(x,region.list)])
str(eda.16.us)
unique(eda.16.us$position)
unique(eda.16.us$remote)

#Save csv dataset for EDA & plotting
#write.csv(eda.16.us, file = "~/Documents/MentalHealth2016_EDA_2.csv")

#======================DATA VISUALIZATION======================================#
#Load datasets
library(ggplot2)
us.16.eda <- read.csv("~/Documents/MentalHealth2016_EDA.csv")
eda.16.us <- read.csv("~/Documents/MentalHealth2016_EDA_2.csv")
names(us.16.eda)
names(eda.16.us)

ggplot(us.16.eda, aes(x = current_diagnosis, fill = current_diagnosis)) + 
  geom_bar(show.legend = F, color = "black", size = 0.26) +
  labs(title = "Current Diagnosis Status of Tech Workers", subtitle = "In the Survey", y = "Headcount") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 14),
        axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(face = "bold", size = 14)) +
  scale_fill_brewer(palette = "OrRd")

  

#Plot 1: Age range of diagnosis status by gender & region
age_diag_gen <- ggplot(us.16.eda, aes(current_diagnosis, age, color = gender)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(title = "Age Distribution of Diagnosis Status", subtitle = "By Gender & Region", y = "Age", color = "Gender") +
  ylim(10, 80) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 14),
        axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(face = "bold", size = 14)) 
age_diag_reg <- ggplot(us.16.eda, aes(current_diagnosis, age, color = regions)) +
  geom_boxplot(outlier.alpha = 0.6) +
  labs(x = "Diagnosed with Mental Health Issues?", y = "Age", color = "Region") +
  ylim(10, 80) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 14), text = element_text(family = "serif", size = 14),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.ticks.y = element_blank(), axis.text.x = element_text(face = "bold", size = 14)) 
plot_grid(age_diag_gen, age_diag_reg, ncol = 1, nrow = 2)

#histogram of treatment
#change treatment category type name
us.16.eda.2 <- us.16.eda
table(us.16.eda.2$treatment)
us.16.eda.2 %>% 
  mutate(treatment = ifelse(treatment == "0", "Have not sought treatment", "Have sought treatment")) -> us.16.eda.2

#Sort by afe, group and color by cylinder 
x <- us.16.eda.2[order(us.16.eda.2$age),] # sort by age
x$treatment <- factor(x$treatment) # it must be a factor

age_treat_gen <- ggplot(x,aes(age, fill = gender)) +
  geom_histogram(alpha = 0.7, binwidth = 2.9) + 
  labs(title = "Age Distribution of Treatment Status", subtitle = "By Gender & Region", x = "Age", y = "Headcount", fill = "Gender") +
  xlim(10,78) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        axis.ticks.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.title.x = element_blank(),
        strip.background=element_rect(color="black",fill="papayawhip")) +
  guides(size="none") +
  facet_wrap(~treatment) 
age_treat_reg <- ggplot(x,aes(age, fill = regions)) +
  geom_histogram(alpha = 0.7, binwidth = 2.9) + 
  labs(x = "Age", y = "Headcount", fill = "Region") +
  xlim(10,78) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        axis.ticks.y = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
        strip.background=element_rect(color="black",fill="papayawhip")) +
  guides(size="none") +
  facet_wrap(~treatment) 

#Plot 2: Age distribution of Treatment status by gender and region
plot_grid(age_treat_gen, age_treat_reg, ncol = 1, nrow = 2)

#Plot 3: mental health benefits and size of company (no employees)
benefit_real <- ggplot(us.16.eda.2, aes(no_employees, fill = benefits)) +
  geom_bar(color = "black", size = 0.26, width = 0.6) +
  labs(title = "Availability of Mental Health Benefits", subtitle = "as part of Healthcare Coverage in Companies", y = "Raw headcount", fill = "Company offers\n coverage?") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_text(face = "bold", size = 13)) +
  scale_fill_brewer(palette = "OrRd")

benefit_perc <- ggplot(us.16.eda.2, aes(x = no_employees, y = (..count..)/sum(..count..), fill = benefits)) +
  scale_y_continuous(labels=scales::percent,"Percent") +
  geom_bar(color = "black", size = 0.26, position = "fill", show.legend = F, width = 0.5) +
  labs(subtitle = "Larger-sized tech companies more likely to provide mental health coverage", x = "Company Size", y = "Raw headcount") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_text(face = "bold", size = 13)) +
  scale_fill_brewer(palette = "OrRd")

plot_grid(benefit_real, benefit_perc, ncol = 1, nrow = 2)

#Plot 4: current diagnosis by work setting (remote/in person)
diag_remote_raw <- ggplot(eda.16.us, aes(remote, fill = current_diagnosis)) +
  geom_bar(color = "black", size = 0.26) +
  labs(title = "Mental Health Diagnosis by Work Setting", subtitle = "Remote / In person", y = "Raw headcount", fill = "Ever been\ndiagnosed?") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        axis.title.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_text(face = "bold", size = 13)) +
  scale_fill_brewer(palette = "OrRd") 
  
diag_remote_perc <- ggplot(eda.16.us, aes(remote, y = (..count..)/sum(..count..), fill = current_diagnosis)) +
  scale_y_continuous(labels=scales::percent,"Percent") +
  geom_bar(color = "black", size = 0.26, position = "fill", show.legend = F, width = 0.69) +
  labs(x = "Ever Work Remotely?", y = "Percent") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16), plot.subtitle = element_text(hjust = 0.5, size = 12), text = element_text(family = "serif", size = 12),
        panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), axis.text.x = element_text(face = "bold", size = 13)) +
  scale_fill_brewer(palette = "OrRd")
plot_grid(diag_remote_raw, diag_remote_perc, ncol = 1, nrow = 2)

#=============================MODELING=========================================#
names(us.16.model)
#Response variable: current_diagnosis

#Split data into train and test set: 80:20
set.seed(26)
# Store row numbers for training set: index_train
Z <- sample(1:nrow(us.16.model), 0.8 * nrow(us.16.model))
# Create training set: training_set
train <- us.16.model[Z, ]
# Create test set: test_set
test <- us.16.model[-Z, ]

#I. FEATURE SELECTION
#------------------------------------------------------------------------------#
#1. RANDOM FOREST
library(randomForest)
library(tree)
library(rpart)
library(rpart.plot)

#------------------------------------------------------------------------------#
#Bagging
set.seed(26)
bag_model <- randomForest(current_diagnosis~., data = train, mtry = 19, importance = TRUE)
print(bag_model)
bag.pred <- predict(bag_model, newdata = test)
#MSE
mean(bag.pred != test$current_diagnosis)

#Tree
tree.diagnosis = rpart(current_diagnosis~., train)
rpart.plot(tree.diagnosis)
printcp(tree.diagnosis)
#------------------------------------------------------------------------------#

#Default random forest model using randomForest package
set.seed(26)
rf_model <- randomForest(current_diagnosis~., data = train, importance = TRUE)
rf.pred <- predict(rf_model, newdata = test)
rf_model$err.rate[500,1]
#OOB estimate of  error rate: 12.87% --> accuracy = 87.13%

#Tuning Parameters
#------------------------------------------------------------------------------#
#a. ntree: Determine ntree = 500 by comparing OOB error rate
par(mfrow = c(2,1)) 
plot(rf_model5$err.rate[,1], main = "OOB error rate is stabilized at ntree = 500", type = "l", xlab = "ntree", ylab = "OOB Error Rate")
plot(rf_model5, main = "Random forest model using ntree = 500, mtry = 5")

#b. mtry
dev.off()
plot(x=NA, y=NA, xlim=c(1,20), ylim=c(0.11,0.15), xlab="mtry", ylab="OOB error rate", main="Choose best mtry by comparing OOB error rate")
seed <- 26
#plot oob error rate for each mtry 
for (i in 2:19) {
  set.seed(seed)
  tree.rf <- randomForest(current_diagnosis~., data = train, mtry = i, importance = TRUE)
  tree.pred <- predict(tree.rf, newdata = test)
  oob_err = tree.rf$err.rate[500,1]
  print(cbind(i, oob_err))
  points(x=i, y=oob_err)
}
#mtry = 5 is the best parameter with lowest oob error rare
#OOB estimate of  error rate: 12.69% 

#Random forest using mtry = 5, ntree = 500
set.seed(26)
rf_model5 <- randomForest(current_diagnosis~., data = train, mtry = 5)
print(rf_model5)

#------------------------------------------------------------------------------#
#Variable Importance from default randomForest model
imp_df <- as.data.frame.table(importance(rf_model5))
imp_df2 <- imp_df[-2]
colnames(imp_df2) <- c("Predictor", "Importance")
imp_df2

#Select out 15 most significant variables
list <- sort(imp_df2$Importance, decreasing = TRUE)
most_imp_predictors <- imp_df2[imp_df2$Importance %in% list,]

most_imp_predictors <- most_imp_predictors %>% 
  arrange(desc(Importance))
most_imp_predictors

plot(most_imp_predictors$Importance, main = 'Variable Importance - RF', ylab = "Frequency", xlab = "Variable Index")
lines(most_imp_predictors$Importance)

#Model Rf1 (all variables): current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share + mentalhealth_interview + coworkers + views + anonyimity + benefits + hurt_career + no_employees + tech_company + obs_neg_consequence 
#Model Rf2 (5 most important variables): current_diagnosis ~ treatment + family_history + care_options + seek_help + leave
#Model Rf3 (10 most important variables): current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share 

#------------------------------------------------------------------------------#
#2. STEPWISE SELECTION
library(MASS)
null.model <- glm(current_diagnosis ~1, data = train, family = "binomial")
full.model <- glm(current_diagnosis ~., data = train, family = "binomial")

backward.model <- stepAIC(full.model, direction = "backward", trace = FALSE)
summary(backward.model)

forward.model <- stepAIC(null.model, scope = list(lower = null.model, upper = full.model), direction = "forward", trace = FALSE)
summary(forward.model)

both.model <- stepAIC(null.model, scope = list(lower = null.model, upper = full.model), direction = "both", trace = FALSE)
summary(both.model)

#Model S1 (backward): current_diagnosis ~ care_options + wellness_program + anonyimity + family_history + treatment
#Model S2 (forward & both): current_diagnosis ~ care_options + mental_vs_physical + anonyimity + family_history + treatment

#------------------------------------------------------------------------------#
#3. Recursive Partitioning / Rpart model
#recursively partitioning the explanatory variables into the "purest groups" of two of the levels of the response variable
#Assessment of the purity of the resulting groups is decided by any number of metrics, but the most commonly used metric is the gini index. 
library(caret)
rPartMod <- train(current_diagnosis ~ ., data=train, method="rpart")
rankVars <- varImp(rPartMod)
print(rankVars)

#Rank Features By Importance
plot(rankVars, top = 5, main = 'Variable Importance')

#Model Rp: (4 variables) current_diagnosis ~ treatment + family_history + care_options + mentalhealth_interview


#II. LOGISTIC REGRESSION: COMPARE MODELS' Performance
#------------------------------------------------------------------------------#
#Model Rf1: RF Model (All variables)
#Fit logistic regression model on training set
rf_mod1 <- glm(current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share + mentalhealth_interview + coworkers + views + anonyimity + benefits + hurt_career + no_employees + tech_company + obs_neg_consequence, data = train, family = "binomial")
summary(rf_mod1)
#ROC curve
library(Epi)
ROC(form = current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share + mentalhealth_interview + coworkers + views + anonyimity + benefits + hurt_career + no_employees + tech_company + obs_neg_consequence, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 2 as shown in the ROC graph is 0.449
#Area under the curve is 0.926, indicating a great fit of model.

#Fit test model to training data
test$Prob = predict(rf_mod1, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.449)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(5+12) / (52 + 5 + 12 + 73)

#--> Model Rf1's test error rate: 0.1197183

#------------------------------------------------------------------------------#
#Model Rf2: RF Model (5 var)
#Fit logistic regression model on training set
rf_mod2 <- glm(current_diagnosis ~ treatment + family_history + care_options + seek_help + leave, data = train, family = "binomial")
summary(rf_mod2)
#ROC curve
library(Epi)
ROC(form = current_diagnosis ~ treatment + family_history + care_options + seek_help + leave, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 2 as shown in the ROC graph is 0.626
#Area under the curve is 0.906, not as good as model Rf1

#Fit test model to training data
test$Prob = predict(rf_mod2, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.626)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(5+14) / (50 + 5 + 14 + 73)

#--> Model Rf2's test error rate: 0.1338028 --> higher than Rf1

#------------------------------------------------------------------------------#
#Model Rf3: RF Model (10 var)
#Fit logistic regression model on training set
rf_mod3 <- glm(current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share, data = train, family = "binomial")
summary(rf_mod3)
#ROC curve
ROC(form = current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 3 as shown in the ROC graph is 0.537
#Area under the curve is 0.914, indicating a better fit of model than model 

#Fit test model to training data
test$Prob = predict(rf_mod3, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.537)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(5+12) / (52 + 5 + 12 + 73)

#--> Model 3's test error rate: 0.1197183, same as model 1 
#==> Choose Rf3 over Rf1 (and Rf2) as it performs almost as well as Rf1 with signifcantly less variables

#------------------------------------------------------------------------------#
#Model S1: Backward Selection

#Fit logistic regression model on training set
back_mod <- glm(current_diagnosis ~ care_options + wellness_program + anonyimity + family_history + treatment, data = train, family = "binomial")
summary(back_mod)
#ROC curve
ROC(form = current_diagnosis ~ care_options + wellness_program + anonyimity + family_history + treatment, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 4 as shown in the ROC graph is 0.568
#Area under the curve is 0.917, indicating better performance than model Rf3

#Fit test model to training data
test$Prob = predict(back_mod, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.568)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(6+12) / (52 + 6 + 12 + 72)

#--> Model S1's test error rate: 0.1267606, higher than model Rf3

#------------------------------------------------------------------------------#
#Model S2: Forward Selection

#Fit logistic regression model on training set
forward_both <- glm(current_diagnosis ~ care_options + mental_vs_physical + anonyimity + family_history + treatment, data = train, family = "binomial")
summary(forward_both)
#ROC curve
ROC(form = current_diagnosis ~ care_options + mental_vs_physical + anonyimity + family_history + treatment, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 4 as shown in the ROC graph is 0.496
#Area under the curve is 0.918, indicating better performance than model S1

#Fit test model to training data
test$Prob = predict(forward_both, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.496)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(4+13) / (51 + 4 + 13 + 74)

#--> Model S2's test error rate: 0.1197183, which is lower than higher S1
#==> Chose model S2 over both S1 and Rf3

#---------------------------------------------------------------------------------------#
#Model Rp: Recursive Partitioning
#Fit logistic regression model on training set
rpart_mod <- glm(current_diagnosis ~ treatment + family_history + care_options + mentalhealth_interview, data = train, family = "binomial")
summary(rpart_mod)
#ROC curve
ROC(form = current_diagnosis ~ treatment + family_history + care_options + mentalhealth_interview, data = train, plot = "ROC", MX = TRUE, PV = TRUE)

#The best cutoff point for model 3 as shown in the ROC graph is 0.603
#Area under the curve is 0.907, indicating slightly worse performance than S2 

#Fit test model to training data
test$Prob = predict(rpart_mod, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.603)

#Confusion matrix on test data
table(test$Prediction, test$current_diagnosis)

#test error rate
(5+12) / (52 + 5 + 12 + 73)

#--> Model Rp's test error rate: 0.1197183, which is the same as model S2

#III. NEURAL NETWORK: Classification 
#---------------------------------------------------------------------------------------#
library(neuralnet)
#Finalists from logistic regression are model S2 (highest AUC and lowest error rate) and model Rp (lower AUC than S2, same error rate)
#We'll compare these 2 models using classification neural networks
#The number of neurons should be between the input layer size and the output layer size, usually 2/3 of the input size
#Neural Network requires numerical input --> one-hot encode factor variables

#---------------------------------------------------------------------------------------#
#Model S2: current_diagnosis ~ care_options + mental_vs_physical + anonyimity + family_history + treatment
library(caret)
dummy <- dummyVars(" ~ .", data=us.16.model[c(4, 7, 11, 18, 20)])
us16.s2.encoded <- data.frame(predict(dummy, newdata = us.16.model[c(4, 7, 11, 18, 20)])) 
str(us16.s2.encoded)
us16.s2.encoded$current_diagnosis <- us.16.model$current_diagnosis
dim(us16.s2.encoded)

#Change response variable to TRUE/FALSE
table(us16.s2.encoded$current_diagnosis)
us16.s2.encoded %>% 
  mutate(current_diagnosis = ifelse(current_diagnosis == "Yes", TRUE, FALSE)) -> us16.s2.encoded
table(us16.s2.encoded$current_diagnosis)

#eliminate treatment0, mental_vs_physical.I.don.t.know, family_history.I.don.t.know, care_options.No, anonyimity.I.don.t.know
us16.s2.encoded.clean <- us16.s2.encoded[,-c(1, 4, 7, 10, 13)]
names(us16.s2.encoded.clean)

#Split data into train and test set: 80:20
set.seed(26)
# Store row numbers for training set: index_train
Z_2 <- sample(1:nrow(us16.s2.encoded.clean), 0.8 * nrow(us16.s2.encoded.clean))
# Create training set: training_set
train_2 <- us16.s2.encoded.clean[Z_2,]
# Create test set: test_set
test_2 <- us16.s2.encoded.clean[-Z_2, ]

#Rebuild neural network with new response variable
set.seed(26)
s2_nn <- neuralnet(current_diagnosis ~ .,
                        data = train_2,
                        err.fct = "ce",
                        likelihood = TRUE,
                        hidden = c(2), #2 layers: 2 nodes in 1 layer
                        linear.output = FALSE,
                        stepmax=1e7)
plot(s2_nn)
s2_nn$result.matrix

#Train data on test set
set.seed(26)
pred2 <- predict(s2_nn, test_2)
table(test_2$current_diagnosis, pred2[,1] > 0.5)

#Error rate: 0.1267606 --> Accuracy = 0.8732394 = 87.32%

#---------------------------------------------------------------------------------------#
#Model Rf3: current_diagnosis ~ treatment + family_history + care_options + seek_help + leave + mental_vs_physical + wellness_program + physhealth_interview + supervisor + willing_share
#One-hot encoding 
summary(us.16.model)
library(caret)
dummy2 <- dummyVars(" ~ .", data=us.16.model[c(20, 18, 4, 6, 8, 11, 5, 13, 10, 17)])
us16.rf3.encoded <- data.frame(predict(dummy2, newdata = us.16.model[c(20, 18, 4, 6, 8, 11, 5, 13, 10, 17)])) 
us16.rf3.encoded$current_diagnosis <- us.16.model$current_diagnosis
names(us16.rf3.encoded)

#Change response variable to TRUE/FALSE
table(us16.rf3.encoded$current_diagnosis)
us16.rf3.encoded %>% 
  mutate(current_diagnosis = ifelse(current_diagnosis == "Yes", TRUE, FALSE)) -> us16.rf3.encoded
table(us16.rf3.encoded$current_diagnosis)

#eliminate treatment0, family_history.I.don.t.know, care_options.No, seek_help.I.don.t.know, leave.Difficult, mental_vs_physical.I.don.t.know, wellness_program.I.don.t.know, physhealth_interview.Maybe, supervisor.Maybe, willing_share.No
names(us16.rf3.encoded)
us16.rf3.encoded.clean <- us16.rf3.encoded[,-c(1, 3, 6, 9, 12, 15, 18, 21, 24, 27)]
table(us16.rf3.encoded.clean$current_diagnosis)

#Split data into train and test set: 80:20
set.seed(26)
# Store row numbers for training set: index_train
Z_3 <- sample(1:nrow(us16.rf3.encoded.clean), 0.8 * nrow(us16.rf3.encoded.clean))
# Create training set: training_set
train_3 <- us16.rf3.encoded.clean[Z_3,]
# Create test set: test_set
test_3 <- us16.rf3.encoded.clean[-Z_3, ]

set.seed(26)
#Rebuild neural network with new response variable
rf_nn <- neuralnet(current_diagnosis ~.,
                        data = train_3,
                        err.fct = "ce",
                        likelihood = TRUE,
                        hidden = c(2), #2 layers: 2 nodes in 1 layer
                        linear.output = FALSE)
plot(rf_nn)
rf_nn$result.matrix
pred3 <- predict(rf_nn, test_3)
table(test_3$current_diagnosis, pred3[,1] > 0.5)

#Error rate: 0.1478873 --> Accuracy = 0.8521127 = 85.21%
(13 + 8) / (51 + 13 + 8 + 70)
#--> Further proof that model S2 is better than model Rf3
#---------------------------------------------------------------------------------------#
#===> From both logistic regression and neural networks, model S2 performs the best
#===> Final Model for Predicting Mental heath disorder diagnosis in tech employees: current_diagnosis ~ care_options + mental_vs_physical + anonyimity + family_history + treatment

