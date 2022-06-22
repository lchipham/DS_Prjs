hmeq <- read.csv("~/Documents/Data Science /R/projects/risk calculator app/credit_risk_calculator/hmeq.csv")
names(hmeq)
dim(hmeq)
str(hmeq)

#-----------------------------------------------------------------
#CLEAN & PREPARE DATA
#missing value plot
library(Amelia)
library(mlbench)
missmap(hmeq, col = c("brown", "pink"), legend = TRUE)
#--> Missing values account for 6% of total observations, or 357 out of 5960.
#--> Thus, we cannot just delete the rows with missing values, as we would likely be left with a biased dataset.
#--> For numerical variable, replace NAs with mean of the column.

hmeq$DEBTINC[which(is.na(hmeq$DEBTINC))] <- mean(hmeq$DEBTINC, na.rm = TRUE)
head(hmeq$DEBTINC)
hmeq$DEROG[which(is.na(hmeq$DEROG))] <- round(mean(hmeq$DEROG, na.rm = TRUE), 0)
head(hmeq$DEROG)
hmeq$DELINQ[which(is.na(hmeq$DELINQ))] <- round(mean(hmeq$DELINQ, na.rm = TRUE), 0)
head(hmeq$DELINQ)
hmeq$MORTDUE[which(is.na(hmeq$MORTDUE))] <- round(mean(hmeq$MORTDUE, na.rm = TRUE), 0)
head(hmeq$MORTDUE)
hmeq$YOJ[which(is.na(hmeq$YOJ))] <- round(mean(hmeq$YOJ, na.rm = TRUE), 1)
head(hmeq$YOJ)
hmeq$NINQ[which(is.na(hmeq$NINQ))] <- round(mean(hmeq$NINQ, na.rm = TRUE), 0)
head(hmeq$NINQ)
hmeq$CLAGE[which(is.na(hmeq$CLAGE))] <- mean(hmeq$CLAGE, na.rm = TRUE)
head(hmeq$CLAGE)
hmeq$CLNO[which(is.na(hmeq$CLNO))] <- round(mean(hmeq$CLNO, na.rm = TRUE), 0)
head(hmeq$CLNO)
hmeq$VALUE[which(is.na(hmeq$VALUE))] <- round(mean(hmeq$VALUE, na.rm = TRUE), 0)
head(hmeq$VALUE)
hmeq$JOB[hmeq$JOB== ""] <- "N/A"
hmeq$JOB
hmeq$REASON[hmeq$REASON== ""] <- "N/A"
hmeq$REASON
#Data is cleaned of NAs
missmap(hmeq, col = c("brown", "pink"), legend = TRUE)

#-----------------------------------------------------------------
#Variable Correlation
#exclude categorical variables in string format
library(dplyr)
library(cowplot)
num_var <- hmeq %>% 
  select(BAD, LOAN, MORTDUE, VALUE, YOJ, DEROG, DELINQ, CLAGE, NINQ, CLNO, DEBTINC)
names(num_var)
#quantify correlation
dev.off()
library(corrplot)
corrplot.mixed(cor(num_var), order = "AOE", tl.cex = 0.66, outline = TRUE)

#-----------------------------------------------------------------
#Rank variables based on correlation with BAD (default probability) (descending order)
#Positive correlation: DELINQ -> DEROG -> NINQ -> DEBTINC 
#Negative correlation: CLAGE -> LOAN -> YOJ ->  MORTDUE -> VALUE
#No correlation: CLNO

#-----------------------------------------------------------------
#VISUALIZE DATA
#Factorize BAD into 2 levels: 0: Paid, 1: Defaulted
plot_dt <- hmeq
plot_dt$BAD <- factor(plot_dt$BAD, labels = c("Paid", "Defaulted"))
head(plot_dt$BAD)
library(tidyverse)

#Disable exponential notation in graphs
options(scipen = 999) 

#Plot predictors' relationship to reponse variable BAD
dev.off()
bar1 <- ggplot(plot_dt, aes(BAD, DELINQ, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Delinquent Credit Lines", x = "", y = "Delinquent Credit Lines", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown")) 

bar2 <- ggplot(plot_dt, aes(BAD, DEBTINC, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Debt-Income Ratio", x = "", y = "Debt:Income", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown")) 

bar3 <- ggplot(plot_dt, aes(BAD, DEROG, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Derogatory Reports", x = "", y = "Derogatory Reports", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown"))  

bar4 <- ggplot(plot_dt, aes(BAD, CLAGE, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Oldest Credit Line", x = "", y = "Age", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown"))  

bar5 <- ggplot(plot_dt, aes(BAD, LOAN, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Loan Request Amount", x = "", y = "Loan", color = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown"))

bar6 <- ggplot(plot_dt, aes(BAD, NINQ, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Recent Credit Inquiries", x = "", y = "Recent Inquiries", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown"))

bar7 <- ggplot(plot_dt, aes(BAD, CLNO, fill = BAD)) +
  geom_bar(stat = "identity", show.legend = F, width = 0.4) +
  labs(title = "Total Credit Lines", x = "", y = "Credit Lines", fill = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 10), text = element_text(family = "serif"),
        aspect.ratio = 2/1, panel.grid = element_blank()) +
  scale_fill_manual(values = c("steelblue", "brown"))

plot_grid(bar1, bar3, bar2, bar4, bar5, bar6, bar7, ncol = 4, nrow = 2)
plot_grid(bar1, bar3)
plot_grid(bar4, bar6, bar7, ncol = 3, nrow = 1)
plot_grid(bar2, bar5)
#Borrowers who defaulted on their mortgage in general have more delinquent credit lines and major derogatory reports than those who paid loan.
#Intuitively, people who did not default are credible to the bank, thus have the oldest credit lines, make more recent credit inquiries, and have much more credit lines than default customers.
#Their debt-income ratio as well as loan request amount are also significantly higher, as they are able to continue borrowing from the bank given their extensive credit lines.

#----------------------------------------------------------
#Plot predictors' relationship to each other
fit <- lm(plot_dt$MORTDUE~plot_dt$VALUE)
scatter1 <- ggplot(plot_dt, aes(VALUE, MORTDUE, color = BAD)) +
  geom_count(position = "jitter", alpha = 0.6, size = 1.6, show.legend = F) +
  geom_line(aes(y = predict(fit)), col = "brown", size = 0.9) +
  ylim(c(0, 225000)) +
  scale_x_log10() +
  labs(title = "Property value vs Outstanding mortgage", x = "Property value", y = "Outstanding mortgage", size = "Size", color = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), text = element_text(family = "serif"),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("steelblue", "brown"))

fit2 <- lm(plot_dt$CLNO~plot_dt$NINQ)
scatter2 <- ggplot(plot_dt, aes(NINQ, CLNO, color = BAD)) +
  geom_count(position = "jitter", alpha = 0.6, size = 1.6) +
  labs(title = "Recent credit inquiries vs Total credit lines", x = "Recent inquiries", y = "Total credit lines", size = "Size", color = "Status") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11), text = element_text(family = "serif"),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("steelblue", "brown"))

plot_grid(scatter1, scatter2)

#To check for collinearity, we will now take a look at the relationship between "seemingly" related variables
#There is a strong positive exponential relationship between customers' current property value and outstanding mortgages.
#This indicates that MORTDUE and VALUE should not be included together in the final model, as multicollinearity can significantly reduce the precision of estimated beta coefficients, as a result weakening the statistical significance of regression model.

#On the other hand, there is an extremely weak relationship between customers' recent credit inquiries and total credit lines.
#This means that including NINQ and CLNO together would not sacrifice much of the model's precision.
#----------------------------------------------------------

ggplot(plot_dt, aes(x = factor(JOB), y=MORTDUE, fill = BAD)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(fill = "Status") +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme_bw()

ggplot(plot_dt, aes(x = factor(JOB), y=DEBTINC, fill = BAD)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(fill = "Status") +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme_bw()

ggplot(plot_dt, aes(x = factor(REASON), y=MORTDUE, fill = BAD)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(fill = "Status") +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme_bw()

ggplot(plot_dt, aes(x = factor(REASON), y=DEBTINC, fill = BAD)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  labs(fill = "Status") +
  scale_fill_manual(values = c("steelblue", "brown")) +
  theme_bw()

#-----------------------------------------------------------------
#ONE-HOT ENCODING: JOB & REASON (categorical variables with more than 2 unrelated categories)
library(caret)
dummy <- dummyVars(" ~ .", data=hmeq)
hmeq_new <- data.frame(predict(dummy, newdata = hmeq)) 
names(hmeq_new)
sum(is.na(hmeq_new))

#-----------------------------------------------------------------
#HMEQ_NEW
#Split data into train and test set: 80:20
set.seed(269)
# Store row numbers for training set: index_train
Z <- sample(1:nrow(hmeq_new), 0.8 * nrow(hmeq_new))
# Create training set: training_set
train <- hmeq_new[Z, ]
# Create test set: test_set
test <- hmeq_new[-Z, ]

#-----------------------------------------------------------------
#HMEQ
#Split data into train and test set: 80:20
set.seed(269)
# Store row numbers for training set: index_train
Z <- sample(1:nrow(hmeq), 0.8 * nrow(hmeq))
# Create training set: training_set
train <- hmeq[Z, ]
# Create test set: test_set
test <- hmeq[-Z, ]

#-----------------------------------------------------------------
#Stepwise regression
library(MASS)
full_model <- glm(BAD~., data = hmeq, family = "binomial")
#-----------------------------------------------------------------
#BACKWARD & FORWARD SELECTION
null.model = lm(BAD~1, data = hmeq_new)
full.model = lm(BAD~., data = hmeq_new)

#BACKWARD SELECTION
bwd <- step(full.model, direction = "backward")
formula(bwd)
summary(bwd)

#FORWARD SELECTION
fwd <- step(null.model, scope=list(lower=null.model,upper=full.model), direction="forward")
formula(fwd)
summary(fwd)
#-----------------------------------------------------------------
#Factorize BAD into two levels
hmeq$BAD <- as.factor(hmeq$BAD)
hmeq_new$BAD <- as.factor(hmeq_new$BAD)

# Train rpart model and compute variable importance.
library(caret)
set.seed(26)
model <- train(BAD ~ ., data=hmeq_new, method = "rpart")
rankVars <- varImp(model)
print(rankVars)

#Rank Features By Importance
plot(rankVars, top = 18, main = 'Variable Importance')
#--> top vars selected: DEBTINC, DELINQ, DEROG, CLAGE, LOAN, NINQ, CLNO

#Importance function, Random Forest model
library(randomForest)
rf_model <- randomForest(BAD~., data = hmeq_new)
importance(rf_model)
list<- sort(importance(rf_model), decreasing = TRUE)
plot(list, top = 18, main = 'Variable Importance', ylab = "", xlab = "Variable Index")
lines(list)
#--> top vars selected: DEBTINC, CLAGE, DELINQ, LOAN, VALUE, CLNO, MORTDUE
#-----------------------------------------------------------------
#COMPARE 3 MODELS

#Model 1: BAD ~ DEBTINC + DELINQ + DEROG + CLAGE + LOAN + NINQ + CLNO
#Fit logistic regression model on training set
train_model <- glm(BAD ~ DEBTINC + DELINQ + DEROG + CLAGE + LOAN + NINQ + CLNO, data = train, family = "binomial")
summary(train_model)
#ROC curve
library(Epi)
ROC(form = BAD ~ DEBTINC + DELINQ + DEROG + CLAGE + LOAN + NINQ + CLNO, data = train, plot = "ROC", MX = TRUE, PV = TRUE)
#--> Best cutoff point: 0.236

#training error
train$Prob = fitted.values(train_model)
train$Prediction = 1*(train$Prob > 0.236)
mean(train$Prediction != train$BAD)
head(train)

# fit test model to training data
test$Prob = predict(train_model, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.236)

#Confusion matrix on test data
table(test$Prediction, test$BAD)

#test error rate
mean(test$Prediction != test$BAD)

#Model 2: BAD ~ DEBTINC + CLAGE + DELINQ + LOAN + VALUE + CLNO + MORTDUE
dev.off()
#Fit logistic regression model on training set
train_model2 <- glm(BAD ~ DEBTINC + CLAGE + DELINQ + LOAN + VALUE + CLNO + MORTDUE, data = train, family = "binomial")
summary(train_model2)
#ROC curve
ROC(form = BAD ~ DEBTINC + CLAGE + DELINQ + LOAN + VALUE + CLNO + MORTDUE, data = train, plot = "ROC", MX = TRUE, PV = TRUE)
#--> Best cutoff point: 0.182

#training error
train$Prob = fitted.values(train_model2)
train$Prediction = 1*(train$Prob > 0.182)
head(train)

# fit test model to training data
test$Prob = predict(train_model2, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.182)
#Confusion matrix on test data
table(test$Prediction, test$BAD)

#test error rate
mean(test$Prediction != test$BAD)

#Model 3: BAD ~ DELINQ + DEROG + DEBTINC + CLAGE + CLNO + NINQ + LOAN (from stepwise regresison)
#Fit logistic regression model on training set
train_model3 <- glm(BAD ~ DELINQ + DEROG + DEBTINC + CLAGE + CLNO + NINQ + LOAN, data = train, family = "binomial")
summary(train_model3)
#ROC curve
ROC(form = BAD ~ DELINQ + DEROG + DEBTINC + CLAGE + CLNO + NINQ + LOAN, data = train, plot = "ROC", MX = TRUE, PV = TRUE)
#--> Best cutoff point: 0.236

#training error
train$Prob = fitted.values(train_model3)
train$Prediction = 1*(train$Prob > 0.236)
head(train)

# fit test model to training data
test$Prob = predict(train_model3, newdata = test, type = "response") 
test$Prediction = 1*(test$Prob > 0.182)
#Confusion matrix on test data
table(test$Prediction, test$BAD)

#test error rate
mean(test$Prediction != test$BAD)
#-----------------------------------------------------------------
#==> After comparing the 3 models above, we choose model 1:
#==> Final model: BAD ~ DEBTINC + DELINQ + DEROG + CLAGE + LOAN + NINQ + CLNO
fitControl <- trainControl(method = "cv", number = 10, savePredictions = T)
modCV <- train(BAD ~ DEBTINC + DELINQ + DEROG + CLAGE + LOAN + NINQ + CLNO, data = hmeq_new, method = "glm", family = "binomial", trControl = fitControl)
summary(modCV)
confusionMatrix(table((modCV$pred)$pred, (modCV$pred)$obs))
