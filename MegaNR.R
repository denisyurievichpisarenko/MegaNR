library(tidyverse)
library(Hmisc)
library(caTools)
library(party)
library(magrittr)
library(mltools)
library(data.table)
library(caret)
library(randomForest)
library(corrr)
library(FactoMineR)
library(factoextra)
library(plyr)
library(class)

df <- read_tsv('https://raw.githubusercontent.com/denisyurievichpisarenko/MegaNR/main/mega-negraising-v1-normalized.tsv')
df <- filter(df, acceptability > 0.5)
df <- df %>% mutate(is_nr = as.factor(ifelse(negraising > 0.5, 1, 0)))
df <- df[,!names(df) %in% c("sentence1", "sentence2", "acceptability", "negraising")]

grouped <- df %>% group_by(verb) %>% dplyr::summarise(total = n())
frequent <- filter(grouped, total>6)
df <- df[df$verb %in% frequent$verb, ]
df <- df[sample(1:nrow(df)), ]
#filter(df, verb=='expect')
#filter(df, verb=='utter')

X <- df[,!names(df) %in% c('is_nr')]
y <- df['is_nr']

X$frame[X$frame=="NP be V to VP[-eventive]"]<-"NP be V to VP noneventive"
X$frame[X$frame=="NP V to VP[-eventive]"]<-"NP V to VP noneventive"
X$frame[X$frame=="NP be V to VP[+eventive]"]<-"NP be V to VP eventive"
X$frame[X$frame=="NP V to VP[+eventive]"]<-"NP V to VP eventive"

factor_X <- X %>% mutate_if(is.character, as.factor)
one_hot_X <- one_hot(as.data.table(factor_X))
colnames(one_hot_X) <- make.names(colnames(one_hot_X))

X_train <- as.data.frame(one_hot_X[1:3000, ])
X_test <- as.data.frame(one_hot_X[3001:4123, ])
y_train <- as.data.frame(y[1:3000, ])
y_test <- as.data.frame(y[3001:4123, ])

logreg <- glm(y_train$is_nr ~.,family=binomial(link='logit'),data=X_train)
logreg
preds <- predict(logreg, newdata = X_test, type = "response")
max(preds)
min(preds)
mean(preds)
quantile(preds, probs = seq(0, 1, 0.1))
y_hat <- sapply(preds, function(x) round(x))
yy <- tibble(y_test, y_hat)
confusionMatrix(data=as.factor(y_hat), reference = as.factor(y_test$is_nr))

rf <- randomForest(y_train$is_nr~., data=X_train, proximity=TRUE)
#print(rf)
y_rf <- predict(rf, X_test, predict.all=TRUE)
confusionMatrix(data=as.factor(y_rf$aggregate), reference = as.factor(y_test$is_nr))
yy <- tibble(y_test, y_hat = y_rf$aggregate)

######

X_train_no_verbs <- X_train[, -c(400:409)]
X_test_no_verbs <- X_test[, -c(400:409)]

logreg <- glm(y_train$is_nr ~.,family=binomial(link='logit'),data=X_train_no_verbs)
logreg
preds <- predict(logreg, newdata = X_test_no_verbs, type = "response")
max(preds)
min(preds)
mean(preds)
quantile(preds, probs = seq(0, 1, 0.1))
y_hat <- sapply(preds, function(x) round(x))
yy <- tibble(y_test, y_hat)
confusionMatrix(data=as.factor(y_hat), reference = as.factor(y_test$is_nr))


#knn_clf <- knn(train=X_train, test=X_test, cl=y_train$is_nr, k=1)
#100 * sum(y_test$is_nr == knn_clf)/nrow(y_test)

#nb <- naive_bayes(y_train$is_nr ~ ., data = X_train, usekernel = T) 
#nb_pred <- predict(nb, X_test, type = 'class')

corr_matrix <- cor(one_hot_X)
data.pca <- princomp(corr_matrix)
summary(data.pca)
# Graph of the variables
fviz_pca_var(data.pca, col.var = "black")
fviz_pca_var(data.pca, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

num_x <- as.numeric(as.character(X_train$subject_first))
num_y <- as.numeric(as.character(y_train$is_nr))
num_l <- as.numeric(as.character(X_train$tense_present))

#t.test(x = num_x, y = num_l)
#cor(num_x, num_l)

###

nr_verbs <- df %>% group_by(verb) %>% dplyr::summarise(total = sum(as.numeric(as.character(is_nr))))
nr_verbs <- filter(nr_verbs, total < 6)$verb

df_nr <- filter(df, verb %in% nr_verbs)
describe(df_nr)

X_nr <- df_nr[,!names(df_nr) %in% c('is_nr')]
y_nr <- df_nr['is_nr']

X_nr$frame[X_nr$frame=="NP be V to VP[-eventive]"]<-"NP be V to VP noneventive"
X_nr$frame[X_nr$frame=="NP V to VP[-eventive]"]<-"NP V to VP noneventive"
X_nr$frame[X_nr$frame=="NP be V to VP[+eventive]"]<-"NP be V to VP eventive"
X_nr$frame[X_nr$frame=="NP V to VP[+eventive]"]<-"NP V to VP eventive"

factor_X_nr <- X_nr %>% mutate_if(is.character, as.factor)
one_hot_X_nr <- one_hot(as.data.table(factor_X_nr))
colnames(one_hot_X_nr) <- make.names(colnames(one_hot_X_nr))

X_train_nr <- as.data.frame(one_hot_X_nr[1:200, ])
X_test_nr <- as.data.frame(one_hot_X_nr[201:270, ])
y_train_nr <- as.data.frame(y_nr[1:200, ])
y_test_nr <- as.data.frame(y_nr[201:270, ])

X_train_nr <- X_train_nr[, -c(400:409)]
X_test_nr <- X_test_nr[, -c(400:409)]

logreg <- glm(y_train_nr$is_nr ~.,family=binomial(link='logit'),data=X_train_nr)
logreg
preds <- predict(logreg, newdata = X_test_nr, type = "response")
max(preds)
min(preds)
mean(preds)
quantile(preds, probs = seq(0, 1, 0.1))
y_hat_nr <- sapply(preds, function(x) round(x))
yy <- tibble(y_test_nr, y_hat_nr)
confusionMatrix(data=as.factor(y_hat_nr), reference = as.factor(y_test_nr$is_nr))