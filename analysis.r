#! /usr/local/bin/Rscript

library(pROC)
library(dplyr)
library(ResourceSelection)
library(DAAG)

load("laligadata.Rda")

cor(d_df %>% select(Season, days, point_diff, goal_diff))

model <- glm(result ~ goal_diff + point_diff + Season + days, data=d_df, family=binomial(link='logit'))
summary(model)

#Psuedo-R^2
1 - model$deviance / model$null.deviance

#Test if goal difference should remain in the model
model2 <- glm(result ~ point_diff + Season + days, data=d_df, family="binomial")

pchisq(deviance(model2) - deviance(model), df.residual(model2) - df.residual(model), lower=F)

#Variable Selection

#PCA
X.scale<-scale(cbind(d_df$Season, d_df$days, d_df$goal_diff, d_df$point_diff))
pca<-prcomp(X.scale)

screeplot(pca, type="lines")

#Backwards
model_test <- glm(result ~ goal_diff + point_diff + Season + days + point_diff:days, data=d_df, family="binomial")
backwards = step(model_test)

#Influential observations
co <- qf(.50, 4, nrow(d_df) - 4)
cd1 <- cooks.distance(model)
cd1 <- cd1[abs(cd1) > co]
print("Influential Observations")
cd1

#HL test
hoslem.test(d_df$result, fitted(model))

#Cross validation

#randomize data, split it into folds
r_df <- d_df[sample(nrow(d_df)),]
N <- 10

size <- nrow(r_df)/N
a <- list()
start <- 1
end <- size
for (i in 1:N) {
    a[[i]] <- r_df[start:end,]
    start = start + size
    end = end + size
}

trials <- list()
aucs <- list()
for (i in 1:N) {
    test <- a[[i]]
    train <- data.frame()
    for (j in 1:N) {
        if (j != i) {
            train <- rbind(train, a[[j]]) 
        } 
    }
    model <- glm(result ~ goal_diff + point_diff + Season + days, data=train, family=binomial(link='logit'))
    trial <- data.frame(prob=plogis(predict(model, test)), actual=test$result)
    
    #calculate AUC
    aucs[[i]] <- auc(test$result, plogis(predict(model, test)))

    #find prediction success rate
    trial <- trial %>% 
        mutate(pred=ifelse(prob > .5, 1, 0)) %>%
        mutate(result=ifelse(pred == actual, 1, 0)) %>%
        summarize(percent=sum(result)/n())
    
    trials[[i]] <- trial

}

#Average values
print(Reduce("+", trials)/N)
print(Reduce("+", aucs)/N)

