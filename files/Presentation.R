####tidyverse####
setwd("/Users/apple/Downloads/同步空间/大三上课程/R/Pre/数据")
library(tidyverse)
library(readxl)
rawdata <- read_xlsx("data_raw.xlsx")
id <- read_xlsx("沪深A非ST非金融上市公司.xlsx",col_names  = FALSE)
colnames(id) <- c("股票代码","股票简称")
data <- inner_join(id,rawdata,by="股票代码")
data <- filter(data,上市状态=="正常上市")
data <- data  %>%
  select(1,2,3,5,6,7,12:27)
data <- drop_na(data)
data <- filter(data,between(年份,2012,2022))
data <- data  %>%
  select(1:9,12:22)
data <- data %>%
  mutate(Innovation=log(专利总共申请总量+1))
data <- data %>%
  mutate(Digit=log(数字化转型程度_A+1))
data <- data %>%
  mutate(Size=log(营业收入_元))
data <- data %>%
  mutate(Lev=资产负债率)
data <- data %>%
  mutate(PPE=固定资产总额占总资产之比)
data <- data %>%
  mutate(ROA=总资产净利润率)
data <- data %>%
  mutate(Cash=现金资产比率)
data <- data %>%
  mutate(SOE=控股股东性质)
data <- data %>%
  mutate(Boardsize=log(董事会规模_人))
data <- data %>%
  mutate(Indboard=独立董事占比)
data <- data %>%
  mutate(BM=账面市值比)
data <- data %>%
  mutate(Dual=两职合一)
data <- data %>%
  mutate(Age=企业年龄)
data <- data  %>%
  select(1:6,21:33)
data <- data %>%
  mutate(Id=1)
data[1:11,20] <- 0
data <- data %>%
  mutate(Year=年份-2012)
data <- na.omit(data)
write.csv(data,file = "data.csv")
####Panelized Regression####
install.packages("glmnet")
library(glmnet)
X <- as.matrix(data[, c("Digit", "Size", "Lev", "PPE", "ROA", "Cash", "SOE", "Boardsize", "Indboard", "BM", "Dual", "Age","Id","Year")])
y <- data$Innovation
X <- scale(X)
model <- glmnet(X, y, alpha = 1) # LASSO回归
cv_model <- cv.glmnet(X, y, alpha = 1)
best_lambda <- cv_model$lambda.min
coef(cv_model, s = best_lambda)
model <- glmnet(X, y, alpha = 0.5) # 这里设置alpha为0.5，表示弹性网回归
cv_model <- cv.glmnet(X, y, alpha = 0.5)
best_lambda <- cv_model$lambda.min
coef(cv_model, s = best_lambda)
####Trees####
data$Digit <- scale(data$Digit)
data$Size <- scale(data$Size)
data$Lev <- scale(data$Lev)
data$PPE <- scale(data$PPE)
data$ROA <- scale(data$ROA)
data$Cash <- scale(data$Cash)
data$SOE <- scale(data$SOE)
data$Boardsize <- scale(data$Boardsize)
data$Indboard <- scale(data$Indboard)
data$BM <- scale(data$BM)
data$Dual <- scale(data$Dual)
data$Age <- scale(data$Age)
data$Id <- scale(data$Id)
data$Year <- scale(data$Year)
install.packages("rpart")
library(rpart)
formula <- Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age + Id + Year
model <- rpart(formula, data = data, method = "anova")#决策树回归
plot(model,margin=0.1)
text(model)
model_pruned <- prune(model, cp = model$cptable[which.min(model$cptable[, "xerror"]), "CP"])
printcp(model_pruned)
plotcp(model_pruned)
install.packages("randomForest")
library(randomForest)
formula <- Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age + Id + Year
model <- randomForest(formula, data = data, ntree = 50)#随机森林回归
print(model)
varImpPlot(model,main="Variable Importance Plot")
install.packages("gbm")
library(gbm)#AdaBoost回归
formula <- Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age + Id + Year
set.seed(123) 
model <- gbm(formula,
             data = data,
             distribution = "gaussian",
             shrinkage = 0.01,
             interaction.depth = 3,
             n.trees = 50,
             n.minobsinnode = 10,
             cv.folds = 5)
summary(model)
####Neural Network####
install.packages("e1071")
library(e1071)
model <- svm(Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age + Id + Year,
             data = data, type = "eps-regression")#支持向量机回归
print(model)
install.packages("neuralnet")
library(neuralnet)
formula <- Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age
model <- neuralnet(formula, data = data)#人工神经网络回归
plot(model)
####Clustering####
install.packages("pls")
library(pls)
pca_result <- princomp(data[, c("Digit", "Size", "Lev", "PPE", "ROA", "Cash", "SOE", "Boardsize", "Indboard", "BM", "Dual", "Age","Id","Year")], 
                       cor = TRUE)
summary(pca_result)
pca_scores <- scale(pca_result$scores)
pcr_model <- pcr(Innovation ~ ., data = cbind(data, pca_scores),
                 validation = "CV")#主成分分析回归
summary(pcr_model)
install.packages("NbClust")
library(NbClust)
nb <- NbClust(your_data_scaled, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
best_clusters <- nb$Best.nc[["Number_clusters"]][1]
clusters <- cutree(hc, k = best_clusters)
your_data$cluster <- clusters
library(MASS)
models <- list()
for (i in 1:best_clusters) {
  cluster_data <- your_data[clusters == i, ]
  formula <- as.formula(paste("Innovation ~ Digit + Size + Lev + PPE + ROA + Cash + SOE + Boardsize + Indboard + BM + Dual + Age + Id + Year"))
  model <- stepAIC(lm(formula, data = cluster_data), direction = "both")
  models[[i]] <- model
}#分层聚类回归
summary(models)