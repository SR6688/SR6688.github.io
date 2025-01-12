####导入数据####
# 加载模块
# Env: Python == 3.9.6
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(readxl)
library(plm)
library(glmnet)
library(randomForest)
library(keras)
library(tensorflow)
install_keras()
install_tensorflow()

# 写入数据
Data <- read_excel("Data.xlsx")

####描述性统计####
# 直方图
hist_plot <- ggplot(Data, aes(x = Innovation_D)) + 
  geom_histogram(bins = 30, fill = "#69b3a2", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Innovation_D", x = "Innovation_D", y = "Frequency")
# 散点图
scatter_plot1 <- ggplot(Data, aes(x = Trans, y = Innovation_D)) + 
  geom_point(alpha = 0.5, color = "#69b3a2") +
  theme_minimal() +
  labs(title = "Scatter Plot of Inovation_D vs Trans", x = "Trans", y = "Innovation_D")

# 散点图
scatter_plot2 <- ggplot(Data, aes(x = HHI, y = Innovation_D)) + 
  geom_point(alpha = 0.5, color = "#69b3a2") +
  theme_minimal() +
  labs(title = "Scatter Plot of Inovation_D vs HHI", x = "HHI", y = "Innovation_D")

# 散点图
scatter_plot3 <- ggplot(Data, aes(x = KZ, y = Innovation_D)) + 
  geom_point(alpha = 0.5, color = "#69b3a2") +
  theme_minimal() +
  labs(title = "Scatter Plot of Inovation_D vs KZ", x = "KZ", y = "Innovation_D")

# 将所有图形组合在一起
grid.arrange(hist_plot, scatter_plot1, scatter_plot2, scatter_plot3, ncol = 2)

# 计算相关矩阵
cor_matrix <- cor(Data[, sapply(Data, is.numeric)], use = "complete.obs")  # 处理缺失值

# 绘制相关图
corrplot(cor_matrix, method = "circle", # 只显示上三角
         tl.col = "black", tl.srt = 45, # 添加标签颜色和旋转角度
         col = colorRampPalette(c("red", "white", "blue"))(200), # 自定义颜色
         title = "Correlation Matrix", # 添加标题
         ) # 不显示对角线

####基准模型####

# 固定效应模型估计
panel <- pdata.frame(Data,index=c("Stock","Year"))
fe_model <- plm(Innovation_D ~ Trans + HHI + KZ + Size + Age + Lev + Grow + ROA + Cashflow + Capital + Board + Tops + Ins + Idr + Pop + GDP + Human + Stru + Tax + Fin, 
                data = panel, model="within",effect = "twoways")

# 查看回归结果
summary(fe_model)

####弹性网模型####

# 定义变量
data <- select(Data,c(1,8:28))  # 选择第1到第28列作为特征数据
y <- Data$Innovation_D  # 响应变量
X <- model.matrix(~.-1, data = data)[, ]  # 预测变量，排除截距项

# 拟合弹性网模型
fit <- glmnet(X, y, alpha = 0.5)  # alpha = 0.5 表示50%的岭回归和50%的Lasso

# 使用交叉验证选择最佳的 lambda
cv_fit <- cv.glmnet(X, y, alpha = 0.5)

# 最佳 lambda 值
best_lambda <- cv_fit$lambda.min

# 使用最佳 lambda 值重新拟合模型
best_fit <- glmnet(X, y, alpha = 0.5, lambda = best_lambda)

# 预测和评估模型
predictions <- predict(best_fit, X, s = best_lambda)
coef(best_fit)

####随机森林模型####

# 定义变量
data <- select(Data,7:28)  # 选择第1到第28列作为特征数据
trainIndex <- sample(1:nrow(data), size = 0.7 * nrow(data))
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# 训练随机森林模型
rf_model <- randomForest(Innovation_D ~ ., data = trainData, ntree = 500)

# 查看模型摘要
print(rf_model)

# 获取变量重要性
importance <- importance(rf_model, type = 1)

# 绘制变量重要性图
varImpPlot(rf_model)

####神经网络模型####

# 定义变量
data <- select(Data,c(1,8:28))  # 选择第1到第28列作为特征数据
y <- Data$Innovation_D  # 响应变量
X <- model.matrix(~.-1, data = data)[, ]  # 预测变量，排除截距项

# 创建序贯模型 (Sequential Model)
model <- keras_model_sequential()

# 添加层到模型
# 第一层是具有64个单元的隐藏层，使用relu激活函数，输入形状为特征数量
model %>% layer_dense(units = 64, activation = 'relu', input_shape = c(ncol(X)))

# 第二层是输出层，具有1个单元，使用线性激活函数
model %>% layer_dense(units = 1, activation = 'linear')

# 编译模型，指定优化器、损失函数和评估指标
model %>% compile(
  optimizer = 'adam',
  loss = 'mse',
  metrics = 'mae'
  )

# 拟合模型，指定训练的迭代次数（epochs）、批次大小（batch_size）和验证集比例（validation_split）
history <- model %>% fit(
  X, y,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
  )

# 查看模型结果
summary(model)

# 查看最后一层的权重
last_layer_weights <- get_weights(model)[["dense_1"]][, , drop = FALSE]
abs(last_layer_weights)