# 实验一
# 1）读取系统当前日期时间
date1 <- Sys.Date()  # 只包含日期
date2 <- Sys.time()  # 包含日期和时间
date3 <- date()      # 包含日期

# 2）使用类型辨别函数class()判断读取的3个不同的结果的类型
class(date1)
class(date2)
class(date3)

# 3）将读取到的日期时间值转换为另外一种数据类型
date1_str <- format(date1, "%m-%d-%Y")  # 转换为月-日-年格式的字符串
date2_str <- format(date2, "%m-%d-%Y %H:%M:%S")  # 转换为月-日-年 时:分:秒格式的字符串
date3_str <- format(date3, "%m-%d-%Y")  # 转换为月-日-年格式的字符串

# 4）判断转换后的结果的类型，判定是否转换成功
class(date1_str)
class(date2_str)
class(date3_str)
# 实验二
# 1）设置工作空间目录
setwd("path/to/your/working/directory")  # 请替换为实际路径

# 2）创建一个向量x
x <- c(5.1, 4.9, 4.7, 4.6, 5.0, 3.5, 3.0, 3.2, 3.1, 3.6)

# 3）查询向量x中序号为3,6,9的元素
x[c(3, 6, 9)]

# 查询向量x中大于4.0小于等于5.0的元素的位置
which(x > 4.0 & x <= 5.0)

# 4）创建一个向量Petal.Length
Petal.Length <- seq(1.7, by = 0.1, length.out = 5)

# 5）创建一个向量Petal.Width
Petal.Width <- rep(0.2, 5)

# 6）创建一个向量Species
Species <- rep(c("setosa", "versicolor", "virginica"), each = 2)

# 7）创建一个5行2列的矩阵
matrix_x <- matrix(x, nrow = 5, byrow = TRUE)

# 8）将矩阵写入数据框data_iris
data_iris <- as.data.frame(matrix_x)
colnames(data_iris) <- c("Sepal.Length", "Sepal.Width")

# 9）将向量合并至数据框data_iris中
data_iris <- cbind(data_iris, Petal.Length, Petal.Width, Species)

# 10）将数据框data_iris保存为txt文件
write.table(data_iris, "test/data_iris.txt", sep = "\t", row.names = FALSE)
# 实验三
# 1）读取实验二保存在test目录下的txt文件data_iris
data_iris <- read.table("test/data_iris.txt", header = TRUE, sep = "\t")

# 2）将R的示例数据集iris中的第6~10行写入数据框data_iris1中
data_iris1 <- iris[6:10, ]

# 3）将数据框data_iris与data_iris1合并为数据框data_iris2，并保存为csv文件
data_iris2 <- rbind(data_iris, data_iris1)
write.csv(data_iris2, "test/data_iris2.csv", row.names = FALSE)