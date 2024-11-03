# 加载数据集
data(CO2)

# 1) 查看数据集CO2中的变量名称，并将变量Treatment的名称更改为Treat。
names(CO2)
names(CO2)[names(CO2) == "Treatment"] <- "Treat"

# 2) 检验CO2中是否存在缺失值，若有，检测缺失值的位置并删除含有缺失值的行。
sum(is.na(CO2))
CO2 <- na.omit(CO2)

# 3) 对变量uptake按从大到小和从小到大排序，并对数据集CO2按照uptake排序。
CO2_desc <- CO2[order(-CO2$uptake), ]
CO2_asc <- CO2[order(CO2$uptake), ]

# 4) 将CO2随机分成两组数据，第一组和第二组的比例为6:4。
set.seed(123) # 设置随机种子以保证结果可重复
index <- sample(1:nrow(CO2), size = 0.6 * nrow(CO2))
CO2_group1 <- CO2[index, ]
CO2_group2 <- CO2[-index, ]

# 5) 应用tapply()函数，计算不同植物(Plant)对应的uptake的平均值。
tapply(CO2$uptake, CO2$Plant, mean)

# 6) 应用aggregate()函数，计算不同植物(Plant)、不同类型(Type)对应的uptake的平均值。
aggregate(uptake ~ Plant + Type, data = CO2, FUN = mean)

# 7) 应用lapply()函数，同时计算con和uptake的均值。
lCOs <- as.list(CO2)
lapply(CO2[c("conc", "uptake")], mean)

# 8) 使用grep()函数，查找出植物名称(Plant)中含有“Qn”的行的位置，并将这些行储存于变量Plant_Qn中。
Plant_Qn <- CO2[grep("Qn", CO2$Plant), ]

# 9) 使用gsub()函数，将CO2中植物名称(Plant)中的字符串“Qn”改为“QN”。
CO2$Plant <- gsub("Qn", "QN", CO2$Plant)

# 加载fBasics包
library(fBasics)

# 1) 编写函数stat，要求该函数同时计算均值、最大值、最小值、标准差、峰度和偏度。
stat <- function(x) {
  list(
    mean = mean(x),
    max = max(x),
    min = min(x),
    sd = sd(x),
    skewness = skewness(x),
    kurtosis = kurtosis(x)
  )
}

# 2) 生成自由度为2的t分布的100个随机数t，并通过函数stat计算t的均值、最大值、最小值、标准差、峰度和偏度。
t_data <- rt(100, df = 2)
t_stats <- stat(t_data)
print(t_stats)