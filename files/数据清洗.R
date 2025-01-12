####数据导入####
library(tidyverse)
library(readxl)
library(writexl)
df1 = read_xlsx("企业基本信息.xlsx")
df2 = read_xlsx("企业创新申请.xlsx")
df3 = read_xlsx("解释变量.xlsx")
df4 = read_xlsx("企业控制变量.xlsx")
df5 = read_xlsx("区域控制变量.xlsx")
Data <- df1 %>% 
  left_join(df2, by = c("Stock","Year"),suffix=c("","")) %>% 
  left_join(df3, by = c("Stock","Year"),suffix=c("","")) %>% 
  left_join(df4, by = c("Stock","Year"),suffix=c("","")) %>% 
  left_join(df5, by = c("Province","Year"),suffix=c("","")) 
####数据预处理####
Data <- Data %>%
  mutate(Innovation_D = log(Innovation_In+1))
Data <- Data %>%
  mutate(Trans = log((A+B+C+D+E)+1))
Data <- Data %>%
  mutate(Size_log = log(Size))
Data <- Data %>%
  mutate(Age_log = log(Age))
Data <- Data %>%
  mutate(Board_log = log(Board))
Data <- Data %>%
  mutate(Pop_log = log(Pop))
Data <- Data %>%
  mutate(GDP_per = GDP / Cor)
Data <- Data %>%
  mutate(Edu_per = Edu / (Pop*100))
Data <- Data %>%
  mutate(Human_ratio = Human / Pop)
Data <- Data %>%
  mutate(Stru = GDP3 / GDP2)
Data <- Data %>%
  mutate(Tax_per = Tax / Cor)
Data <- Data %>%
  mutate(Fin_per = Finance / Cor)
Data <- Data %>% 
  select(Stock,Name,Year,Province,Ind,IndName,Innovation_D,
         Trans,HHI,KZ,
         Size_log,Age_log,Lev,Grow,ROA,Cashflow,Capital,Board_log,Tops,Ins,Idr,
         Pop_log,GDP_per,Edu_per,Human_ratio,Stru,Tax_per,Fin_per
         )
colnames(Data)=c("Stock","Name","Year","Province","Industry","IndustryName","Innovation_D",
                 "Trans","HHI","KZ",
                 "Size","Age","Lev","Grow","ROA","Cashflow","Capital","Board","Tops","Ins","Idr",
                 "Pop","GDP","Edu","Human","Stru","Tax","Fin"
                 )
####数据清洗####
Data <- na.omit(Data)
Data <- Data[!is.infinite(Data$Age),]
####数据输出####
write_xlsx(Data,"Data.xlsx")