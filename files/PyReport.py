#%%词频统计
import jieba
import xlwt
import os

def matchKeyWords(ThePath, keyWords, aim_path):
    if not os.path.isdir(ThePath):
        print(f"The provided path '{ThePath}' is not a directory.")
        return

    dir_list = os.listdir(ThePath)
    book = xlwt.Workbook(encoding='utf-8', style_compression=0)
    sheet = book.add_sheet('金融评论关键词词频统计', cell_overwrite_ok=True)
    sheet.write(0, 0, '文章题目')
    for i, c_word in enumerate(keyWords):
        sheet.write(0, i+1, c_word)
    index = 1  

    for file in dir_list:
        if os.path.splitext(file)[-1] == ".txt":  
            txt_path = os.path.join(ThePath, file)
            article_title = file 
            sheet.write(index, 0, article_title)
            print(f'正在统计{file}')
            with open(txt_path, "r", encoding='utf-8', errors='ignore') as fp:
                text = fp.read()
                words_list = list(jieba.cut(text))  
                for ind, word in enumerate(keyWords):
                    word_freq = words_list.count(word)  
                    sheet.write(index, ind + 1, str(word_freq))
            index += 1

    book.save(aim_path)

ThePath = r'/Users/apple/Desktop/test_py'
aim_path = r'/Users/apple/Desktop/test_py/词频统计—Jieba.xls'
keywords = [
    "人工智能",
    '…' #此处省略其他关键词
    ]
matchKeyWords(ThePath, keywords, f'{aim_path}')
#%%描述性统计
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# 加载数据
df = pd.read_csv('data_py.csv')

# 将无法转换为数值的数据替换为NaN
df = df.apply(pd.to_numeric, errors='coerce')

# 填充NaN值，这里选择用0填充，也可以选择删除这些行或列
df = df.fillna(0)

# 选择数值型变量，忽略字符型变量和股票代码
# 假设股票代码列名包含'Code'或'ID'，可以根据实际情况调整
numeric_df = df.select_dtypes(include=[float, int]).drop(columns=[col for col in df.columns if 'Code' in col or 'ID' in col])

# 描述性统计
desc_stats = numeric_df.describe()

# 打印描述性统计结果
print(desc_stats)

# 计算相关性矩阵
corr_matrix = numeric_df.corr()

# 打印相关性矩阵
print(corr_matrix)