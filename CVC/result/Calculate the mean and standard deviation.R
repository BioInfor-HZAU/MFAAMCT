# 安装并加载必要的包
if (!require("readxl")) install.packages("readxl")
library(readxl)

# 手动选择Excel文件并读取数据
file_path <- file.choose()
data <- read_excel(file_path)

# 计算各列的均值和标准差（自动跳过非数值列）
results <- lapply(names(data), function(col_name) {
  column_data <- data[[col_name]]
  if (is.numeric(column_data)) {
    data.frame(
      列名 = col_name,
      均值 = round(mean(column_data, na.rm = TRUE), 4),
      标准差 = round(sd(column_data, na.rm = TRUE), 4)
    )
  } else {
    data.frame(
      列名 = col_name,
      均值 = "非数值列",
      标准差 = "非数值列"
    )
  }
})

# 合并结果并格式化输出
result_df <- do.call(rbind, results)
print(result_df)