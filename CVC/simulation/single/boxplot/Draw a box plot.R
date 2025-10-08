# 读取CSV文件（通过对话框选择文件）
file_path <- file.choose()
data <- read.csv(file_path, check.names = FALSE)

# 确保列顺序正确（处理可能的乱序情况）
interference_order <- c("5%", "10%", "15%", "20%", "25%", "30%")
data <- data[, interference_order]

# 绘制箱线图（添加 boxwex 参数控制宽度）
boxplot(data,
        names = interference_order,
        xlab = "干扰率",
        ylab = "运行时间/s",
        # main = "Algorithm Running Time Distribution by Interference Rate",
        col = "#f56582",  # 修改颜色1 SCC算法颜色和前一章保持一致
        border = "darkblue",
        outpch = 19,
        outcol = "red",
        boxwex = 0.3
)

# -----------------------------------

# 读取CSV文件（通过对话框选择文件）
file_path <- file.choose()
data <- read.csv(file_path, check.names = FALSE)

# 确保列顺序正确（处理可能的乱序情况）
interference_order <- c("5%", "10%", "15%", "20%", "25%", "30%")
data <- data[, interference_order]

# 绘制箱线图（添加 boxwex 参数控制宽度）
boxplot(data,
        names = interference_order,
        xlab = "干扰率",
        ylab = "运行时间/s",
        # main = "Algorithm Running Time Distribution by Interference Rate",
        col = "lavender",  # 修改颜色2
        border = "darkblue",
        outpch = 19,
        outcol = "red",
        boxwex = 0.3
)

# -----------------------------------

# 读取CSV文件（通过对话框选择文件）
file_path <- file.choose()
data <- read.csv(file_path, check.names = FALSE)

# 确保列顺序正确（处理可能的乱序情况）
interference_order <- c("5%", "10%", "15%", "20%", "25%", "30%")
data <- data[, interference_order]

# 绘制箱线图（添加 boxwex 参数控制宽度）
boxplot(data,
        names = interference_order,
        xlab = "干扰率",
        ylab = "运行时间/s",
        # main = "Algorithm Running Time Distribution by Interference Rate",
        col = "wheat",  # 修改颜色3
        border = "darkblue",
        outpch = 19,
        outcol = "red",
        boxwex = 0.3
)

# -----------------------------------

# 读取CSV文件（通过对话框选择文件）
file_path <- file.choose()
data <- read.csv(file_path, check.names = FALSE)

# 确保列顺序正确（处理可能的乱序情况）
interference_order <- c("5%", "10%", "15%", "20%", "25%", "30%")
data <- data[, interference_order]

# 绘制箱线图（添加 boxwex 参数控制宽度）
boxplot(data,
        names = interference_order,
        xlab = "干扰率",
        ylab = "运行时间/s",
        # main = "Algorithm Running Time Distribution by Interference Rate",
        col = "lightpink",  # 修改颜色4
        border = "darkblue",
        outpch = 19,
        outcol = "red",
        boxwex = 0.3
)

