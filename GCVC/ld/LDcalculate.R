library(ggplot2)
library(dplyr)
library(scales)  # 新增scales包用于刻度处理

# 读取LD文件
ld_data <- read.table(file.choose(),
                      header = TRUE,
                      sep = "",
                      stringsAsFactors = FALSE)

# 计算物理距离
ld_data$Distance <- abs(ld_data$BP_B - ld_data$BP_A)

# 分箱处理（每5kb为一个区间）
max_distance <- max(ld_data$Distance, na.rm = TRUE)
breaks <- seq(0, max_distance + 5000, by = 5000)
ld_data$Bin <- cut(ld_data$Distance,
                   breaks = breaks,
                   include.lowest = TRUE)

# 计算每个分箱的平均R²和区间中点
summary_data <- ld_data %>%
  group_by(Bin) %>%
  summarise(
    Mean_R2 = mean(R2, na.rm = TRUE),
    Midpoint = (min(Distance) + max(Distance)) / 2
  ) %>%
  filter(!is.na(Bin))

# 计算关键指标
max_r2 <- max(summary_data$Mean_R2, na.rm = TRUE)
half_max_r2 <- max_r2 / 2
half_max_point <- summary_data %>% 
  filter(Mean_R2 >= half_max_r2) %>% 
  slice_max(Midpoint) %>% 
  pull(Midpoint) %>% 
  first() / 1000

# 在控制台输出结果
cat("分析结果：\n",
    "┌───────────────────────┐\n",
    "│ 最大R²值：", sprintf("%.4f", max_r2), "      │\n",
    "│ 半衰R²值：", sprintf("%.4f", half_max_r2), "      │\n",
    "│ 半衰位置：", sprintf("%5.1f", half_max_point), "kb      │\n",
    "└───────────────────────┘\n")

# 绘制最终图表
p <- ggplot(summary_data, aes(x = Midpoint / 1000, y = Mean_R2)) +
  geom_line(color = "#1f77b4", linewidth = 0.8) +
  geom_point(color = "#1f77b4", size = 1, alpha = 0.3) +
  labs(
    x = "Distance (kb)",
    y = expression(paste("Mean ", R^2))
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    text = element_text(family = "sans")
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    limits = c(0, max_r2 * 1.1),
    expand = expansion(mult = c(0, 0)),
    breaks = breaks_extended(n = 5),  # 自动生成合理刻度
    labels = function(x) ifelse(x == 0, "", number(x, accuracy = 0.01))  # 隐藏0值标签
  )

# 显示图形
windows(width = 7, height = 5)
print(p)

# 导出PDF
ggsave("LD_decay_final.pdf", 
       plot = p,
       width = 7, 
       height = 5,
       device = "pdf")
