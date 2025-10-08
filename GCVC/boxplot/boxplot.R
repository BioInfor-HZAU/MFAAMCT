library(ggplot2)
library(dplyr)

# 数据准备 ---------------------------------------------------------------
data <- read.csv(file.choose(), header = TRUE)
observed_genotypes <- sort(unique(data$Genotype))
data$Genotype <- factor(data$Genotype, levels = observed_genotypes)

# 计算统计量并添加颜色分组 ------------------------------------------------
overall_median <- median(data$DSI)  # 计算全局中位数

data <- data %>%
  group_by(Genotype) %>%
  mutate(
    genotype_median = median(DSI),  # 计算各基因型中位数
    color_group = ifelse(genotype_median > overall_median, "high", "low")
  ) %>%
  ungroup()

# 计算标注位置 ----------------------------------------------------------
summary_stats <- data %>% 
  group_by(Genotype) %>%
  summarise(
    N = n(),
    Upper_Whisker = max(DSI[DSI <= quantile(DSI, 0.75) + 1.5*IQR(DSI)]),
    .groups = 'drop'
  )

# 计算均值和方差并输出 ----------------------------------------------------
summary_mean_var <- data %>%
  group_by(Genotype) %>%
  summarise(
    Mean = sprintf("%.3f", mean(DSI)),  # 强制保留三位小数格式
    Variance = var(DSI),
    .groups = 'drop'
  )

# 在控制台显示结果
print("各基因型统计量：")
print(summary_mean_var, width = Inf)  # 确保完整显示所有小数位

# 写入txt文件
write.table(summary_mean_var, "Genotype_Statistics.txt", 
           sep = "\t", row.names = FALSE, quote = FALSE)

# 可视化构建 -------------------------------------------------------------
ggplot(data, aes(x = Genotype, y = DSI)) +
  stat_boxplot(
    geom = "errorbar", 
    width = 0.2,
    color = "black"
  ) +
  geom_boxplot(
    aes(fill = color_group),  # 按颜色分组映射
    width = 0.25,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.6
  ) +
  geom_text(
    data = summary_stats,
    aes(y = Upper_Whisker,
        label = paste0("n=", N)),
    vjust = -1.2,
    size = 4.5,
    family = "serif",
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("low" = "#e0f3db", "high" = "#ffcccc")  # 自定义颜色映射
  ) +
  labs(
    x = "SNP (ss715595462)",  # 修改后的横坐标标签
    y = "Disease Severity Index"
  ) +
  theme_classic(base_size = 14) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.margin = unit(c(12, 12, 12, 12), "mm"),
    legend.position = "none"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.12))
  )

# 保存高清PDF -----------------------------------------------------------
ggsave("Enhanced_Genotype_Plot.pdf", 
       width = length(unique(data$Genotype)) * 2.5 + 4,
       height = 10,
       units = "cm",
       device = "pdf")
