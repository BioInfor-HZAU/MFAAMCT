# 单位点检测遗传关联分析模拟研究 (比较Fisher, Trend, GLM,CVC)

library(lme4)
library(rms)

# 全局变量 ----
n <- 300                     # 样本量
genetic_coef <- 0.55          # 遗传效应系数
pthreshold <- 0.025          # p值阈值

# 定义核心函数 ----
simulate_and_analyze <- function(seed) {
  set.seed(seed)
  
  # 生成模拟数据
  snp1 <- rbinom(n, 1, 0.3)
  snp2 <- rbinom(n, 1, 0.3)
  
  # 效应生成
  genetic_effect <- genetic_coef * snp1

  linear_predictor <- (
    genetic_effect -0.5
  )
  prob <- plogis(linear_predictor)
  phenotype <- rbinom(n, 1, prob)
  
  dataset <- data.frame(
    SNP1 = snp1, SNP2 = snp2,
    Phenotype = phenotype
  )

    # 分析方法
  analyze_snp <- function(snp_name) {
    snp <- dataset[[snp_name]]
    tbl <- table(snp, dataset$Phenotype)
    prev <- mean(dataset$Phenotype)
    
    # Fisher检验
    time_fisher <- system.time({
      fisher_p <- tryCatch(fisher.test(tbl)$p.value, error = function(e) 1)
    })[3]
    
    # 趋势检验
    time_trend <- system.time({
      trend_p <- tryCatch(prop.trend.test(tbl[,2], rowSums(tbl))$p.value,
                          error = function(e) 1)
    })[3]
    
    # GLM
    time_glm <- system.time({
      glm_p <- tryCatch(
        coef(summary(glm(Phenotype ~ snp, data = dataset, family = binomial)))[2,4],
        error = function(e) 1
      )
    })[3]
    
    # SCC
    time_scc <- system.time({
      scc_consist <- tbl[2,2] / sum(tbl[2,])
      scc_p <- chisq.test(tbl)$p.value
    })[3]
    
    list(
      fisher = fisher_p,
      trend  = trend_p,
      glm    = glm_p,
      scc    = c(consist = scc_consist, p = scc_p),
      time   = c(fisher = time_fisher,
                 trend  = time_trend,
                 glm    = time_glm,
                 scc    = time_scc)
    )
  }
  
  # 分析两个SNP
  res1 <- analyze_snp("SNP1")
  res2 <- analyze_snp("SNP2")
  prev <- mean(dataset$Phenotype)
  detection <- function(res, snp_type) {
    fisher_detect <- res$fisher < pthreshold
    trend_detect  <- res$trend  < pthreshold
    glm_detect    <- res$glm    < pthreshold
    scc_detect    <- (res$scc["consist"] >= prev) & (res$scc["p"] < pthreshold)
    detected <- c(fisher_detect, trend_detect, glm_detect, scc_detect)
    if (snp_type == "SNP1") {
      detected
    } else {
      !detected
    }
  }
  
  list(
    tp   = detection(res1, "SNP1"),
    fp   = detection(res2, "SNP2"),
    #fp   = detection(res3, "SNP3"),
    time = res1$time + res2$time
  )
}

# 运行模拟 ----
n_runs <- 1000
seeds <- 11223 + (1:n_runs) * 50
results <- list(
  tp   = matrix(0, n_runs, 4, dimnames = list(NULL, c("Fisher","Trend","GLM","SCC"))),
  fp   = matrix(0, n_runs, 4),
  time = matrix(0, n_runs, 4)
)

for (i in seq_len(n_runs)) {
  r <- simulate_and_analyze(seeds[i])
  results$tp[i,]   <- r$tp
  results$fp[i,]   <- r$fp
  results$time[i,] <- r$time
}

# 汇总指标 ----
metrics <- list(
  TP   = colMeans(results$tp),
  FP   = colMeans(results$fp),
  Time = colMeans(results$time)
)

# 输出结果 ----
# 文件名包含全局参数
output_file <- paste0("simulation_n", n, "_coef", genetic_coef, ".txt")
conn <- file(output_file, "w")
cat("=== 全局参数 ===\n", file = conn)
cat("样本量 (n):", n, "\n", file = conn)
cat("遗传效应系数:", genetic_coef, "\n", file = conn)
cat("运行次数 (n_runs):", n_runs, "\n", file = conn)
cat("=== 性能指标 ===\n", file = conn)
cat("真阳性 (SNP1):\n", file = conn)
write.table(t(round(metrics$TP, 3)), file = conn, sep = " ", col.names = FALSE)
#cat("\nLD排除 (SNP2):\n", file = conn)
#write.table(t(round(metrics$LD, 3)), file = conn, sep = " ", col.names = FALSE)
cat("\nFP排除 (SNP3):\n", file = conn)
write.table(t(round(metrics$FP, 3)), file = conn, sep = " ", col.names = FALSE)
cat("\n平均时间 (秒):\n", file = conn)
# 保留小数点后6位，空格分隔
write.table(t(formatC(metrics$Time, format = "f", digits = 6)),
            file = conn, sep = " ", col.names = FALSE)
close(conn)

# 控制台打印
cat("=== 性能指标 ===\n")
# 打印时保留6位
print(lapply(metrics, function(x) if(is.numeric(x)) round(x, 6) else x))
