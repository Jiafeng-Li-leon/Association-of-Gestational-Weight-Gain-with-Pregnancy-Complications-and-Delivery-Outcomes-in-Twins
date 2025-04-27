library(dplyr)

options(scipen = 200)
multivariate_data<-read.table("All_person_data.txt",header = T,sep = "\t")
new_covariates<-read.table("补充信息covariatexlsx.txt",sep = "\t",header = T)
new_covariates_unique <- new_covariates%>%
  distinct()
new_covariates_unique<-new_covariates_unique[,c("ID","户籍类别","民族","农籍类别","文化程度","职业","产次","有无吸烟")]

multivariate_cor<- multivariate_data %>%
  left_join(new_covariates_unique, by = c("ID" = "ID"))  # 明确指定左右列名
# 列类型转换
factor_cols <- c("户籍类别","民族","农籍类别","文化程度","职业","产次","有无吸烟")
multivariate_cor[factor_cols] <- lapply(multivariate_cor[factor_cols], as.factor)

# 计算GWG阶段
multivariate_cor <- multivariate_cor %>%
  mutate(
    GWG_1st = Rate * 12,
    GWG_2nd = Rate * 27,
    GWG_3rd = Rate * 分娩孕周
  )

# 初始化状态列
state_cols <- c("GWG_state_1st_CHINA", "GWG_state_2nd_CHINA", "GWG_state_3rd_CHINA",
                "GWG_state_1st_WHO", "GWG_state_2nd_WHO", "GWG_state_3rd_WHO")
multivariate_cor[state_cols] <- 0

# 重构后的fill_matrix函数
fill_matrix <- function(data, BMI, BMI_data, China = TRUE) {
  bmi_rows <- which(data$BMI分级 == BMI)
  if (length(bmi_rows) == 0) return(data)  # 防止无匹配行
  
  # 处理第1、2阶段
  suffix <- ifelse(China, "CHINA", "WHO")
  for (phase in c(1, 2)) {
    weeks <- ifelse(phase == 1, 12, 27)
    gw_col <- paste0("GWG_", ifelse(phase == 1, "1st", "2nd"))
    state_col <- paste0("GWG_state_", ifelse(phase == 1, "1st", "2nd"), "_", suffix)
    
    threshold_low <- BMI_data$C25[BMI_data$x == weeks]
    threshold_high <- BMI_data$C75[BMI_data$x == weeks]
    
    data[bmi_rows, state_col] <- ifelse(
      data[bmi_rows, gw_col] <= threshold_low, -1,
      ifelse(data[bmi_rows, gw_col] >= threshold_high, 1, 0)
    )
  }
  
  # 处理第3阶段
  delivery_weeks <- data[bmi_rows, "分娩孕周"]
  matched_rows <- match(delivery_weeks, BMI_data$x)
  valid_match <- !is.na(matched_rows)
  
  state_col_3rd <- paste0("GWG_state_3rd_", suffix)
  data[bmi_rows, state_col_3rd][!valid_match] <- 0  # 明确处理NA
  
  if (any(valid_match)) {
    c25 <- BMI_data$C25[matched_rows[valid_match]]
    c75 <- BMI_data$C75[matched_rows[valid_match]]
    gw3 <- data[bmi_rows, "GWG_3rd"][valid_match]
    
    data[bmi_rows, state_col_3rd][valid_match] <- case_when(
      gw3 <= c25 ~ -1,
      gw3 >= c75 ~ 1,
      TRUE ~ 0
    )
  }
  
  return(data)
}

# 批量调用
bmi_groups <- list(
  list(BMI="UW", China_data=result_UW_add, WHO_data=result_UW_add),
  list(BMI="NW", China_data=result_NW_new_add, WHO_data=result_NW_add),
  list(BMI="OW", China_data=result_OW_new_add, WHO_data=result_OW_add),
  list(BMI="OB", China_data=result_OB_new_add, WHO_data=result_OB_add)
)

for (group in bmi_groups) {
  multivariate_cor <- fill_matrix(
    data = multivariate_cor,
    BMI = group$BMI,
    BMI_data = group$China_data,
    China = TRUE
  )
  multivariate_cor <- fill_matrix(
    data = multivariate_cor,
    BMI = group$BMI,
    BMI_data = group$WHO_data,
    China = FALSE
  )
}


# 加载必要包
library(tidyverse)
library(broom)       # 模型结果整理
library(forestplot)  # 可视化结果

# 定义分析要素

colnames(multivariate_cor)[46]="Adverse_pregnancy_outcome"
colnames(multivariate_cor)[47]="Abnormal_birthweight"
colnames(multivariate_cor)[42]="Postpartum_hemorrhage"
colnames(multivariate_cor)[49]="Birth_defect"
multivariate_cor<-multivariate_cor[,-45]
outcomes <- c("Anemia", "Diabetes", "Postpartum_hemorrhage", 
              "Hypertension", "Thyroid_diseases", "Birth_defect",
              "Adverse_pregnancy_outcome", "Abnormal_birthweight", "Premature")

gwg_vars <- c("GWG_state_1st_CHINA", "GWG_state_2nd_CHINA", "GWG_state_3rd_CHINA",
              "GWG_state_1st_WHO", "GWG_state_2nd_WHO", "GWG_state_3rd_WHO")

# 定义协变量（根据实际数据调整）
covariates <- c("年龄", "BMI", "文化程度", "民族", "户籍类别","户籍类别","民族","农籍类别","文化程度","职业","产次","有无吸烟") 

# 创建存储结果的空数据框
results_df <- data.frame()

# 循环分析每个结局和GWG指标
for (outcome in outcomes) {
  for (gwg_var in gwg_vars) {
    
    # 检查结局变量是否存在
    if (!outcome %in% colnames(multivariate_cor)) next
    
    # 创建二元结局变量（假设原始变量为二分类）
    # 若原始变量非二分类需先转换，例如：mutate(outcome_bin = ifelse({{outcome}} > threshold, 1, 0))
    
    # 构建公式（分开统计过高和过低）
    formula_high <- as.formula(
      paste(outcome, "~ factor(", gwg_var, ", levels = c(0,1,-1)) +", 
            paste(covariates, collapse = " + "))
    )
    
    formula_low <- as.formula(
      paste(outcome, "~ factor(", gwg_var, ", levels = c(0,-1,1)) +", 
            paste(covariates, collapse = " + "))
    )
    
    # 运行logistic回归
    model_high <- glm(formula_high, 
                      data = multivariate_cor, 
                      family = binomial(link = "logit"))
    
    model_low <- glm(formula_low, 
                     data = multivariate_cor, 
                     family = binomial(link = "logit"))
    
    # 提取结果（过高 vs 正常）
    res_high <- tidy(model_high, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == paste0("factor(", gwg_var, ", levels = c(0, 1, -1))1")) %>%
      mutate(Group = "High", Outcome = outcome, GWG_var = gwg_var)
    
    # 提取结果（过低 vs 正常） 
    res_low <- tidy(model_low, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == paste0("factor(", gwg_var, ", levels = c(0, -1, 1))-1")) %>%
      mutate(Group = "Low", Outcome = outcome, GWG_var = gwg_var)
    
    # 合并结果
    results_df <- bind_rows(results_df, res_high, res_low)
  }
}

# 结果后处理
final_results <- results_df %>%
  select(Outcome, GWG_var, Group, 
         aOR = estimate, 
         CI_low = conf.low, 
         CI_high = conf.high,
         p_value = p.value) %>%
  mutate(
    Significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    Report = sprintf("aOR=%.2f (95%% CI: %.2f-%.2f)%s", 
                     aOR, CI_low, CI_high, Significance)
  )

# 保存结果
write_csv(final_results, "GWG_Association_Results.csv")

final_results[which(str_detect(final_results$GWG_var, "1st")),1]<-paste0(final_results[which(str_detect(final_results$GWG_var, "1st")),1],"(1)")
final_results[which(str_detect(final_results$GWG_var, "2nd")),1]<-paste0(final_results[which(str_detect(final_results$GWG_var, "2nd")),1],"(2)")
final_results[which(str_detect(final_results$GWG_var, "3rd")),1]<-paste0(final_results[which(str_detect(final_results$GWG_var, "3rd")),1],"(3)")
CHINA_P_H2N_C <- final_results %>%
  filter(
    str_detect(GWG_var, "CHINA"),  # 使用stringr的向量化检测
    Group == "High"
  ) %>%
  select(Outcome, aOR, CI_low, CI_high, p_value)
colnames(CHINA_P_H2N_C)<-c("Risk", "Point.estimate","Lower","Upper","P.value")
library(dplyr)
library(tidyr)
library(forestplot)
library("metafor")
library(magrittr)
CHINA_P_H2N<-CHINA_P_H2N_C
base_data <- tibble(study = as.character(CHINA_P_H2N$Risk),
                    mean  = round(CHINA_P_H2N$Point.estimate,2), 
                    lower = round(CHINA_P_H2N$Lower,2),
                    upper = round(CHINA_P_H2N$Upper,2),
                    CI=paste0(mean,"(",lower,"-",upper,")"),
                    PValue=CHINA_P_H2N$P.value)
dfHRQoL<-bind_rows(base_data)
dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
dfHRQoL <- dfHRQoL %>% 
  mutate(
    # 统一使用三位小数格式
    PValue = ifelse(PValue <= 0.001, 
                      "<0.001",
                      formatC(PValue, 
                              format = "f", 
                              digits = 3)),
    # 强制等宽显示
    PValue = sprintf("%-7s", PValue)  # 7字符固定宽度
  )

tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                  append(list("Excessive to IQR"), dfHRQoL %>% pull(CI)),
                  append(list("P Value"), dfHRQoL %>% pull(PValue)))


coef <- CHINA_P_H2N$Point.estimate
coef<-c(NA,coef)
low <- CHINA_P_H2N$Lower
low<-c(NA,low)
high <- CHINA_P_H2N$Upper
high<-c(NA,high)

pdf(file =paste0("CHINA New FRplot H2N",".pdf"),width = 10,height = 14,onefile = F)
forestplot(tabletext, 
           coef, 
           low, 
           high,
           pvalue=p,
           graph.pos = 4,
           psignif=0.05,
           title = title,
           is.summary = c(TRUE,rep(FALSE,27)),
           zero = 1, 
           colour=P_g,
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(10,'mm'),#设置图形中的行距
           colgap = unit(5,'mm'),#设置图形中的列间距
           lwd.zero = 2,#设置参考线的粗细
           lwd.ci = 2,#设置区间估计线的粗细
           col=fpColors(box='black',summary="#8B008B",lines = 'black',zero = 'black'),
           #使用fpColors()函数定义图形元素的颜色，从左至右分别对应点估计方形，汇总值，区间估计线，参考线
           xlab="OR (95%CI)",#设置x轴标签
           lwd.xaxis=2,#设置X轴线的粗细
           lty.ci = "solid",
           clip = c(0,4,1),#图像范围
           xticks.digits = 5,
           hrzl_lines=list("1"=gpar(lwd=1)),
           txt_gp = fpTxtGp(label = gpar(fontfamily = "mono", cex=1), # 使用等宽字体
                            ticks = gpar(cex=1),
                            xlab  = gpar(fontfamily = "sans")
                            )
           )
dev.off()


