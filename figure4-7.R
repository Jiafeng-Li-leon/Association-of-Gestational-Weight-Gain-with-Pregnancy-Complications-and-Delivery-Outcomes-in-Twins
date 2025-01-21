library(dplyr)
library(tidyr)
library(forestplot)
library("metafor")
library(magrittr)
#WHO L2N

  RF_WHO_Preg_1_H2N<-read.csv("risk_factor_WHO_Preg_1_high2normal.csv")
  RF_WHO_Preg_2_H2N<-read.csv("risk_factor_WHO_Preg_2_high2normal.csv")
  RF_WHO_Preg_3_H2N<-read.csv("risk_factor_WHO_Preg_3_high2normal.csv")
  RF_WHO_Preg_1_H2N$Risk<-paste0(as.character(RF_WHO_Preg_1_H2N$Risk),"(1)")
  RF_WHO_Preg_2_H2N$Risk<-paste0(as.character(RF_WHO_Preg_2_H2N$Risk),"(2)")
  RF_WHO_Preg_3_H2N$Risk<-paste0(as.character(RF_WHO_Preg_3_H2N$Risk),"(3)")
  WHO_P_H2N<-rbind(RF_WHO_Preg_1_H2N,RF_WHO_Preg_2_H2N,RF_WHO_Preg_3_H2N)
  WHO_P_H2N$P.adjust=
    p.adjust(WHO_P_H2N$P.value)
  WHO_P_H2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",WHO_P_H2N$Risk)
  WHO_P_H2N$Risk<-gsub("Baby weight","Abnormal birthweight",WHO_P_H2N$Risk)
  
  base_data <- tibble(study = as.character(WHO_P_H2N$Risk),
                      mean  = round(WHO_P_H2N$Point.estimate,2), 
                      lower = round(WHO_P_H2N$Lower,2),
                      upper = round(WHO_P_H2N$Upper,2),
                      CI=paste0(mean,"(",lower,"-",upper,")"),
                      P_adjust=WHO_P_H2N$P.adjust)
  dfHRQoL<-bind_rows(base_data)
  dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
  
  dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(WHO_P_H2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
  dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
  dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
  
  tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                    append(list("High to Interquartile Range"), dfHRQoL %>% pull(CI)),
                    append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))
  
  
  coef <- WHO_P_H2N$Point.estimate
  coef<-c(NA,coef)
  low <- WHO_P_H2N$Lower
  low<-c(NA,low)
  high <- WHO_P_H2N$Upper
  high<-c(NA,high)
  
  pdf(file =paste0("WHO New FRplot H2N",".pdf"),width = 8,height = 12,onefile = F)
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
             lineheight = unit(8,'mm'),#设置图形中的行距
             colgap = unit(2,'mm'),#设置图形中的列间距
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
             txt_gp = fpTxtGp(label = gpar(cex=1),
                              ticks = gpar(cex=1),
                              xlab =  gpar(cex=1.1)
             ))
  dev.off()



  RF_China_Preg_1_H2N<-read.csv("risk_factor_China_Preg_1_high2normal.csv")
  RF_China_Preg_2_H2N<-read.csv("risk_factor_China_Preg_2_high2normal.csv")
  RF_China_Preg_3_H2N<-read.csv("risk_factor_China_Preg_3_high2normal.csv")
  RF_China_Preg_1_H2N$Risk<-paste0(as.character(RF_China_Preg_1_H2N$Risk),"(1)")
  RF_China_Preg_2_H2N$Risk<-paste0(as.character(RF_China_Preg_2_H2N$Risk),"(2)")
  RF_China_Preg_3_H2N$Risk<-paste0(as.character(RF_China_Preg_3_H2N$Risk),"(3)")
  China_P_H2N<-rbind(RF_China_Preg_1_H2N,RF_China_Preg_2_H2N,RF_China_Preg_3_H2N)
  China_P_H2N$P.adjust=
    p.adjust(China_P_H2N$P.value)
  China_P_H2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",China_P_H2N$Risk)
  China_P_H2N$Risk<-gsub("Baby weight","Abnormal birthweight",China_P_H2N$Risk)
  
  base_data <- tibble(study = as.character(China_P_H2N$Risk),
                      mean  = round(China_P_H2N$Point.estimate,2), 
                      lower = round(China_P_H2N$Lower,2),
                      upper = round(China_P_H2N$Upper,2),
                      CI=paste0(mean,"(",lower,"-",upper,")"),
                      P_adjust=China_P_H2N$P.adjust)
  dfHRQoL<-bind_rows(base_data)
  dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
  
  dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(China_P_H2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
  dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
  dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
  
  tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                    append(list("High to Interquartile Range"), dfHRQoL %>% pull(CI)),
                    append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))
  
  
  coef <- China_P_H2N$Point.estimate
  coef<-c(NA,coef)
  low <- China_P_H2N$Lower
  low<-c(NA,low)
  high <- China_P_H2N$Upper
  high<-c(NA,high)
  
  pdf(file =paste0("China New FRplot H2N",".pdf"),width = 8,height = 12,onefile = F)
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
             lineheight = unit(8,'mm'),#设置图形中的行距
             colgap = unit(2,'mm'),#设置图形中的列间距
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
             txt_gp = fpTxtGp(label = gpar(cex=1),
                              ticks = gpar(cex=1),
                              xlab =  gpar(cex=1.1)
             ))
  dev.off()

  
RF_WHO_BMI_UW_H2N<-read.csv("risk_factor_WHO_BMI_UW_high2normal.csv")
RF_WHO_BMI_NW_H2N<-read.csv("risk_factor_WHO_BMI_NW_high2normal.csv")
RF_WHO_BMI_OW_H2N<-read.csv("risk_factor_WHO_BMI_OW_high2normal.csv")
RF_WHO_BMI_OB_H2N<-read.csv("risk_factor_WHO_BMI_OB_high2normal.csv")
RF_WHO_BMI_UW_H2N$Risk<-paste0(as.character(RF_WHO_BMI_UW_H2N$Risk),"(UW)")
RF_WHO_BMI_NW_H2N$Risk<-paste0(as.character(RF_WHO_BMI_NW_H2N$Risk),"(NW)")
RF_WHO_BMI_OW_H2N$Risk<-paste0(as.character(RF_WHO_BMI_OW_H2N$Risk),"(OW)")
RF_WHO_BMI_OB_H2N$Risk<-paste0(as.character(RF_WHO_BMI_OB_H2N$Risk),"(OB)")
WHO_H2N<-rbind(RF_WHO_BMI_UW_H2N,RF_WHO_BMI_NW_H2N,RF_WHO_BMI_OW_H2N,RF_WHO_BMI_OB_H2N)
RF_WHO_BMI_UW_L2N<-read.csv("risk_factor_WHO_BMI_UW_low2normal.csv")
RF_WHO_BMI_NW_L2N<-read.csv("risk_factor_WHO_BMI_NW_low2normal.csv")
RF_WHO_BMI_OW_L2N<-read.csv("risk_factor_WHO_BMI_OW_low2normal.csv")
RF_WHO_BMI_OB_L2N<-read.csv("risk_factor_WHO_BMI_OB_low2normal.csv")
RF_WHO_BMI_UW_L2N$Risk<-paste0(as.character(RF_WHO_BMI_UW_L2N$Risk),"(UW)")
RF_WHO_BMI_NW_L2N$Risk<-paste0(as.character(RF_WHO_BMI_NW_L2N$Risk),"(NW)")
RF_WHO_BMI_OW_L2N$Risk<-paste0(as.character(RF_WHO_BMI_OW_L2N$Risk),"(OW)")
RF_WHO_BMI_OB_L2N$Risk<-paste0(as.character(RF_WHO_BMI_OB_L2N$Risk),"(OB)")
WHO_L2N<-rbind(RF_WHO_BMI_UW_L2N,RF_WHO_BMI_NW_L2N,RF_WHO_BMI_OW_L2N,RF_WHO_BMI_OB_L2N)
RF_China_BMI_UW_H2N<-read.csv("risk_factor_China_BMI_UW_high2normal.csv")
RF_China_BMI_NW_H2N<-read.csv("risk_factor_China_BMI_NW_high2normal.csv")
RF_China_BMI_OW_H2N<-read.csv("risk_factor_China_BMI_OW_high2normal.csv")
RF_China_BMI_OB_H2N<-read.csv("risk_factor_China_BMI_OB_high2normal.csv")
RF_China_BMI_UW_H2N$Risk<-paste0(as.character(RF_China_BMI_UW_H2N$Risk),"(UW)")
RF_China_BMI_NW_H2N$Risk<-paste0(as.character(RF_China_BMI_NW_H2N$Risk),"(NW)")
RF_China_BMI_OW_H2N$Risk<-paste0(as.character(RF_China_BMI_OW_H2N$Risk),"(OW)")
RF_China_BMI_OB_H2N$Risk<-paste0(as.character(RF_China_BMI_OB_H2N$Risk),"(OB)")
China_H2N<-rbind(RF_China_BMI_UW_H2N,RF_China_BMI_NW_H2N,RF_China_BMI_OW_H2N,RF_China_BMI_OB_H2N)
RF_China_BMI_UW_L2N<-read.csv("risk_factor_China_BMI_UW_low2normal.csv")
RF_China_BMI_NW_L2N<-read.csv("risk_factor_China_BMI_NW_low2normal.csv")
RF_China_BMI_OW_L2N<-read.csv("risk_factor_China_BMI_OW_low2normal.csv")
RF_China_BMI_OB_L2N<-read.csv("risk_factor_China_BMI_OB_low2normal.csv")
RF_China_BMI_UW_L2N$Risk<-paste0(as.character(RF_China_BMI_UW_L2N$Risk),"(UW)")
RF_China_BMI_NW_L2N$Risk<-paste0(as.character(RF_China_BMI_NW_L2N$Risk),"(NW)")
RF_China_BMI_OW_L2N$Risk<-paste0(as.character(RF_China_BMI_OW_L2N$Risk),"(OW)")
RF_China_BMI_OB_L2N$Risk<-paste0(as.character(RF_China_BMI_OB_L2N$Risk),"(OB)")
WHO_L2N<-rbind(RF_WHO_BMI_UW_L2N,RF_WHO_BMI_NW_L2N,RF_WHO_BMI_OW_L2N,RF_WHO_BMI_OB_L2N)
WHO_L2N$P.adjust=p.adjust(WHO_L2N$P.value)
WHO_L2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",WHO_L2N$Risk)
WHO_L2N$Risk<-gsub("Baby weight","Abnormal birthweight",WHO_L2N$Risk)

base_data <- tibble(study = as.character(WHO_L2N$Risk),
                    mean  = round(WHO_L2N$Point.estimate,2), 
                    lower = round(WHO_L2N$Lower,2),
                    upper = round(WHO_L2N$Upper,2),
                    CI=paste0(mean,"(",lower,"-",upper,")"),
                    P_adjust=WHO_L2N$P.adjust)
dfHRQoL<-bind_rows(base_data)
dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(WHO_L2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                  append(list("Low to Interquartile Range"), dfHRQoL %>% pull(CI)),
                  append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))


coef <- WHO_L2N$Point.estimate
coef<-c(NA,coef)
low <- WHO_L2N$Lower
low<-c(NA,low)
high <- WHO_L2N$Upper
high<-c(NA,high)

pdf(file =paste0("WHO New FRplot L2N(BMI)",".pdf"),width = 8,height = 14,onefile = F)
forestplot(tabletext, 
           coef, 
           low, 
           high,
           pvalue=p,
           graph.pos = 4,
           psignif=0.05,
           title = title,
           is.summary = c(TRUE,rep(FALSE,36)),
           zero = 1, 
           colour=P_g,
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
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
           txt_gp = fpTxtGp(label = gpar(cex=1),
                            ticks = gpar(cex=1),
                            xlab =  gpar(cex=1.1)
           ))
dev.off()

WHO_H2N$P.adjust=p.adjust(WHO_H2N$P.value)
WHO_H2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",WHO_H2N$Risk)
WHO_H2N$Risk<-gsub("Baby weight","Abnormal birthweight",WHO_H2N$Risk)

base_data <- tibble(study = as.character(WHO_H2N$Risk),
                    mean  = round(WHO_H2N$Point.estimate,2), 
                    lower = round(WHO_H2N$Lower,2),
                    upper = round(WHO_H2N$Upper,2),
                    CI=paste0(mean,"(",lower,"-",upper,")"),
                    P_adjust=WHO_H2N$P.adjust)
dfHRQoL<-bind_rows(base_data)
dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(WHO_H2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                  append(list("High to Interquartile Range"), dfHRQoL %>% pull(CI)),
                  append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))


coef <- WHO_H2N$Point.estimate
coef<-c(NA,coef)
low <- WHO_H2N$Lower
low<-c(NA,low)
high <- WHO_H2N$Upper
high<-c(NA,high)

pdf(file =paste0("WHO New FRplot H2N(BMI)",".pdf"),width = 8,height = 14,onefile = F)
forestplot(tabletext, 
           coef, 
           low, 
           high,
           pvalue=p,
           graph.pos = 4,
           psignif=0.05,
           title = title,
           is.summary = c(TRUE,rep(FALSE,36)),
           zero = 1, 
           colour=P_g,
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
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
           txt_gp = fpTxtGp(label = gpar(cex=1),
                            ticks = gpar(cex=1),
                            xlab =  gpar(cex=1.1)
           ))
dev.off()
China_L2N$P.adjust=p.adjust(China_L2N$P.value)
China_L2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",China_L2N$Risk)
China_L2N$Risk<-gsub("Baby weight","Abnormal birthweight",China_L2N$Risk)

base_data <- tibble(study = as.character(China_L2N$Risk),
                    mean  = round(China_L2N$Point.estimate,2), 
                    lower = round(China_L2N$Lower,2),
                    upper = round(China_L2N$Upper,2),
                    CI=paste0(mean,"(",lower,"-",upper,")"),
                    P_adjust=China_L2N$P.adjust)
dfHRQoL<-bind_rows(base_data)
dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(China_L2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                  append(list("Low to Interquartile Range"), dfHRQoL %>% pull(CI)),
                  append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))


coef <- China_L2N$Point.estimate
coef<-c(NA,coef)
low <- China_L2N$Lower
low<-c(NA,low)
high <- China_L2N$Upper
high<-c(NA,high)

pdf(file =paste0("China New FRplot L2N(BMI)",".pdf"),width = 8,height = 14,onefile = F)
forestplot(tabletext, 
           coef, 
           low, 
           high,
           pvalue=p,
           graph.pos = 4,
           psignif=0.05,
           title = title,
           is.summary = c(TRUE,rep(FALSE,36)),
           zero = 1, 
           colour=P_g,
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
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
           txt_gp = fpTxtGp(label = gpar(cex=1),
                            ticks = gpar(cex=1),
                            xlab =  gpar(cex=1.1)
           ))
dev.off()

China_H2N$P.adjust=p.adjust(China_H2N$P.value)
China_H2N$Risk<-gsub("Pregnancy outcome","Adverse pregnancy outcome",China_H2N$Risk)
China_H2N$Risk<-gsub("Baby weight","Abnormal birthweight",China_H2N$Risk)

base_data <- tibble(study = as.character(China_H2N$Risk),
                    mean  = round(China_H2N$Point.estimate,2), 
                    lower = round(China_H2N$Lower,2),
                    upper = round(China_H2N$Upper,2),
                    CI=paste0(mean,"(",lower,"-",upper,")"),
                    P_adjust=China_H2N$P.adjust)
dfHRQoL<-bind_rows(base_data)
dfHRQoL <- dfHRQoL %>% mutate(est = sprintf("%.2f", mean), .after = study)
dfHRQoL <- dfHRQoL %>% mutate(P_adjust =  format(China_H2N$P.adjust, scientific = TRUE, digits = 2), .after = study)
dfHRQoL$P_adjust<-ifelse(as.numeric(dfHRQoL$P_adjust)<=0.001,"<0.001",dfHRQoL$P_adjust)
dfHRQoL$P_adjust<-ifelse(dfHRQoL$P_adjust=="1.0e+00","1         ",paste0(dfHRQoL$P_adjust,"       "))
tabletext <- list(c("Risk Factors", dfHRQoL %>% pull(study)),
                  append(list("High to Interquartile Range"), dfHRQoL %>% pull(CI)),
                  append(list("Adjust P Value"), dfHRQoL %>% pull(P_adjust)))


coef <- China_H2N$Point.estimate
coef<-c(NA,coef)
low <- China_H2N$Lower
low<-c(NA,low)
high <- China_H2N$Upper
high<-c(NA,high)

pdf(file =paste0("China New FRplot H2N(BMI)",".pdf"),width = 8,height = 14,onefile = F)
forestplot(tabletext, 
           coef, 
           low, 
           high,
           pvalue=p,
           graph.pos = 4,
           psignif=0.05,
           title = title,
           is.summary = c(TRUE,rep(FALSE,36)),
           zero = 1, 
           colour=P_g,
           boxsize = 0.3, #设置点估计的方形大小
           lineheight = unit(8,'mm'),#设置图形中的行距
           colgap = unit(2,'mm'),#设置图形中的列间距
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
           txt_gp = fpTxtGp(label = gpar(cex=1),
                            ticks = gpar(cex=1),
                            xlab =  gpar(cex=1.1)
           ))
dev.off()
