#All_person_data<-read.csv("Data/All individual.csv",header = T)
#combind_all_data_china<-read.csv("Data/data_bmi_china.csv",header = T)
#All_person_data$"GWG"<-All_person_data$分娩体重-All_person_data$孕前体重KG
#plot(All_person_data$GWG)
#All_person_data$"Rate"<-All_person_data$GWG/All_person_data$分娩孕周
#All_person_data$"GWG_1st"<-All_person_data$"Rate"*12
#All_person_data$"GWG_2nd"<-All_person_data$"Rate"*15
#All_person_data$"GWG_3rd"<-All_person_data$"Rate"*(All_person_data$分娩孕周-27)
#All_person_data[,"GWG_state_1st_CHINA"]<-All_person_data[,"GWG_state_2nd_CHINA"]<-All_person_data[,"GWG_state_3rd_CHINA"]<-0
#All_person_data[,"GWG_state_1st_WHO"]<-All_person_data[,"GWG_state_2nd_WHO"]<-All_person_data[,"GWG_state_3rd_WHO"]<-0
fill_matrix<-function(data=All_person_data,BMI="UW",BMI_data=result_UW_add,China=T){
  
  if (China==T) {
    data[which(data[which(data$BMI分级==BMI),"GWG_1st"]<=BMI_data[which(BMI_data$x==12),"C25"]),"GWG_state_1st_CHINA"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_1st"]>=BMI_data[which(BMI_data$x==12),"C75"]),"GWG_state_1st_CHINA"]<-1
    data[which(data[which(data$BMI分级==BMI),"GWG_2nd"]<=BMI_data[which(BMI_data$x==27),"C25"]),"GWG_state_2nd_CHINA"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_2nd"]>=BMI_data[which(BMI_data$x==27),"C75"]),"GWG_state_2nd_CHINA"]<-1
    data[which(data[which(data$BMI分级==BMI),"GWG_3rd"]<=BMI_data[which(BMI_data$x==40),"C25"]),"GWG_state_3rd_CHINA"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_3rd"]>=BMI_data[which(BMI_data$x==40),"C75"]),"GWG_state_3rd_CHINA"]<-1
  }else{
    data[which(data[which(data$BMI分级==BMI),"GWG_1st"]<=BMI_data[which(BMI_data$x==12),"C25"]),"GWG_state_1st_WHO"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_1st"]>=BMI_data[which(BMI_data$x==12),"C75"]),"GWG_state_1st_WHO"]<-1
    data[which(data[which(data$BMI分级==BMI),"GWG_2nd"]<=BMI_data[which(BMI_data$x==27),"C25"]),"GWG_state_2nd_WHO"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_2nd"]>=BMI_data[which(BMI_data$x==27),"C75"]),"GWG_state_2nd_WHO"]<-1
    data[which(data[which(data$BMI分级==BMI),"GWG_3rd"]<=BMI_data[which(BMI_data$x==40),"C25"]),"GWG_state_3rd_WHO"]<-(-1)
    data[which(data[which(data$BMI分级==BMI),"GWG_3rd"]>=BMI_data[which(BMI_data$x==40),"C75"]),"GWG_state_3rd_WHO"]<-1
  }
  return(data)
}
All_person_data<-fill_matrix(data=All_person_data,BMI = "UW",BMI_data=result_UW_add,China = T)
All_person_data<-fill_matrix(data=All_person_data,BMI = "NW",BMI_data=result_NW_new_add,China = T)
All_person_data<-fill_matrix(BMI = "OW",BMI_data=result_OW_new_add,China = T)
All_person_data<-fill_matrix(BMI = "OB",BMI_data=result_OB_new_add,China = T)
All_person_data<-fill_matrix(BMI = "UW",BMI_data=result_UW_add,China = F)
All_person_data<-fill_matrix(BMI = "NW",BMI_data=result_NW_add,China = F)
All_person_data<-fill_matrix(BMI = "OW",BMI_data=result_OW_add,China = F)
All_person_data<-fill_matrix(BMI = "OB",BMI_data=result_OB_add,China = F)
for (i in c(1:nrow(All_person_data))) {
  All_person_data[i,"Birth defect"]<-ifelse(All_person_data[i,"Birth defect1"]==""&All_person_data[i,"Birth defect2"]=="",0,1)
  All_person_data[i,"Pregnancy outcome"]<-ifelse((All_person_data[i,"健康情况2"]=="健康"|All_person_data[i,"健康情况2"]=="活产")&
                                                   (All_person_data[i,"健康情况1"]=="健康"|All_person_data[i,"健康情况1"]=="活产"),0,1)
  All_person_data[i,"Baby weight"]<-ifelse(All_person_data[i,"出生体重1"]>=1000&All_person_data[i,"出生体重1"]<=4000&
                                             All_person_data[i,"出生体重2"]>=1000&All_person_data[i,"出生体重2"]<=4000,0,1)
  All_person_data[i,"Premature"]<-ifelse(All_person_data[i,"分娩孕周"]>=28,0,1)
  
}

if (T) {
  #China&WHO
  #UW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]==3))
    CHINA_2nd_matrix_UW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Diabetes)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Diabetes)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Diabetes,"./CHINA_2nd_matrix_UW_Diabetes.csv")
    chisq.test(CHINA_2nd_matrix_UW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]==3))
    CHINA_2nd_matrix_UW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Anemia)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Anemia)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_UW_Anemia)
    write.csv(CHINA_2nd_matrix_UW_Anemia,"./CHINA_2nd_matrix_UW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]==1))
    CHINA_2nd_matrix_UW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Postpartum_hemorrhage)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_UW_Postpartum_hemorrhage)
    write.csv(CHINA_2nd_matrix_UW_Postpartum_hemorrhage,"./CHINA_2nd_matrix_UW_Postpartum_hemorrhage.csv")
    #Hypertension
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]==5))
    CHINA_2nd_matrix_UW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Hypertension)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Hypertension)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Hypertension,"./CHINA_2nd_matrix_UW_Hypertension.csv")
    chisq.test(CHINA_2nd_matrix_UW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]==6))
    CHINA_2nd_matrix_UW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Thyroid_diseases)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Thyroid_diseases,"./CHINA_2nd_matrix_UW_Thyroid_diseases.csv")
    chisq.test(CHINA_2nd_matrix_UW_Thyroid_diseases)

    #Birth defect
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]==0))
    CHINA_2nd_matrix_UW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Birth_defect)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Birth_defect)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Birth_defect,"./CHINA_2nd_matrix_UW_Birth_defect.csv")
    chisq.test(CHINA_2nd_matrix_UW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]==0))
    CHINA_2nd_matrix_UW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Pregnancy_outcome)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Pregnancy_outcome,"./CHINA_2nd_matrix_UW_Pregnancy_outcome.csv")
    chisq.test(CHINA_2nd_matrix_UW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]==0))
    CHINA_2nd_matrix_UW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Baby_weight)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Baby_weight)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Baby_weight,"./CHINA_2nd_matrix_UW_Baby_weight.csv")
    chisq.test(CHINA_2nd_matrix_UW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]==0))
    CHINA_2nd_matrix_UW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_UW_Premature)=c("1","0")
    colnames(CHINA_2nd_matrix_UW_Premature)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_UW_Premature,"./CHINA_2nd_matrix_UW_Premature.csv")
    chisq.test(CHINA_2nd_matrix_UW_Premature)
  }
  #NW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]==3))
    CHINA_2nd_matrix_NW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Diabetes)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Diabetes)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Diabetes,"./CHINA_2nd_matrix_NW_Diabetes.csv")
    chisq.test(CHINA_2nd_matrix_NW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]==3))
    CHINA_2nd_matrix_NW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Anemia)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Anemia)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_NW_Anemia)
    write.csv(CHINA_2nd_matrix_NW_Anemia,"./CHINA_2nd_matrix_NW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]==1))
    CHINA_2nd_matrix_NW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Postpartum_hemorrhage)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_NW_Postpartum_hemorrhage)
    write.csv(CHINA_2nd_matrix_NW_Postpartum_hemorrhage,"./CHINA_2nd_matrix_NW_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]==5))
    CHINA_2nd_matrix_NW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Hypertension)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Hypertension)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Hypertension,"./CHINA_2nd_matrix_NW_Hypertension.csv")
    chisq.test(CHINA_2nd_matrix_NW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]==6))
    CHINA_2nd_matrix_NW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Thyroid_diseases)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Thyroid_diseases,"./CHINA_2nd_matrix_NW_Thyroid_diseases.csv")
    chisq.test(CHINA_2nd_matrix_NW_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]==0))
    CHINA_2nd_matrix_NW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Birth_defect)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Birth_defect)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Birth_defect,"./CHINA_2nd_matrix_NW_Birth_defect.csv")
    chisq.test(CHINA_2nd_matrix_NW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]==0))
    CHINA_2nd_matrix_NW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Pregnancy_outcome)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Pregnancy_outcome,"./CHINA_2nd_matrix_NW_Pregnancy_outcome.csv")
    chisq.test(CHINA_2nd_matrix_NW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]==0))
    CHINA_2nd_matrix_NW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Baby_weight)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Baby_weight)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Baby_weight,"./CHINA_2nd_matrix_NW_Baby_weight.csv")
    chisq.test(CHINA_2nd_matrix_NW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]==0))
    CHINA_2nd_matrix_NW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_NW_Premature)=c("1","0")
    colnames(CHINA_2nd_matrix_NW_Premature)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_NW_Premature,"./CHINA_2nd_matrix_NW_Premature.csv")
    chisq.test(CHINA_2nd_matrix_NW_Premature)
  }
  #OW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]==3))
    CHINA_2nd_matrix_OW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Diabetes)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Diabetes)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Diabetes,"./CHINA_2nd_matrix_OW_Diabetes.csv")
    chisq.test(CHINA_2nd_matrix_OW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]==3))
    CHINA_2nd_matrix_OW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Anemia)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Anemia)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_OW_Anemia)
    write.csv(CHINA_2nd_matrix_OW_Anemia,"./CHINA_2nd_matrix_OW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]==1))
    CHINA_2nd_matrix_OW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Postpartum_hemorrhage)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_OW_Postpartum_hemorrhage)
    write.csv(CHINA_2nd_matrix_OW_Postpartum_hemorrhage,"./CHINA_2nd_matrix_OW_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]==5))
    CHINA_2nd_matrix_OW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Hypertension)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Hypertension)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Hypertension,"./CHINA_2nd_matrix_OW_Hypertension.csv")
    chisq.test(CHINA_2nd_matrix_OW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]==6))
    CHINA_2nd_matrix_OW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Thyroid_diseases)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Thyroid_diseases,"./CHINA_2nd_matrix_OW_Thyroid_diseases.csv")
    chisq.test(CHINA_2nd_matrix_OW_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]==0))
    CHINA_2nd_matrix_OW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Birth_defect)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Birth_defect)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Birth_defect,"./CHINA_2nd_matrix_OW_Birth_defect.csv")
    chisq.test(CHINA_2nd_matrix_OW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]==0))
    CHINA_2nd_matrix_OW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Pregnancy_outcome)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Pregnancy_outcome,"./CHINA_2nd_matrix_OW_Pregnancy_outcome.csv")
    chisq.test(CHINA_2nd_matrix_OW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]==0))
    CHINA_2nd_matrix_OW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Baby_weight)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Baby_weight)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Baby_weight,"./CHINA_2nd_matrix_OW_Baby_weight.csv")
    chisq.test(CHINA_2nd_matrix_OW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]==0))
    CHINA_2nd_matrix_OW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OW_Premature)=c("1","0")
    colnames(CHINA_2nd_matrix_OW_Premature)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OW_Premature,"./CHINA_2nd_matrix_OW_Premature.csv")
    chisq.test(CHINA_2nd_matrix_OW_Premature)
  }
  #OB
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,9]==3))
    CHINA_2nd_matrix_OB_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Diabetes)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Diabetes)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Diabetes,"./CHINA_2nd_matrix_OB_Diabetes.csv")
    chisq.test(CHINA_2nd_matrix_OB_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,10]==3))
    CHINA_2nd_matrix_OB_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Anemia)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Anemia)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_OB_Anemia)
    write.csv(CHINA_2nd_matrix_OB_Anemia,"./CHINA_2nd_matrix_OB_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,12]==1))
    CHINA_2nd_matrix_OB_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Postpartum_hemorrhage)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(CHINA_2nd_matrix_OB_Postpartum_hemorrhage)
    write.csv(CHINA_2nd_matrix_OB_Postpartum_hemorrhage,"./CHINA_2nd_matrix_OB_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,8]==5))
    CHINA_2nd_matrix_OB_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Hypertension)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Hypertension)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Hypertension,"./CHINA_2nd_matrix_OB_Hypertension.csv")
    chisq.test(CHINA_2nd_matrix_OB_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,25]==6))
    CHINA_2nd_matrix_OB_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Thyroid_diseases)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Thyroid_diseases,"./CHINA_2nd_matrix_OB_Thyroid_diseases.csv")
    chisq.test(CHINA_2nd_matrix_OB_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,37]==0))
    CHINA_2nd_matrix_OB_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Birth_defect)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Birth_defect)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Birth_defect,"./CHINA_2nd_matrix_OB_Birth_defect.csv")
    chisq.test(CHINA_2nd_matrix_OB_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,38]==0))
    CHINA_2nd_matrix_OB_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Pregnancy_outcome)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Pregnancy_outcome,"./CHINA_2nd_matrix_OB_Pregnancy_outcome.csv")
    chisq.test(CHINA_2nd_matrix_OB_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,39]==0))
    CHINA_2nd_matrix_OB_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Baby_weight)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Baby_weight)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Baby_weight,"./CHINA_2nd_matrix_OB_Baby_weight.csv")
    chisq.test(CHINA_2nd_matrix_OB_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_CHINA"]==-1&All_person_data[,40]==0))
    CHINA_2nd_matrix_OB_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(CHINA_2nd_matrix_OB_Premature)=c("1","0")
    colnames(CHINA_2nd_matrix_OB_Premature)=c("High","Normal","Low")
    write.csv(CHINA_2nd_matrix_OB_Premature,"./CHINA_2nd_matrix_OB_Premature.csv")
    chisq.test(CHINA_2nd_matrix_OB_Premature)
  }
  #UW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]==3))
    WHO_2nd_matrix_UW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Diabetes)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Diabetes)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Diabetes,"./WHO_2nd_matrix_UW_Diabetes.csv")
    chisq.test(WHO_2nd_matrix_UW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]==3))
    WHO_2nd_matrix_UW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Anemia)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Anemia)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_UW_Anemia)
    write.csv(WHO_2nd_matrix_UW_Anemia,"./WHO_2nd_matrix_UW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]==1))
    WHO_2nd_matrix_UW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Postpartum_hemorrhage)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_UW_Postpartum_hemorrhage)
    write.csv(WHO_2nd_matrix_UW_Postpartum_hemorrhage,"./WHO_2nd_matrix_UW_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]==5))
    WHO_2nd_matrix_UW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Hypertension)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Hypertension)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Hypertension,"./WHO_2nd_matrix_UW_Hypertension.csv")
    chisq.test(WHO_2nd_matrix_UW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]==6))
    WHO_2nd_matrix_UW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Thyroid_diseases)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Thyroid_diseases,"./WHO_2nd_matrix_UW_Thyroid_diseases.csv")
    chisq.test(WHO_2nd_matrix_UW_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]==0))
    WHO_2nd_matrix_UW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Birth_defect)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Birth_defect)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Birth_defect,"./WHO_2nd_matrix_UW_Birth_defect.csv")
    chisq.test(WHO_2nd_matrix_UW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]==0))
    WHO_2nd_matrix_UW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Pregnancy_outcome)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Pregnancy_outcome,"./WHO_2nd_matrix_UW_Pregnancy_outcome.csv")
    chisq.test(WHO_2nd_matrix_UW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]==0))
    WHO_2nd_matrix_UW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Baby_weight)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Baby_weight)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Baby_weight,"./WHO_2nd_matrix_UW_Baby_weight.csv")
    chisq.test(WHO_2nd_matrix_UW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='UW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]==0))
    WHO_2nd_matrix_UW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_UW_Premature)=c("1","0")
    colnames(WHO_2nd_matrix_UW_Premature)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_UW_Premature,"./WHO_2nd_matrix_UW_Premature.csv")
    chisq.test(WHO_2nd_matrix_UW_Premature)
  }
  #NW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]==3))
    WHO_2nd_matrix_NW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Diabetes)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Diabetes)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Diabetes,"./WHO_2nd_matrix_NW_Diabetes.csv")
    chisq.test(WHO_2nd_matrix_NW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]==3))
    WHO_2nd_matrix_NW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Anemia)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Anemia)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_NW_Anemia)
    write.csv(WHO_2nd_matrix_NW_Anemia,"./WHO_2nd_matrix_NW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]==1))
    WHO_2nd_matrix_NW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Postpartum_hemorrhage)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_NW_Postpartum_hemorrhage)
    write.csv(WHO_2nd_matrix_NW_Postpartum_hemorrhage,"./WHO_2nd_matrix_NW_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]==5))
    WHO_2nd_matrix_NW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Hypertension)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Hypertension)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Hypertension,"./WHO_2nd_matrix_NW_Hypertension.csv")
    chisq.test(WHO_2nd_matrix_NW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]==6))
    WHO_2nd_matrix_NW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Thyroid_diseases)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Thyroid_diseases,"./WHO_2nd_matrix_NW_Thyroid_diseases.csv")
    chisq.test(WHO_2nd_matrix_NW_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]==0))
    WHO_2nd_matrix_NW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Birth_defect)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Birth_defect)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Birth_defect,"./WHO_2nd_matrix_NW_Birth_defect.csv")
    chisq.test(WHO_2nd_matrix_NW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]==0))
    WHO_2nd_matrix_NW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Pregnancy_outcome)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Pregnancy_outcome,"./WHO_2nd_matrix_NW_Pregnancy_outcome.csv")
    chisq.test(WHO_2nd_matrix_NW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]==0))
    WHO_2nd_matrix_NW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Baby_weight)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Baby_weight)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Baby_weight,"./WHO_2nd_matrix_NW_Baby_weight.csv")
    chisq.test(WHO_2nd_matrix_NW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='NW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]==0))
    WHO_2nd_matrix_NW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_NW_Premature)=c("1","0")
    colnames(WHO_2nd_matrix_NW_Premature)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_NW_Premature,"./WHO_2nd_matrix_NW_Premature.csv")
    chisq.test(WHO_2nd_matrix_NW_Premature)
  }
  #OW
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]==3))
    WHO_2nd_matrix_OW_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Diabetes)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Diabetes)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Diabetes,"./WHO_2nd_matrix_OW_Diabetes.csv")
    chisq.test(WHO_2nd_matrix_OW_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]==3))
    WHO_2nd_matrix_OW_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Anemia)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Anemia)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_OW_Anemia)
    write.csv(WHO_2nd_matrix_OW_Anemia,"./WHO_2nd_matrix_OW_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]==1))
    WHO_2nd_matrix_OW_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Postpartum_hemorrhage)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_OW_Postpartum_hemorrhage)
    write.csv(WHO_2nd_matrix_OW_Postpartum_hemorrhage,"./WHO_2nd_matrix_OW_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]==5))
    WHO_2nd_matrix_OW_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Hypertension)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Hypertension)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Hypertension,"./WHO_2nd_matrix_OW_Hypertension.csv")
    chisq.test(WHO_2nd_matrix_OW_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]==6))
    WHO_2nd_matrix_OW_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Thyroid_diseases)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Thyroid_diseases,"./WHO_2nd_matrix_OW_Thyroid_diseases.csv")
    chisq.test(WHO_2nd_matrix_OW_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]==0))
    WHO_2nd_matrix_OW_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Birth_defect)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Birth_defect)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Birth_defect,"./WHO_2nd_matrix_OW_Birth_defect.csv")
    chisq.test(WHO_2nd_matrix_OW_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]==0))
    WHO_2nd_matrix_OW_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Pregnancy_outcome)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Pregnancy_outcome,"./WHO_2nd_matrix_OW_Pregnancy_outcome.csv")
    chisq.test(WHO_2nd_matrix_OW_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]==0))
    WHO_2nd_matrix_OW_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Baby_weight)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Baby_weight)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Baby_weight,"./WHO_2nd_matrix_OW_Baby_weight.csv")
    chisq.test(WHO_2nd_matrix_OW_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='OW'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]==0))
    WHO_2nd_matrix_OW_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OW_Premature)=c("1","0")
    colnames(WHO_2nd_matrix_OW_Premature)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OW_Premature,"./WHO_2nd_matrix_OW_Premature.csv")
    chisq.test(WHO_2nd_matrix_OW_Premature)
  }
  #OB
  if (T) {
    #Diabetes
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,9]==3))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]!=3&All_person_data[,9]!=5))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,9]==3))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]!=3&All_person_data[,9]!=5))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,9]==3))
    WHO_2nd_matrix_OB_Diabetes<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Diabetes)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Diabetes)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Diabetes,"./WHO_2nd_matrix_OB_Diabetes.csv")
    chisq.test(WHO_2nd_matrix_OB_Diabetes)
    #Anemia
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]!=3))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,10]==3))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]!=3))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,10]==3))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]!=3))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,10]==3))
    WHO_2nd_matrix_OB_Anemia<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Anemia)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Anemia)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_OB_Anemia)
    write.csv(WHO_2nd_matrix_OB_Anemia,"./WHO_2nd_matrix_OB_Anemia.csv")
    #Postpartum hemorrhage
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]!=1))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,12]==1))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]!=1))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,12]==1))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]!=1))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,12]==1))
    WHO_2nd_matrix_OB_Postpartum_hemorrhage<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Postpartum_hemorrhage)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Postpartum_hemorrhage)=c("High","Normal","Low")
    chisq.test(WHO_2nd_matrix_OB_Postpartum_hemorrhage)
    write.csv(WHO_2nd_matrix_OB_Postpartum_hemorrhage,"./WHO_2nd_matrix_OB_Postpartum_hemorrhage.csv")
    #Hypertension
    
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]!=5))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,8]==5))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]!=5))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,8]==5))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]!=5))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,8]==5))
    WHO_2nd_matrix_OB_Hypertension<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Hypertension)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Hypertension)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Hypertension,"./WHO_2nd_matrix_OB_Hypertension.csv")
    chisq.test(WHO_2nd_matrix_OB_Hypertension)
    #Thyroid-related diseases
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,25]==6))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]!=6&All_person_data[,25]!=9))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,25]==6))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]!=6&All_person_data[,25]!=9))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,25]==6))
    WHO_2nd_matrix_OB_Thyroid_diseases<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Thyroid_diseases)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Thyroid_diseases)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Thyroid_diseases,"./WHO_2nd_matrix_OB_Thyroid_diseases.csv")
    chisq.test(WHO_2nd_matrix_OB_Thyroid_diseases)
    #（这里注意matrix的编码发生了改变）
    #Birth defect
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,37]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,37]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,37]==0))
    WHO_2nd_matrix_OB_Birth_defect<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Birth_defect)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Birth_defect)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Birth_defect,"./WHO_2nd_matrix_OB_Birth_defect.csv")
    chisq.test(WHO_2nd_matrix_OB_Birth_defect)
    #Pregnancy outcome
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,38]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,38]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,38]==0))
    WHO_2nd_matrix_OB_Pregnancy_outcome<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Pregnancy_outcome)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Pregnancy_outcome)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Pregnancy_outcome,"./WHO_2nd_matrix_OB_Pregnancy_outcome.csv")
    chisq.test(WHO_2nd_matrix_OB_Pregnancy_outcome)
    #Baby weight
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,39]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,39]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,39]==0))
    WHO_2nd_matrix_OB_Baby_weight<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Baby_weight)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Baby_weight)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Baby_weight,"./WHO_2nd_matrix_OB_Baby_weight.csv")
    chisq.test(WHO_2nd_matrix_OB_Baby_weight)
    #Premature
    n=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]!=0))
    m=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==1&All_person_data[,40]==0))
    p=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]!=0))
    q=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==0&All_person_data[,40]==0))
    a=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]!=0))
    b=length(which(All_person_data$BMI分级=='OB'&All_person_data[,"GWG_state_2nd_WHO"]==-1&All_person_data[,40]==0))
    WHO_2nd_matrix_OB_Premature<-matrix(c(n,m,p,q,a,b),nrow = 2,ncol = 3)
    rownames(WHO_2nd_matrix_OB_Premature)=c("1","0")
    colnames(WHO_2nd_matrix_OB_Premature)=c("High","Normal","Low")
    write.csv(WHO_2nd_matrix_OB_Premature,"./WHO_2nd_matrix_OB_Premature.csv")
    chisq.test(WHO_2nd_matrix_OB_Premature)
  }
}

calculate_risk_factor<-function(P_list,count_matrix,compare,filename){
  P_list<-matrix(nrow=9,ncol=6)
  colnames(P_list)<-c("Risk","Point estimate","Lower","Upper","P value","N")
  P_list[1,1]="Hypertension"
  if (length(which(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")]<5))==0) {
    P_list[1,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")])[1])
    P_list[1,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")])[3])
    P_list[1,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")])[2])[1])
    P_list[1,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")])[2])[2])
    P_list[1,6]=sum(get(paste0(count_matrix,"Hypertension"))[,c(compare,"Normal")])
  }
  P_list[2,1]="Diabetes"
  if (length(which(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")]<5))==0) {
  P_list[2,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")])[1])
  P_list[2,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")])[3])
  P_list[2,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")])[2])[1])
  P_list[2,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")])[2])[2])
  P_list[2,6]=sum(get(paste0(count_matrix,"Diabetes"))[,c(compare,"Normal")])
  }
  P_list[3,1]="Anemia"
  if (length(which(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")]<5))==0) {
  P_list[3,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")])[1])
  P_list[3,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")])[3])
  P_list[3,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")])[2])[1])
  P_list[3,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")])[2])[2])
  P_list[3,6]=sum(get(paste0(count_matrix,"Anemia"))[,c(compare,"Normal")])
  }
  P_list[4,1]="Postpartum_hemorrhage"
  if (length(which(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")]<5))==0) {
  P_list[4,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")])[1])
  P_list[4,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")])[3])
  P_list[4,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")])[2])[1])
  P_list[4,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")])[2])[2])
  P_list[4,6]=sum(get(paste0(count_matrix,"Postpartum_hemorrhage"))[,c(compare,"Normal")])
  }
  P_list[5,1]="Thyroid_diseases"
  if (length(which(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")]<5))==0) {
  P_list[5,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")])[1])
  P_list[5,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")])[3])
  P_list[5,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")])[2])[1])
  P_list[5,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")])[2])[2])
  P_list[5,6]=sum(get(paste0(count_matrix,"Thyroid_diseases"))[,c(compare,"Normal")])
  }
  P_list[6,1]="Birth_defect"
  if (length(which(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")]<5))==0) {
  P_list[6,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")])[1])
  P_list[6,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")])[3])
  P_list[6,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")])[2])[1])
  P_list[6,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")])[2])[2])
  P_list[6,6]=sum(get(paste0(count_matrix,"Birth_defect"))[,c(compare,"Normal")])
  }
  P_list[7,1]="Pregnancy_outcome"
  if (length(which(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")]<5))==0) {
  P_list[7,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")])[1])
  P_list[7,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")])[3])
  P_list[7,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")])[2])[1])
  P_list[7,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")])[2])[2])
  P_list[7,6]=sum(get(paste0(count_matrix,"Pregnancy_outcome"))[,c(compare,"Normal")])
  }
  P_list[8,1]="Baby_weight"
  if (length(which(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")]<5))==0) {
  P_list[8,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")])[1])
  P_list[8,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")])[3])
  P_list[8,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")])[2])[1])
  P_list[8,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")])[2])[2])
  P_list[8,6]=sum(get(paste0(count_matrix,"Baby_weight"))[,c(compare,"Normal")])
  }
  P_list[9,1]="Premature"
  if (length(which(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")]<5))==0) {
  P_list[9,5]=as.numeric(fisher.test(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")])[1])
  P_list[9,2]=as.numeric(fisher.test(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")])[3])
  P_list[9,3]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")])[2])[1])
  P_list[9,4]=as.numeric(unlist(fisher.test(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")])[2])[2])
  P_list[9,6]=sum(get(paste0(count_matrix,"Premature"))[,c(compare,"Normal")])
  }
  write.csv(P_list,filename,row.names = F)
}
#
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_UW_high2normal,
                      count_matrix = "WHO_3rd_matrix_UW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_UW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_NW_high2normal,
                      count_matrix = "WHO_3rd_matrix_NW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_NW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_OW_high2normal,
                      count_matrix = "WHO_3rd_matrix_OW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_OW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_OB_high2normal,
                      count_matrix = "WHO_3rd_matrix_OB_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_OB_high2normal.csv")
#####for low2normal
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_UW_low2normal,
                      count_matrix = "WHO_3rd_matrix_UW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_UW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_NW_low2normal,
                      count_matrix = "WHO_3rd_matrix_NW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_NW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_OW_low2normal,
                      count_matrix = "WHO_3rd_matrix_OW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_OW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_WHO_3rd_BMI_OB_low2normal,
                      count_matrix = "WHO_3rd_matrix_OB_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_WHO_3rd_BMI_OB_low2normal.csv")
###
#
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_UW_high2normal,
                      count_matrix = "CHINA_3rd_matrix_UW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_UW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_NW_high2normal,
                      count_matrix = "CHINA_3rd_matrix_NW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_NW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_OW_high2normal,
                      count_matrix = "CHINA_3rd_matrix_OW_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_OW_high2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_OB_high2normal,
                      count_matrix = "CHINA_3rd_matrix_OB_",
                      compare = "High",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_OB_high2normal.csv")
#####for low2normal
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_UW_low2normal,
                      count_matrix = "CHINA_3rd_matrix_UW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_UW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_NW_low2normal,
                      count_matrix = "CHINA_3rd_matrix_NW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_NW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_OW_low2normal,
                      count_matrix = "CHINA_3rd_matrix_OW_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_OW_low2normal.csv")
calculate_risk_factor(P_list = risk_factor_CHINA_3rd_BMI_OB_low2normal,
                      count_matrix = "CHINA_3rd_matrix_OB_",
                      compare = "Low",
                      filename = "./Risk_factor_New/risk_factor_CHINA_3rd_BMI_OB_low2normal.csv")

#Check
for (i in list.files(path = "C:\\Users\\jfl95\\Documents\\双胎孕妇\\Risk_factor_New")) {
  check_data<-read.csv(paste0("C:\\Users\\jfl95\\Documents\\双胎孕妇\\Risk_factor_New\\",i))
}
