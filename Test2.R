library(dplyr)
options(scipen = 999)#prevent e forming

#Read CSV
demographics <- read.csv("~/Documents/Phd/Work/Interview/SDS RWE Data Challenge Dataset/demographics.csv")
#Read clinical_data and rename clinical_data patient ID
clinical_data <- read.csv("~/Documents/Phd/Work/Interview/SDS RWE Data Challenge Dataset/clinical_data.csv")%>%
  rename(patient_id=id)
bill_id <- read.csv("~/Documents/Phd/Work/Interview/SDS RWE Data Challenge Dataset/bill_id.csv")
bill_amount <- read.csv("~/Documents/Phd/Work/Interview/SDS RWE Data Challenge Dataset/bill_amount.csv")

#standardize data-clinical_data
clinical_data$medical_history_hbp=ifelse(clinical_data$medical_history_hbp=="No",0,
                                                     ifelse(clinical_data$medical_history_hbp=="Yes",1,
                                                            clinical_data$medical_history_hbp))

#standardize data-demographics
demographics$gender=ifelse(demographics$gender=="f","Female",
                                        ifelse(demographics$gender=="m","Male",
                                               demographics$gender))
demographics$race=ifelse(demographics$race=="chinese","Chinese",
                                      ifelse(demographics$race=="India","Indian",
                                             demographics$race))
demographics$resident_status=ifelse(demographics$resident_status=="Singaporean","Singapore citizen",
                                                 demographics$resident_status)
demographics$resident_status <- gsub("Singaporean", "Singapore citizen", demographics$resident_status)#gsub has the risk not exact fit/match

#Join bill_id and bill_amount
bill_id_amount=left_join(bill_id,bill_amount,by=c("bill_id"))

#Find out number of patients
no.of_patient=length(unique(demographics$patient_id))
no.of_patient_in_bill=length(unique(bill_id_amount$patient_id))
no.of_patient_in_clinical_data=length(unique(clinical_data$id))


#Group bill_id_amount belonging to same date
str(bill_id_amount)
bill_id_amount_2=bill_id_amount%>%
  group_by(patient_id,date_of_admission)%>%
  summarize(cost_peradmission_perid=sum(amount,na.rm = TRUE))



#Standardize date format
library(lubridate)
clinical_data$date_of_admission<- dmy(clinical_data$date_of_admission)
clinical_data$date_of_discharge<- dmy(clinical_data$date_of_discharge)
bill_id_amount_2$date_of_admission=as.Date(bill_id_amount_2$date_of_admission)


#Join clinical_data and bill_amount_2
clinical_data_bill=left_join(clinical_data,bill_id_amount_2,by=c("patient_id","date_of_admission"))

#Join clinical_data_bill and demographics
clinical_data_bill_demo=left_join(clinical_data_bill,demographics,by=c("patient_id"))



#Standardize data
clinical_data_bill_demo_2=clinical_data_bill_demo
clinical_data_bill_demo_2$LOS=clinical_data_bill_demo_2$date_of_discharge-clinical_data_bill_demo_2$date_of_admission
clinical_data_bill_demo_2$BMI=clinical_data_bill_demo_2$weight/((clinical_data_bill_demo_2$height/100)^2)
clinical_data_bill_demo_2$Age=2024-year(clinical_data_bill_demo_2$date_of_birth)


#Combine MDD treatments
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_anx_2 = ifelse(trt_anx == 1, "Anxiolytics", NA))

clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_con_2 = ifelse(trt_con == 1, "Anticonvulsants", NA))
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_adt_2 = ifelse(trt_adt == 1, "Antidepressants", NA))
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_ssr_2 = ifelse(trt_ssr == 1, "SSRI", NA))
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_the_2 = ifelse(trt_the == 1, "Psychotherapy", NA))
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%mutate(trt_oth_2 = ifelse(trt_oth == 1, "Other psychiatric medications", NA))#other no need to put in combination

clinical_data_bill_demo_2=clinical_data_bill_demo_2%>%
  rowwise()%>%
  mutate(combined_trt=paste(sort(na.omit(c(trt_anx_2, trt_con_2, trt_adt_2, trt_ssr_2, trt_the_2,trt_oth_2))),collapse=","))%>%
  select(-c(trt_anx_2, trt_con_2, trt_adt_2, trt_ssr_2, trt_the_2,trt_oth_2))


#Number of treatment
clinical_data_bill_demo_2 <- clinical_data_bill_demo_2 %>%
  rowwise() %>%
  mutate(treatment_totalnumber= sum(c_across(trt_anx:trt_oth))) %>%
  ungroup()


#Find out which nth hospitalization for each admission
clinical_data_bill_demo_3=clinical_data_bill_demo_2%>%
  group_by(patient_id) %>%
  arrange(date_of_admission)%>%
  mutate(hospitalization_nth=row_number())%>%
  ungroup()

#Find out total number of rehospitalization for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(rehospitalization_totalnumber= max(hospitalization_nth)-1)

#Find out total days of rehospitalization for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(rehospitalization_LOS = ifelse(hospitalization_nth >= 2, as.numeric(LOS), 0))%>%
  mutate(rehospitalization_LOS_totalnumber = sum(rehospitalization_LOS))%>%
  ungroup()%>%
  select(-rehospitalization_LOS)#no need, this is just to calculate rehospitalization_LOS_totalnumber

#Find out total cost of rehospitalization for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(rehospitalization_cost = ifelse(hospitalization_nth >= 2, as.numeric(cost_peradmission_perid), 0))%>%
  mutate(rehospitalization_cost_totalnumber = sum(rehospitalization_cost))%>%
  ungroup()%>%
  select(-rehospitalization_cost)#no need, this is just to calculate rehospitalization_cost_totalnumber

#Find out subsequent number of treatments for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(treatment_totalnumberT_1 = ifelse(hospitalization_nth == 2, treatment_totalnumber, 0))%>%#1st rehospitalization
  mutate(treatment_totalnumberT1 = sum(treatment_totalnumberT_1))%>%
  group_by(patient_id) %>%
  mutate(treatment_totalnumberT_2 = ifelse(hospitalization_nth == 3, treatment_totalnumber, 0))%>%
  mutate(treatment_totalnumberT2 = sum(treatment_totalnumberT_2))%>%
  group_by(patient_id) %>%
  mutate(treatment_totalnumberT_3 = ifelse(hospitalization_nth == 4, treatment_totalnumber, 0))%>%
  mutate(treatment_totalnumberT3 = sum(treatment_totalnumberT_3))%>%
  ungroup()%>%
  select(-treatment_totalnumberT_1,-treatment_totalnumberT_2,-treatment_totalnumberT_3)#no need


#Find out subsequent GAF score for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(GAFscoreT_1 = ifelse(hospitalization_nth == 2, gaf_lv, 0))%>%#1st rehospitalization
  mutate(GAFscoreT1 = sum(GAFscoreT_1))%>%
  group_by(patient_id) %>%
  mutate(GAFscoreT_2 = ifelse(hospitalization_nth == 3, gaf_lv, 0))%>%
  mutate(GAFscoreT2 = sum(GAFscoreT_2))%>%
  group_by(patient_id) %>%
  mutate(GAFscoreT_3 = ifelse(hospitalization_nth == 4, gaf_lv, 0))%>%
  mutate(GAFscoreT3 = sum(GAFscoreT_3))%>%
  ungroup()%>%
  select(-GAFscoreT_1,-GAFscoreT_2,-GAFscoreT_3)#no need

#Find out subsequent cgis_adm score for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(cgis_adm_scoreT_1 = ifelse(hospitalization_nth == 2, cgis_adm, 0))%>%#1st rehospitalization
  mutate(cgis_adm_scoreT1 = sum(cgis_adm_scoreT_1))%>%
  group_by(patient_id) %>%
  mutate(cgis_adm_scoreT_2 = ifelse(hospitalization_nth == 3, cgis_adm, 0))%>%
  mutate(cgis_adm_scoreT2 = sum(cgis_adm_scoreT_2))%>%
  group_by(patient_id) %>%
  mutate(cgis_adm_scoreT_3 = ifelse(hospitalization_nth == 4, cgis_adm, 0))%>%
  mutate(cgis_adm_scoreT3 = sum(cgis_adm_scoreT_3))%>%
  ungroup()%>%
  select(-cgis_adm_scoreT_1,-cgis_adm_scoreT_2,-cgis_adm_scoreT_3)#no need

#Find out subsequent cgis_dis score for each patient
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  group_by(patient_id) %>%
  mutate(cgis_dis_scoreT_1 = ifelse(hospitalization_nth == 2, cgis_dis, 0))%>%#1st rehospitalization
  mutate(cgis_dis_scoreT1 = sum(cgis_dis_scoreT_1))%>%
  group_by(patient_id) %>%
  mutate(cgis_dis_scoreT_2 = ifelse(hospitalization_nth == 3, cgis_dis, 0))%>%
  mutate(cgis_dis_scoreT2 = sum(cgis_dis_scoreT_2))%>%
  group_by(patient_id) %>%
  mutate(cgis_dis_scoreT_3 = ifelse(hospitalization_nth == 4, cgis_dis, 0))%>%
  mutate(cgis_dis_scoreT3 = sum(cgis_dis_scoreT_3))%>%
  ungroup()%>%
  select(-cgis_dis_scoreT_1,-cgis_dis_scoreT_2,-cgis_dis_scoreT_3)#no need

colnames(clinical_data_bill_demo_3)
clinical_data_bill_demo_3=clinical_data_bill_demo_3%>%
  mutate_at(vars(41:52),~ifelse(.==0,NA,.))

#Select the earliest admission date
earliest_admissions <- clinical_data_bill_demo_3 %>%
  group_by(patient_id) %>%
  slice_min(date_of_admission) %>%
  ungroup()



#Data selection
descriptive=earliest_admissions#earliest_admissions#demographics #clinical_data_bill_demo_3

#Descriptive stats

#Remove unneccesary columns
descriptive_2=descriptive%>%
  select(-c("date_of_birth"))

#Missing data
colnames(descriptive_2)


descriptive_2[, -c(22:27,31:33,35,38:51)] <- lapply(descriptive_2[, -c(22:27,31:33,35,38:51)] , as.factor)
descriptive_2[, c(22:27,31:33,35,38:51)] <- lapply(descriptive_2[, c(22:27,31:33,35,38:51)] , as.numeric)


#Find variables significant in predicting outcomes (logistic regression models,retain those with an association having a p-value of ≤ 0.1)

vars <- colnames(descriptive_2)[-c(1:3,27,31,36:51)]#remove ID, date of admission and discharge, cost, LOS,hospitalization
xvars = list()

#rehospitalization_totalnumber
for (var in vars) {
  mod <- as.formula(sprintf("rehospitalization_totalnumber ~ %s", var))
  fit <- glm(formula = mod, data = descriptive_2, family=binomial(link = "logit"))
  p_value <- summary(fit)$coefficients[,4][2]
  if (p_value <= 0.1){
    xvars[[var]] <- fit
  }
}#"cgis_dis"

#rehospitalization_LOS_totalnumber
for (var in vars) {
  mod <- as.formula(sprintf("rehospitalization_LOS_totalnumber ~ %s", var))
  fit <- glm(formula = mod, data = descriptive_2, family= gaussian(link = "identity"))
  p_value <- summary(fit)$coefficients[,4][2]
  if (p_value <= 0.1){
    xvars[[var]] <- fit
  }
}#"medical_history_ren" "cgis_dis" 

xvars <- names(xvars)
print(xvars)

#Missing data
library(naniar)
library(mice)
Missingdata=descriptive_2
gg_miss_var(Missingdata,show_pct = TRUE)#show missing data#completedData

tempData  <- mice(descriptive_2[-c(1:3,27,31,36:51)],#remove ID, date of admission and discharge, cost, LOS,hospitalization
                  m = 5,#no of imputations, 5 is sufficient
                  maxit = 5,#number of iterations for each imputation
                  method = "cart",
                  seed = 127493,threshold=1.1)
summary(tempData)
completeddata<- complete(tempData,1)
descriptive_2_completeddata=cbind(completeddata,descriptive_2[,c(27,31,36:48)])#remove ID, date of admission and discharge


#Propensity score matching
library(MatchIt)

matchitdata=descriptive_2_completeddata


set.seed(123)
m.out <- matchit(as.factor(trt_ssr) ~ gender + race +Age+resident_status+medical_history_dia + medical_history_sud + medical_history_hbp +
                   medical_history_ren + medical_history_tum + medical_history_anx + medical_history_mood +treatment_totalnumber +
                   trt_anx + trt_con + trt_adt + trt_the + trt_oth + cgis_adm +gaf_lv ,
                 data = descriptive_2_completeddata, methods = 'nearest',caliper = .2,distance = 'glm', replace = FALSE)#,ratio = 1,caliper = .2,replace = FALSE,


summary(m.out)
md <- match.data(m.out)
md <- md%>%select(-c("distance","weights","subclass"))

#Select population of interest
descriptive_3=descriptive_2[, -c(1,2,3)]#descriptive_2_completeddata#descriptive_2[, -c(1,2,3)]#md

descriptive_3=descriptive_3%>%select(gender,Age,race,resident_status,weight,height,BMI,medical_history_dia,medical_history_sud,medical_history_hbp,medical_history_ren,medical_history_tum,medical_history_anx,medical_history_mood,
                                     trt_anx,trt_con,trt_adt,trt_ssr,trt_the,trt_oth,combined_trt,treatment_totalnumber,symptom_1,symptom_2,symptom_3,symptom_4,symptom_5,
                                     cgis_adm,cgis_dis,gaf_lv,LOS,cost_peradmission_perid,hospitalization_nth,rehospitalization_totalnumber,
                                     rehospitalization_LOS_totalnumber,rehospitalization_cost_totalnumber,treatment_totalnumberT1,treatment_totalnumberT2,treatment_totalnumberT3,cgis_adm_scoreT1,
                                     cgis_adm_scoreT2,cgis_adm_scoreT3,cgis_dis_scoreT1,cgis_dis_scoreT2,cgis_dis_scoreT3,GAFscoreT1,GAFscoreT2,GAFscoreT3)
descriptive_3 =descriptive_3 %>% rename("Diabetes"=medical_history_dia,"Substance use disorder"=medical_history_sud,"High blood pressure"=medical_history_hbp,              
                         "Renal failure"=medical_history_ren,"Solid tumour"=medical_history_tum, "Anxiety disorder"=medical_history_anx,
                         "Other mood disorders"=medical_history_mood,"Anxiolytics"=trt_anx,"Anticonvulsants"=trt_con,"Antidepressants"=trt_adt,
                         "SSRI"=trt_ssr,"Psychotherapy"=trt_the,"Other psychiatric medications"=trt_oth,"Abnormal sleep patterns"=symptom_1,
                         "Anhedonia"=symptom_2,"Poor appetite"=symptom_3,"Feeling depressed or hopeless"=symptom_4,"Suicidal thoughts"=symptom_5,                        
                         "Weight"=weight,"Height"=height,"CGIS at admission"=cgis_adm,"CGIS at discharge"=cgis_dis,"GAF Score"=gaf_lv,"Gender"=gender,                           
                         "Race"=race,"Resident status"=resident_status,"Treatment combination"=combined_trt,"Number of treatments"=treatment_totalnumber,                       
                         "Cost of admission"=cost_peradmission_perid,"Hospitalization length-of-stay"=LOS,"Nth hospitalization"=hospitalization_nth,             
                         "Number of Rehospitalization"=rehospitalization_totalnumber,"Total Rehospitalization length-of-stay"=rehospitalization_LOS_totalnumber,
                         "Total Rehospitalization cost"=rehospitalization_cost_totalnumber,"CGIS at admission T1"=cgis_adm_scoreT1,"CGIS at admission T2"=cgis_adm_scoreT2,"CGIS at admission T3"=cgis_adm_scoreT3,
                         "CGIS at discharge T1"=cgis_dis_scoreT1,"CGIS at discharge T2"=cgis_dis_scoreT2,"CGIS at discharge T3"=cgis_dis_scoreT3,"GAF score T1"=GAFscoreT1,
                         "GAF score T2"=GAFscoreT2,"GAF score T3"=GAFscoreT3,"No. of treatment T1"=treatment_totalnumberT1,"No. of treatment T2"=treatment_totalnumberT2,"No. of treatment T3"=treatment_totalnumberT3)



library(tableone)
#strata = "SSRI",
descriptive_3.table <- CreateTableOne(addOverall = TRUE,includeNA=TRUE,vars = colnames(descriptive_3),data = descriptive_3,test = TRUE)

print(descriptive_3.table, smd = TRUE)#after matching
descriptive_3.tablecsv=print(descriptive_3.table, smd = TRUE,quote = FALSE, noSpaces = TRUE, printToggle = FALSE)#prematching#SMDs which are greater than 0.1 because those are the variables which shows imbalance in the dataset and that is where we actually need to do propensity score matching.
write.csv(descriptive_3.tablecsv, file = "~/Downloads/cat1.csv")
View(descriptive_3.tablecsv)
View(descriptive_3.table)

## Variables smd>0.2, not good balance
smd_values1 <- ExtractSmd(descriptive_3.table)
smd_values1_df <- as.data.frame(smd_values1)
smd_subset <- smd_values1_df[smd_values1_df > 0.2, , drop = FALSE]



####Graphs

###Treatment combinations
# Count occurrences of each treatment combination
treatment_counts <- table(descriptive_3$`Treatment combination`)


treatment_data <- data.frame(
  Treatment = names(treatment_counts),
  Count = as.numeric(treatment_counts)
)
# Sort the data frame by Count in descending order
treatment_data <- treatment_data[order(-treatment_data$Count), ]

library(ggplot2)
# Pie chart with smaller legend and without legend title
ggplot(treatment_data, aes(x = "", y = Count, fill = Treatment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "lines"),  # Adjust the size of legend symbols
        legend.text = element_text(size = 5),  # Adjust the size of legend text
        legend.title = element_blank()) +  # Remove legend title
  labs(title = "Distribution of Treatment Combinations")

###No. of Treatment
treatment_number <- table(descriptive_3$`Number of treatments`)
treatment_data=treatment_number
  
treatment_data <- data.frame( 
  Treatment_number = names(treatment_number),
  Count = as.numeric(treatment_number)
)

ggplot(treatment_data, aes(x = "", y = Count, fill = as.factor(Treatment_number))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.key.size = unit(2, "lines"),  # Adjust the size of legend symbols
        legend.text = element_text(size = 8),  # Adjust the size of legend text
        legend.title = element_blank()) +  # Remove legend title
  labs(title = "Number of Treatments")

# Plot the pie chart
ggplot(treatment_data, aes(x = "", y = Percentage, fill = Treatment)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Breakdown of Treatment Combinations for MDD Patients") +
  scale_fill_discrete(name = "Treatment Combinations") +
  theme(legend.position = "right") +
  scale_y_continuous(labels = percent)

# Plot the boxplot
library(dplyr)
library(tidyr)
# Reshape data to long format
long_data=descriptive_2 %>%
  mutate(treatment_totalnumberT0=treatment_totalnumber)%>%
  mutate(GAFscoreT0=gaf_lv)%>%
  mutate(cgis_adm_scoreT0=cgis_adm)%>%
  mutate(cgis_dis_scoreT0=cgis_dis)%>%
  mutate(trt_ssr = ifelse(trt_ssr == 1, "SSRI", ifelse(trt_ssr == 0, "Non-SSRI", trt_ssr)))

f <- function(y) c(label=length(y), y=median(y))

#Option1
long_data <-long_data %>%
  pivot_longer(cols = starts_with("GAFscoreT"), 
               names_to = "time_point", 
               values_to = "GAF_score")%>%
  filter(!is.na(GAF_score))

long_data$GAF_score=as.numeric(long_data$GAF_score)
my_comparisons <- list( c("GAFscoreT0", "GAFscoreT1"), c("GAFscoreT0", "GAFscoreT2"), c("GAFscoreT0", "GAFscoreT3") )

ggplot(long_data, aes(x = time_point, y = GAF_score, fill = time_point))+geom_boxplot(varwidth = T,alpha=.2)+
  labs(title = "Distribution of GAF Scores Over Time",x = "Time Point",y = "GAF Score") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox.test")+ # Add pairwise comparisons p-value #wilconxon= Mann-Whitney U test
  stat_compare_means(method = "anova",label.y = max(long_data$GAF_score)+3)  +   # Add global p-value
  stat_summary(fun.data=f, geom="text", vjust=-0.5, col="black")+
  theme_minimal() +scale_x_discrete(labels=c('T0', 'T1', 'T2','T3'))+
  facet_wrap(.~trt_ssr, scales = "free")

#Option2
long_data <-long_data %>%
  pivot_longer(cols = starts_with("cgis_adm_scoreT"), 
               names_to = "time_point", 
               values_to = "CGIS_adm_score")%>%
  filter(!is.na(CGIS_adm_score))

long_data$CGIS_adm_score=as.numeric(long_data$CGIS_adm_score)
my_comparisons <- list( c("cgis_adm_scoreT0", "cgis_adm_scoreT1"), c("cgis_adm_scoreT0", "cgis_adm_scoreT2"), c("cgis_adm_scoreT0", "cgis_adm_scoreT3") )

ggplot(long_data, aes(x = time_point, y = CGIS_adm_score, fill = time_point))+geom_boxplot(varwidth = T,alpha=.2)+
  labs(title = "Distribution of CGIS Scores At Admission Over Time",x = "Time Point",y = "CGIS Scores At Admission") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox.test")+ # Add pairwise comparisons p-value #wilconxon= Mann-Whitney U test
  stat_compare_means(method = "anova",label.y = max(long_data$CGIS_adm_score)+3)  +   # Add global p-value
  stat_summary(fun.data=f, geom="text", vjust=-0.5, col="black")+
  theme_minimal() +scale_x_discrete(labels=c('T0', 'T1', 'T2','T3'))+
  facet_wrap(.~trt_ssr, scales = "free")

#Option3
long_data <-long_data %>%
  pivot_longer(cols = starts_with("cgis_dis_scoreT"), 
               names_to = "time_point", 
               values_to = "CGIS_dis_score")%>%
  filter(!is.na(CGIS_dis_score))

long_data$CGIS_dis_score=as.numeric(long_data$CGIS_dis_score)
my_comparisons <- list( c("cgis_dis_scoreT0", "cgis_dis_scoreT1"), c("cgis_dis_scoreT0", "cgis_dis_scoreT2"), c("cgis_dis_scoreT0", "cgis_dis_scoreT3") )

ggplot(long_data, aes(x = time_point, y = CGIS_dis_score, fill = time_point))+geom_boxplot(varwidth = T,alpha=.2)+
  labs(title = "Distribution of CGIS Scores At Discharge Over Time",x = "Time Point",y = "CGIS Scores At Discharge") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox.test")+ # Add pairwise comparisons p-value #wilconxon= Mann-Whitney U test
  stat_compare_means(method = "anova",label.y = max(long_data$CGIS_dis_score)+3)  +   # Add global p-value
  stat_summary(fun.data=f, geom="text", vjust=-0.5, col="black")+
  theme_minimal() +scale_x_discrete(labels=c('T0', 'T1', 'T2','T3'))+
  facet_wrap(.~trt_ssr, scales = "free")

#Option4
long_data <-long_data %>%#descriptive_3
  pivot_longer(cols = starts_with("treatment_totalnumberT"), 
               names_to = "time_point", 
               values_to = "treatment_total_number")%>%
  filter(!is.na(treatment_total_number))

long_data$treatment_total_number=as.numeric(long_data$treatment_total_number)
my_comparisons <- list( c("treatment_totalnumberT0", "treatment_totalnumberT1"), c("treatment_totalnumberT0", "treatment_totalnumberT2"), c("treatment_totalnumberT0", "treatment_totalnumberT3") )

library(scales)
ggplot(long_data, aes(x = time_point, y = treatment_total_number, fill = time_point))+geom_boxplot(varwidth = T,alpha=.2)+
  labs(title = "Distribution of Number of Treatments Over Time",x = "Time Point",y = "Number of Treatments") +
  stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red")+
  stat_compare_means(comparisons = my_comparisons,method = "wilcox.test")+ # Add pairwise comparisons p-value #wilconxon= Mann-Whitney U test
  stat_compare_means(method = "anova",label.y = max(long_data$treatment_total_number)+3)  +   # Add global p-value
  stat_summary(fun.data=f, geom="text", vjust=-0.5, col="black")+
  theme_minimal()+scale_x_discrete(labels=c('T0', 'T1', 'T2','T3'))+
  facet_wrap(.~trt_ssr, scales = "free")





