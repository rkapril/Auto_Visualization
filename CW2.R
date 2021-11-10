library(tidyverse)

getwd()

setwd("C:\\Users\\User\\OneDrive\\Desktop\\Courses\\RP\\SDBA\\Post-Diploma Certificate in Advanced Business Analytics\\PAML\\C3439C_Coursework-2")

getwd()

df <- read_csv("CW2_Telco_Churn_Data.csv")

df_new <- df %>% mutate_if(sapply(., is.character),
            as.factor)

summary(df_new)
# CustomerID        Gender        SeniorCitizen   
# 0002-ORFBO:   1   Female:3485   Min.   :0.0000  
# 0003-MKNFE:   1   Male  :3558   1st Qu.:0.0000  
# 0004-TLHLJ:   1                 Median :0.0000  
# 0011-IGKFF:   1                 Mean   :0.1621  
# 0013-EXCHZ:   1                 3rd Qu.:0.0000  
# 0013-MHZWF:   1                 Max.   :1.0000  
# (Other)   :7037

# Partner    Dependents     Tenure       PhoneService
# No :3641   No :4933   Min.   :  0.00   No : 682    
# Yes:3402   Yes:2110   1st Qu.:  9.00   Yes:6361    
# Median : 29.00               
# Mean   : 32.41               
# 3rd Qu.: 55.00               
# Max.   :172.00               
# 
# MultipleLines     InternetService
# No              :3390   DSL        :2421  
# No phone service: 682   Fiber optic:3096  
# Yes             :2971   No         :1526  
# 
# OnlineSecurity              OnlineBackup 
# No                 :3498   No                 :3088  
# No internet service:1526   No internet service:1526  
# Yes                :2019   Yes                :2429  
# 
# DeviceProtection              TechSupport  
# No                 :3095    No                 :3473  
# No internet service:1526    No internet service:1526  
# Yes                :2422    Yes                :2044  
# 
# StreamingTV              StreamingMovies
# No                 :2810   No                 :2785  
# No internet service:1526   No internet service:1526  
# Yes                :2707   Yes                :2732  
# 
# Contract    PaperlessBilling
# Month-to-month:3875   No :2872        
# One year      :1473   Yes:4171        
# Two year      :1695                   
# 
# PaymentMethod  MonthlyCharges  
# Bank transfer (automatic):1544   Min.   : 18.25  
# Credit card (automatic)  :1522   1st Qu.: 35.50  
# Electronic check         :2365   Median : 70.35  
# Mailed check             :1612   Mean   : 64.77  
# 3rd Qu.: 89.85  
# Max.   :118.75  
# NA's   :6       
#   TotalCharges      Churn     
#  Min.   :     0.5   No :5174  
#  1st Qu.:   402.2   Yes:1869  
#  Median :  1397.5             
#  Mean   :  2361.9             
#  3rd Qu.:  3786.6             
#  Max.   :568805.0 

df_new <- df_new %>% mutate(Senior_Citizen = case_when(
  SeniorCitizen == 1 ~ "Yes",
  SeniorCitizen == 0 ~ "No"
)) %>% relocate(Senior_Citizen, .after = SeniorCitizen) %>% 
  select(-SeniorCitizen) %>% 
  mutate_if(sapply(., is.character), as.factor)
                          
summary(df_new)                          
# CustomerID      Gender     Senior_Citizen Partner   
# 0002-ORFBO:   1   Female:3485   No :5901       No :3641  
# 0003-MKNFE:   1   Male  :3558   Yes:1142       Yes:3402  
# 0004-TLHLJ:   1                                          
# 0011-IGKFF:   1                                          
# 0013-EXCHZ:   1                                          
# 0013-MHZWF:   1                                          
# (Other)   :7037

# Dependents     Tenure       PhoneService          MultipleLines 
# No :4933   Min.   :  0.00   No : 682     No              :3390  
# Yes:2110   1st Qu.:  9.00   Yes:6361     No phone service: 682  
# Median : 29.00                Yes             :2971  
# Mean   : 32.41                                       
# 3rd Qu.: 55.00                                       
# Max.   :172.00                                       
# 
# InternetService             OnlineSecurity
# DSL        :2421   No                 :3498  
# Fiber optic:3096   No internet service:1526  
# No         :1526   Yes                :2019  
# 
# OnlineBackup             DeviceProtection
# No                 :3088   No                 :3095   
# No internet service:1526   No internet service:1526   
# Yes                :2429   Yes                :2422   
# 
# TechSupport                StreamingTV  
# No                 :3473   No                 :2810  
# No internet service:1526   No internet service:1526  
# Yes                :2044   Yes                :2707  
# 
# StreamingMovies           Contract   
# No                 :2785   Month-to-month:3875  
# No internet service:1526   One year      :1473  
# Yes                :2732   Two year      :1695  
# 
# PaperlessBilling                   PaymentMethod 
# No :2872         Bank transfer (automatic):1544  
# Yes:4171         Credit card (automatic)  :1522  
# Electronic check         :2365  
# Mailed check             :1612  
# 
# MonthlyCharges    TotalCharges      Churn     
# Min.   : 18.25   Min.   :     0.5   No :5174  
# 1st Qu.: 35.50   1st Qu.:   402.2   Yes:1869  
# Median : 70.35   Median :  1397.5             
# Mean   : 64.77   Mean   :  2361.9             
# 3rd Qu.: 89.85   3rd Qu.:  3786.6             
# Max.   :118.75   Max.   :568805.0             
# NA's   :6                                           
                          
library(moments)

skewness(df_new$Tenure)
# [1] 0.304915

kurtosis(df_new$Tenure)
# [1] 2.001854

levels(factor(df_new$Tenure))
# [1] "0"   "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"  
# [11] "10"  "11"  "12"  "13"  "14"  "15"  "16"  "17"  "18"  "19" 
# [21] "20"  "21"  "22"  "23"  "24"  "25"  "26"  "27"  "28"  "29" 
# [31] "30"  "31"  "32"  "33"  "34"  "35"  "36"  "37"  "38"  "39" 
# [41] "40"  "41"  "42"  "43"  "44"  "45"  "46"  "47"  "48"  "49" 
# [51] "50"  "51"  "52"  "53"  "54"  "55"  "56"  "57"  "58"  "59" 
# [61] "60"  "61"  "62"  "63"  "64"  "65"  "66"  "67"  "68"  "69" 
# [71] "70"  "71"  "72"  "172" 

df_new[df_new$Tenure==172,]
# A tibble: 3 x 21
# CustomerID Gender Senior_Citizen Partner Dependents Tenure
# <fct>      <fct>  <fct>          <fct>   <fct>       <dbl>
#   1 4900-MSOMT Female No             Yes     Yes           172
# 2 5304-EFJLP Male   Yes            Yes     No            172
# 3 0401-WDBXM Male   No             Yes     Yes           172

df_new <- df_new %>% mutate(Tenure1 = case_when(
  Tenure == 172 ~ 29,
  TRUE ~ Tenure)) %>% 
  relocate(Tenure1, .after = Tenure) %>% 
  select(-Tenure) %>% 
  rename(Tenure1, Tenure = Tenure1)

levels(factor(df_new$Tenure))
# [1] "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11"
# [13] "12" "13" "14" "15" "16" "17" "18" "19" "20" "21" "22" "23"
# [25] "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35"
# [37] "36" "37" "38" "39" "40" "41" "42" "43" "44" "45" "46" "47"
# [49] "48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59"
# [61] "60" "61" "62" "63" "64" "65" "66" "67" "68" "69" "70" "71"
# [73] "72"

skewness(df_new$Tenure)
# [1] 0.2403298

kurtosis(df_new$Tenure)
# [1] 1.61414

skewness(df_new$MonthlyCharges, na.rm = TRUE)
# [1] -0.2214398

kurtosis(df_new$MonthlyCharges, na.rm = TRUE)
# [1] 1.743058

df_new[is.na(df_new$MonthlyCharges),]

df_new <- df_new %>% mutate(Monthly_Charges = case_when(
  is.na(MonthlyCharges) ~ 70.35,
  TRUE ~ MonthlyCharges)) %>% 
  relocate(Monthly_Charges, .after = MonthlyCharges)

df_new[is.na(df_new$Monthly_Charges),]
# A tibble: 0 x 21

summary(df_new)
# CustomerID        Gender        Senior_Citizen Partner   
# 0002-ORFBO:   1   Female:3485   No :5901       No :3641  
# 0003-MKNFE:   1   Male  :3558   Yes:1142       Yes:3402  
# 0004-TLHLJ:   1                                          
# 0011-IGKFF:   1                                          
# 0013-EXCHZ:   1                                          
# 0013-MHZWF:   1                                          
# (Other)   :7037  

# Dependents    Tenure      PhoneService          MultipleLines 
# No :4933   Min.   : 0.00   No : 682     No              :3390  
# Yes:2110   1st Qu.: 9.00   Yes:6361     No phone service: 682  
# Median :29.00                Yes             :2971  
# Mean   :32.35                                       
# 3rd Qu.:55.00                                       
# Max.   :72.00                                       
# 
# InternetService             OnlineSecurity
# DSL        :2421   No                 :3498  
# Fiber optic:3096   No internet service:1526  
# No         :1526   Yes                :2019  
# 
# OnlineBackup             DeviceProtection
# No                 :3088   No                 :3095   
# No internet service:1526   No internet service:1526   
# Yes                :2429   Yes                :2422   
# 
# TechSupport                StreamingTV  
# No                 :3473   No                 :2810  
# No internet service:1526   No internet service:1526  
# Yes                :2044   Yes                :2707  
# 
# StreamingMovies           Contract   
# No                 :2785   Month-to-month:3875  
# No internet service:1526   One year      :1473  
# Yes                :2732   Two year      :1695  
# 
# PaperlessBilling                   PaymentMethod 
# No :2872         Bank transfer (automatic):1544  
# Yes:4171         Credit card (automatic)  :1522  
# Electronic check         :2365  
# Mailed check             :1612  
# 
# MonthlyCharges   Monthly_Charges   TotalCharges      Churn     
# Min.   : 18.25   Min.   : 18.25   Min.   :     0.5   No :5174  
# 1st Qu.: 35.50   1st Qu.: 35.55   1st Qu.:   402.2   Yes:1869  
# Median : 70.35   Median : 70.35   Median :  1397.5             
# Mean   : 64.77   Mean   : 64.77   Mean   :  2361.9             
# 3rd Qu.: 89.85   3rd Qu.: 89.85   3rd Qu.:  3786.6             
# Max.   :118.75   Max.   :118.75   Max.   :568805.0             
# NA's   :6     

df_new <- df_new %>% select(-MonthlyCharges)

skewness(df_new$Monthly_Charges)
# [1] -0.2219931

kurtosis(df_new$Monthly_Charges)
# [1] 1.744583

skewness(df_new$TotalCharges)
# [1] 71.52601

kurtosis(df_new$TotalCharges)
# [1] 5688.309

library(DataExplorer)
introduce(df_new)
plot_intro(df_new)
plot_missing(df_new)
plot_bar(df_new)
plot_bar(df_new, by = "Churn")
plot_histogram(df_new)
plot_correlation(df_new)
plot_boxplot(df_new, by = "Churn")
create_report(df_new, y = "Churn", 
              output_file = "R report1.html")

t(df_new[df_new$TotalCharges==568805,])

df_new <- df_new %>% mutate(Total_Charges = case_when(
  TotalCharges == 568805 ~ 1397.5,
  TRUE ~ TotalCharges)) %>% 
  relocate(Total_Charges, .after = TotalCharges) %>% 
  select(-TotalCharges)

library(DataExplorer)
plot_histogram(df_new)
plot_correlation(df_new)
plot_boxplot(df_new, by = "Churn")
create_report(df_new, y = "Churn", 
              output_file = "R report2.html")

skewness(df_new$Total_Charges)
# [1] 0.9643296

kurtosis(df_new$Total_Charges)
# [1] 2.774914

write.csv(df_new,"CW2_Telco_Churn_Data_cleaned.csv", 
          row.names = FALSE)

library(explore)
explore(df_new)
