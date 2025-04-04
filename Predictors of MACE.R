##### HELPFUL CITATION ###

https://bookdown.org/wadetroberts/r-you-ready-for-r/multiple-logistic-regression.html

library(haven) # for importing SAS data
library(gmodels) # for CrossTable crosstab 
library(psych) # for describe & descirbeBy
library(summarytools) # for freq
library(tidyverse)
library(ggplot2)

data <- read_sas('filepath/filename.sas7bdat') 

# *identify outcomes
freq(data$Age)
# set age age value of '90+' to 90, 'NULL' to missing and convert to numeric
data_ <- data %>%
  mutate(Age = case_when(
    Age == '90+' ~ 90,         # If Age is '90', set it to 90
    Age == 'NULL' ~ NA_real_, # If Age is 'NULL' (string), set it to NA
    TRUE ~ as.numeric(Age)    # Otherwise, convert Age to numeric (assuming it’s a string)
  ))
freq(data_$Age)
describe(data_$Age)
quantiles <- quantile(data_$Age, c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)
print(quantiles)
qqnorm(data_$Age)
qqline(data_$Age, col = "red")
ggplot(data_, aes(Age)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()

# create groupings for age
data_a <- data_ %>% mutate(
  age_cat = case_when(
    Age >= 0 & Age <= 39 ~ 0,
    Age >= 40 & Age <= 58 ~ 1,
    Age >= 59 & Age <= 73 ~ 2,
    Age >= 74 ~ 3))
data_a$age_cat <- factor(data_a$age_cat, 
                         levels = c(0, 1, 2, 3), 
                         labels = c("< 40", "40 - 58", "59 - 73", "> 73"))
freq(data_a$age_cat)

# create groupings for sex 
freq(data_a$SEX)
data_b <- data_a %>% mutate(
  sex_ = case_when(
    SEX=='female' ~ 0,
    SEX=='male' ~ 1,
    SEX=='non-bi' ~ 2,
    SEX=='NULL' ~ NA_real_))
data_b$sex_ <- factor(data_b$sex_, 
                         levels = c(0, 1, 2), 
                         labels = c("Female", "Male", "Non-binary"))
freq(data_b$sex_)

# create groupings for race
freq(data_b$RACE_NEW)
data_c <- data_b %>% mutate(race = case_when(
  RACE_NEW=='American Indian or Alaska Native' ~ 1,
  RACE_NEW=='Asian' ~ 2,
  RACE_NEW=='Black or African American' ~ 3,
  (RACE_NEW=='Native Hawaiian or Other Pacific Islander' | RACE_NEW=='Native Hawaiian or Pacific Islander') ~ 4,
  #grepl("Native Hawaiian", RACE_NEW) ~ 4,     # grepl("Native Hawaiian", RACE_NEW) returns TRUE where the text "Native Hawaiian" is found in the RACE_NEW column
  RACE_NEW=='Some Other Race' ~ 5,
  (RACE_NEW=='Unknown/Not Reported' | RACE_NEW=='Unknown') ~ 6,
  (RACE_NEW=='Black or African American,American Indian or Alask' | RACE_NEW=='Black or African American,American Indian or Alaska Native' | 
     RACE_NEW=='Native Hawaiian or Other Pacific Islander,Asian' | RACE_NEW=='Race combinations with low frequency' |
     RACE_NEW=='White,American Indian or Alaska Native' | RACE_NEW=='White,Asian' | RACE_NEW=='White,Black or African American' |
     RACE_NEW=='White,Native Hawaiian or Other Pacific Islander' | RACE_NEW=='White,Native Hawaiian or Other Pacific Islander,As' |
     RACE_NEW=='White,Some Other Race' | RACE_NEW=='White,Unknown/Not Reported' | RACE_NEW=='Asian,Some Other Race') ~ 7,
  RACE_NEW=='White' ~ 0))

data_d <- data_c %>% mutate(race_cat = case_when(
  race==0 ~ 0,
  race==1 ~ 3,
  race==2 ~ 1,
  race==3 ~ 2,
  race >= 4 ~ 3))

data_e <- data_d %>% mutate(binary_race = if_else(race_cat==0, 0, 1))
data_e$race_cat <- factor(data_e$race_cat, 
                      levels = c(0, 1, 2, 3), 
                      labels = c("white", "Asian", "Black or African American", "Other"))
data_e$binary_race <- factor(data_e$binary_race, 
                          levels = c(0, 1), 
                          labels = c("white", "Minority Race"))
freq(data_e$race)
freq(data_e$race_cat)
freq(data_e$binary_race)

# create ethnicity grouping
freq(data_e$ETHNICITY_HISPANIC)
data_f <- data_e %>% mutate(ethnicity = case_when(
  (ETHNICITY_HISPANIC=='Y' | ETHNICITY_HISPANIC=='Yes') ~ 1,
  (ETHNICITY_HISPANIC=='N' | ETHNICITY_HISPANIC=='No') ~ 0,
  (ETHNICITY_HISPANIC=='U' | ETHNICITY_HISPANIC=='Unknown') ~ 2))
data_f$ethnicity <- factor(data_f$ethnicity, 
                             levels = c(0, 1, 2), 
                             labels = c("Not Hispanic", "Hispanic", "Unkown/Not Reported"))
freq(data_f$ethnicity)

#create BMI variable
data_g <- data_f %>% mutate(HEIGHT = ifelse(HEIGHT == -99, NA_real_, HEIGHT))
freq(data_g$HEIGHT)
data_g <- data_g %>% mutate(WEIGHT = ifelse(WEIGHT == -99, NA_real_, WEIGHT))
freq(data_g$WEIGHT)
data_g <- data_g %>% mutate(height_sq = HEIGHT*HEIGHT)
freq(data_g$height_sq)
data_g <- data_g %>% mutate(BMI = (WEIGHT/height_sq)*703)
freq(data_g$BMI)
describe(data_g$BMI)
quantiles <- quantile(data_g$BMI, c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99), na.rm = TRUE)
print(quantiles)
qqnorm(data_g$BMI)
qqline(data_g$BMI, col = "red")
ggplot(data_g, aes(BMI)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()

#create obesity variable
data_g <- data_g %>% mutate(obesity = case_when(
                              (BMI >= 0 & BMI < 30) ~ 0,
                              BMI >= 30 ~ 1,
                              is.na(BMI) ~ NA_real_))
freq(data_g$obesity)
data_g <- data_g %>% mutate(obesity_cat = case_when(
  (BMI >= 0 & BMI < 20) ~ 1,
  (BMI >= 20 & BMI < 30) ~ 0,
  (BMI >=30 & BMI < 40) ~ 2,
  BMI >= 40 ~ 3,
  is.na(BMI) ~ NA_real_))
freq(data_g$obesity_cat)

#create diabetes groupings
freq(data_g$DIABETES)
data_h <- data_g %>% mutate(diabetes_ = case_when(
  (DIABETES == 'INSULIN' | DIABETES=='NON-INSULIN') ~ 1,
  DIABETES=='NO' ~ 0,
  is.na(DIABETES) ~ NA_real_))
freq(data_h$diabetes_)

#create functional health groupings
freq(data_h$FNSTATUS2)
data_i <- data_h %>% mutate(fnstat = case_when(
  (FNSTATUS2 == 'Partially Dependent' | FNSTATUS2=='Totally Dependent') ~ 1,
  FNSTATUS2=='Independent' ~ 0,
  (FNSTATUS2=='Unknown' | FNSTATUS2=='NULL') ~ 2))
freq(data_i$fnstat)

#create smoking var
freq(data_i$SMOKE)
data_i <- data_i %>% mutate(smoker = case_when(
  SMOKE == 'Yes' ~ 1,
  SMOKE=='NULL' ~ NA_real_,
  SMOKE=='No' ~ 0))
freq(data_i$smoker)

#create copd
freq(data_i$HXCOPD)
data_i <- data_i %>% mutate(copd = case_when(
  HXCOPD == 'Yes' ~ 1,
  HXCOPD=='No' ~ 0))
freq(data_i$copd)

#create ascites_
freq(data_i$ASCITES)
data_i <- data_i %>% mutate(ascites_ = case_when(
  ASCITES == 'Yes' ~ 1,
  ASCITES=='No' ~ 0))
freq(data_i$ascites_)

#create chf
freq(data_i$HXCHF)
data_i <- data_i %>% mutate(chf = case_when(
  HXCHF == 'Yes' ~ 1,
  HXCHF=='No' ~ 0))
freq(data_i$chf)

#create hypermed_
data_i <- data_i %>% mutate(hypermed_ = case_when(
  HYPERMED == 'Yes' ~ 1,
  HYPERMED=='No' ~ 0))
freq(data_i$hypermed_)

#create dialysis
data_i <- data_i %>% mutate(dialysis_ = case_when(
  DIALYSIS == 'Yes' ~ 1,
  DIALYSIS=='No' ~ 0))
freq(data_i$dialysis_)

#create cancer
data_i <- data_i %>% mutate(cancer = case_when(
  DISCANCR == 'Yes' ~ 1,
  DISCANCR=='No' ~ 0))
freq(data_i$cancer)

#create steroid
data_i <- data_i %>% mutate(steroid_ = case_when(
  STEROID == 'Yes' ~ 1,
  STEROID=='No' ~ 0))
freq(data_i$steroid_)

#create bleed
data_i <- data_i %>% mutate(bleed = case_when(
  BLEEDDIS == 'Yes' ~ 1,
  BLEEDDIS=='No' ~ 0))
freq(data_i$bleed)

#create transfus_
data_i <- data_i %>% mutate(transfus_ = case_when(
  TRANSFUS == 'Yes' ~ 1,
  TRANSFUS=='No' ~ 0))
freq(data_i$transfus_)

#create sepsis
freq(data_i$PRSEPIS)
data_i <- data_i %>% mutate(sepsis = case_when(
  (PRSEPIS == 'Sepsis' | PRSEPIS=='Septic Shock' | PRSEPIS=='SIRS') ~ 1,
  PRSEPIS=='None' ~ 0,
  PRSEPIS=='NULL' ~ NA_real_))
freq(data_i$sepsis)

#create ten_pct_wtloss
data_i <- data_i %>% mutate(ten_pct_wtloss = case_when(
  WTLOSS == 'Yes' ~ 1,
  WTLOSS=='No' ~ 0))
freq(data_i$ten_pct_wtloss)

###### adjust -99 values for biomakers to missing ####
# PRSODM
data_j <- data_i %>%
  mutate(PRSODM = ifelse(PRSODM == -99, NA_real_, PRSODM))

# PRBUN
data_k <- data_j %>%
  mutate(PRBUN = ifelse(PRBUN == -99, NA_real_, PRBUN))

# PRCREAT
data_l <- data_k %>%
  mutate(PRCREAT = ifelse(PRCREAT == -99, NA_real_, PRCREAT))

# PRALBUM
data_m <- data_l %>%
  mutate(PRALBUM = ifelse(PRALBUM == -99, NA_real_, PRALBUM))

# PRBILI
data_n <- data_m %>%
  mutate(PRBILI = ifelse(PRBILI == -99, NA_real_, PRBILI))

# PRSGOT
data_o <- data_n %>%
  mutate(PRSGOT = ifelse(PRSGOT == -99, NA_real_, PRSGOT))

# PRALKPH
data_p <- data_o %>%
  mutate(PRALKPH = ifelse(PRALKPH == -99, NA_real_, PRALKPH))

# PRWBC 
data_q <- data_p %>%
  mutate(PRWBC = ifelse(PRWBC == -99, NA_real_, PRWBC))

# PRHCT
data_r <- data_q %>%
  mutate(PRHCT = ifelse(PRHCT == -99, NA_real_, PRHCT))

# PRPLATE
data_s <- data_r %>%
  mutate(PRPLATE = ifelse(PRPLATE == -99, NA_real_, PRPLATE))

# PRINR
data_t <- data_s %>%
  mutate(PRINR = ifelse(PRINR == -99, NA_real_, PRINR))

# PRPTT
data_u <- data_t %>%
  mutate(PRPTT = ifelse(PRPTT == -99, NA_real_, PRPTT))

# drop datasets created in data management process
rm("data_", "data_a", "data_b", "data_c", "data_d", "data_e", "data_f", "data_g", "data_h", "data_i", "data_j", "data_k", "data_l", "data_m", "data_n", "data_o",
   "data_p", "data_q", "data_r", "data_s", "data_t")
# at this point you should only have the original import (data) and the most recent dataset (data_u)

##### import procedure category dataset to limit to only procedures of interest #####
library(readxl)
procedure_cat <- read_excel("filepath/filename.xlsx")

## inner join current dataset to procedure category dataset to have proc_category 

#re-code CPT to a numerical column to join
data_u$CPT <- as.numeric(data_u$CPT)
#drop unneccesary columns
procedure_cat <- subset(procedure_cat, select = c("proc_category", "CPT", "laparoscopic"))
#drop laparoscopic column in data_u
data_u <- subset(data_u, select = -laparoscopic)

# Perform an inner join
mace_cat <- inner_join(data_u, procedure_cat, by='CPT')
freq(mace_cat$OperYR)

mace_cat <- mace_cat %>% filter(OperYR < 2022)
freq(mace_cat$OperYR)
#2016 - 2020 = 583500
#2021 = 106264

# create binary age_category 
mace_cat <- mace_cat %>% mutate(age_binary = case_when(
  (Age >= 0 & Age <= 59) ~ 0,
  Age >= 60 ~ 1))

# create male binary sex
freq(mace_cat$sex_)
# we need to use labels below because labels were applied to final dataset
mace_cat <- mace_cat %>% mutate(male = case_when(
  sex_=='Male' ~ 1,
  sex_=='Female' ~ 0,
  sex_=='Non-binary' ~ 0))
mace_cat$male <- factor(mace_cat$male, 
                             levels = c(0, 1), 
                             labels = c("Famale or Non-binary", "Male"))
freq(mace_cat$male)
freq(mace_cat$binary_race)
freq(mace_cat$ethnicity)

#create hispanic indicator
mace_cat <- mace_cat %>% mutate(hispanic = case_when(
  ethnicity=='Hispanic' ~ 1,
  ethnicity=='Not Hispanic' ~ 0,
  ethnicity=='Unknown/Not Reported' ~ 0))
mace_cat$hispanic <- factor(data_u$hispanic, 
                      levels = c(0, 1), 
                      labels = c("Not Hispanic or Unknown/Not Reported", "Hispanic"))
freq(mace_cat$hispanic)

##### DEFINE CLINICAL BINARY CATEGORIES #####
# Sodium #
mace_cat <- mace_cat %>% mutate(sodium_low = case_when(
  (PRSODM >= 0 & PRSODM < 135) ~ 1,
  PRSODM >= 135 ~ 0))
freq(mace_cat$sodium_low)

# Blood Urea Nitrogen #
mace_cat <- mace_cat %>% mutate(BUN_high = case_when(
  (PRBUN >= 0 & PRBUN < 25) ~ 0,
  PRBUN >= 25 ~ 1))
freq(mace_cat$BUN_high)

# Serum Creatinine #
mace_cat <- mace_cat %>% mutate(creat_high = case_when(
  (PRCREAT >= 0 & PRCREAT < 1.2) ~ 0,
  PRCREAT >= 1.2 ~ 1))
freq(mace_cat$creat_high)

# Albumin #
mace_cat <- mace_cat %>% mutate(albumin_high = case_when(
  (PRALBUM >= 0 & PRALBUM < 3.5) ~ 0,
  PRALBUM >= 3.5 ~ 1))
freq(mace_cat$albumin_high)

# Total Bilirubin #
mace_cat <- mace_cat %>% mutate(bili_high = case_when(
  (PRBILI >= 0 & PRBILI < 1.0) ~ 0,
  PRBILI >= 1.0 ~ 1))
freq(mace_cat$bili_high)

# Aspartate Transaminase / Serum Glutamic-Oxaloacetic Transsaminase #
mace_cat <- mace_cat %>% mutate(SGOT_high = case_when(
  (PRSGOT >= 0 & PRSGOT < 40) ~ 0,
  PRSGOT >= 40 ~ 1))
freq(mace_cat$SGOT_high)

# Alkaline Phosphatase #
mace_cat <- mace_cat %>% mutate(alk_high = case_when(
  (PRALKPH >= 0 & PRALKPH < 148) ~ 0,
  PRALKPH >= 148 ~ 1))
freq(mace_cat$alk_high)

# White Blood Cell Count #
mace_cat <- mace_cat %>% mutate(wbc_cat = case_when(
  (PRWBC >= 0 & PRWBC < 4) ~ 1,
  (PRWBC >= 4 & PRWBC <= 11) ~ 0,
  PRWBC > 11 ~ 2))
freq(mace_cat$wbc_cat)

# Hematocrit #
mace_cat <- mace_cat %>% mutate(hema_high = case_when(
  (PRHCT >= 0 & PRHCT < 30) ~ 0,
  PRHCT >= 30 ~ 1))
freq(mace_cat$hema_high)

# Platelets #
mace_cat <- mace_cat %>% mutate(plate_high = case_when(
  (PRPLATE >= 0 & PRPLATE < 150) ~ 0,
  PRPLATE >= 150 ~ 1))
freq(mace_cat$plate_high)

# International Normalized Ratio #
mace_cat <- mace_cat %>% mutate(INR_high = case_when(
  (PRINR >= 0 & PRINR < 1.5) ~ 0,
  PRINR >= 1.5 ~ 1))
freq(mace_cat$INR_high)

# Partial Thromboplastin Time #
mace_cat <- mace_cat %>% mutate(thrombo_high = case_when(
  (PRPTT >= 0 & PRPTT < 35) ~ 0,
  PRPTT >= 35 ~ 1))
freq(mace_cat$thrombo_high)

# DEFINE FNSTAT FINAL CATEGORY #
freq(mace_cat$fnstat)
mace_cat <- mace_cat %>% mutate(fnstat_final = case_when(
  fnstat ==1 ~ 1,
  fnstat ==0 ~ 0,
  fnstat ==2 ~ NA_real_))
mace_cat <- mace_cat %>%
  mutate(laparoscopic = ifelse(is.na(laparoscopic), 0, laparoscopic))
freq(mace_cat$fnstat_final)
freq(mace_cat$MACE_b)
freq(mace_cat$laparoscopic)
freq(mace_cat$OperYR)

## r crashed, so I am importing dataset that is trimmed down to make the process easier #
mace_a_ <- read_sas('filepath/filename.sas7bdat') 
freq(mace_a_$MACE_b)
freq(mace_a_$laparoscopic)
freq(mace_a_$OperYR)

# define low BMI
mace_a_ <- mace_a_ %>% mutate(low_BMI_ = case_when(
  obesity_cat ==1 ~ 1,
  (obesity_cat ==0 | obesity_cat > 1) ~ 0))

# define high risk procedure
mace_a_ <- mace_a_ %>% mutate(high_risk_proc = case_when(
  proc_category %in% c('BILI', 'COLO', 'GAST', 'NEPH', 'REC', 'XLAP', 'SB') ~ 1,
  TRUE ~ 0
))

# create training & test data
mace_train <- mace_a_ %>% filter(OperYR != 2021)
mace_test <- mace_a_ %>% filter(OperYR == 2021)

# to produce descriptive stats & chi-square rests
CrossTable(mace_train$male, mace_train$MACE_b, expected=TRUE)

# create training model 
mace_train$laparoscopic <- factor(mace_train$laparoscopic, levels = c(0, 1), labels = c("Open Surgery", "Laparoscopic"))
mace_train$laparoscopic <- relevel(mace_train$laparoscopic, ref = 'Laparoscopic')
mace_train$albumin_high <- factor(mace_train$albumin_high, levels = c(0, 1), labels = c("Low Albumin", "Normal Albumin"))
mace_train$albumin_high <- relevel(mace_train$albumin_high, ref = 'Normal Albumin')
mace_train$plate_high <- factor(mace_train$plate_high, levels = c(0, 1), labels = c("Low Plate", "Normal Plate"))
mace_train$plate_high <- relevel(mace_train$plate_high, ref = 'Normal Plate')
train_mod <- glm(MACE_b ~ age_binary + male + low_BMI_ + diabetes_ + smoker + copd + chf + hypermed_ + dialysis_ +
                   fnstat_final + bleed + ventilator + sepsis + ascites_ + laparoscopic + high_risk_proc + sodium_low + 
                   creat_high + albumin_high + alk_high + plate_high, data = mace_train, family = "binomial")
summary(train_mod)
exp(cbind(OR = coef(train_mod), confint(train_mod)))

# generate AUC & ROC Curve for training model 
library(pROC)

# Create a cleaned dataset by removing rows with NAs in the selected columns
# Check for missing values in each of the relevant columns
sapply(mace_train[, c("MACE_b", "age_binary", "male", "low_BMI_", "diabetes_", "smoker", "copd", "chf", 
                      "hypermed_", "dialysis_", "fnstat_final", "bleed", "ventilator", "sepsis", 
                      "ascites_", "laparoscopic", "high_risk_proc", "sodium_low", "creat_high", 
                      "albumin_high", "alk_high", "plate_high")], function(x) sum(is.na(x)))

train_clean <- mace_train[complete.cases(mace_train[, c("MACE_b", "age_binary", "male", "low_BMI_", "diabetes_", "smoker",
                                                        "copd", "chf", "hypermed_", "dialysis_", "fnstat_final", "bleed", 
                                                        "ventilator", "sepsis", "ascites_", "laparoscopic", "high_risk_proc",
                                                        "sodium_low", "creat_high", "albumin_high", "alk_high", "plate_high")]), ]
predicted_probs <- predict(train_mod, type = "response")
roc_curve <- roc(train_clean$MACE_b, predicted_probs)

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")

# AUC with 95% Confidence Interval
auc_value <- auc(roc_curve)
ci_value <- ci.auc(roc_curve)

# Print the AUC and its 95% CI
print(paste("AUC: ", auc_value))
print(paste("95% CI: ", ci_value))

# generate VIF for model
library(car)
vif(train_mod)
#all values < 2

# create training model risk score 
# assign risk score based on aOR in train_model, then sum risk scores together to get risk_score variable for each patient 
train_clean_score <- train_clean %>% mutate(male_score = ifelse(male==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(age_score = ifelse(age_binary==1, 2, 0))
train_clean_score <- train_clean_score %>% mutate(low_bmi_score = ifelse(low_BMI_==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(diabetes_score = ifelse(diabetes_==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(smoker_score = ifelse(smoker==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(copd_score = ifelse(copd==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(chf_score = ifelse(chf==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(hypermed_score = ifelse(hypermed_==1, 2, 0))
train_clean_score <- train_clean_score %>% mutate(dialysis_score = ifelse(dialysis_==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(fnstat_score = ifelse(fnstat_final==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(bleed_score = ifelse(bleed==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(ventilator_score = ifelse(ventilator==1, 2, 0))
train_clean_score <- train_clean_score %>% mutate(sepsis_score = ifelse(sepsis==1, 2, 0))
train_clean_score <- train_clean_score %>% mutate(ascites_score = ifelse(ascites_==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(laparoscopic_score = ifelse(laparoscopic==0, 2, 0))
train_clean_score <- train_clean_score %>% mutate(proc_score = ifelse(high_risk_proc==1, 3, 0))
train_clean_score <- train_clean_score %>% mutate(sodium_score = ifelse(sodium_low==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(creat_score = ifelse(creat_high==1, 2, 0))
train_clean_score <- train_clean_score %>% mutate(albumin_score = ifelse(albumin_high==0, 1, 0))
train_clean_score <- train_clean_score %>% mutate(alk_score = ifelse(alk_high==1, 1, 0))
train_clean_score <- train_clean_score %>% mutate(plate_score = ifelse(plate_high==0, 1, 0))
train_clean_score <- train_clean_score %>% mutate(risk_score = male_score + age_score + low_bmi_score + diabetes_score + smoker_score + copd_score +
                                                    chf_score + hypermed_score + dialysis_score + fnstat_score + bleed_score + ventilator_score + 
                                                    sepsis_score + ascites_score + laparoscopic_score + proc_score + sodium_score + creat_score +
                                                    albumin_score + alk_score + plate_score)
freq(train_clean_score$risk_score)
rm("mace_train", "train_clean")
describe(train_clean_score$risk_score)

# determine cut-offs for risk score - THIS IS DONE BY GENERATING YUDEN J VALUES - higher the better
train_mod_risk_score <- glm(MACE_b ~ risk_score, 
                 data = train_clean_score, family = "binomial")
summary(train_mod_risk_score)
exp(cbind(OR = coef(train_mod_risk_score), confint(train_mod_risk_score)))

predicted_probs_score <- predict(train_mod_risk_score, type = "response")

roc_curve_score <- roc(train_clean_score$MACE_b, predicted_probs_score)

# Plot the ROC curve
plot(roc_curve_score, main = "ROC Curve", col = "blue")

# AUC with 95% Confidence Interval
auc_value_score <- auc(roc_curve_score)
ci_value_score <- ci.auc(roc_curve_score)

# Print the AUC and its 95% CI
print(paste("AUC: ", auc_value_score))
print(paste("95% CI: ", ci_value_score))

# get youden J value and find cut-off based off balance of youden J, sensitivity, and specificity
# install.packages("cutpointr")
library(cutpointr)
opt_cut <- cutpointr(
  train_clean_score, 
  x = risk_score, 
  class = MACE_b, 
  direction = ">=", 
  pos_class = 1, 
  neg_class = 0, 
  method = maximize_metric, 
  metric = youden
)
summary(opt_cut)
plot_metric(opt_cut)

roc_result <- cutpointr::roc(
  train_clean_score, 
  x = risk_score, 
  class = MACE_b, 
  pos_class = 1, 
  neg_class = 0, 
  direction = ">="
)

# Add Youden's J index, sensitivity, and specificity as metrics
roc_result_with_metrics <- roc_result %>% mutate(sensitivity = tp/(tp + fn))
roc_result_with_metrics <- roc_result_with_metrics %>% mutate(specificity = tn/(tn + fp))
roc_result_with_metrics <- roc_result_with_metrics %>% mutate(PPV = tp/(tp + fp))
roc_result_with_metrics <- roc_result_with_metrics %>% mutate(NPV = tn/(tn + fn))
roc_result_with_metrics <- roc_result_with_metrics %>% mutate(youden = sensitivity + (specificity - 1))
rounded_roc_res <- round(roc_result_with_metrics, digits = 3)
colnames(rounded_roc_res)[colnames(rounded_roc_res) == "x.sorted"] <- "risk_score"

# View the result
roc_result_comp <- rounded_roc_res %>% select(risk_score, youden, sensitivity, specificity, PPV, NPV)

# create training model categories
### use case_when statement here to create risk categories decided upon above from youden J
train_clean_score <- train_clean_score %>% mutate(risk_category = case_when(
  risk_score >= 0 & risk_score < 5 ~ 0,
  risk_score >= 5 & risk_score < 9 ~ 1,
  risk_socre >= 9 ~ 3,
  TRUE ~ NA_real_))

### then plug risk categories into logit model 
train_mod_risk_score_cat <- glm(MACE_b ~ as.factor(risk_category), 
                            data = train_clean_score, family = "binomial")
summary(train_mod_risk_score_cat)
exp(cbind(OR = coef(train_mod_risk_score_cat), confint(train_mod_risk_score_cat)))

predicted_probs_score_cat <- predict(train_mod_risk_score_cat, type = "response")

roc_curve_score_cat <- roc(train_mod_risk_score_cat$MACE_b, predicted_probs_score_cat)

# Plot the ROC curve
plot(roc_curve_score_cat, main = "ROC Curve", col = "blue")

# AUC with 95% Confidence Interval
auc_value_score_cat <- auc(roc_curve_score_cat)
ci_value_score_cat <- ci.auc(roc_curve_score_cat)

# Print the AUC and its 95% CI
print(paste("AUC: ", auc_value_score_cat))
print(paste("95% CI: ", ci_value_score_cat))

# create test model 
mace_test$laparoscopic <- factor(mace_test$laparoscopic, levels = c(0, 1), labels = c("Open Surgery", "Laparoscopic"))
mace_test$laparoscopic <- relevel(mace_test$laparoscopic, ref = 'Laparoscopic')
mace_test$albumin_high <- factor(mace_test$albumin_high, levels = c(0, 1), labels = c("Low Albumin", "Normal Albumin"))
mace_test$albumin_high <- relevel(mace_test$albumin_high, ref = 'Normal Albumin')
mace_test$plate_high <- factor(mace_test$plate_high, levels = c(0, 1), labels = c("Low Plate", "Normal Plate"))
mace_test$plate_high <- relevel(mace_test$plate_high, ref = 'Normal Plate')
test_mod <- glm(MACE_b ~ age_binary + male + low_BMI_ + diabetes_ + smoker + copd + chf + hypermed_ + dialysis_ +
                   fnstat_final + bleed + ventilator + sepsis + ascites_ + laparoscopic + high_risk_proc + sodium_low + 
                   creat_high + albumin_high + alk_high + plate_high, data = mace_test, family = "binomial")
summary(test_mod)
exp(cbind(OR = coef(test_mod), confint(test_mod)))

# generate AUC & ROC Curve for test model 
sapply(mace_test[, c("MACE_b", "age_binary", "male", "low_BMI_", "diabetes_", "smoker", "copd", "chf", 
                      "hypermed_", "dialysis_", "fnstat_final", "bleed", "ventilator", "sepsis", 
                      "ascites_", "laparoscopic", "high_risk_proc", "sodium_low", "creat_high", 
                      "albumin_high", "alk_high", "plate_high")], function(x) sum(is.na(x)))

test_clean <- mace_test[complete.cases(mace_test[, c("MACE_b", "age_binary", "male", "low_BMI_", "diabetes_", "smoker",
                                                        "copd", "chf", "hypermed_", "dialysis_", "fnstat_final", "bleed", 
                                                        "ventilator", "sepsis", "ascites_", "laparoscopic", "high_risk_proc",
                                                        "sodium_low", "creat_high", "albumin_high", "alk_high", "plate_high")]), ]
predicted_probs_test <- predict(test_mod, type = "response")
roc_curve_test <- roc(test_clean$MACE_b, predicted_probs_test)

# Plot the ROC curve
plot(roc_curve_test, main = "ROC Curve Test Data", col = "blue")

# AUC with 95% Confidence Interval
auc_value_test <- auc(roc_curve_test)
ci_value_test <- ci.auc(roc_curve_test)

# Print the AUC and its 95% CI
print(paste("AUC: ", auc_value_test))
print(paste("95% CI: ", ci_value_test))

# generate VIF for model
vif(test_mod)
#all values < 2

# create test model risk score 
# assign risk score based on aOR in train_model, then sum risk scores together to get risk_score variable for each patient 
test_clean_score <- test_clean %>% mutate(male_score = ifelse(male==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(age_score = ifelse(age_binary==1, 2, 0))
test_clean_score <- test_clean_score %>% mutate(low_bmi_score = ifelse(low_BMI_==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(diabetes_score = ifelse(diabetes_==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(smoker_score = ifelse(smoker==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(copd_score = ifelse(copd==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(chf_score = ifelse(chf==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(hypermed_score = ifelse(hypermed_==1, 2, 0))
test_clean_score <- test_clean_score %>% mutate(dialysis_score = ifelse(dialysis_==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(fnstat_score = ifelse(fnstat_final==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(bleed_score = ifelse(bleed==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(ventilator_score = ifelse(ventilator==1, 2, 0))
test_clean_score <- test_clean_score %>% mutate(sepsis_score = ifelse(sepsis==1, 2, 0))
test_clean_score <- test_clean_score %>% mutate(ascites_score = ifelse(ascites_==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(laparoscopic_score = ifelse(laparoscopic==0, 2, 0))
test_clean_score <- test_clean_score %>% mutate(proc_score = ifelse(high_risk_proc==1, 3, 0))
test_clean_score <- test_clean_score %>% mutate(sodium_score = ifelse(sodium_low==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(creat_score = ifelse(creat_high==1, 2, 0))
test_clean_score <- test_clean_score %>% mutate(albumin_score = ifelse(albumin_high==0, 1, 0))
test_clean_score <- test_clean_score %>% mutate(alk_score = ifelse(alk_high==1, 1, 0))
test_clean_score <- test_clean_score %>% mutate(plate_score = ifelse(plate_high==0, 1, 0))
test_clean_score <- test_clean_score %>% mutate(risk_score = male_score + age_score + low_bmi_score + diabetes_score + smoker_score + copd_score +
                                                    chf_score + hypermed_score + dialysis_score + fnstat_score + bleed_score + ventilator_score + 
                                                    sepsis_score + ascites_score + laparoscopic_score + proc_score + sodium_score + creat_score +
                                                    albumin_score + alk_score + plate_score)

freq(test_clean_score$risk_score)
describe(test_clean_score$risk_score)

# risk score continuous on test data
test_mod_risk_score <- glm(MACE_b ~ risk_score, 
                            data = test_clean_score, family = "binomial")
summary(test_mod_risk_score)
exp(cbind(OR = coef(test_mod_risk_score), confint(test_mod_risk_score)))

predicted_probs_score_test <- predict(test_mod_risk_score, type = "response")

roc_curve_score_test <- roc(test_mod_risk_score$MACE_b, predicted_probs_score_test)

# Plot the ROC curve
plot(roc_curve_score_test, main = "ROC Curve", col = "blue")

# AUC with 95% Confidence Interval
auc_value_score <- auc(roc_curve_score_test)
ci_value_score <- ci.auc(roc_curve_score_test)

# risk score categorical on test data
train_clean_score <- train_clean_score %>% mutate(risk_category = case_when(
  risk_score >= 0 & risk_score < 5 ~ 0,
  risk_score >= 5 & risk_score < 9 ~ 1,
  risk_socre >= 9 ~ 3,
  TRUE ~ NA_real_))

### then plug risk categories into logit model 
test_mod_risk_score_cat <- glm(MACE_b ~ as.factor(risk_category), 
                                data = test_mod_risk_score, family = "binomial")
summary(test_mod_risk_score_cat)
exp(cbind(OR = coef(test_mod_risk_score_cat), confint(test_mod_risk_score_cat)))

predicted_probs_score_cat_test <- predict(test_mod_risk_score_cat, type = "response")

roc_curve_score_cat_test <- roc(test_mod_risk_score_cat$MACE_b, predicted_probs_score_cat_test)

# Plot the ROC curve
plot(roc_curve_score_cat_test, main = "ROC Curve", col = "blue")

# AUC with 95% Confidence Interval
auc_value_score_cat_test <- auc(roc_curve_score_cat_test)
ci_value_score_cat_test <- ci.auc(roc_curve_score_cat_test)

# Print the AUC and its 95% CI
print(paste("AUC: ", auc_value_score_cat_test))
print(paste("95% CI: ", ci_value_score_cat_test))
