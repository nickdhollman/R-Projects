library(haven)
library(survey)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)

R.version
citation()

#XPT FILES DOWNLOADED FROM NHANES WEBSITE 
Demo_a <- read_xpt("U:/Moen_CVD_risk/DEMO_J.xpt")
CBC_a <- read_xpt("U:/Moen_CVD_risk/CBC_J.xpt")
HDL_a <- read_xpt("U:/Moen_CVD_risk/HDL_J.xpt")
TG_a <- read_xpt("U:/Moen_CVD_risk/TRIGLY_J.xpt")
MCQ_a <- read_xpt("U:/Moen_CVD_risk/MCQ_J.xpt")
ALQ_a <- read_xpt("U:/Moen_CVD_risk/ALQ_J.xpt")
SMQ_a <- read_xpt("U:/Moen_CVD_risk/SMQ_J.xpt")
BPX_a <- read_xpt("U:/Moen_CVD_risk/BPX_J.xpt")
TCHOL_a <- read_xpt("U:/Moen_CVD_risk/TCHOL_J.xpt")
BPQ_a <- read_xpt("U:/Moen_CVD_risk/BPQ_J.xpt")
DIQ_a <- read_xpt("U:/Moen_CVD_risk/DIQ_J.xpt")
BMX_a <- read_xpt("U:/Moen_CVD_risk/BMX_J.xpt")

#IMPORTING OTHER CYCLE OF DATA TO INCREASE POWER 
Demo_b <- read_xpt("U:/Moen_CVD_risk/DEMO_I.xpt")
CBC_b <- read_xpt("U:/Moen_CVD_risk/CBC_I.xpt")
HDL_b <- read_xpt("U:/Moen_CVD_risk/HDL_I.xpt")
TG_b <- read_xpt("U:/Moen_CVD_risk/TRIGLY_I.xpt")
MCQ_b <- read_xpt("U:/Moen_CVD_risk/MCQ_I.xpt")
ALQ_b <- read_xpt("U:/Moen_CVD_risk/ALQ_I.xpt")
SMQ_b <- read_xpt("U:/Moen_CVD_risk/SMQ_I.xpt")
BPX_b <- read_xpt("U:/Moen_CVD_risk/BPX_I.xpt")
TCHOL_b <- read_xpt("U:/Moen_CVD_risk/TCHOL_I.xpt")
BPQ_b <- read_xpt("U:/Moen_CVD_risk/BPQ_I.xpt")
DIQ_b <- read_xpt("U:/Moen_CVD_risk/DIQ_I.xpt")
BMX_b <- read_xpt("U:/Moen_CVD_risk/BMX_I.xpt")

# data modification needed prior to merge
library(gmodels)
ALQ_a <- ALQ_a %>% mutate(life_ALQ = ifelse(ALQ111==1, 1, 0))
ALQ_a <- ALQ_a %>% mutate(year_ALQ = ifelse((ALQ121 != 77 & ALQ121 != 99 & ALQ121 > 0), 1, 0))
CrossTable(ALQ_a$life_ALQ, ALQ_a$ALQ111, expected = TRUE)
CrossTable(ALQ_a$year_ALQ, ALQ_a$ALQ121, expected = TRUE)
ALQ_b <- ALQ_b %>% mutate(life_ALQ = ifelse(ALQ110==1, 1, 0))
ALQ_b <- ALQ_b %>% mutate(year_ALQ = ifelse((ALQ120Q != 777 & ALQ120Q != 999 & ALQ120Q > 0), 1, 0))
table(ALQ_b$life_ALQ, ALQ_b$ALQ110)
table(ALQ_b$year_ALQ, ALQ_b$ALQ120Q)

# drop columns not in both datasets 
common_cols <- intersect(colnames(Demo_a), colnames(Demo_b))
Demo_a_subset <- Demo_a[, common_cols]
Demo_b_subset <- Demo_b[, common_cols]
common_cols_ <- intersect(colnames(CBC_a), colnames(CBC_b))
CBC_a_subset <- CBC_a[, common_cols_]
CBC_b_subset <- CBC_b[, common_cols_]
common_cols_a <- intersect(colnames(HDL_a), colnames(HDL_b))
HDL_a_subset <- HDL_a[, common_cols_a]
HDL_b_subset <- HDL_b[, common_cols_a]
common_cols_b <- intersect(colnames(TG_a), colnames(TG_b))
TG_a_subset <- TG_a[, common_cols_b]
TG_b_subset <- TG_b[, common_cols_b]
common_cols_c <- intersect(colnames(MCQ_a), colnames(MCQ_b))
MCQ_a_subset <- MCQ_a[, common_cols_c]
MCQ_b_subset <- MCQ_b[, common_cols_c]
common_cols_d <- intersect(colnames(ALQ_a), colnames(ALQ_b))
ALQ_a_subset <- ALQ_a[, common_cols_d]
ALQ_b_subset <- ALQ_b[, common_cols_d]
common_cols_e <- intersect(colnames(SMQ_a), colnames(SMQ_b))
SMQ_a_subset <- SMQ_a[, common_cols_e]
SMQ_b_subset <- SMQ_b[, common_cols_e]
common_cols_f <- intersect(colnames(BPX_a), colnames(BPX_b))
BPX_a_subset <- BPX_a[, common_cols_f]
BPX_b_subset <- BPX_b[, common_cols_f]
common_cols_g <- intersect(colnames(TCHOL_a), colnames(TCHOL_b))
TCHOL_a_subset <- TCHOL_a[, common_cols_g]
TCHOL_b_subset <- TCHOL_b[, common_cols_g]
common_cols_h <- intersect(colnames(BPQ_a), colnames(BPQ_b))
BPQ_a_subset <- BPQ_a[, common_cols_h]
BPQ_b_subset <- BPQ_b[, common_cols_h]
common_cols_i <- intersect(colnames(DIQ_a), colnames(DIQ_b))
DIQ_a_subset <- DIQ_a[, common_cols_i]
DIQ_b_subset <- DIQ_b[, common_cols_i]
common_cols_j <- intersect(colnames(BMX_a), colnames(BMX_b))
BMX_a_subset <- BMX_a[, common_cols_j]
BMX_b_subset <- BMX_b[, common_cols_j]

# STACK BOTH CYCLES OF DATA 
Demo = rbind(Demo_a_subset, Demo_b_subset)
CBC = rbind(CBC_a_subset, CBC_b_subset)
HDL = rbind(HDL_a_subset, HDL_b_subset)
TG = rbind(TG_a_subset, TG_b_subset)
MCQ = rbind(MCQ_a_subset, MCQ_b_subset)
ALQ = rbind(ALQ_a_subset, ALQ_b_subset)
SMQ = rbind(SMQ_a_subset, SMQ_b_subset)
BPX = rbind(BPX_a_subset, BPX_b_subset)
TCHOL = rbind(TCHOL_a_subset, TCHOL_b_subset)
BPQ = rbind(BPQ_a_subset, BPQ_b_subset)
DIQ = rbind(DIQ_a_subset, DIQ_b_subset)
BMX = rbind(BMX_a_subset, BMX_b_subset)

#remove datasets no longer needed
rm("ALQ_a", "ALQ_b", "ALQ_a_subset", "ALQ_b_subset")
rm("BMX_a", "BMX_b", "BMX_a_subset", "BMX_b_subset")
rm("BPQ_a", "BPQ_b", "BPQ_a_subset", "BPQ_b_subset")
rm("BPX_a", "BPX_b", "BPX_a_subset", "BPX_b_subset")
rm("CBC_a", "CBC_b", "CBC_a_subset", "CBC_b_subset")
rm("Demo_a", "Demo_b", "Demo_a_subset", "Demo_b_subset")
rm("DIQ_a", "DIQ_b", "DIQ_a_subset", "DIQ_b_subset")
rm("HDL_a", "HDL_b", "HDL_a_subset", "HDL_b_subset")
rm("MCQ_a", "MCQ_b", "MCQ_a_subset", "MCQ_b_subset")
rm("SMQ_a", "SMQ_b", "SMQ_a_subset", "SMQ_b_subset")
rm("TCHOL_a", "TCHOL_b", "TCHOL_a_subset", "TCHOL_b_subset")
rm("TG_a", "TG_b", "TG_a_subset", "TG_b_subset")
rm("common_cols", "common_cols_", "common_cols_a", "common_cols_b", "common_cols_c", "common_cols_d",
   "common_cols_e", "common_cols_f", "common_cols_g", "common_cols_h", "common_cols_i", "common_cols_j")

# unique ID to tie data together = SEQN

# sample stratum var = SDMVSTRA

# sample cluster/PSU var = SDMVPSU

#survey design object -- you will update this (re-run) everytime a new dataset is created 
design <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF2YR, nest=TRUE, data=master_)

#inner merge datasets
df_list <- list(Demo, CBC, HDL, TG, MCQ, ALQ, SMQ, BPX, TCHOL, BPQ, DIQ, BMX) 
#n=19,225 interveiwed based on Demo data

#merge all data frames together
master <- df_list %>% reduce(inner_join, by='SEQN')

# sample w/ TG, HDL, CBC data - n=5,381 w/ demo data & all our primary exposures ()
df_list_ <- list(Demo, CBC, HDL, TG)
master_exp <- df_list %>% reduce(inner_join, by='SEQN')
#sample w/ all primary exposures & positive sample weights (n=4,766)
master_exp %>% filter(WTSAF2YR != 0) %>% freq(master_exp$RIAGENDR)
#sample w/ all primary exposures, positive sample weights, and CVD data (n=)
master_exp_CVD <- master_exp %>%
  mutate(CVD = case_when(
    MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | MCQ160E == 1 | MCQ160F == 1 ~ 1,
    is.na(MCQ160B) & is.na(MCQ160C) & is.na(MCQ160D) & is.na(MCQ160E) & is.na(MCQ160F) ~ NA_real_,
    TRUE ~ 0
  ))
master_exp_CVD <- master_exp_CVD %>% filter(!is.na(CVD))
master_exp_CVD %>% filter(WTSAF2YR != 0) %>% freq(master_exp$CVD)

########## RE-FORMAT RACE ####### 
master_ <- master %>% 
  mutate(race = case_when(
    RIDRETH1 == 1 ~ 1,
    RIDRETH1 == 3 ~ 2,
    RIDRETH1 == 4 ~ 3,
    (RIDRETH1 == 2 | RIDRETH1 ==5) ~ 5))

library(magrittr)
#weighted freq & % - I DO NOT LIKE THIS AS MUCH, USE THE SVYTOTAL W/ INTERACTION TERM TO GET SAME OUTPUT AS SAS - WEIGHTED N, WEIGHTED %, AND SE FOR BOTH
#svytable(~race, design=design) %>% prop.table() %>% multiply_by(100) %>% round(digits=3) 
#svytable(~race, design=design)
#weighted freq & SE
svytotal(~as.factor(race), design=design)
#weighted % & SE -- remember to multiply both times 100 to get % and SE 
svymean(~as.factor(race), design=design)
#unweighted n -- IGNORE %, THIS IS UNWEIGHTED   
library(summarytools)
master_ %>% filter(WTSAF2YR != 0) %>% freq(master_$race)

########## RE-FORMAT EDUCATION ##########
master_a <- master_ %>% 
  mutate(educat = case_when (
    # above 20
    DMDEDUC2 == 1 ~ 0,
    DMDEDUC2 == 2 ~ 1,
    DMDEDUC2 == 3 ~ 1,
    DMDEDUC2 == 4 ~ 2,
    DMDEDUC2 == 5 ~ 2,
    DMDEDUC2 == 7 ~ NA,
    DMDEDUC2 == 9 ~ NA,
    # 18 - 20
    (DMDEDUC3 >= 0 & DMDEDUC3 < 9) ~ 0,
    (DMDEDUC3 >= 9 & DMDEDUC3 <= 14) ~ 1,
    DMDEDUC3 == 15 ~ 2,
    DMDEDUC3 == 55 ~ 0,
    DMDEDUC3 == 66 ~ 0,
    DMDEDUC3 == 77 ~ NA,
    DMDEDUC3 == 99 ~ NA))
#unweighted
educ_miss <- master_a %>% filter(is.na(educat)) %>%  dplyr::select(DMDEDUC2, DMDEDUC3)
master_a %>% filter(WTSAF2YR != 0) %>% freq(master_a$educat)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(educat), subset(design,!is.na(educat)))
svymean(~as.factor(educat), subset(design,!is.na(educat)))

############ create NLR, TG:HDL-C ##########
# NLE
master_b <- master_a %>% mutate(nlr = LBDNENO/LBDLYMNO)
#use na = TRUE if missing data is present
svymean(~nlr, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~nlr, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~nlr, design=design, na= TRUE))
library(jtools) #jtools gives standard deviation 
svysd(~nlr, design=design, na = TRUE)
#plot histogram
svyhist(~nlr, design=design)
svyqqmath(~nlr, design=design)

master_c <- master_b %>% mutate(tg_hdl = LBXTR/LBDHDD)
#use na = TRUE if missing data is present
svymean(~tg_hdl, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~tg_hdl, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~tg_hdl, design=design, na= TRUE))
svyhist(~tg_hdl, design=design)
svyqqmath(~tg_hdl, design=design)

########### DEFINE CVD OUTCOME ##########
# TRUE in case_when is equivalent to else statement in R
# you must use the NA_real_ option in the second condition or else R will not pick up these values 
master_d <- master_c %>%
  mutate(CVD = case_when(
    MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | MCQ160E == 1 | MCQ160F == 1 ~ 1,
    is.na(MCQ160B) & is.na(MCQ160C) & is.na(MCQ160D) & is.na(MCQ160E) & is.na(MCQ160F) ~ NA_real_,
    TRUE ~ 0
  ))
#unweighted
master_d %>% filter(WTSAF2YR != 0) %>% freq(master_d$CVD)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(CVD), subset(design,!is.na(CVD)))
svymean(~as.factor(CVD), subset(design,!is.na(CVD)))

########## RE-FORMAT ALCOHOL #########)
# re-format for updated analysis 
master_e <- master_d %>% mutate(alcohol = case_when (
  (life_ALQ==1 & year_ALQ==0) ~ 1,
  (year_ALQ==1 & RIAGENDR==1 & ALQ130 >= 0 & ALQ130 <= 2) ~ 2,
  (year_ALQ==1 & RIAGENDR==2 & ALQ130 >= 0 & ALQ130 <= 1) ~ 2,
  (year_ALQ==1 & RIAGENDR==1 & ALQ130 > 2) ~ 3,
  (year_ALQ==1 & RIAGENDR==2 & ALQ130 > 1) ~ 3,
  year_ALQ==0 & is.na(life_ALQ) ~ 0,
  life_ALQ==0 ~ 0))

#unweighted
master_e %>% filter(WTSAF2YR != 0) %>% freq(master_e$alcohol)
alc_miss <- master_e %>% filter(is.na(alcohol)) %>% dplyr::select(life_ALQ, year_ALQ, RIAGENDR, ALQ130)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(alcohol), subset(design,!is.na(alcohol)))
svymean(~as.factor(alcohol), subset(design,!is.na(alcohol)))

########## RE-FORMAT SMOKING #########
master_f <- master_e %>% mutate(smoker = case_when (
  SMQ020==2 ~ 0,
  (SMQ020==1 & SMQ040==3) ~ 1,
  (SMQ020==1 & (SMQ040==1 | SMQ040==2)) ~ 2))

#unweighted
master_f %>% filter(WTSAF2YR != 0) %>% freq(master_f$smoker)
smoke_miss <- master_f %>% filter(is.na(smoker)) %>% dplyr::select(SMQ020, SMQ040)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(smoker), subset(design,!is.na(smoker)))
svymean(~as.factor(smoker), subset(design,!is.na(smoker)))


###### CALCULATE AVG BLOOD PRESSURE ###########
master_g <- master_f %>% mutate(sys_bp_avg = (BPXSY1 + BPXSY2 + BPXSY3)/3)
master_g <- master_g %>% mutate(dia_bp_avg = (BPXDI1 + BPXDI2 + BPXDI3)/3)


#use na = TRUE if missing data is present
svymean(~dia_bp_avg, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~dia_bp_avg, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~dia_bp_avg, design=design, na= TRUE))
svyhist(~dia_bp_avg, design=design)
svyqqmath(~dia_bp_avg, design=design)

#use na = TRUE if missing data is present
svymean(~sys_bp_avg, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~sys_bp_avg, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~sys_bp_avg, design=design, na= TRUE))
svyhist(~sys_bp_avg, design=design)
svyqqmath(~sys_bp_avg, design=design)

####### RE-FORMAT CHOLEST_MED QUESTION ########
# TRUE in case_when is equivalent to else statement in R
master_h <- master_g %>% mutate(cholest_med = case_when (
  BPQ100D==1 ~ 1,
  (BPQ080==9 | BPQ080==7 | BPQ090D==9 | BPQ090D==7) ~ NA_real_,
  TRUE ~ 0))
master_h <- master_h %>% mutate(bp_med = case_when (
  BPQ050A==1 ~ 1,
  (BPQ020==9 | BPQ040A==7 | BPQ040A==9) ~ NA_real_,
  TRUE ~ 0))

#unweighted
master_h %>% filter(WTSAF2YR != 0) %>% freq(master_h$cholest_med)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(cholest_med), subset(design,!is.na(cholest_med)))
svymean(~as.factor(cholest_med), subset(design,!is.na(cholest_med)))

#unweighted
master_h %>% filter(WTSAF2YR != 0) %>% freq(master_h$bp_med)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(bp_med), subset(design,!is.na(bp_med)))
svymean(~as.factor(bp_med), subset(design,!is.na(bp_med)))


####### RE-FORMAT DIABETES STATUS #########
# this code is stating that if the value is equal to 9, set to missing, else set value as original DIQ010 value 
master_i <- master_h %>%
  mutate(DIQ010 = ifelse(DIQ010 == 9, NA_real_, DIQ010))

#unweighted
master_i %>% filter(WTSAF2YR != 0) %>% freq(master_i$DIQ010)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(DIQ010), subset(design,!is.na(DIQ010)))
svymean(~as.factor(DIQ010), subset(design,!is.na(DIQ010)))

####### LOG-TRANSFORM NLR, RDW, AND TG-HDL (NOT USED IN FINAL MODEL) #####
master_j <- master_i %>% mutate(log_rdw = log(LBXRDW))
master_j <- master_j %>% mutate(log_nlr = log(nlr))
master_j <- master_j %>% mutate(log_tg_hdl = log(tg_hdl))

#use na = TRUE if missing data is present
svymean(~log_rdw, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~log_rdw, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~log_rdw, design=design, na= TRUE))
svyhist(~log_rdw, design=design)
svyqqmath(~log_rdw, design=design)

#use na = TRUE if missing data is present
svymean(~log_nlr, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~log_nlr, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~log_nlr, design=design, na= TRUE))
svyhist(~log_nlr, design=design)
svyqqmath(~log_nlr, design=design)

#use na = TRUE if missing data is present
svymean(~log_tg_hdl, design=design, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~log_tg_hdl, design = design, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~log_tg_hdl, design=design, na= TRUE))
svyhist(~log_tg_hdl, design=design)
svyqqmath(~log_tg_hdl, design=design)

### LIMIT TO ONLY THOSE WITH OUTCOME (CVD ne (!) NA)
master_k <- master_j %>% filter(!is.na(CVD))
#check that filtering worked appropriately 
#unweighted
master_k %>% filter(WTSAF2YR != 0) %>% freq(master_k$alcohol)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(alcohol), subset(design,!is.na(alcohol)))
svymean(~as.factor(alcohol), subset(design,!is.na(alcohol)))
# filter worked appropriately as this is the same results as SAS

########### RE-FORMAT RDW BASED ON QUARTILE #########
master_l <- master_k %>% mutate(rdw_cat = case_when (
            is.na(LBXRDW) ~ NA_real_,                                                                                                                                                       
            (LBXRDW >= 0 & LBXRDW <= 12.923784) ~ 0,
            (LBXRDW > 12.923784 & LBXRDW <= 13.374452) ~ 1,
            (LBXRDW > 13.374452 & LBXRDW <= 13.989312) ~ 2,
            (LBXRDW > 13.989312 ~ 3)))

#unweighted
master_l %>% filter(WTSAF2YR != 0) %>% freq(master_l$rdw_cat)
rdw_miss <- master_l %>% filter(is.na(rdw_cat)) %>% dplyr::select(LBXRDW)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(rdw_cat), subset(design,!is.na(rdw_cat)))
svymean(~as.factor(rdw_cat), subset(design,!is.na(rdw_cat)))


########### RE-FORMAT NLR BASED ON QUARTILE #########
master_m <- master_l %>% mutate(nlr_cat = case_when (
  is.na(nlr) ~ NA_real_,                                                                                                                                                       
  (nlr >= 0 & nlr <= 1.424364) ~ 0,
  (nlr > 1.424364 & nlr <= 1.888010) ~ 1,
  (nlr > 1.888010 & nlr <= 2.616994) ~ 2,
  (nlr > 2.616994 ~ 3)))

#unweighted
master_m %>% filter(WTSAF2YR != 0) %>% freq(master_m$nlr_cat)
nlr_miss <- master_m %>% filter(is.na(nlr_cat)) %>% dplyr::select(nlr)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(nlr_cat), subset(design,!is.na(nlr_cat)))
svymean(~as.factor(nlr_cat), subset(design,!is.na(nlr_cat)))

########### RE-FORMAT TG:HDL BASED ON QUARTILE #########
master_n <- master_m %>% mutate(tg_hdl_cat = case_when (
  is.na(tg_hdl) ~ NA_real_,                                                                                                                                                       
  (tg_hdl >= 0 & tg_hdl <= 1.048466) ~ 0,
  (tg_hdl > 1.048466 & tg_hdl <= 1.754766) ~ 1,
  (tg_hdl > 1.754766 & tg_hdl <= 2.977639) ~ 2,
  (tg_hdl > 2.977639 ~ 3)))

#unweighted
master_n %>% filter(WTSAF2YR != 0) %>% freq(master_n$tg_hdl_cat)
tg_hdl_miss <- master_n %>% filter(is.na(tg_hdl_cat)) %>% dplyr::select(tg_hdl)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(tg_hdl_cat), subset(design,!is.na(tg_hdl_cat)))
svymean(~as.factor(tg_hdl_cat), subset(design,!is.na(tg_hdl_cat)))

# ###### ORIGINAL MODELS USING QUARTILE VARIABLES ########
# install.packages("svyROC")
# library(svyROC)
# ########### MODEL 1 ###########
# # Ensure no missing values in key columns before modeling
# master_n_clean <- master_n[complete.cases(master_n$tg_hdl_cat, master_n$rdw_cat, master_n$nlr_cat, master_n$CVD, master_n$SDMVSTRA), ]
# 
# # Set up the survey design
# design_a <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF2YR, nest=TRUE, data=master_n_clean)
# 
# # Fit the model
# model_a <- svyglm(CVD ~ rdw_cat + nlr_cat + tg_hdl_cat, design=design_a, family = "binomial")
# 
# # Summary and odds ratios
# summary(model_a)
# confint(model_a)
# OR.CI <- cbind("AOR" = exp(   coef(model_a)),
#                exp(confint(model_a,
#                            df.resid=degf(model_a$survey.design))))[-1,]
# round(OR.CI, 3)
# 
# # Predict probabilities using the fitted model
# predicted_prob_a <- predict(model_a, newdata = master_n_clean, type = "response")
# 
# # Assign the predicted probabilities back to master_n_clean
# master_n_clean$predicted_prob_a <- predicted_prob_a
# 
# # Ensure no missing values in response, predicted probabilities, and weights
# master_n_clean <- master_n_clean[complete.cases(master_n_clean$CVD, master_n_clean$predicted_prob_a, master_n_clean$SDMVSTRA), ]
# 
# # Calculate AUC using svyROC
# auc.obj <- wauc(response.var = master_n_clean$CVD,
#                 phat.var = master_n_clean$predicted_prob_a,
#                 weights.var = master_n_clean$SDMVSTRA,
#                 tag.event = 1, tag.nonevent = 0,
#                 data = master_n_clean)
# 
# print(auc.obj)
# 
# # reference for AUC calculation: https://cran.r-project.org/web/packages/svyROC/svyROC.pdf
# # page 5-6 goes into detail about Estimation of the AUC of logistic regression models with complex survey data.
# 
# ######## MODEL 2 ##########
# # Ensure no missing values in key columns before modeling
# master_n_clean_b <- master_n_clean[complete.cases(master_n_clean$RIDAGEYR, master_n_clean$RIAGENDR, master_n_clean$educat), ]
# 
# # Set up the survey design
# design_b <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF2YR, nest=TRUE, data=master_n_clean_b)
# 
# # Fit the model
# model_b <- svyglm(CVD ~ rdw_cat + nlr_cat + tg_hdl_cat + RIDAGEYR + RIAGENDR + educat, design=design_b, family = "binomial")
# 
# # Summary and odds ratios
# summary(model_b)
# confint(model_b)
# OR.CI <- cbind("AOR" = exp(   coef(model_b)),
#                exp(confint(model_b,
#                            df.resid=degf(model_b$survey.design))))[-1,]
# round(OR.CI, 3)
# 
# # Predict probabilities using the fitted model
# predicted_prob_b <- predict(model_b, newdata = master_n_clean_b, type = "response")
# 
# # Assign the predicted probabilities back to master_n_clean
# master_n_clean_b$predicted_prob_b <- predicted_prob_b
# 
# # Ensure no missing values in response, predicted probabilities, and weights
# master_n_clean_b <- master_n_clean_b[complete.cases(master_n_clean_b$CVD, master_n_clean_b$predicted_prob_b, master_n_clean_b$SDMVSTRA), ]
# 
# # Calculate AUC using svyROC
# auc.obj <- wauc(response.var = master_n_clean_b$CVD,
#                 phat.var = master_n_clean_b$predicted_prob_b,
#                 weights.var = master_n_clean_b$SDMVSTRA,
#                 tag.event = 1, tag.nonevent = 0,
#                 data = master_n_clean_b)
# 
# print(auc.obj)
# 
# ######## MODEL 3 ##########
# # Ensure no missing values in key columns before modeling
# master_n_clean_c <- master_n_clean_b[complete.cases(master_n_clean_b$alcohol, master_n_clean_b$smoker, master_n_clean_b$BMXBMI), ]
# 
# # Set up the survey design
# design_c <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF2YR, nest=TRUE, data=master_n_clean_c)
# 
# # Fit the model
# model_c <- svyglm(CVD ~ rdw_cat + nlr_cat + tg_hdl_cat + RIDAGEYR + RIAGENDR + educat + alcohol + smoker + BMXBMI, design=design_c, family = "binomial")
# 
# # Summary and odds ratios
# summary(model_c)
# confint(model_c)
# OR.CI <- cbind("AOR" = exp(   coef(model_c)),
#                exp(confint(model_c,
#                            df.resid=degf(model_c$survey.design))))[-1,]
# round(OR.CI, 3)
# 
# # Predict probabilities using the fitted model
# predicted_prob_c <- predict(model_c, newdata = master_n_clean_c, type = "response")
# 
# # Assign the predicted probabilities back to master_n_clean
# master_n_clean_c$predicted_prob_c <- predicted_prob_c
# 
# # Ensure no missing values in response, predicted probabilities, and weights
# master_n_clean_c <- master_n_clean_c[complete.cases(master_n_clean_c$CVD, master_n_clean_c$predicted_prob_c, master_n_clean_c$SDMVSTRA), ]
# 
# # Calculate AUC using svyROC
# auc.obj <- wauc(response.var = master_n_clean_c$CVD,
#                 phat.var = master_n_clean_c$predicted_prob_c,
#                 weights.var = master_n_clean_c$SDMVSTRA,
#                 tag.event = 1, tag.nonevent = 0,
#                 data = master_n_clean_c)
# 
# print(auc.obj)

############## CREATE RISK-FACTOR CUT-OFFS FOR NLR, RDW, AND TG:HDL ############
master_o <- master_n %>% mutate(rdw_bi = case_when (
  is.na(LBXRDW) ~ NA_real_,                                                                                                                                                       
  (LBXRDW >= 0 & LBXRDW < 13.10) ~ 0,
  (LBXRDW >= 13.10 ~ 1)))
master_o %>%
  group_by(rdw_bi) %>%
  summarise(
    min_value = min(LBXRDW),
    max_value = max(LBXRDW)
  )

master_p <- master_o %>% mutate(nlr_bi = case_when (
  is.na(nlr) ~ NA_real_,                                                                                                                                                       
  (nlr >= 0 & nlr < 2.56) ~ 0,
  (nlr >= 2.56 ~ 1)))
master_p %>%
  group_by(nlr_bi) %>%
  summarise(
    min_value = min(nlr),
    max_value = max(nlr)
  )

master_q <- master_p %>% mutate(tg_hdl_bi = case_when (
  is.na(tg_hdl) ~ NA_real_,                                                                                                                                                       
  (tg_hdl >= 0 & tg_hdl < 4.00) ~ 0,
  (tg_hdl >= 4.00 ~ 1)))
master_q %>%
  group_by(tg_hdl_bi) %>%
  summarise(
    min_value = min(tg_hdl),
    max_value = max(tg_hdl)
  )

master_q %>% filter(WTSAF2YR != 0) %>% freq(master_q$rdw_bi)
master_q %>% filter(WTSAF2YR != 0) %>% freq(master_q$nlr_bi)
master_q %>% filter(WTSAF2YR != 0) %>% freq(master_q$tg_hdl_bi)

### DROP DATAFRAMES NO LONGER DESIRED IN THE ANALYSIS - KEEP ORIGINAL MASTER BEFORE DROPPING CASES W/O CVD DATA IN CASE THAT IS DESIRED
rm("master_", "master_a", "master_b", "master_c", "master_d", "master_e", "master_f", "master_g", "master_h", "master_i", "master_j", 
   "master_k", "master_l", "master_m", "master_n", "master_o", "master_p")
rm("df_list", "df_list_")

freq(master_q$SDDSRVYR)

master_q <- master_q %>% mutate(WTSAF4YR = 1/2 * WTSAF2YR)
head(master_q$WTSAF4YR)
head(master_q$WTSAF2YR)
#combined weight worked appropriately 

#trim dataframe to only variables needed in the final analysis 
master_q = subset(master_q, select = c("SEQN", "SDMVPSU", "SDMVSTRA", "WTSAF4YR", "CVD", "rdw_bi", "nlr_bi", "tg_hdl_bi", "RIDAGEYR", "RIAGENDR", "race", "educat", "alcohol", "smoker", "BMXBMI"))
rm(master_q_)

############# RE-RUN MODELS 1 - 3 WITH RISK FACTORS INSTEAD OF QUARTILES ##########
########### MODEL 1 ###########
# Ensure no missing values in key columns before modeling
master_q_clean <- master_q[complete.cases(master_q$tg_hdl_bi, master_q$rdw_bi, master_q$nlr_bi, master_q$CVD, master_q$WTSAF4YR), ]

# Set up the survey design
design_a_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean)

# Fit the model
model_a_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi, design=design_a_, family = "binomial")

# Summary and odds ratios
summary(model_a_)
confint(model_a_)
OR.CI <- cbind("AOR" = exp(   coef(model_a_)),
               exp(confint(model_a_,
                           df.resid=degf(model_a_$survey.design))))[-1,]
round(OR.CI, 3)

# Predict probabilities using the fitted model
predicted_prob_a_ <- predict(model_a_, newdata = master_q_clean, type = "response")

# Assign the predicted probabilities back to master_n_clean
master_q_clean$predicted_prob_a_ <- predicted_prob_a_

# Ensure no missing values in response, predicted probabilities, and weights
master_q_clean <- master_q_clean[complete.cases(master_q_clean$CVD, master_q_clean$predicted_prob_a_, master_q_clean$WTSAF4YR), ]

# Calculate AUC using svyROC
library(svyROC)
auc.obj <- wauc(response.var = master_q_clean$CVD,
                phat.var = master_q_clean$predicted_prob_a_,
                weights.var = master_q_clean$WTSAF4YR,
                tag.event = 1, tag.nonevent = 0,
                data = master_q_clean)

print(auc.obj)

######## MODEL 2 ##########
# Ensure no missing values in key columns before modeling
master_q_clean_b_ <- master_q_clean[complete.cases(master_q_clean$RIDAGEYR, master_q_clean$RIAGENDR, master_q_clean$educat), ]

# Set up the survey design
design_b_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_b_)

# Fit the model
model_b_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(RIAGENDR) + as.factor(educat), design=design_b_, family = "binomial")

# Summary and odds ratios
summary(model_b_)
confint(model_b_)
OR.CI <- cbind("AOR" = exp(   coef(model_b_)),
               exp(confint(model_b_,
                           df.resid=degf(model_b_$survey.design))))[-1,]
round(OR.CI, 3)

# Predict probabilities using the fitted model
predicted_prob_b_ <- predict(model_b_, newdata = master_q_clean_b_, type = "response")

# Assign the predicted probabilities back to master_n_clean
master_q_clean_b_$predicted_prob_b_ <- predicted_prob_b_

# Ensure no missing values in response, predicted probabilities, and weights
master_q_clean_b_ <- master_q_clean_b_[complete.cases(master_q_clean_b_$CVD, master_q_clean_b_$predicted_prob_b_, master_q_clean_b_$WTSAF4YR), ]

# Calculate AUC using svyROC
auc.obj <- wauc(response.var = master_q_clean_b_$CVD,
                phat.var = master_q_clean_b_$predicted_prob_b_,
                weights.var = master_q_clean_b_$WTSAF4YR,
                tag.event = 1, tag.nonevent = 0,
                data = master_q_clean_b_)

print(auc.obj)

######## MODEL 3 ##########
# Ensure no missing values in key columns before modeling
master_q_clean_c_ <- master_q_clean_b_[complete.cases(master_q_clean_b_$alcohol, master_q_clean_b_$smoker, master_q_clean_b_$BMXBMI), ]

# Set up the survey design
design_c_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_c_)

# Fit the model
model_c_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(RIAGENDR) + as.factor(educat) + as.factor(alcohol) + as.factor(smoker) + BMXBMI, design=design_c_, family = "binomial")

# Summary and odds ratios
summary(model_c_)
confint(model_c_)
OR.CI <- cbind("AOR" = exp(   coef(model_c_)),
               exp(confint(model_c_,
                           df.resid=degf(model_c_$survey.design))))[-1,]
round(OR.CI, 3)

# Predict probabilities using the fitted model
predicted_prob_c_ <- predict(model_c_, newdata = master_q_clean_c_, type = "response")

# Assign the predicted probabilities back to master_n_clean
master_q_clean_c_$predicted_prob_c_ <- predicted_prob_c_

# Ensure no missing values in response, predicted probabilities, and weights
master_q_clean_c_ <- master_q_clean_c_[complete.cases(master_q_clean_c_$CVD, master_q_clean_c_$predicted_prob_c_, master_q_clean_c_$WTSAF4YR), ]

# Calculate AUC using svyROC - previously accidentally had stratum instead of weight var in weigths.var option
auc.obj <- wauc(response.var = master_q_clean_c_$CVD,
                phat.var = master_q_clean_c_$predicted_prob_c_,
                weights.var = master_q_clean_c_$WTSAF4YR,
                tag.event = 1, tag.nonevent = 0,
                data = master_q_clean_c_)

print(auc.obj)

# test for multicollinearity in each model
install.packages("svydiags")
library(svydiags)
#values of 10 or more are considered a sign of collinearity among predictors
#svyvif is the alternative to use for linear models 
X <- model.matrix(~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(RIAGENDR) + as.factor(educat) + as.factor(alcohol) + as.factor(smoker) + BMXBMI,
                  data = master_q_clean_c_)
svycollinear(model_c_, X[,-1], master_q_clean_c_$WTSAF4YR, sc=TRUE,
             rnd=2, fuzz=FALSE)
# X[-1] removes the intercept column
svyvif(model_c_, X[,-1], 
       w = master_q_clean_c_$WTSAF4YR, 
       stvar = "SDMVSTRA", 
       clvar = "SDMVPSU")
# no VIF values >= 2 in svy.vif which is the vif for complex sample 

############# TEST FOR INTERACTION W/ GENDER TO SEE IF STRATIFIED MODELS ARE NEEDED ##########
# Fit the model
model_a_int <- svyglm(CVD ~ rdw_bi*RIAGENDR + nlr_bi*RIAGENDR + tg_hdl_bi*RIAGENDR, design=design_c_, family = "binomial")

# Summary and odds ratios
summary(model_a_int)
confint(model_a_int)
OR.CI <- cbind("AOR" = exp(   coef(model_a_int)),
               exp(confint(model_a_int,
                           df.resid=degf(model_a_int$survey.design))))[-1,]
round(OR.CI, 3)
# not statistically significant 

########## Export master_q dataset #########
write.csv(master_q, file="U:/Moen_CVD_risk/master_q.csv")
master_q <- read_csv("U:/Moen_CVD_risk/master_q.csv", show_col_types = TRUE)

########### DESCRIPTIVE STATISTICS FOR TABLE 1 ########
design_fin <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q)
# CVD Overall 
#unweighted
master_q %>% freq(master_q$CVD)
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$CVD)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(CVD), subset(design_fin,!is.na(CVD)))
svymean(~as.factor(CVD), subset(design_fin,!is.na(CVD)))

# Age Overall
#use na = TRUE if missing data is present
svymean(~RIDAGEYR, design=design_fin, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~RIDAGEYR, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~RIDAGEYR, design=design_fin, na= TRUE))

# Gender Overall 
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$RIAGENDR)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(RIAGENDR), subset(design_fin,!is.na(RIAGENDR)))
svymean(~as.factor(RIAGENDR), subset(design_fin,!is.na(RIAGENDR)))

# Race Overall - 1=mexican american, 2=non-hispanic white, 3=non-hispanic black, 5=other
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$race)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(race), subset(design_fin,!is.na(race)))
svymean(~as.factor(race), subset(design_fin,!is.na(race)))

# Education level Overall - 0=less than 9th grade, 1=9-11 grade/high school or equiv, 2=college grad or above
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$educat)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(educat), subset(design_fin,!is.na(educat)))
svymean(~as.factor(educat), subset(design_fin,!is.na(educat)))

# Alcohol Use Overall - 0=never, 1=former drinker, 2=light/moderate, 3=heavy 
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$alcohol)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(alcohol), subset(design_fin,!is.na(alcohol)))
svymean(~as.factor(alcohol), subset(design_fin,!is.na(alcohol)))

# Smoking Status Overall - 0=never, 1=former, 2=current
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$smoker)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(smoker), subset(design_fin,!is.na(smoker)))
svymean(~as.factor(smoker), subset(design_fin,!is.na(smoker)))

# BMI Overall 
#use na = TRUE if missing data is present
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$BMXBMI)
svymean(~BMXBMI, design=design_fin, na = TRUE)
# get quantiles (including 95% CI)
svyquantile(~BMXBMI, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
#get 95% CI - does not give equivalent to SAS (can be used for categorical data too)
confint(svymean(~BMXBMI, design=design_fin, na= TRUE))

# RDW Overall 
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$rdw_bi)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(rdw_bi), subset(design_fin,!is.na(rdw_bi)))
svymean(~as.factor(rdw_bi), subset(design_fin,!is.na(rdw_bi)))

# NLR Overall 
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$nlr_bi)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(nlr_bi), subset(design_fin,!is.na(nlr_bi)))
svymean(~as.factor(nlr_bi), subset(design_fin,!is.na(nlr_bi)))

# TG:HDL Overall 
master_q %>% filter(WTSAF4YR != 0) %>% freq(master_q$tg_hdl_bi)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svytotal(~as.factor(tg_hdl_bi), subset(design_fin,!is.na(tg_hdl_bi)))
svymean(~as.factor(tg_hdl_bi), subset(design_fin,!is.na(tg_hdl_bi)))

#### DESCRIPTIVE STATISTICS BY CVD
### AGE ###
svyby(~RIDAGEYR, 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(RIDAGEYR)), 
      FUN = svymean)
svyttest(RIDAGEYR~CVD, design = subset(design_fin, !is.na(RIDAGEYR)))

### GENDER ###
master_q_raw <- master_q %>% filter(WTSAF4YR != 0)
CrossTable(master_q_raw$RIAGENDR, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(RIAGENDR), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(RIAGENDR)), 
      FUN = svymean)
svychisq(~CVD+RIAGENDR, design=design_fin)

### RACE ###
CrossTable(master_q_raw$race, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(race), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(race)), 
      FUN = svymean)
svychisq(~CVD+race, design=design_fin)

### EDUCATION ###
CrossTable(master_q_raw$educat, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(educat), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(educat)), 
      FUN = svymean)
svychisq(~CVD+educat, design=design_fin)

### ALCOHOL USE ###
CrossTable(master_q_raw$alcohol, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(alcohol), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(alcohol)), 
      FUN = svymean)
svychisq(~CVD+alcohol, design=design_fin)

### SMOKING ###
CrossTable(master_q_raw$smoker, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(smoker), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(smoker)), 
      FUN = svymean)
svychisq(~CVD+smoker, design=design_fin)

### BMI ###
svyby(~BMXBMI, 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(BMXBMI)), 
      FUN = svymean)
svyttest(BMXBMI~CVD, design = subset(design_fin, !is.na(BMXBMI)))

### RDW ###
CrossTable(master_q_raw$rdw_bi, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(rdw_bi), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(rdw_bi)), 
      FUN = svymean)
svychisq(~CVD+rdw_bi, design=design_fin)

### NLR ###
CrossTable(master_q_raw$nlr_bi, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(nlr_bi), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(nlr_bi)), 
      FUN = svymean)
svychisq(~CVD+nlr_bi, design=design_fin)

### TG:HDL ###
CrossTable(master_q_raw$tg_hdl_bi, master_q_raw$CVD, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(tg_hdl_bi), 
      ~as.factor(CVD), 
      design = subset(design_fin, !is.na(tg_hdl_bi)), 
      FUN = svymean)
svychisq(~CVD+tg_hdl_bi, design=design_fin)

# REPEAT ABOVE, BUT SUBSTITUTE CVD W/ NLR_BI, RDW_BI, AND TG_HDL_BI #
# RDW_BI - TABLE 3 #
### AGE ###
svyby(~RIDAGEYR, 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(RIDAGEYR)), 
      FUN = svymean)
svyttest(RIDAGEYR~rdw_bi, design = subset(design_fin, !is.na(RIDAGEYR)))

### GENDER ###
CrossTable(master_q_raw$RIAGENDR, master_q_raw$rdw_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(RIAGENDR), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(RIAGENDR)), 
      FUN = svymean)
svychisq(~rdw_bi+RIAGENDR, design=design_fin)

### RACE ###
CrossTable(master_q_raw$race, master_q_raw$rdw_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(race), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(race)), 
      FUN = svymean)
svychisq(~rdw_bi+race, design=design_fin)

### EDUCATION ###
CrossTable(master_q_raw$educat, master_q_raw$rdw_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(educat), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(educat)), 
      FUN = svymean)
svychisq(~rdw_bi+educat, design=design_fin)

### ALCOHOL USE ###
CrossTable(master_q_raw$alcohol, master_q_raw$rdw_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(alcohol), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(alcohol)), 
      FUN = svymean)
svychisq(~rdw_bi+alcohol, design=design_fin)

### SMOKING ###
CrossTable(master_q_raw$smoker, master_q_raw$rdw_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(smoker), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(smoker)), 
      FUN = svymean)
svychisq(~rdw_bi+smoker, design=design_fin)

### BMI ###
svyby(~BMXBMI, 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(BMXBMI)), 
      FUN = svymean)
svyttest(BMXBMI~rdw_bi, design = subset(design_fin, !is.na(BMXBMI)))

# NLR_BI - TABLE 4 #
### AGE ###
svyby(~RIDAGEYR, 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(RIDAGEYR)), 
      FUN = svymean)
svyttest(RIDAGEYR~nlr_bi, design = subset(design_fin, !is.na(RIDAGEYR)))

### GENDER ###
CrossTable(master_q_raw$RIAGENDR, master_q_raw$nlr_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(RIAGENDR), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(RIAGENDR)), 
      FUN = svymean)
svychisq(~nlr_bi+RIAGENDR, design=design_fin)

### RACE ###
CrossTable(master_q_raw$race, master_q_raw$nlr_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(race), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(race)), 
      FUN = svymean)
svychisq(~nlr_bi+race, design=design_fin)

### EDUCATION ###
CrossTable(master_q_raw$educat, master_q_raw$nlr_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(educat), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(educat)), 
      FUN = svymean)
svychisq(~nlr_bi+educat, design=design_fin)

### ALCOHOL USE ###
CrossTable(master_q_raw$alcohol, master_q_raw$nlr_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(alcohol), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(alcohol)), 
      FUN = svymean)
svychisq(~nlr_bi+alcohol, design=design_fin)

### SMOKING ###
CrossTable(master_q_raw$smoker, master_q_raw$nlr_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(smoker), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(smoker)), 
      FUN = svymean)
svychisq(~nlr_bi+smoker, design=design_fin)

### BMI ###
svyby(~BMXBMI, 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(BMXBMI)), 
      FUN = svymean)
svyttest(BMXBMI~nlr_bi, design = subset(design_fin, !is.na(BMXBMI)))

# TG_HDL_BI - TABLE 5 #
### AGE ###
svyby(~RIDAGEYR, 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(RIDAGEYR)), 
      FUN = svymean)
svyttest(RIDAGEYR~tg_hdl_bi, design = subset(design_fin, !is.na(RIDAGEYR)))

### GENDER ###
CrossTable(master_q_raw$RIAGENDR, master_q_raw$tg_hdl_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(RIAGENDR), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(RIAGENDR)), 
      FUN = svymean)
svychisq(~tg_hdl_bi+RIAGENDR, design=design_fin)

### RACE ###
CrossTable(master_q_raw$race, master_q_raw$tg_hdl_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(race), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(race)), 
      FUN = svymean)
svychisq(~tg_hdl_bi+race, design=design_fin)

### EDUCATION ###
CrossTable(master_q_raw$educat, master_q_raw$tg_hdl_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(educat), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(educat)), 
      FUN = svymean)
svychisq(~tg_hdl_bi+educat, design=design_fin)

### ALCOHOL USE ###
CrossTable(master_q_raw$alcohol, master_q_raw$tg_hdl_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(alcohol), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(alcohol)), 
      FUN = svymean)
svychisq(~tg_hdl_bi+alcohol, design=design_fin)

### SMOKING ###
CrossTable(master_q_raw$smoker, master_q_raw$tg_hdl_bi, expected = TRUE)
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(smoker), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(smoker)), 
      FUN = svymean)
svychisq(~tg_hdl_bi+smoker, design=design_fin)

### BMI ###
svyby(~BMXBMI, 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(BMXBMI)), 
      FUN = svymean)
svyttest(BMXBMI~tg_hdl_bi, design = subset(design_fin, !is.na(BMXBMI)))


############## COMPARE % CVD BY GENDER ACROSS DIFFERENT LEVELS OF NLR, RDW, TG:HDL
############ interaction between nlr & CVD by gender ###### CIRCLE BACK TO THIS
# Fit the model
model_a_int <- svyglm(CVD ~ rdw_bi*as.factor(RIAGENDR) + nlr_bi*as.factor(RIAGENDR) + tg_hdl_bi*as.factor(RIAGENDR), design=design_fin, family = "binomial")

# Summary and odds ratios
summary(model_a_int)
confint(model_a_int)
OR.CI <- cbind("AOR" = exp(   coef(model_a_int)),
               exp(confint(model_a_int,
                           df.resid=degf(model_a_int$survey.design))))[-1,]
round(OR.CI, 3)
# not statistically significant 

#model 2 interaction #
model_b_int <- svyglm(CVD ~ rdw_bi*as.factor(RIAGENDR) + nlr_bi*as.factor(RIAGENDR) + tg_hdl_bi*as.factor(RIAGENDR) + RIDAGEYR + as.factor(educat), design=design_fin, family = "binomial")

# Summary and odds ratios
summary(model_b_int)
confint(model_b_int)
OR.CI <- cbind("AOR" = exp(   coef(model_b_int)),
               exp(confint(model_b_int,
                           df.resid=degf(model_b_int$survey.design))))[-1,]
round(OR.CI, 3)
#not statistically significant

# Fit the model
model_c_int <- svyglm(CVD ~ rdw_bi*as.factor(RIAGENDR) + nlr_bi*as.factor(RIAGENDR) + tg_hdl_bi*as.factor(RIAGENDR) + RIDAGEYR + as.factor(educat) + as.factor(alcohol) + as.factor(smoker) + BMXBMI, design=design_fin, family = "binomial")

# Summary and odds ratios
summary(model_c_int)
confint(model_c_int)
OR.CI <- cbind("AOR" = exp(   coef(model_c_int)),
               exp(confint(model_c_int,
                           df.resid=degf(model_c_int$survey.design))))[-1,]
round(OR.CI, 3)
#not statistically significant

########## STRATIFIED MODELS TO DETERMINE IF THERE IS A DESCRIPTIVE DIFFERENCE IN THE ASSOCIATION BETWEEN OUR RISK FACTORS OF INTEREST
########## AND CVD ##########

########### MODEL 1 - MALE ###########
# Ensure no missing values in key columns before modeling
master_q_clean <- master_q[complete.cases(master_q$tg_hdl_bi, master_q$rdw_bi, master_q$nlr_bi, master_q$CVD, master_q$WTSAF4YR), ]

# Set up the survey design
design_a_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean)

# Fit the model
model_a_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi, design=subset(design_a_, RIAGENDR==1), family = "binomial")

# Summary and odds ratios
summary(model_a_)
confint(model_a_)
OR.CI <- cbind("AOR" = exp(   coef(model_a_)),
               exp(confint(model_a_,
                           df.resid=degf(model_a_$survey.design))))[-1,]
round(OR.CI, 3)

########### MODEL 1 - FEMALE ###########
# Ensure no missing values in key columns before modeling
master_q_clean <- master_q[complete.cases(master_q$tg_hdl_bi, master_q$rdw_bi, master_q$nlr_bi, master_q$CVD, master_q$WTSAF4YR), ]

# Set up the survey design
design_a_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean)

# Fit the model
model_a_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi, design=subset(design_a_, RIAGENDR==2), family = "binomial")

# Summary and odds ratios
summary(model_a_)
confint(model_a_)
OR.CI <- cbind("AOR" = exp(   coef(model_a_)),
               exp(confint(model_a_,
                           df.resid=degf(model_a_$survey.design))))[-1,]
round(OR.CI, 3)

######## MODEL 2 - MALE ##########
# Ensure no missing values in key columns before modeling
master_q_clean_b_ <- master_q_clean[complete.cases(master_q_clean$RIDAGEYR, master_q_clean$RIAGENDR, master_q_clean$educat), ]

# Set up the survey design
design_b_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_b_)

# Fit the model
model_b_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(educat), design=subset(design_b_, RIAGENDR==1), family = "binomial")

# Summary and odds ratios
summary(model_b_)
confint(model_b_)
OR.CI <- cbind("AOR" = exp(   coef(model_b_)),
               exp(confint(model_b_,
                           df.resid=degf(model_b_$survey.design))))[-1,]
round(OR.CI, 3)

######## MODEL 2 - FEMALE ##########
# Ensure no missing values in key columns before modeling
master_q_clean_b_ <- master_q_clean[complete.cases(master_q_clean$RIDAGEYR, master_q_clean$RIAGENDR, master_q_clean$educat), ]

# Set up the survey design
design_b_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_b_)

# Fit the model
model_b_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(educat), design=subset(design_b_, RIAGENDR==2), family = "binomial")

# Summary and odds ratios
summary(model_b_)
confint(model_b_)
OR.CI <- cbind("AOR" = exp(   coef(model_b_)),
               exp(confint(model_b_,
                           df.resid=degf(model_b_$survey.design))))[-1,]
round(OR.CI, 3)

######## MODEL 3 - MALE ##########
# Ensure no missing values in key columns before modeling
master_q_clean_c_ <- master_q_clean_b_[complete.cases(master_q_clean_b_$alcohol, master_q_clean_b_$smoker, master_q_clean_b_$BMXBMI), ]

# Set up the survey design
design_c_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_c_)

# Fit the model
model_c_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(educat) + as.factor(alcohol) + as.factor(smoker) + BMXBMI, design=subset(design_c_, RIAGENDR==1), family = "binomial")

# Summary and odds ratios
summary(model_c_)
confint(model_c_)
OR.CI <- cbind("AOR" = exp(   coef(model_c_)),
               exp(confint(model_c_,
                           df.resid=degf(model_c_$survey.design))))[-1,]
round(OR.CI, 3)

######## MODEL 3 - FEMALE ##########
# Ensure no missing values in key columns before modeling
master_q_clean_c_ <- master_q_clean_b_[complete.cases(master_q_clean_b_$alcohol, master_q_clean_b_$smoker, master_q_clean_b_$BMXBMI), ]

# Set up the survey design
design_c_ <- svydesign(id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTSAF4YR, nest=TRUE, data=master_q_clean_c_)

# Fit the model
model_c_ <- svyglm(CVD ~ rdw_bi + nlr_bi + tg_hdl_bi + RIDAGEYR + as.factor(educat) + as.factor(alcohol) + as.factor(smoker) + BMXBMI, design=subset(design_c_, RIAGENDR==2), family = "binomial")

# Summary and odds ratios
summary(model_c_)
confint(model_c_)
OR.CI <- cbind("AOR" = exp(   coef(model_c_)),
               exp(confint(model_c_,
                           df.resid=degf(model_c_$survey.design))))[-1,]
round(OR.CI, 3)

####### DESCRIPTIVE STATISTICS ON WEIGHTED CVD % ABOVE & BELOW NLR STRATIFIED BY GENDER ####
### RDW --THESE GIVE ROW % OPPOSED TO COLUMN %###
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(CVD), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(rdw_bi)), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(rdw_bi) & RIAGENDR==1), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(rdw_bi), 
      design = subset(design_fin, !is.na(rdw_bi) & RIAGENDR==2), 
      FUN = svymean)

### NLR ###
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(CVD), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(nlr_bi)), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(nlr_bi) & RIAGENDR==1), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(nlr_bi), 
      design = subset(design_fin, !is.na(nlr_bi) & RIAGENDR==2), 
      FUN = svymean)


### TG:HDL ###
#weighted -- SUBSET(DESIGN,!IS.NA()) IS NEEDED TO HANDLE MISSING VALUES
svyby(~as.factor(CVD), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(tg_hdl_bi)), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(tg_hdl_bi) & RIAGENDR==1), 
      FUN = svymean)
svyby(~as.factor(CVD), 
      ~as.factor(tg_hdl_bi), 
      design = subset(design_fin, !is.na(tg_hdl_bi) & RIAGENDR==2), 
      FUN = svymean)

######## SUPPLEMENTAL TABLE 5 DR. MOEN REQUESTED #######
# NEUTROPHILS - LBDNENO #
svymean(~LBDNENO, design=design_fin, na = TRUE)
svyby(formula = ~LBDNENO, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDNENO)), 
      FUN = svymean)
svyquantile(~LBDNENO, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBDNENO, 
  by = ~as.factor(CVD), 
  design = subset(design_fin, !is.na(LBDNENO)), 
  FUN = svyquantile, 
  c(0.25, 0.5, 0.75))

# LYMPHOCYTES - LBDLYMNO #
svymean(~LBDLYMNO, design=design_fin, na = TRUE)
svyby(formula = ~LBDLYMNO, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDLYMNO)), 
      FUN = svymean)
svyquantile(~LBDLYMNO, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBDLYMNO, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDLYMNO)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# TRIGLYCERIDES - LBXTR #
svymean(~LBXTR, design=design_fin, na = TRUE)
svyby(formula = ~LBXTR, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBXTR)), 
      FUN = svymean)
svyquantile(~LBXTR, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBXTR, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBXTR)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# HDL - LBDHDD #
svymean(~LBDHDD, design=design_fin, na = TRUE)
svyby(formula = ~LBDHDD, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDHDD)), 
      FUN = svymean)
svyquantile(~LBDHDD, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBDHDD, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDHDD)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# LDL - LBDLDL #
svymean(~LBDLDL, design=design_fin, na = TRUE)
svyby(formula = ~LBDLDL, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDLDL)), 
      FUN = svymean)
svyquantile(~LBDLDL, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBDLDL, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBDLDL)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# SYSTOLIC BP - sys_bp_avg #
svymean(~sys_bp_avg, design=design_fin, na = TRUE)
svyby(formula = ~sys_bp_avg, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(sys_bp_avg)), 
      FUN = svymean)
svyquantile(~sys_bp_avg, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~sys_bp_avg, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(sys_bp_avg)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# DIASTOLIC BP - dia_bp_avg #
svymean(~dia_bp_avg, design=design_fin, na = TRUE)
svyby(formula = ~dia_bp_avg, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(dia_bp_avg)), 
      FUN = svymean)
svyquantile(~dia_bp_avg, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~dia_bp_avg, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(dia_bp_avg)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# TOTAL CHOLESTEROL - LBXTC #
svymean(~LBXTC, design=design_fin, na = TRUE)
svyby(formula = ~LBXTC, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBXTC)), 
      FUN = svymean)
svyquantile(~LBXTC, design = design_fin, na = TRUE, c(.25,.5,.75),ci=TRUE)
svyby(formula = ~LBXTC, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(LBXTC)), 
      FUN = svyquantile, 
      c(0.25, 0.5, 0.75))

# TAKING BP MEDICATION - bp_med #
svymean(~bp_med, design=design_fin, na = TRUE)
svyby(formula = ~bp_med, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(bp_med)), 
      FUN = svymean)

# TAKING CHOLESTEROL MED - cholest_med #
svymean(~cholest_med, design=design_fin, na = TRUE)
svyby(formula = ~cholest_med, 
      by = ~as.factor(CVD), 
      design = subset(design_fin, !is.na(cholest_med)), 
      FUN = svymean)








