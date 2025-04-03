#load packages
#import CVS data
library(readr)
#data managment
library(tidyverse)

#the show_col_types option if specified as TRUE will set the first row of the dataset as the column names
data1 <- read_csv('U:/BBS_Richards_Shelton/BardetBiedlSyndromeB_DATA_2024-08-02_1351.csv',show_col_types = TRUE)

#filter to those with BBS diagnosis
BBS <- data1 %>% filter(bbs_dx==1)
#exlude record_id #2 as a test patient
BBS_ <- BBS %>% filter(record_id != 2)

########## Data Cleaning ########

#recode age of '14 Years' to 14
BBS_$age[BBS_$age == "14 Years" ] <- 14
BBS_$age = as.numeric(BBS_$age)

#create race variable 
BBS_$race_total = rowSums(BBS_[, c("race___0", "race___1", "race___2", "race___3", "race___4", "race___5")], na.rm=TRUE)
BBS_a <- BBS_ %>%
  mutate(race_cat = case_when(race_total > 1 ~ 'Multiracial',
                             race___0 == 1 ~ 'Asian',
                             race___1 == 1 ~ 'Black',
                             race___2 == 1 ~ 'Hispanic',
                             race___3 == 1 ~ 'Indigenous',
                             race___4 == 1 ~ 'White',
                             race___5 == 1 ~ 'Other',
                             race_oth == 'White/Hispanic' ~ 'Multiracial'))
BBS_b <- BBS_a %>% 
  mutate(race_binary = if_else(race_cat != 'White', 1,0))

#recode bbs_dx_age variable 
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "4 months" ] <- 0.33
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "18 months" ] <- 1.50
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "6 Months" ] <- 0.50
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "17 months" ] <- 1.42
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "1 year 6 months" ] <- 1.50
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "2 years" ] <- 2.00
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "3.5 years" ] <- 3.50
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "8 Years" ] <- 8.00
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "1 day" ] <- 0.00
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "1 day (testing at birth)" ] <- 0.00
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "Birth" ] <- 0.00
BBS_b$bbs_dx_age[BBS_b$bbs_dx_age == "two" ] <- 2.00
BBS_b$bbs_dx_age = as.numeric(BBS_b$bbs_dx_age)

BBS_c <- BBS_b %>%
  mutate(dx_age_cat = case_when(bbs_dx_age < 1 ~ 0,
                                bbs_dx_age >=1 & bbs_dx_age < 6 ~ 1,
                                bbs_dx_age >=6 & bbs_dx_age < 11 ~ 2,
                                bbs_dx_age >=11 & bbs_dx_age < 16 ~ 3,
                                bbs_dx_age >=16 & bbs_dx_age < 21 ~ 4,
                                bbs_dx_age >= 21 ~ 5))

#genetic mutations
BBS_c$bbs_gen_tst_gene <- toupper(BBS_c$bbs_gen_tst_gene)
BBS_d <- BBS_c %>%
  mutate(BBS1 = case_when(bbs_gen_tst_gene == '1' ~ 1,
                          bbs_gen_tst_gene == 'BB1' ~ 1,
                          bbs_gen_tst_gene == 'BBS 1' ~ 1,
                          bbs_gen_tst_gene == 'BBS1' ~ 1,
                          bbs_gen_tst_gene == 'BBS1 AND BBS10' ~ 1,
                          bbs_gen_tst_gene == 'BBS1 M390R' ~ 1,
                          bbs_gen_tst_gene == 'BBS1 ONLY ONE GENE FOUND' ~ 1,
                          bbs_gen_tst_gene == 'GENE 1' ~ 1,
                          .default = 0))
BBS_e <- BBS_d %>%
  mutate(BBS2 = case_when(bbs_gen_tst_gene == 'BB2' ~ 1,
                          bbs_gen_tst_gene == 'BBS 2' ~ 1,
                          bbs_gen_tst_gene == 'BBS2' ~ 1,
                          .default = 0))

BBS_f <- BBS_e %>%
  mutate(BBS7 = case_when(bbs_gen_tst_gene == '7' ~ 1,
                          bbs_gen_tst_gene == '7 AND 10' ~ 1,
                          .default = 0))

BBS_g <- BBS_f %>%
  mutate(BBS10 = case_when(bbs_gen_tst_gene == '10' ~ 1,
                          bbs_gen_tst_gene == '7 AND 10' ~ 1,
                          bbs_gen_tst_gene == 'BBS 10' ~ 1,
                          bbs_gen_tst_gene == 'BBS10' ~ 1,
                          bbs_gen_tst_gene == 'BBS1 AND BBS10' ~ 1,
                          .default = 0))

BBS_h <- BBS_g %>%
  mutate(Other_mut = case_when(bbs_gen_tst_gene == 'BBS4' ~ 1,
                           bbs_gen_tst_gene == 'BBS5 AND PCSK1' ~ 1,
                           bbs_gen_tst_gene == 'BBS6' ~ 1,
                           bbs_gen_tst_gene == 'BBS9' ~ 1,
                           bbs_gen_tst_gene == 'LB12' ~ 1,
                           bbs_gen_tst_gene == 'MKKS' ~ 1,
                           .default = 0))
freq(BBS_h$Other_mut)

BBS_i <- BBS_h %>%
  mutate(Unknown_na_mut = case_when(bbs_gen_tst_gene == 'NO MUTATIONS FOUND' ~ 1,
                           bbs_gen_tst_gene == 'UNKNOWN' ~ 1,
                           bbs_gen_tst_gene == 'UNSURE' ~ 1,
                           is.na(bbs_gen_tst_gene) ~ 1,
                           .default = 0))
                

mutate_test <- BBS_d %>% select(bbs_gen_tst_gene, BBS1)
mutate_test_ <- BBS_e %>% select(bbs_gen_tst_gene, BBS2)
mutate_test_a <- BBS_f %>% select(bbs_gen_tst_gene, BBS7)
mutate_test_b <- BBS_g %>% select(bbs_gen_tst_gene, BBS10)
mutate_test_c <- BBS_h %>% select(bbs_gen_tst_gene, Other_mut)
mutate_test_d <- BBS_i %>% select(bbs_gen_tst_gene, Unknown_na_mut)  

#create inidicator of any family members w/ BBS?
BBS_i$fam_member_tot = rowSums(BBS_i[, c("bbs_fam___1", "bbs_fam___2", "bbs_fam___3", "bbs_fam___4", "bbs_fam___5",
                                         "bbs_fam___6", "bbs_fam___7", "bbs_fam___8", "bbs_fam___9", "bbs_fam___10", 
                                         "bbs_fam___11", "bbs_fam___12", "bbs_fam___13", "bbs_fam___14", "bbs_fam___15",
                                         "bbs_fam___16", "bbs_fam___17", "bbs_fam___18", "bbs_fam___19", "bbs_fam___20",
                                         "bbs_fam___21", "bbs_fam___22")], na.rm=TRUE)

fam_mem_check <- BBS_i %>% select(bbs_fam___1, bbs_fam___2, bbs_fam___3, bbs_fam___4, bbs_fam___5,
                                         bbs_fam___6, bbs_fam___7, bbs_fam___8, bbs_fam___9, bbs_fam___10, 
                                         bbs_fam___11, bbs_fam___12, bbs_fam___13, bbs_fam___14, bbs_fam___15,
                                         bbs_fam___16, bbs_fam___17, bbs_fam___18, bbs_fam___19, bbs_fam___20,
                                         bbs_fam___21, bbs_fam___22, fam_member_tot)

BBS_j <- BBS_i %>% 
  mutate(any_fam_BBS = if_else(fam_member_tot > 0, 1,0))
BBS_k <- BBS_j %>% 
  mutate(multi_mem_BBS = if_else(fam_member_tot > 1, 1, 0))

## birthmark
BBS_l <- BBS_k %>% 
  mutate(dime_size_bm = if_else(bmarksize > 1, 1, 0))

BBS_m <- BBS_l %>%
  mutate(bmark_loc_cat = case_when(bmark_loc == 'upper arm' ~ 1,
                                   bmark_loc == 'back of left shoulder blade' ~ 3,
                                   bmark_loc == 'back of leg' ~ 2,
                                   bmark_loc == 'Chest' ~ 3,
                                   bmark_loc == "Eye, torso, knee" ~ 5,
                                   bmark_loc == 'Forehead' ~ 4,
                                   bmark_loc == 'head' ~ 4,
                                   bmark_loc == 'Left ankle' ~ 2,
                                   bmark_loc == 'Leg' ~ 2,
                                   bmark_loc == 'light brown and red around belly button' ~ 3,
                                   bmark_loc == 'R) thigh removed aged 7 years' ~ 2,
                                   bmark_loc == 'Right thigh' ~ 2,
                                   bmark_loc == 'Side of torso' ~ 3,
                                   bmark_loc == 'Top of leg at back' ~ 2,
                                   bmark_loc == 'Upper arm' ~ 1,
                                   bmark_loc == 'upper right back' ~ 3))

birthmark_cat_check <- BBS_m %>% select(bmark_loc, bmark_loc_cat)

## create total number of symptoms experienced
BBS_m$tot_symptoms = rowSums(BBS_m[, c("pmsight", "pmobes", "pmextra", "pmrep", "pmlearn",
                                         "pmkidney", "pmtype2", "pmheart", "pmhear", "pmspeech", 
                                         "pmincrease", "pmdev", "pmmusc", "pmimbal", "pmmouth",
                                         "pmshort", "pmwebbed", "pmeye", "pmliver")], na.rm=TRUE)
symptom_check <- BBS_m %>% select(pmsight, pmobes, pmextra, pmrep, pmlearn,
                                  pmkidney, pmtype2, pmheart, pmhear, pmspeech, 
                                  pmincrease, pmdev, pmmusc, pmimbal, pmmouth,
                                  pmshort, pmwebbed, pmeye, pmliver, tot_symptoms)

BBS_n <- BBS_m %>%
  mutate(symptom_num_cat = case_when(tot_symptoms < 5 ~ 1,
                                    tot_symptoms > 4 & tot_symptoms < 10 ~ 2,
                                    tot_symptoms >= 10 ~ 3))

BBS_o <- BBS_n %>% 
  mutate(symtom_gt_8 = if_else(tot_symptoms > 8, 1, 0))


### create summation of major and minor symptoms
BBS_o$major_symptoms = rowSums(BBS_o[, c("pmsight", "pmobes", "pmextra", "pmrep", "pmlearn",
                                       "pmkidney")], na.rm=TRUE)

BBS_o$minor_symptoms = rowSums(BBS_o[, c("pmtype2", "pmheart", "pmhear", "pmspeech", 
                                         "pmincrease", "pmdev", "pmmusc", "pmimbal", "pmmouth",
                                         "pmshort", "pmwebbed", "pmeye", "pmliver")], na.rm=TRUE)

symptom_check_b <- BBS_o %>% select(pmsight, pmobes, pmextra, pmrep, pmlearn,
                                  pmkidney, pmtype2, pmheart, pmhear, pmspeech, 
                                  pmincrease, pmdev, pmmusc, pmimbal, pmmouth,
                                  pmshort, pmwebbed, pmeye, pmliver, tot_symptoms, major_symptoms, minor_symptoms)

#define clinical diag
BBS_o <- BBS_o %>%
  mutate(clinic_diag = case_when(major_symptoms >= 4 ~ 1,
                           major_symptoms >=3 & minor_symptoms >= 2 ~ 1,
                           .default = 0))

symptom_check_c <- BBS_o %>% select(tot_symptoms, major_symptoms, minor_symptoms, clinic_diag)

#descriptive stats
library(summarytools)
library(psych)
## table 1 - overall descriptive stats
describe(BBS_$age)
IQR(BBS_$age)

freq(BBS_$gender)

freq(BBS_$race___0)
freq(BBS_$race___1)
freq(BBS_$race___2)
freq(BBS_$race___3)
freq(BBS_$race___4)
freq(BBS_$race___5)
freq(BBS_$race_oth)
freq(BBS_a$race_cat)
freq(BBS_b$race_binary)

freq(BBS_b$bbs_dx)
freq(BBS_b$bbs_dx_age)
freq(BBS_c$dx_age_cat)

freq(BBS_c$bbs_gen_tst)
freq(BBS_c$bmark)

## table 2 - genetics descriptive stats
freq(BBS_c$bbs_gen_tst_gene)
freq(BBS_i$BBS1)
freq(BBS_i$BBS2)
freq(BBS_i$BBS7)
freq(BBS_i$BBS10)
freq(BBS_i$Other_mut)
freq(BBS_i$Unknown_na_mut)

## table 3 - family members descriptive stats
freq(BBS_i$bbs_fam___1)
freq(BBS_i$bbs_fam___2)
freq(BBS_i$bbs_fam___3)
freq(BBS_i$bbs_fam___4)
freq(BBS_i$bbs_fam___5)
freq(BBS_i$bbs_fam___6)
freq(BBS_i$bbs_fam___7)
freq(BBS_i$bbs_fam___8)
freq(BBS_i$bbs_fam___9)
freq(BBS_i$bbs_fam___10)
freq(BBS_i$bbs_fam___11)
freq(BBS_i$bbs_fam___12)
freq(BBS_i$bbs_fam___13)
freq(BBS_i$bbs_fam___14)
freq(BBS_i$bbs_fam___15)
freq(BBS_i$bbs_fam___16)
freq(BBS_i$bbs_fam___17)
freq(BBS_i$bbs_fam___18)
freq(BBS_i$bbs_fam___19)
freq(BBS_i$bbs_fam___20)
freq(BBS_i$bbs_fam___21)
freq(BBS_i$bbs_fam___22)
freq(BBS_i$bbs_fam___23)
freq(BBS_i$bbs_fam___24)
freq(BBS_i$bbs_fam___25)

freq(BBS_k$any_fam_BBS)
freq(BBS_k$multi_mem_BBS)


## table 4 - symptoms 
freq(BBS_k$pmsight) #legal blindness
freq(BBS_k$pmobes) #obesity
freq(BBS_k$pmextra) #extra fingers or toes
freq(BBS_k$pmrep) #abnormal reproductive issues
freq(BBS_k$pmlearn) #learning difficulties 
freq(BBS_k$pmkidney) #kidney abnormalities
freq(BBS_k$pmtype2) #type 2 diab
freq(BBS_k$pmheart) #heart disease
freq(BBS_k$pmhear) #hearing loss
freq(BBS_k$pmspeech) #speech deficiency
freq(BBS_k$pmincrease) #increased thirst or urination
freq(BBS_k$pmdev) #developmental delay
freq(BBS_k$pmmusc) #muscl spasms
freq(BBS_k$pmimbal) #imbalance or poor coordination
freq(BBS_k$pmmouth) #high arched palate, missing teeth, dental crowding, short teeth roots
freq(BBS_k$pmshort) #abnormally short finger/toes
freq(BBS_k$pmwebbed) #webbed fingers or toes
freq(BBS_k$pmeye) #astigmatism or cataracts or lazy eye
freq(BBS_k$pmliver) #liver abnormalities
freq(BBS_k$bmark) #do you have a birthmark 
freq(BBS_n$symptom_num_cat)
freq(BBS_o$symtom_gt_8)
describe(BBS_o$tot_symptoms)
IQR(BBS_o$tot_symptoms)

freq(BBS_o$clinic_diag)
describe(BBS_o$major_symptoms)
IQR(BBS_o$major_symptoms)

## table 5 - birthmark
freq(BBS_k$bmark)
freq(BBS_k$bmarksize)
freq(BBS_l$dime_size_bm)
freq(BBS_l$bmarksize1)
freq(BBS_l$bmark_loc)

freq(BBS_l$melanoma)
freq(BBS_m$bmark_loc_cat)

######## IMPORT FINAL DATASET ########
BBS_o <- read_csv('U:/BBS_Richards_Shelton/BBS data after analysis.csv',show_col_types = TRUE)

### set na values for all symptoms to 0 ###
BBS_o$pmrep[is.na(BBS_o$pmrep)] <- 0
BBS_o$pmtype2[is.na(BBS_o$pmtype2)] <- 0
BBS_o$pmheart[is.na(BBS_o$pmheart)] <- 0
BBS_o$pmhear[is.na(BBS_o$pmhear)] <- 0
BBS_o$pmspeech[is.na(BBS_o$pmspeech)] <- 0
BBS_o$pmmouth[is.na(BBS_o$pmmouth)] <- 0
BBS_o$pmshort[is.na(BBS_o$pmshort)] <- 0
BBS_o$pmwebbed[is.na(BBS_o$pmwebbed)] <- 0

freq(BBS_o$pmsight) #legal blindness
freq(BBS_o$pmobes) #obesity
freq(BBS_o$pmextra) #extra fingers or toes
freq(BBS_o$pmrep) #abnormal reproductive issues
freq(BBS_o$pmlearn) #learning difficulties 
freq(BBS_o$pmkidney) #kidney abnormalities
freq(BBS_o$pmtype2) #type 2 diab
freq(BBS_o$pmheart) #heart disease
freq(BBS_o$pmhear) #hearing loss
freq(BBS_o$pmspeech) #speech deficiency
freq(BBS_o$pmincrease) #increased thirst or urination
freq(BBS_o$pmdev) #developmental delay
freq(BBS_o$pmmusc) #muscl spasms
freq(BBS_o$pmimbal) #imbalance or poor coordination
freq(BBS_o$pmmouth) #high arched palate, missing teeth, dental crowding, short teeth roots
freq(BBS_o$pmshort) #abnormally short finger/toes
freq(BBS_o$pmwebbed) #webbed fingers or toes
freq(BBS_o$pmeye) #astigmatism or cataracts or lazy eye
freq(BBS_o$pmliver) #liver abnormalities
freq(BBS_o$bmark) #do you have a birthmark 

### create summation of major and minor symptoms w/ new imputed values 
BBS_o$major_symptoms = rowSums(BBS_o[, c("pmsight", "pmobes", "pmextra", "pmrep", "pmlearn",
                                         "pmkidney")], na.rm=TRUE)

BBS_o$minor_symptoms = rowSums(BBS_o[, c("pmtype2", "pmheart", "pmhear", "pmspeech", 
                                         "pmincrease", "pmdev", "pmmusc", "pmimbal", "pmmouth",
                                         "pmshort", "pmwebbed", "pmeye", "pmliver")], na.rm=TRUE)

#define clinical diag w/ new imputed values 
BBS_o <- BBS_o %>%
  mutate(clinic_diag = case_when(major_symptoms >= 4 ~ 1,
                                 major_symptoms >=3 & minor_symptoms >= 2 ~ 1,
                                 .default = 0))

symptom_check_c <- BBS_o %>% select(tot_symptoms, major_symptoms, minor_symptoms, clinic_diag)

## export final dataset w/ imputed values 
write.csv(BBS_o, "U:/BBS data after analysis and imputation.csv", na='')


#### Primary aim -- prevalence of birthmarks among those with BBS
freq(BBS_k$bmark)
freq(BBS_l$dime_size_bm)

########## SECONDARY AIMS #####

## Aim 1 - Association between mutation and birthmark
table = table(BBS_m$BBS1, 
      BBS_m$bmark)
chisq <- chisq.test(table(BBS_m$BBS1, 
                          BBS_m$bmark))
#make sure expected values gt 5
round(chisq$expected,2)
#install.packages("epitools")
library(epitools)
#compute OR and 95% CI
oddsratio.wald(table(BBS_m$BBS1, 
                     BBS_m$bmark), rev = c("both"))
5/16
22/51



oddsratio.wald(table(BBS_m$BBS2, 
                     BBS_m$bmark), rev = c("both"))
1/16
5/51
fisher.test(BBS_m$BBS2, 
            BBS_m$bmark)

chisq_i <- chisq.test(table(BBS_o$BBS2, 
                          BBS_o$bmark))
#make sure expected values gt 5
round(chisq_i$expected,2)


oddsratio.wald(table(BBS_m$BBS7, 
                     BBS_m$bmark), rev = c("both"))
2/16
chisq_j <- chisq.test(table(BBS_o$BBS7, 
                            BBS_o$bmark))
#make sure expected values gt 5
round(chisq_j$expected,2)

oddsratio.wald(table(BBS_m$BBS10, 
                     BBS_m$bmark), rev = c("both"))
6/16
14/51

oddsratio.wald(table(BBS_m$Other_mut, 
                     BBS_m$bmark), rev = c("both"))
CrossTable(BBS_m$Other_mut, BBS_m$bmark)
2/16
5/51
chisq_k <- chisq.test(table(BBS_o$Other_mut, 
                            BBS_o$bmark))
#make sure expected values gt 5
round(chisq_k$expected,2)

oddsratio.wald(table(BBS_m$Unknown_na_mut, 
                     BBS_m$bmark), rev = c("both"))
6/51
chisq_l <- chisq.test(table(BBS_o$Unknown_na_mut, 
                            BBS_o$bmark))
#make sure expected values gt 5
round(chisq_l$expected,2)

### Aim 2 - Association between gender and birthmark
gender_analysis <- BBS_m %>% filter(gender < 3)
freq(gender_analysis$gender) 

oddsratio.wald(table(gender_analysis$gender, 
                     gender_analysis$bmark), rev = c("both"))
7/39
32/39
9/27
18/27

### Aim 3 - descriptive report of size and location
freq(BBS_m$bmarksize)
freq(BBS_m$bmark_loc_cat)

### Aim 4 - Association between fam hx and birthmark
freq(BBS_m$any_fam_BBS) 

oddsratio.wald(table(BBS_m$any_fam_BBS, 
                     BBS_m$bmark), rev = c("both"))

freq(BBS_m$bmark)
5/19
14/19
11/48
37/48

## Aim 5 - Association between specific symptoms and birthmark
BBS_o$bmark[BBS_o$bmark == "Birthmark Reported (n=16)"] <- 1
BBS_o$bmark[BBS_o$bmark == "No Birthmark Reported (n=51)"] <- 0

oddsratio.wald(table(BBS_o$pmsight, BBS_o$bmark), rev = c("both")) #legal blindness
13/16
38/51
chisq_b <- chisq.test(table(BBS_o$pmsight, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_b$expected,2)

oddsratio.wald(table(BBS_o$pmobes, BBS_o$bmark), rev = c("both")) #obesity
15/16
42/51
chisq_c <- chisq.test(table(BBS_o$pmobes, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_c$expected,2)

oddsratio.wald(table(BBS_o$pmextra, BBS_o$bmark), rev = c("both")) #extra fingers or toes
11/16
39/51
chisq_d <- chisq.test(table(BBS_o$pmextra, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_d$expected,2)

oddsratio.wald(table(BBS_o$pmrep, BBS_o$bmark), rev = c("both")) #abnormal reproductive issues
9/16
14/51
chisq_e <- chisq.test(table(BBS_o$pmrep, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_e$expected,2)

oddsratio.wald(table(BBS_o$pmlearn, BBS_o$bmark), rev = c("both")) #learning difficulties 
14/16
38/51
chisq_f <- chisq.test(table(BBS_o$pmlearn, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_f$expected,2)
#### If no warning is given, use Chi-square test, if a warning is given, use Fisher's Exact

oddsratio.wald(table(BBS_o$pmkidney, BBS_o$bmark), rev = c("both")) #kidney abnormalities
8/16
17/51

oddsratio.wald(table(BBS_o$pmtype2, BBS_o$bmark), rev = c("both")) #type 2 diab
1/16
2/51
chisq_g <- chisq.test(table(BBS_o$pmtype2, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_g$expected,2)

oddsratio.wald(table(BBS_o$pmheart, BBS_o$bmark), rev = c("both")) #heart disease
3/16
4/51
chisq_h <- chisq.test(table(BBS_o$pmheart, BBS_o$bmark))
#make sure expected values gt 5
round(chisq_h$expected,2)

oddsratio.wald(table(BBS_o$pmhear, BBS_o$bmark), rev = c("both")) #hearing loss
5/16
5/51

oddsratio.wald(table(BBS_o$pmspeech, BBS_o$bmark), rev = c("both")) #speech deficiency
9/16
25/51

oddsratio.wald(table(BBS_o$pmincrease, BBS_o$bmark), rev = c("both")) #increased thirst or urination
9/16
15/51

oddsratio.wald(table(BBS_o$pmdev, BBS_o$bmark), rev = c("both")) #developmental delay
12/16
36/51

oddsratio.wald(table(BBS_o$pmmusc, BBS_o$bmark), rev = c("both")) #muscl spasms
4/16
8/51

oddsratio.wald(table(BBS_o$pmimbal, BBS_o$bmark), rev = c("both")) #imbalance or poor coordination
12/16
35/51

oddsratio.wald(table(BBS_o$pmmouth, BBS_o$bmark), rev = c("both")) #high arched palate, missing teeth, dental crowding, short teeth roots
11/16
16/51

oddsratio.wald(table(BBS_o$pmshort, BBS_o$bmark), rev = c("both")) #abnormally short finger/toes
8/16
20/51

oddsratio.wald(table(BBS_o$pmwebbed, BBS_o$bmark), rev = c("both")) #webbed fingers or toes
9/16
7/51

oddsratio.wald(table(BBS_o$pmeye, BBS_o$bmark), rev = c("both")) #astigmatism or cataracts or lazy eye
9/16
30/51

oddsratio.wald(table(BBS_o$pmliver, BBS_o$bmark), rev = c("both")) #liver abnormalities
4/16
5/51

write.csv(BBS_o, "U:/BBS data after analysis.csv", na='')

## Aim 6 - Association between total number of symptoms and birthmark
oddsratio.wald(table(BBS_o$symtom_gt_8, BBS_o$bmark), rev = c("both"))
11/31
5/36
20/31
31/36

#install.packages("qqplotr")
library(ggplot2)
library(qqplotr)
p1<-ggplot(data = BBS_o, mapping = aes(sample = tot_symptoms)) +
  stat_qq_band(fill="Purple",alpha = 0.15) +
  stat_qq_line(col="Purple") +
  stat_qq_point(col="black",size=1)+
  facet_wrap(~bmark)+
  labs(title = "Total Symptoms by Birthmark")+theme_bw()
p1

BBS_o %>% group_by(bmark) %>%
  shapiro_test(tot_symptoms)
#test for equality of variance
var.test(tot_symptoms ~ bmark, BBS_o, alternative = "two.sided")
#variances are equal, must use the variance=TRUE option in t-test

#t-test
library(rstatix)
BBS_o %>%
  t_test(tot_symptoms ~ bmark, var.equal=TRUE, detailed = TRUE) %>%
  add_significance()
#p-value is less than 0.05 indicating that the means between group 0(male) and 1(female) significantly differ, females had a higher score 

#Cohen's D for two independent samples t-test, if you used var.equal=TRUE for t-test you would need to change cohen's d to var.equal=TRUE
BBS_o %>% cohens_d(tot_symptoms ~ bmark, var.equal=TRUE)

BBS_o %>%
  group_by(bmark) %>%
  get_summary_stats(tot_symptoms, type = "mean_sd")

BBS_o %>%
  get_summary_stats(tot_symptoms, type = "mean_sd")

### EXPORT RESULTS TO DOUBLE CHECK IN SAS
write.csv(BBS_o, "U:/BBS data after analysis.csv", na='')

## Association between diagnostic criteria and birthmark
oddsratio.wald(table(BBS_o$clinic_diag, BBS_o$bmark), rev = c("both"))
15/55
1/12
40/55
11/12

## Association between total number of major symptoms and birthmark 
ggplot(data = BBS_o, mapping = aes(sample = major_symptoms)) +
  stat_qq_band(fill="Purple",alpha = 0.15) +
  stat_qq_line(col="Purple") +
  stat_qq_point(col="black",size=1)+
  facet_wrap(~bmark)+
  labs(title = "Major Symptoms by Birthmark")+theme_bw()

BBS_o %>% group_by(bmark) %>%
  shapiro_test(major_symptoms)

var.test(major_symptoms ~ bmark, BBS_o, alternative = "two.sided")

BBS_o %>%
  t_test(major_symptoms ~ bmark, var.equal=TRUE, detailed = TRUE) %>%
  add_significance()

BBS_o %>%
  group_by(bmark) %>%
  get_summary_stats(major_symptoms, type = "mean_sd")

BBS_o %>%
  get_summary_stats(major_symptoms, type = "mean_sd")

BBS_o %>% cohens_d(major_symptoms ~ bmark, var.equal=TRUE)

BBS_o <- read_csv('U:/BBS_Richards_Shelton/BBS data after analysis and imputation.csv',show_col_types = TRUE)
################# DATA VISUALIZATION OF POSITIVE RESULTS #######################
BBS_o$bmark[BBS_o$bmark == "Birthmark Reported (n=16)"] <- 1
BBS_o$bmark[BBS_o$bmark == "No Birthmark Reported (n=51)"] <- 0

BBS_o$bmark <- factor(BBS_o$bmark,
                    levels = c(0,1),
                    labels = c("No Melanocytic Nevi (n=51)", "Melanocytic Nevi (n=16)"))
boxplot <- ggplot(data= BBS_o,
       mapping = aes(x=bmark, y=tot_symptoms, group=bmark)) +
  geom_boxplot(fill = "#0099f8") +
  labs(
    title = "Total Number of BBS Symptoms Reported",
    subtitle = "Comparing those with and without melanocytic nevi",
    x = "Melanocytic Nevi",
    y = "Total Number of Reported Symptoms"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(color = "#0099f8", size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(face = "bold.italic", hjust = 0.5),
    plot.caption = element_text(face = "italic"),
    axis.title.x=element_blank(),
    axis.text.x = element_text(face="bold", size=12, color="black"),
    axis.text.y = element_text(face="bold", size=10, color="black"),
    axis.title.y = element_text(face="bold", size=12, vjust = +3)
  )

boxplot

boxplot_b <- boxplot + scale_y_continuous(limits = c(0, 16),
                                          breaks = c(0, 2, 4, 6, 8, 10, 12, 14, 16))
boxplot_b

#add p-value
compare_means(tot_symptoms ~ bmark, data = BBS_o, method="t.test", method.args = list(var.equal = TRUE))

boxplot_b + stat_compare_means(method="t.test", method.args = list(var.equal = TRUE))

R.version


##### Other option for boxplot #############
library(ggpubr)

pubr <- ggboxplot(BBS_o, x="bmark", y="tot_symptoms",
               color = "bmark", palette = "jco",
               add = "jitter", legend="none", title = "Total Number of BBS Symptoms Reported",
               subtitle = "Comparing those with and without melanocytic nevi",
               ylab ="Total Number of Reported Symptoms", xlab=FALSE)
pubr

pubr_b <- pubr + stat_compare_means(method = "t.test", method.args = list(var.equal = TRUE), label.y= 18)
pubr_b

pubr_c <- pubr_b + font("title", size = 14, color = "black", face = "bold.italic")+
  font("subtitle", size = 10, color = "black", face="bold") + font("ylab", face= "bold", vjust = +3) +
  font("xy.text", face="bold")
pubr_c

pubr_d <- ggpar(pubr_c, ylim = c(0, 20))
pubr_d

###### ADDITIONAL ANALYSIS REQURESTED BY REVIEW - AGE BY BMARK #######
library(ggplot2)
library(qqplotr)
describe(BBS_o$age)
p_AGE<-ggplot(data = BBS_o, mapping = aes(sample = age)) +
  stat_qq_band(fill="Purple",alpha = 0.15) +
  stat_qq_line(col="Purple") +
  stat_qq_point(col="black",size=1)+
  facet_wrap(~bmark)+
  labs(title = "Age by Birthmark")+theme_bw()
p_AGE # bmark group is normally distributed

BBS_o %>% group_by(bmark) %>%
  shapiro_test(age)
#test for equality of variance
var.test(age ~ bmark, BBS_o, alternative = "two.sided")
#variances are equal, must use the variance=TRUE option in t-test

#t-test
library(rstatix)
BBS_o %>%
  t_test(age ~ bmark, var.equal=TRUE, detailed = TRUE) %>%
  add_significance()
describeBy(BBS_o$age, BBS_o$bmark)

#Cohen's D for two independent samples t-test, if you used var.equal=TRUE for t-test you would need to change cohen's d to var.equal=TRUE
BBS_o %>% cohens_d(age ~ bmark, var.equal=TRUE)
