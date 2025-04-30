### install stepreg
#install.packages("StepReg")
#install.packages("gtsummary")

## load packages
library(StepReg)
library(tidyverse)
library(psych)
library(summarytools)
library(readxl)
library(gtsummary)
library(car) #calculates variance and generalized variance inflation factors using vif()
#https://www.rdocumentation.org/packages/car/versions/3.1-3/topics/vif 

## load data
ames <- read.csv("#### TO MAINTAIN ANONYMITY PATH DELETED ######/Ames2_.csv", header=TRUE)

#sum missing data
sum(is.na(ames))
sapply(ames, function(x) sum(is.na(x)))

#exclude those with missing data
ames_no_NA <- na.omit(ames)
sapply(ames_no_NA, function(x) sum(is.na(x)))

## define high or low price
describe(ames_no_NA$SalePrice)
ames_b <- ames_no_NA %>% mutate(High_price = case_when(SalePrice > 159900 ~ 1,
                                                 SalePrice <= 159900 ~ 0))
describeBy(SalePrice ~ High_price, data=ames_b)
#verify coding worked correctly
freq(ames_b$High_price)

#distribution of SalePrice
ggplot(ames_b, aes(SalePrice)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
library(ggpubr)
options(scipen=999)
ggqqplot(ames_b, x = "SalePrice", 
         color = "#0073C2FF",
         ggtheme = theme_pubclean())

#descriptive statistics on all variables
ames_num <- ames_b %>% select_if(is.numeric)
describe(ames_num)
ames_char <- ames_b %>% select_if(is.character)
freq(ames_b$MSZoning)
freq(ames_b$Street)
freq(ames_b$Alley)
freq(ames_b$LotShape)
ames_b <- ames_b %>% mutate(Lot_Shape_cat = if_else(LotShape == "Reg", "Reg", "Other"))
freq(ames_b$Lot_Shape_cat)
freq(ames_b$LandContour)
freq(ames_b$Utilities)
freq(ames_b$LotConfig)
freq(ames_b$LandSlope)
#collapse Mod & Sev into Mod or Sev
ames_c <- ames_b %>% mutate(LandSlope_cat = case_when(LandSlope == "Gtl" ~ 0,
                                                      LandSlope == "Mod" ~ 1,
                                                      LandSlope == "Sev" ~ 1))
freq(ames_c$LandSlope_cat)
freq(ames_c$Neighborhood)
freq(ames_c$Condition1)
freq(ames_c$Condition2)
ames_d <- ames_c %>% mutate(Condition_cat = case_when(Condition1 == "Artery" ~ "Artery",
                                                      Condition1 == "Feedr" ~ "Feedr",
                                                      Condition1 == "Norm" ~ "Norm",
                                                      Condition1 == "PosA" ~ "PosA",
                                                      Condition1 == "PosN" ~ "PosN",
                                                      Condition1 == "RRAe" ~ "Other",
                                                      Condition1 == "RRAn" ~ "Other",
                                                      Condition1 == "RRNe" ~ "Other",
                                                      Condition1 == "RRNn" ~ "Other"))
freq(ames_d$Condition_cat)
freq(ames_d$BldgType)
freq(ames_d$HouseStyle)
freq(ames_d$RoofStyle)
ames_e <- ames_d %>% mutate(Roof_style_cat = case_when(RoofStyle == "Flat" ~ "Other",
                                                       RoofStyle == "Gable" ~ "Gable",
                                                       RoofStyle == "Gambrel" ~ "Gambrel",
                                                       RoofStyle == "Hip" ~ "Hip",
                                                       RoofStyle == "Mansard" ~ "Other",
                                                       RoofStyle == "Shed" ~ "Other"))
freq(ames_e$Roof_style_cat)
freq(ames_e$RoofMatl)
ames_f <- ames_e %>% mutate(Roof_matl_cat = if_else(RoofMatl == "CompShg", "CompShg", "Other"))
freq(ames_f$Roof_matl_cat)
freq(ames_f$Exterior1st)
freq(ames_f$Exterior2nd)
freq(ames_f$MasVnrArea)
freq(ames_f$ExterQual)
freq(ames_f$ExterCond)
ames_g <- ames_f %>% mutate(Ext_cond_cat = case_when(ExterCond == "Ex" ~ "Ex|Fa",
                                                     ExterCond == "Fa" ~ "Ex|Fa",
                                                     ExterCond == "Gd" ~ "Gd",
                                                     ExterCond == "Po" ~ "Po|TA",
                                                     ExterCond == "TA" ~ "Po|TA"))
freq(ames_g$Ext_cond_cat)
freq(ames_g$Foundation)
ames_h <- ames_g %>% mutate(Found_cat = case_when(Foundation == "BrkTil" ~ "BrkTil",
                                                  Foundation == "CBlock" ~ "CBlock",
                                                  Foundation == "PConc" ~ "PConc",
                                                  Foundation == "Slab" ~ "Other",
                                                  Foundation == "Stone" ~ "Other",
                                                  Foundation == "Wood" ~ "Other"))
freq(ames_h$Found_cat)
freq(ames_h$BsmtQual)
ames_i <- ames_h %>% mutate(Base_Qual_cat = case_when(BsmtQual == "" ~ "Other",
                                                  BsmtQual == "Ex" ~ "Ex",
                                                  BsmtQual == "Fa" ~ "Fa",
                                                  BsmtQual == "Gd" ~ "Gd",
                                                  BsmtQual == "TA" ~ "TA",
                                                  BsmtQual == "Po" ~ "Other"))
freq(ames_i$Base_Qual_cat)
freq(ames_i$BsmtCond)
ames_j <- ames_i %>% mutate(Base_Cond_cat = case_when(BsmtCond == "" ~ "Other",
                                                      BsmtCond == "Ex" ~ "Other",
                                                      BsmtCond == "Fa" ~ "Fa",
                                                      BsmtCond == "Gd" ~ "Gd",
                                                      BsmtCond == "TA" ~ "TA"))
freq(ames_j$Base_Cond_cat)
freq(ames_j$BsmtExposure)
freq(ames_j$BsmtFinType1)
freq(ames_j$BsmtFinType2)
freq(ames_j$Heating)
ames_k <- ames_j %>% mutate(Heat_cat = if_else(Heating == "GasA", "GasA", "Other"))
freq(ames_k$Heat_cat)
freq(ames_k$HeatingQC)
ames_l <- ames_k %>% mutate(Heat_QC_cat = case_when(HeatingQC == "Po" ~ "Po|TA",
                                                    HeatingQC == "Ex" ~ "Ex",
                                                    HeatingQC == "Fa" ~ "Fa",
                                                    HeatingQC == "Gd" ~ "Gd",
                                                    HeatingQC == "TA" ~ "Po|TA"))
freq(ames_l$Heat_QC_cat)
freq(ames_l$CentralAir)
freq(ames_l$Electrical)
freq(ames_l$KitchenQual)
ames_m <- ames_l %>% mutate(Kitch_qual_cat = case_when(KitchenQual == "Po" ~ "Po|TA",
                                                       KitchenQual == "Ex" ~ "Ex",
                                                       KitchenQual == "Fa" ~ "Fa",
                                                       KitchenQual == "Gd" ~ "Gd",
                                                       KitchenQual == "TA" ~ "Po|TA"))
freq(ames_m$Kitch_qual_cat)
freq(ames_m$Functional)
freq(ames_m$FireplaceQu)
freq(ames_m$GarageType)
freq(ames_m$GarageQual)
ames_n <- ames_m %>% mutate(Garage_qual_cat = case_when(GarageQual == "Po" ~ "Po|TA",
                                                        GarageQual == "Ex" ~ "Ex|Fa",
                                                        GarageQual == "Fa" ~ "Ex|Fa",
                                                        GarageQual == "Gd" ~ "Gd",
                                                        GarageQual == "TA" ~ "Po|TA"))
freq(ames_n$Garage_qual_cat)
freq(ames_n$GarageFinish)
freq(ames_n$GarageCond)
ames_o <- ames_n %>% mutate(Garage_cond_cat = case_when(GarageCond == "Po" ~ "Po|TA",
                                                        GarageCond == "Ex" ~ "Ex|Fa|Gd",
                                                        GarageCond == "Fa" ~ "Ex|Fa|Gd",
                                                        GarageCond == "Gd" ~ "Ex|Fa|Gd",
                                                        GarageCond == "TA" ~ "Po|TA"))
freq(ames_o$Garage_cond_cat)
freq(ames_o$PavedDrive)
freq(ames_o$PoolQC)
freq(ames_o$Fence)
freq(ames_o$MiscFeature)
freq(ames_o$SaleType)
freq(ames_o$SaleCondition)

#select character var we will include in analysis
ames_char_log <- ames_o %>% select(MSZoning, Lot_Shape_cat, LandContour, LandSlope_cat, BldgType, HouseStyle,
                                   ExterQual, Ext_cond_cat, Base_Qual_cat, Base_Cond_cat, BsmtExposure, 
                                 BsmtFinType1, BsmtFinType2, Heat_QC_cat, CentralAir, Kitch_qual_cat, FireplaceQu,
                                 GarageFinish, Garage_qual_cat, Garage_cond_cat, PavedDrive, High_price)

formula <- ames_char_log$High_price ~ .
res4 <- stepwise(formula = formula,
                 data = ames_char_log,
                 type = "logit",
                 strategy = "backward",
                 metric = "SL",
                 sls=0.05)
res4

model <- glm(High_price ~ Lot_Shape_cat+BldgType+HouseStyle+
             Base_Qual_cat+ 
             Heat_QC_cat+Kitch_qual_cat+FireplaceQu+
             GarageFinish+Garage_qual_cat, family='binomial', data=ames_o)
summary(model)
exp(cbind(Odds_Ratio = coef(model), confint(model)))
#install.packages("car")
library(car)
model_lin <- lm(High_price ~ Lot_Shape_cat+BldgType+HouseStyle+
               Base_Qual_cat+ 
               Heat_QC_cat+Kitch_qual_cat+FireplaceQu+
               GarageFinish+Garage_qual_cat, data=ames_o)
vif(model_lin)

#predicted values

#install.packages("ROCR")
#install.packages("caTools")
#install.packages("caret")
library(ROCR)
library(caTools)
library(caret)

# get the model’s predictions
predicted_probabilities <- predict(model)

# convert the probabilities predicted into 0’s and 1’s based on a 0.5 cutoff value
predicted_values <- ifelse(predicted_probabilities > 0.5, 1, 0)

# confusion matrix comparing predicted to the dataset
levels(predicted_values) <- c(0, 1)
levels(ames_o$High_price) <- c(0, 1)
table(predicted = predicted_values, actual = ames_o$High_price)
confusionMatrix(predicted_values, ames_o$High_price)

roc_pred <- prediction(predictions = predicted_probabilities, labels = ames_o$High_price)
roc_perf <- performance(roc_pred , "tpr" , "fpr")
plot(roc_perf,
     colorize = TRUE,
     print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))
title("ROC Curve")
(auc_ROCR <- performance(roc_pred, measure = "auc"))
(auc_ROCR <- auc_ROCR@y.values[[1]])

write.csv(ames_o, "test_logit_char_only.csv")

###### NOW LET US ADD THE CONTINUOUS PREDICTORS #############
#install.packages("lubridate")
library(lubridate)
ames_o <- ames_o %>% mutate(years_since_build = 2024 - ames_o$YearBuilt)
ames_o <- ames_o %>% mutate(Overall_rating = (OverallQual + OverallCond)/2)
freq(ames_o$X1stFlrSF)
freq(ames_o$X2ndFlrSF)
ames_o <- ames_o %>% mutate(above_ground_sf = X1stFlrSF + X2ndFlrSF)
ames_o <- ames_o %>% mutate(Total_porch = OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch)
ames_o <- ames_o %>% mutate(years_since_garage = 2024 - ames_o$GarageYrBlt)

#distrib of continuous var
ggplot(ames_o, aes(years_since_build)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(years_since_garage)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(GarageCars)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(Total_porch)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(TotalBsmtSF)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(GrLivArea)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(LotArea)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(BsmtFullBath)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(BsmtHalfBath)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(FullBath)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(HalfBath)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(TotalBsmtSF)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(WoodDeckSF)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(Overall_rating)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()
ggplot(ames_o, aes(above_ground_sf)) +
  geom_histogram(color = "#000000", fill = "#0099F8") +
  theme_classic()

ames_log_final <- ames_o %>% select(MSZoning, Lot_Shape_cat, LandContour, LandSlope_cat, BldgType, HouseStyle,
                                   ExterQual, Ext_cond_cat, Base_Qual_cat, Base_Cond_cat, BsmtExposure, 
                                   BsmtFinType1, BsmtFinType2, Heat_QC_cat, CentralAir, Kitch_qual_cat, FireplaceQu,
                                   GarageFinish, Garage_qual_cat, Garage_cond_cat, PavedDrive, High_price, years_since_build,
                                   years_since_garage, GarageCars, Total_porch, TotalBsmtSF, GrLivArea, LotFrontage,
                                   LotArea, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath, TotRmsAbvGrd, 
                                   WoodDeckSF, Overall_rating, above_ground_sf)

formula_final <- ames_log_final$High_price ~ .
res_final <- stepwise(formula = formula_final,
                 data = ames_log_final,
                 type = "logit",
                 strategy = "backward",
                 metric = "SL",
                 sls=0.05)
res_final

## final model chosen
model_final <- glm(High_price ~ BsmtFinType2 + Heat_QC_cat + CentralAir + Kitch_qual_cat +
               FireplaceQu + PavedDrive + above_ground_sf, family='binomial', data=ames_o)
summary(model_final)

#p-value for overall model
1-pchisq(2653.4-1131.4, 1913-1892)
#ORs & 95% CI for infal model
exp(cbind(Odds_Ratio = coef(model_final), confint(model_final)))

#VIF for final model
vif(model_final)

#what factors are important for final model?
var_imp <- caret::varImp(model_final)
print(var_imp)

# get the final model’s predictions
predicted_probabilities_f <- predict(model_final)

# convert the probabilities predicted into 0’s and 1’s based on a 0.5 cutoff value
predicted_values_f <- ifelse(predicted_probabilities_f > 0.5, 1, 0)

# confusion matrix comparing predicted to the dataset
predicted_values_f <- factor(predicted_values_f)
levels(predicted_values_f) <- c(0, 1)
table(predicted = predicted_values_f, actual = ames_o$High_price)
confusionMatrix(predicted_values_f, ames_o$High_price)

roc_pred_f <- prediction(predictions = predicted_probabilities_f, labels = ames_o$High_price)
roc_perf_f <- performance(roc_pred_f , "tpr" , "fpr")
plot(roc_perf_f,
     colorize = TRUE)
title("ROC Curve")
(auc_ROCR_f <- performance(roc_pred_f, measure = "auc"))
(auc_ROCR_f <- auc_ROCR_f@y.values[[1]])
#install.packages("pROC")
## final ROC plot used - like this one better 
library(pROC)
predicted_final_b <- predict(model_final, ames_o, type="response")
rocobj <- roc(ames_o$High_price, predicted_final_b)
auc <- round(auc(ames_o$High_price, predicted_final_b),4)
ggroc(rocobj, legacy.axes=TRUE, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  theme_minimal()

write.csv(ames_o, "all_logit_var.csv")
#what is the R.version for methods
R.version

#### table of results
# initial table of characteristics - did not end up using to save space 
table1 <- tbl_summary(ames_o,
                      include = c(BsmtFinType2, Heat_QC_cat, CentralAir, Kitch_qual_cat,
                                    FireplaceQu, PavedDrive, above_ground_sf),
                      by = High_price)
print(table1)




