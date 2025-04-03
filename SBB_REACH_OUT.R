
# Remove all objects 
rm(list = ls(all = TRUE))

# Optionally set working directory
# setwd("U:/")

## Set-up ##
library(effects) # To calculate the predictive margins
library(margins) # To calculate the marginal effects
library(questionr) # To convert the log odds to odds ratios
library(sjmisc) # To use na.omit command to drop missing data

library(readxl)
df <- read_excel('filepath/filename.xlsx')

#descriptive stats 
library(summarytools)
library(psych)
str(df)
describe(df)
freq(df$ED)
freq(df$LGB)

library(crosstable)
crosstable(df, LGB, by=ED, showNA="always", 
           percent_digits=1, percent_pattern="{n} ({p_row})") %>% 
  as_flextable(keep_id=TRUE)

psych::describeBy(df$loneliness, df$ED)
psych::describeBy(df$loneliness, df$LGB)

# crude unadjusted model - LGB & ED
glm_LGB <- glm(ED ~ LGB, data=df, family=binomial(link='logit'))
logLik(glm_LGB)
summary(glm_LGB)

#calculate odds ratios
glm_LGB.or <- odds.ratio(glm_LGB, level=.95)
glm_LGB.or
exp(confint(glm_LGB))

# crude unadjusted model - loneliness & ED
glm_lonely <- glm(ED ~ loneliness, data=df, family=binomial(link='logit'))
logLik(glm_lonely)
summary(glm_lonely)

#calculate odds ratios
glm_lonely.or <- odds.ratio(glm_lonely, level=.95)
glm_lonely.or
exp(confint(glm_lonely))

#model 1 - adjusted 
glm1 <- glm(ED ~ LGB + loneliness, data=df, family=binomial(link='logit'))
logLik(glm1)
summary(glm1)

#calculate odds ratios
glm1.or <- odds.ratio(glm1, level=.95)
glm1.or
exp(confint(glm1))

png("effect of LGB_loneliness_no_interaction.png") # Open a png file

#"woman" is the independent variable that is the covariate with the two options of "Woman" or "Man"
#addhealth is the dataset
#x is the independent variable you are making predictions off of for the dependent variable of Depressive symptoms 
# Draw the plot
cplot(glm1, x = "loneliness",
      data = df[df[["LGB"]] == 1,], 
      xlab = "Loneliness Scale",
      ylab = "Pr(Eating Disorder Diag)",
      col = "red", se.type = "shade",
      main = "Predicted Probability of Having Eating Disorder")
cplot(glm1, x= "loneliness",
      data = df[df[["LGB"]] == 0,], 
      xlab = "Loneliness Scale",
      ylab = "Pr(Eating Disorder Diag)",
      col = "blue", se.type = "shade", 
      draw = "add")

dev.off() 

# model 2 - interaction 

glm2 <- glm(ED ~ LGB + loneliness + LGB*loneliness, data=df, family=binomial(link='logit'))
logLik(glm2)
summary(glm2)

#calculate odds ratios
glm2.or <- odds.ratio(glm2, level=.95)
glm2.or
exp(confint(glm2))

png("effect of LGB_loneliness_interaction.png")

cplot(glm2, x = "loneliness",
      data = df[df[["LGB"]] == 1,], 
      xlab = "Loneliness Scale",
      ylab = "Pr(Eating Disorder Diag)",
      col = "red", se.type = "shade",
      main = "Predicted Probability of Having Eating Disorder")
cplot(glm2, x= "loneliness",
      data = df[df[["LGB"]] == 0,], 
      xlab = "Loneliness Scale",
      ylab = "Pr(Eating Disorder Diag)",
      col = "blue", se.type = "shade", 
      draw = "add")

dev.off() 

# LGB only 
library(dplyr)
LGB <- df %>% filter(LGB==1)


# Straight only 
Straight <- df %>% filter(LGB==0)

# Stratified modle -> effect of loneliness by LGB status
LGB_ <- glm(ED ~ loneliness, data=LGB, family=binomial(link='logit'))
logLik(LGB_)
summary(LGB_)

#calculate odds ratios
LGB_.or <- odds.ratio(LGB_, level=.95)
LGB_.or
exp(confint(LGB_))

# Stratified modle -> effect of loneliness by LGB status
Straight_ <- glm(ED ~ loneliness, data=Straight, family=binomial(link='logit'))
logLik(Straight_)
summary(Straight_)

#calculate odds ratios
Straight_.or <- odds.ratio(Straight_, level=.95)
Straight_.or
exp(confint(Straight_))

# better looking plots 
library(ggplot2)
library(ggeffects)
ggpredict(glm2, terms = "loneliness") %>% 
  plot()
ggpredict(glm2, terms = "LGB") %>% 
  plot()
dat <- ggpredict(glm2, terms = c("loneliness", "LGB")) %>% 
  plot()

plot_label <- plot(dat) +
  labs(
    y = "P(Eating Disorder Diagnosis)",
    x = "Loneliness Scale Score",
    title = "Predicted Probability of Eating Disorder by Loneliness Score & LGB Status"
  )
plot_label

plot_label_b <- plot(plot_label) +
  labs(colour = "Sexual Orientation") +
  scale_colour_brewer(palette = "Set1", labels = c("Straight", "LGB+")) +
  scale_fill_brewer(palette = "Set1")
plot_label_b

plot_label_b + theme_minimal()

# generate predictions at user-specified values of the predictors
library(marginaleffects)
predictions(glm2, newdata = datagrid(LGB = 0, loneliness=10))
predictions(glm2, newdata = datagrid(LGB = 1, loneliness=10))
predictions(glm2, newdata = datagrid(LGB = 0, loneliness=35))
predictions(glm2, newdata = datagrid(LGB = 1, loneliness=35))
