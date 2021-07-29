################################################
# Descriptive analysis of the NH characteristics
################################################

PACKAGES = c("readxl", "writexl","tidyverse",
             "ggplot2","MASS","broom","pander"
             )

# install packages
for (pack_name in PACKAGES) {
  if (!pack_name %in% rownames(installed.packages()))
    install.packages(pack_name)
}

# load libraries
library(readxl)
library(writexl)
library("tidyverse")
library(dplyr)
library(stringr)
library(ggplot2)
library(MASS)
library(broom)
library(pander)


# Table 1
################################################

  ## Number of beds
nrow(DS_institution[DS_institution$n_total_bed<=20, ])
nrow(DS_institution[DS_institution$n_total_bed>20 & DS_institution$n_total_bed<=40 , ])
nrow(DS_institution[DS_institution$n_total_bed>40 & DS_institution$n_total_bed<=60 , ])
nrow(DS_institution[DS_institution$n_total_bed>60 & DS_institution$n_total_bed<=80 , ])
nrow(DS_institution[DS_institution$n_total_bed>80 & DS_institution$n_total_bed<=100, ])
nrow(DS_institution[DS_institution$n_total_bed>100, ])

  ## Ownership
nrow(DS_institution[DS_institution$status=="1", ])
(nrow(DS_institution[DS_institution$status=="1", ]) / length(DS_institution$count_total))*100

nrow(DS_institution[DS_institution$status=="2", ])
(nrow(DS_institution[DS_institution$status=="2", ]) / length(DS_institution$count_total))*100

nrow(DS_institution[DS_institution$status=="3", ])
(nrow(DS_institution[DS_institution$status=="3", ]) / length(DS_institution$count_total))*100

  ## Type of beds
sum(DS_institution$n_bed_Kortverblijf)
sum(DS_institution$n_rob_bed)
sum(DS_institution$n_rvt_bed)

  ## NH with no infection
xy <- DS_institution %>%
  filter(count_total_pos==0)
nrow(xy)

  ## NH with 1 or 2 cases
xx <- DS_institution %>%
  filter(count_total_pos>0  & count_total_pos<=2 )
nrow(xx)

  ## NH with small outbreak
xa <- DS_institution %>%
  filter(count_total_pos>2 & count_total_pos<10 )
nrow(xa)

  ## NH with large outbreak
xc <- DS_institution %>%
  filter(count_total_pos>=10)
nrow(xc)

  ## Ratio NH staff members/residents
median(DS_institution$ratio_HCW_res, na.rm = TRUE)
quantile(DS_institution$ratio_HCW_res, probs=0.25, na.rm = TRUE)
quantile(DS_institution$ratio_HCW_res, probs=0.75, na.rm = TRUE)

  ## Residents characteristics
sum(DS_institution$denominator_prev_res)
sum(DS_institution$count_res)
median(DS_institution$count_res, na.rm = TRUE)
quantile(DS_institution$count_res, probs=0.25, na.rm = TRUE)
quantile(DS_institution$count_res, probs=0.75, na.rm = TRUE)
median(DS_institution$mean_age_resident, na.rm = TRUE)
quantile(DS_institution$mean_age_resident, probs=0.25, na.rm = TRUE)
quantile(DS_institution$mean_age_resident, probs=0.75, na.rm = TRUE)
sum(DS_institution$count_pos_res)
sum(DS_institution$count_of_pos_asympto_res)

  ## Staff members characteristics
sum(DS_institution$denominator_prev_HCW)
sum(DS_institution$count_worker)
median(DS_institution$count_worker, na.rm = TRUE)
quantile(DS_institution$count_worker, probs=0.25, na.rm = TRUE)
quantile(DS_institution$count_worker, probs=0.75, na.rm = TRUE)
median(DS_institution$mean_age_worker, na.rm = TRUE)
quantile(DS_institution$mean_age_worker, probs=0.25, na.rm = TRUE)
quantile(DS_institution$mean_age_worker, probs=0.75, na.rm = TRUE)
sum(DS_institution$count_pos_HCW)
sum(DS_institution$count_of_pos_asympto_HCW)

# Figure 2
################################################

DS_institution$size_ltcf[DS_institution$n_total_bed<=20] <- "<20 beds"
DS_institution$size_ltcf[DS_institution$n_total_bed>20 & DS_institution$n_total_bed<=40 ] <- "21-40 beds"
DS_institution$size_ltcf[DS_institution$n_total_bed>40 & DS_institution$n_total_bed<=60 ] <- "41-60 beds"
DS_institution$size_ltcf[DS_institution$n_total_bed>60 & DS_institution$n_total_bed<=80 ] <- "61-80 beds"
DS_institution$size_ltcf[DS_institution$n_total_bed>80 & DS_institution$n_total_bed<=100 ] <- "81-100 beds"
DS_institution$size_ltcf[DS_institution$n_total_bed>100] <- ">100 beds"

  ## Select NHs with at least one resident tested positive
prop_0 <- DS_institution %>%
  filter(prop_pos_res>0)

min(prop_0$prop_pos_res)
max(prop_0$prop_pos_res)
median(prop_0$prop_pos_res, na.rm = TRUE)
min(prop_0$prop_pos_HCW)
max(prop_0$prop_pos_HCW)
median(prop_0$prop_pos_HCW, na.rm = TRUE)

  ## Relevel group factor
data_new <- prop_0
data_new$size_ltcf <- factor(data_new$size_ltcf,                
                             levels = c("<20 beds", "21-40 beds", "41-60 beds", "61-80 beds", "81-100 beds", ">100 beds"))

  ## graph 1: proportion of tested positive among residents and NHs staff members  
ggplot(data_new, aes(x = prop_pos_HCW*100, y=prop_pos_res*100, colour=size_ltcf)) +
  geom_point(stat = 'identity') +
  labs(title = "",
       x = "Proportion of tested positive among NH staff members (%)",
       y = "Proportion of tested positive among residents (%)") +
  scale_colour_discrete("Nursing home size") +
  theme_bw()


  ## graph 2: proportion of tested positive among residents and NHs staff members <= 10%
    ### Select NHs with a proportion of tested positive among residents and staff members <=10% (zoom in on graph 1)
prop_10 <- data_new %>%
  filter(prop_pos_res<=0.10 & prop_pos_HCW<=0.10 )

ggplot(prop_10, aes(x = prop_pos_HCW*100, y=prop_pos_res*100, colour=size_ltcf)) +
  geom_point(stat = 'identity') +
  labs(title = "",
       x = "Proportion of tested positive among NH staff members (%)",
       y = "Proportion of tested positive among residents (%)") +
  scale_colour_discrete("Nursing home size") +
  theme_bw()

# Spearman correlation's test
################################################
cor.test(DS_institution$prop_pos_res, DS_institution$prop_pos_HCW, method = "spearman")


################################################
# Risk factor analysis
################################################

# Function to tidy Poisson/negative binomial regression output
glmtidy <- function(x, caption = ''){
  pander(tidy(x, exponentiate = TRUE, conf.int = TRUE),
         caption = caption)
  
}

# Function to tidy Poisson/negative binomial regression statistics
glmstats <- function(x){
  pander(glance(x))
}

get_p_value <- function(model) {
  beta = coef(model)[2]
  B_SE = sqrt(vcov(model)[2,2])
  pvalue =  pnorm(-abs(beta) / B_SE)  * 2  
  return(pvalue)
}

get_p_value_multi <- function(model, variable) {
  beta = coef(model)[variable]
  B_SE = sqrt(vcov(model)[variable,variable])
  pvalue =  pnorm(-abs(beta) / B_SE)  * 2  
  return(pvalue)
}


# Additional variables needed for the negative binomiale regression analysis
DS_institution$n_total_bed_20 <- DS_institution$n_total_bed/20
DS_institution$prop_rvt_bed <- DS_institution$n_rvt_bed/DS_institution$n_total_bed


# Univariate analysis - Negative binomiale regression using the count_res (= number of residents tested) as an offset
######################################################################################################################
ALL_VARIABLES = c("n_total_bed_20", "ratio_HCW_res", "n_rvt_bed_20", "mean_age_worker","mean_age_resident",
                 "prop_asympto_pos_HCW", "prop_asympto_pos_res", "status")

regression_results_prop <- data.frame()

for (variable in ALL_VARIABLES) {
  
  formula_x = as.formula(paste("count_pos_res ~ offset(log(count_res)) + ", variable, sep = ""))
  mx <-  glm.nb(formula_x, data=DS_institution) 
  
  summary(mx)
  (est_mx <- cbind(Estimate = coef(mx), confint(mx)))
  get_p_value(mx)
  

  regression_results_prop[variable, "LTCF characteristic"] = variable
  regression_results_prop[variable, "IRR"] = exp(est_mx)[2, 1]
  regression_results_prop[variable, "CI2.5"] = exp(est_mx)[2, 2]
  regression_results_prop[variable, "CI97.5"] = exp(est_mx)[2, 3]  
  regression_results_prop[variable, "p-value"] = get_p_value(mx)
  regression_results_prop[variable, "univariate"] =  paste(
    round(regression_results_prop[variable, "IRR"], 2),
    " (", round(regression_results_prop[variable, "CI2.5"], 2), "-", round(regression_results_prop[variable, "CI97.5"], 2), ")",
    sep = ""
  )
}

  write_xlsx(regression_results_prop, "nb_univariate_prop_short.xlsx")
  
  
  # Log transformation of the staff members’ positivity rate (="prop_pos_HCW")
  ############################################################################
  hist(log(DS_institution$prop_pos_HCW))
  
  DS_institution$log_prop_pos_HCW = log(DS_institution$prop_pos_HCW+(exp(-7)))
  
  m9 <- glm.nb(count_pos_res ~ offset(log(count_res)) + log_prop_pos_HCW, data=DS_institution)
  (est_m9 <- cbind(Estimate = coef(m9), confint(m9)))
  exp(est_m9)
  get_p_value_multi(m9, 2)
  

  # Multivariate analysis - Negative binomiale regression including NH testing starting dates (="min_date_test")
  ##############################################################################################################
  m10 <- glm.nb(count_pos_res ~ offset(log(count_res)) + min_date_test + prop_rvt_bed+ 
                  mean_age_worker + mean_age_resident + 
                  log_prop_pos_HCW + prop_asympto_pos_HCW + prop_asympto_pos_res, 
                data=DS_institution)
  summary(m10)
  (est_m10 <- cbind(Estimate = coef(m10), confint(m10)))
  exp(est_m10)
  
  get_p_value_multi(m10, 2)
  get_p_value_multi(m10, 3)
  get_p_value_multi(m10, 4)
  get_p_value_multi(m10, 5)
  get_p_value_multi(m10, 6)
  get_p_value_multi(m10, 7)
  get_p_value_multi(m10, 8)
  
  # Check for missing values
  sum(is.na(DS_institution$n_total_bed))
  nrow(DS_institution[DS_institution$n_total_bed==0, ])
  sum(is.na(DS_institution$n_rvt_bed))
  sum(is.na(DS_institution$ratio_HCW_res))
  sum(is.na(DS_institution$mean_age_resident))
  sum(is.na(DS_institution$mean_age_worker))
  sum(is.na(DS_institution$prop_pos_HCW))
  sum(is.na(DS_institution$prop_asympto_pos_HCW))
  sum(is.na(DS_institution$prop_asympto_pos_res))
  sum(is.na(DS_institution$status))
      