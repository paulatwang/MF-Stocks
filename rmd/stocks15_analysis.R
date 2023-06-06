# This code is an analysis of the data at 15 minute intervals.
# This code includes the following steps:
## STEP 0: LOAD LIBRARIES AND DATA   
## STEP 1: PREPOCESSING
### 1.1: Difference and log-transform stock prices
### 1.2: Create morality index
### 1.3: Add lags
### 1.4: Reorder and make time series
### 1.5: Check serial dependencies; ACF & PACF
## STEP 2: DATA EXPLORATION
### 2.1: Histograms
### 2.2: Box & Violine plots
### 2.3: Scatterplots
## STEP 3: FITTING AND EVALUATING MODEL
### 3.1: Removing outliers
### 3.2: Fitting and summarizing models
### 3.3: Model comparisons
### 3.4: Standardized betas
### 3.6: Interaction plot for morality model
### 3.7: Interaction plots for foundation models

knitr::opts_chunk$set(echo = TRUE,
                      collapse = FALSE,
                      warning = FALSE, 
                      tidy = TRUE)
options(width=120)


################################################################################
# STEP 0: LOAD DATA 

## Read data

## Set your working directory if needed, otherwise move to the next line
setwd("/home/pw/Projects/mfstocks/data/csv/60shift") 

stocks15 <- read.table("data15_60shift.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
## Convert datetime variable to POSIX, which will allow us to create a time series later
stocks15$dt_15min <- as.POSIXct(stocks15$dt_15min, tz="EST")
## Factors are used to represent categorical data in statistical analysis. They are stored as unique integers (not strings) with unique labels.
col15 <- c("season_workday_15min", "season_month_15min",
           "tf1_15min", "tf2_15min", "tf3_15min", "tf4_15min", "tf5_15min", "tf6_15min") 
stocks15[col15] <- lapply(stocks15[col15], as.factor) 

################################################################################
# STEP 1: PREPOCESSING
library(RcmdrMisc)
library(lubridate) # date()
library(dplyr) # %>% function
library(reshape2) # melt() 

## 1.1:  Difference and log-transform stock prices

### Difference for moving averages
stocks15$stocks_15min_diff = stocks15$stocks_15min %>% diff() %>% append(NA, 0) 
### Log then difference for variance stabilization
stocks15$stocks_15min_diff_ln = stocks15$stocks_15min %>% log() %>% diff() %>% append(NA, 0) 
### Create variable for counting the days
stocks15$day_count = 
  stocks15$dt_15min %>% date() %>% as.integer() - 
  stocks15$dt_15min %>% date() %>% as.integer() %>% min() + 1 


## 1.2: Create morality index from the sentiment-weighted probability of each foundation

stocks15$morality = 
  stocks15$care_p_15min*stocks15$care_sent_15min + 
  stocks15$fairness_p_15min*stocks15$fairness_sent_15min +
  stocks15$loyalty_p_15min*stocks15$loyalty_sent_15min +
  stocks15$authority_p_15min*stocks15$authority_sent_15min +
  stocks15$sanctity_p_15min*stocks15$sanctity_sent_15min


## 1.3: Add lags

### Lag morality index
stocks15$morality_lag = lag(stocks15$morality)
### Lag foundation probabilities
stocks15$care_p_15min_lag = lag(stocks15$care_p_15min) 
stocks15$fairness_p_15min_lag = (stocks15$fairness_p_15min)
stocks15$loyalty_p_15min_lag = lag(stocks15$loyalty_p_15min)
stocks15$authority_p_15min_lag = lag(stocks15$authority_p_15min)
stocks15$sanctity_p_15min_lag = lag(stocks15$sanctity_p_15min)
### Lag foundation sentiments
stocks15$care_sent_15min_lag = lag(stocks15$care_sent_15min) 
stocks15$fairness_sent_15min_lag = lag(stocks15$fairness_sent_15min)
stocks15$loyalty_sent_15min_lag = lag(stocks15$loyalty_sent_15min)
stocks15$authority_sent_15min_lag = lag(stocks15$authority_sent_15min)
stocks15$sanctity_sent_15min_lag = lag(stocks15$sanctity_sent_15min)
### Lag sentiment-weighted probabilities of foundations
stocks15$care_lag = stocks15$care_p_15min_lag * stocks15$care_sent_15min_lag 
stocks15$fairness_lag = stocks15$fairness_p_15min_lag * stocks15$fairness_sent_15min_lag
stocks15$loyalty_lag = stocks15$loyalty_p_15min_lag * stocks15$loyalty_sent_15min_lag
stocks15$authority_lag = stocks15$authority_p_15min_lag * stocks15$authority_sent_15min_lag
stocks15$sanctity_lag = stocks15$sanctity_p_15min_lag * stocks15$sanctity_sent_15min_lag


## 1.4: Reorder and make time series

colnames(stocks15)
### Reorder columns
stocks15_ordered = stocks15[, c(1, 24, 2:4, 16:21, 5, 22, 23, 25, 6:15, 27:36, 26, 37:41)] 
### Make time series
stocks15ts = ts(stocks15_ordered)


## 1.5: Check serial dependencies; ACF & PACF

### Plot time series of raw stock prices
plot(stocks15ts[,"stocks_15min"])
### Plot ACF and PACF for non-transformed data
acf(stocks15_ordered$stocks_15min, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) 
acf(stocks15_ordered$stocks_15min, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass)
### Plot ACF and PACF for transformed data
acf(stocks15_ordered$stocks_15min_diff_ln, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass)
acf(stocks15_ordered$stocks_15min_diff_ln, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) 
### Plot time series of stock prices after differencing and log-transformation
### Mildly unstable variance in contraction and recovery period
plot(stocks15ts[,"stocks_15min_diff_ln"]) 


################################################################################
# STEP 2: DATA EXPLORATION
library(ggplot2) 
library(gridExtra) # gridExtra
library(ggpubr) 
theme_set(theme_pubclean())

## 2.1: Frequency histograms

### Probabilities and sentiments of foundations
hist(stocks15_ordered[16:25]) 
### Lagged probabilities and sentiments of foundations
hist(stocks15_ordered[26:35]) 
### Morality and lagged morality
hist(stocks15_ordered[, c(15,36)]) 
### Sentiment-weighted probabilities of foundations lagged
hist(stocks15_ordered[37:41]) 
### Transformed stock prices
hist(stocks15_ordered[14]) 


## 2.2: Box & Violine plots

### Define plot functions
#### BOXPLOT
plot_boxplot <- function(input, x_labels) {
  return(input +  
           geom_boxplot(notch = TRUE, fill = "lightgray") +
           stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07") + 
           labs(x="Economic Period", y = "Morality Score") +
           scale_x_discrete(labels=x_labels) )}
#### VIOLIN
plot_violin <- function(input, x_labels) { 
  return(input + 
           geom_violin(trim = FALSE) + 
           stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black") +
           labs(x="Economic Period", y = "Morality Score") +
           scale_x_discrete(labels=x_labels) )}
#### DOUBLE VIOLIN
plot_violin2 <- function(input, x_labels) {
  return(input + 
           geom_violin(aes(color = tf2_15min), trim = FALSE,position = position_dodge(0.9)) + 
           geom_boxplot(aes(color = tf2_15min), width = 0.15, position = position_dodge(0.9)) + 
           scale_color_manual(labels = c("High", "Low"), values = c("#00AFBB","#E7B800")) + 
           scale_x_discrete(labels=x_labels) +
           theme(legend.position="right") + 
           labs(x="Moral Foundations", y = "Foundation Score",colour="Economic Period") )}
### Plot functions
### MORALITY
e <- ggplot(stocks15, aes(x = tf2_15min, y = morality_lag))
plot_boxplot(e, c("High","Low")) 
plot_violin(e, c("High","Low"))
### FOUNDATIONS
stocks15_foundations_long = melt(stocks15_ordered[, c(7,37:41)]) 
e1 <-  ggplot(stocks15_foundations_long, aes(x = variable, y = value))
plot_violin(e1, c("Care","Fairness","Loyalty","Authority", "Sanctity")) 
plot_boxplot(e1, c("Care","Fairness","Loyalty","Authority", "Sanctity")) 
plot_violin2(e1, c("Care","Fairness","Loyalty","Authority", "Sanctity")) 


## 2.3: Scatterplots

### Define plot functions
#### SCATTERPLOT
plot_scatter <- function(input, x_label) {
  return(ggplot(stocks15_ordered, aes(input, stocks_15min_diff_ln, color=tf2_15min)) + 
           geom_point() + 
           geom_smooth(method=lm) +
           scale_color_manual(labels = c("High", "Low"), values = c('#00AFBB','#E7B800')) + 
           theme(legend.position=c(0,1), legend.justification=c(0,1)) + 
           labs(x=x_label, y = "Market Movement", color = "Economic Period") )}
#### X DENSITY
plot_xdensity <- function(input) {
  return(ggplot(stocks15_ordered, aes(input, fill=tf2_15min)) +
           geom_density(alpha=.5) +
           scale_fill_manual(values = c('#00AFBB','#E7B800')) + 
           theme(legend.position = "none")  + 
           labs(x = "") )}
#### Y DENSITY
plot_ydensity <- function() {
  return(ggplot(stocks15_ordered, aes(stocks_15min_diff_ln, fill=tf2_15min)) +
           geom_density(alpha=.5) +
           scale_fill_manual(values = c('#00AFBB','#E7B800')) +
           theme(legend.position = "none")  + 
           labs(x = "") )}
### Plot functions
### MORALITY
scatterPlot <- plot_scatter(stocks15_ordered$morality_lag, "Morality")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$morality_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) 
### FOUNDATIONS
#### Care
scatterPlot <- plot_scatter(stocks15_ordered$care_lag, "Care")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$care_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) 
#### Fairness
scatterPlot <- plot_scatter(stocks15_ordered$fairness_lag, "Fairness")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$fairness_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) 
#### Loyalty
scatterPlot <- plot_scatter(stocks15_ordered$loyalty_lag, "Loyalty")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$loyalty_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))
#### Authority
scatterPlot <- plot_scatter(stocks15_ordered$authority_lag, "Authority")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$authority_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) 
#### Sanctity
scatterPlot <- plot_scatter(stocks15_ordered$sanctity_lag, "Sanctity")
scatterPlot2 <- scatterPlot + theme(legend.position = "none")
xdensity <- plot_xdensity(stocks15_ordered$sanctity_lag)
ydensity <- plot_ydensity() + coord_flip()
legend <- get_legend(scatterPlot)
grid.arrange(xdensity, legend, scatterPlot2, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4))


################################################################################
# STEP 3: FITTING AND EVALUATING MODEL
library(jtools) # summ()
library(lme4) # lme models
library(interactions) # interact()


## 3.1: Removing outliers
### Define function  
outlier_rm_IQR <- function(data, df_str, col_str, threshold){
  data <- na.omit(data)
  Q <- quantile(data, probs=c(.25, .75), na.rm = FALSE) # 25/75 QUANTILES AFTER REMOVING ROW 1 (NA row)
  iqr <- IQR(data) # IQR AFTER REMOVING ROW 1 (NA row)
  upper <- Q[2]+threshold*iqr # Upper Range for outliers
  lower <- Q[1]-threshold*iqr # Lower Range for outliers
  df <- get(df_str)
  column <- get(df_str)[col_str]
  a <- subset.data.frame(df, column > lower)
  b <- subset.data.frame(df, column < upper)
  return(intersect(a,b))}
### Run function
stocks15_outrm_moralityonly <- outlier_rm_IQR(stocks15_ordered$morality_lag[-1], "stocks15_ordered", "morality_lag", 2.5)


## 3.2: Fitting and summarizing models

### Linear model for morality
stocks15.model.lm.outrm <- lm(
  stocks_15min_diff_ln ~ tf2_15min * morality_lag,
  data = stocks15_outrm_moralityonly)
Anova(stocks15.model.lm.outrm, type="III", test="F")
### Linear model for foundations
stocks15.model.lm.foundations.outrm <- lm(
  stocks_15min_diff_ln ~ tf2_15min * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), 
  data = stocks15_outrm_moralityonly)
Anova(stocks15.model.lm.foundations.outrm, type="III", test="F")
### Mixed-effect null model
stocks15.model.lme.null.outrm <-lmer(
  stocks_15min_diff_ln ~ 1 + (1|day_count), 
  data = stocks15_outrm_moralityonly, REML=TRUE)
Anova(stocks15.model.lme.null.outrm, type="III", test="F")
### Mixed-effect model for morality
stocks15.model.lme.outrm <- lmer(
  stocks_15min_diff_ln ~ (1 + season_intraday_15min + tf2_15min*morality_lag + (1 + season_intraday_15min | day_count)), 
  data = stocks15_outrm_moralityonly, REML = TRUE)
Anova(stocks15.model.lme.outrm, type="III", test="F")
### Mixed-effect model for foundations
stocks15.model.lme.foundations.outrm <- lmer(
  stocks_15min_diff_ln ~  (1 + season_intraday_15min + 
                              tf2_15min*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + 
                              ( 1 + season_intraday_15min | day_count)), 
  data = stocks15_outrm_moralityonly, REML = TRUE)
Anova(stocks15.model.lme.foundations.outrm, type="III", test="F")


## 3.3: Model comparisons

### Comparisons between linear and mixed-effects models 
##### Morality models
anova(stocks15.model.lme.outrm, stocks15.model.lm.outrm, type="Chisq")
#### Foundations models
anova(stocks15.model.lme.foundations.outrm, stocks15.model.lm.foundations.outrm, type="Chisq")
### Comparisons between morality and foundations models
#### Linear models
anova(stocks15.model.lm.outrm, stocks15.model.lm.foundations.outrm)
#### Mixed-effects models
anova(stocks15.model.lme.null.outrm, stocks15.model.lme.outrm, stocks15.model.lme.foundations.outrm)

## 3.4: Standardized betas
### Morality mixed-effect model
summ(stocks15.model.lme.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
### Foundations mixed-effect model
summ(stocks15.model.lme.foundations.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

## 3.6: Interaction plots
### Define interaction plots
#### Morality plot
plot1 <- interact_plot(stocks15.model.lme.outrm, 
                       pred = morality_lag, 
                       modx = tf2_15min, 
                       plot.points = TRUE, 
                       linearity.check = FALSE,
                       x.label = "Morality",
                       y.label = "Difference in Market Movement",
                       modx.labels = c("High","Low"),
                       legend.main = "Economic Period") +
  ylim(-0.04,0.04) 
plot1a <- plot1 + theme(legend.position = "none")
#### Fairness plot
plot2 <- interact_plot(stocks15.model.lme.foundations.outrm, 
                       pred = fairness_lag, 
                       modx = tf2_15min, 
                       plot.points = TRUE, 
                       linearity.check = FALSE,
                       x.label = "Fairness",
                       y.label = "Difference in Market Movement") +
  ylim(-0.04,0.04) +
  theme(legend.position = "none")
#### Sanctity plot
plot3 <- interact_plot(stocks15.model.lme.foundations.outrm, 
                       pred = sanctity_lag, 
                       modx = tf2_15min, 
                       plot.points = TRUE, 
                       linearity.check = FALSE,
                       x.label = "Sanctity",
                       y.label = "Difference in Market Movement") +
  ylim(-0.04,0.04) +
  theme(legend.position = "none")
### Plot interactions
legend <- get_legend(plot1)
grid.arrange(plot1)
grid.arrange(plot2, plot3, legend, ncol=3, widths=c(3,3,1.5))







