knitr::opts_chunk$set(echo = TRUE,
                      collapse = FALSE,
                      warning = FALSE, 
                      tidy = TRUE)
options(width=120)

library(RcmdrMisc)
library(jtools)
library(timeSeries)
library(astsa)
library(lubridate)
library(zoo)
library(tsbox)

library(lme4)
library(reghelper)
library(RcmdrMisc)
library(interactions)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

library(ggpubr)
theme_set(theme_pubclean())

setwd("/home/pw/Projects/mfstocks/data/csv/60shift")
stocks15 <- read.table("data15_60shift.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # READ DATA
stocks15$dt_15min <- as.POSIXct(stocks15$dt_15min, tz="EST") # CONVERT DT TO POSIX
col60 <- c("season_workday_15min", "season_month_15min","tf1_15min", "tf2_15min", "tf3_15min", "tf4_15min", "tf5_15min", "tf6_15min") 
stocks15[col60] <- lapply(stocks15[col60], as.factor) # CONVERT CAT VARs TO FACTORs

stocks15$stocks_15min_diff = stocks15$stocks_15min %>% diff() %>% append(NA, 0) # Difference for moving averages
stocks15$stocks_15min_diff_ln = stocks15$stocks_15min %>% log() %>% diff() %>% append(NA, 0) # Log then difference for variance stabilization

stocks15$day_count = stocks15$dt_15min %>% date() %>% as.integer() - stocks15$dt_15min %>% date() %>% as.integer() %>% min() + 1 # Counting the days

stocks15$morality = 
  stocks15$care_p_15min*stocks15$care_sent_15min + 
  stocks15$fairness_p_15min*stocks15$fairness_sent_15min +
  stocks15$loyalty_p_15min*stocks15$loyalty_sent_15min +
  stocks15$authority_p_15min*stocks15$authority_sent_15min +
  stocks15$sanctity_p_15min*stocks15$sanctity_sent_15min

stocks15$morality_lag = lag(stocks15$morality) # lag morality index

stocks15$care_p_15min_lag = lag(stocks15$care_p_15min) # lag moral probabilities
stocks15$fairness_p_15min_lag = (stocks15$fairness_p_15min)
stocks15$loyalty_p_15min_lag = lag(stocks15$loyalty_p_15min)
stocks15$authority_p_15min_lag = lag(stocks15$authority_p_15min)
stocks15$sanctity_p_15min_lag = lag(stocks15$sanctity_p_15min)

stocks15$care_sent_15min_lag = lag(stocks15$care_sent_15min) # lag moral sentiments
stocks15$fairness_sent_15min_lag = lag(stocks15$fairness_sent_15min)
stocks15$loyalty_sent_15min_lag = lag(stocks15$loyalty_sent_15min)
stocks15$authority_sent_15min_lag = lag(stocks15$authority_sent_15min)
stocks15$sanctity_sent_15min_lag = lag(stocks15$sanctity_sent_15min)

stocks15$care_lag = stocks15$care_p_15min_lag * stocks15$care_sent_15min_lag # lag probability*sentiments
stocks15$fairness_lag = stocks15$fairness_p_15min_lag * stocks15$fairness_sent_15min_lag
stocks15$loyalty_lag = stocks15$loyalty_p_15min_lag * stocks15$loyalty_sent_15min_lag
stocks15$authority_lag = stocks15$authority_p_15min_lag * stocks15$authority_sent_15min_lag
stocks15$sanctity_lag = stocks15$sanctity_p_15min_lag * stocks15$sanctity_sent_15min_lag

stocks15_ordered = stocks15[, c(1, 24, 2:4, 16:21, 5, 22, 23, 25, 6:15, 27:36, 26, 37:41)] # REORDER COLUMNS
stocks15_ordered %>% colnames() # DISPLAY COL NAMES
stocks15ts = ts(stocks15_ordered) # MAKE TIME SERIES

plot(stocks15ts[,"stocks_15min"]) # PLOT INITIAL DATA

acf(stocks15_ordered$stocks_15min, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR NON-TRANSFORMED DATA
acf(stocks15_ordered$stocks_15min, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR NON-TRANSFORMED DATA

acf(stocks15_ordered$stocks_15min_diff_ln, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR TRANSFORMED DATA
acf(stocks15_ordered$stocks_15min_diff_ln, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR TRANSFORMED DATA

plot(stocks15ts[,"stocks_15min_diff_ln"]) # UNSTABLE VARIANCE IN CONTRACTION AND RECOVERY PERIOD - NEED GARCH MODEL

hist(stocks15_ordered[16:25]) # PROB & SENT
hist(stocks15_ordered[26:35]) # PROB & SENT LAG
hist(stocks15_ordered[, c(15,36)]) # MORALITY & MORALITY LAG
hist(stocks15_ordered[37:41]) # FOUNDATIONS LAG
hist(stocks15_ordered[14]) # TRANSFORMED STOCKS

# VIOLIN
plot_violin <- function(input) { 
  return(input + geom_violin(trim = FALSE) + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black"))}

# DOUBLE VIOLIN
plot_violin2 <- function(input) {
  return(input + geom_violin(aes(color = tf2_15min), trim = FALSE,position = position_dodge(0.9)) + geom_boxplot(aes(color = tf2_15min),width = 0.60, position = position_dodge(0.9)) + scale_color_manual(values = c("#00AFBB","#E7B800")))}

# BOXPLOT
plot_boxplot <- function(input) {
  return(input +  geom_boxplot(notch = TRUE, fill = "lightgray") +stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07"))}

# SCATTERPLOT
plot_scatter <- function(input) {
  return(ggplot(stocks15_ordered, aes(input, stocks_15min_diff_ln, color=tf2_15min)) + geom_point() + geom_smooth(method=lm) +scale_color_manual(values = c('#999999','#E69F00')) + theme(legend.position=c(0,1), legend.justification=c(0,1)))}

# X DENSITY
plot_xdensity <- function(input) {
  return(ggplot(stocks15_ordered, aes(input, fill=tf2_15min)) +geom_density(alpha=.5) +scale_fill_manual(values = c('#999999','#E69F00')) + theme(legend.position = "none") )}

# Y DENSITY
plot_ydensity <- function() {return(ggplot(stocks15_ordered, aes(stocks_15min_diff_ln, fill=tf2_15min)) +geom_density(alpha=.5) +scale_fill_manual(values = c('#999999','#E69F00')) +theme(legend.position = "none") )}

# BLANK PLOT
plot_blank <- function() {
  return(ggplot() + geom_blank(aes(1,1)) +theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()))}

e <- ggplot(stocks15, aes(x = tf2_15min, y = morality_lag)) 
plot_boxplot(e) # MORALITY BOX PLOT
plot_violin(e) # MORALITY VIOLIN PLOT

stocks15_foundations_long = melt(stocks15_ordered[, c(7,37:41)]) 
e1 <-  ggplot(stocks15_foundations_long, aes(x = variable, y = value))
plot_violin(e1) # FOUNDATIONS VIOLIN PLOT
plot_boxplot(e1) # FOUNDATIONS BOX PLOT
plot_violin2(e1) # FOUNDATIONS BY TF VIOLIN PLOT

scatterPlot <- plot_scatter(stocks15_ordered$morality_lag)
xdensity <- plot_xdensity(stocks15_ordered$morality_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # MORALITY PLOT

scatterPlot <- plot_scatter(stocks15_ordered$care_lag)
xdensity <- plot_xdensity(stocks15_ordered$care_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # CARE PLOT

scatterPlot <- plot_scatter(stocks15_ordered$fairness_lag)
xdensity <- plot_xdensity(stocks15_ordered$fairness_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # FAIRNESS PLOT

scatterPlot <- plot_scatter(stocks15_ordered$loyalty_lag)
xdensity <- plot_xdensity(stocks15_ordered$loyalty_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # LOYALTY PLOT

scatterPlot <- plot_scatter(stocks15_ordered$authority_lag)
xdensity <- plot_xdensity(stocks15_ordered$authority_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # AUTHORITY PLOT

scatterPlot <- plot_scatter(stocks15_ordered$sanctity_lag)
xdensity <- plot_xdensity(stocks15_ordered$sanctity_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # SANCTITY PLOT

omtted15 <- na.omit(stocks15_ordered)

outlier_rm_IQR <- function(data, df_str, col_str, threshold){
  Q <- quantile(data, probs=c(.25, .75), na.rm = FALSE) # Q[1]= 25 QUANTILE; Q[2]=75 QUANTILE OF MORALITY AFTER REMOVING ROW 1 (NA row)
  iqr <- IQR(data) # IQR AFTER REMOVING ROW 1 (NA row)
  upper <- Q[2]+threshold*iqr # Upper Range for outliers
  lower <- Q[1]-threshold*iqr # Lower Range for outliers
  df <- get(df_str) # assign the dataframe to df while in string format
  column <- get(df_str)[col_str] # specifically assignment a column from the dataframe to column variable
  a <- subset.data.frame(df, column > lower) 
  b <- subset.data.frame(df, column < upper)
  return(intersect(a,b))
}

stocks15_outrm_moralityonly <- outlier_rm_IQR(omtted15$morality_lag[-1], "omtted15", "morality_lag", 2.5)

stocks15.model.lm <- lm(stocks_15min_diff_ln ~ tf2_15min * morality_lag,data = omtted15)

stocks15.model.lm.foundations <- lm(stocks_15min_diff_ln ~ tf2_15min * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), data = omtted15)

stocks15.model.lme.null <-lmer(stocks_15min_diff_ln ~ 1 + (1|day_count), data = omtted15, REML=TRUE)

stocks15.model.lme <- lmer(stocks_15min_diff_ln ~ (1 + season_intraday_15min + tf2_15min*morality_lag + (1 + season_intraday_15min | day_count)), data = omtted15, REML = TRUE)

stocks15.model.lme.foundations <- lmer(stocks_15min_diff_ln ~  (1 + season_intraday_15min + tf2_15min*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_15min | day_count)), data = omtted15, REML = TRUE)

stocks15.model.lm.outrm <- lm(stocks_15min_diff_ln ~ tf2_15min * morality_lag,data = stocks15_outrm_moralityonly)

stocks15.model.lm.foundations.outrm <- lm(stocks_15min_diff_ln ~ tf2_15min * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), data = stocks15_outrm_moralityonly)

stocks15.model.lme.null.outrm <-lmer(stocks_15min_diff_ln ~ 1 + (1|day_count), data = stocks15_outrm_moralityonly, REML=TRUE)

stocks15.model.lme.outrm <- lmer(stocks_15min_diff_ln ~ (1 + season_intraday_15min + tf2_15min*morality_lag + (1 + season_intraday_15min | day_count)), data = stocks15_outrm_moralityonly, REML = TRUE)

stocks15.model.lme.foundations.outrm <- lmer(stocks_15min_diff_ln ~  (1 + season_intraday_15min + tf2_15min*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_15min | day_count)), data = stocks15_outrm_moralityonly, REML = TRUE)

Anova(stocks15.model.lm, type="III", test="F")
Anova(stocks15.model.lm.foundations, type="III", test="F")
Anova(stocks15.model.lme.null, type="III", test="F")
Anova(stocks15.model.lme, type="III", test="F")
Anova(stocks15.model.lme.foundations, type="III", test="F")

anova(stocks15.model.lm, stocks15.model.lm.foundations)
anova(stocks15.model.lme.null, stocks15.model.lme, stocks15.model.lme.foundations)

Anova(stocks15.model.lm.outrm, type="III", test="F")
Anova(stocks15.model.lm.foundations.outrm, type="III", test="F")
Anova(stocks15.model.lme.null.outrm, type="III", test="F")
Anova(stocks15.model.lme.outrm, type="III", test="F")
Anova(stocks15.model.lme.foundations.outrm, type="III", test="F")

anova(stocks15.model.lm.outrm, stocks15.model.lm.foundations.outrm)
anova(stocks15.model.lme.null.outrm, stocks15.model.lme.outrm, stocks15.model.lme.foundations.outrm)

summ(stocks15.model.lm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks15.model.lme, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks15.model.lm.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks15.model.lme.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks15.model.lm.foundations, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks15.model.lme.foundations, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks15.model.lm.foundations.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks15.model.lme.foundations.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

plot1 <- interact_plot(stocks15.model.lm, pred = morality_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.outrm, pred = morality_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks15.model.lme, pred = morality_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lme.outrm, pred = morality_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)


plot1 <- interact_plot(stocks15.model.lm.foundations, pred = care_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.foundations.outrm, pred = care_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks15.model.lm.foundations, pred = fairness_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.foundations.outrm, pred = fairness_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks15.model.lm.foundations, pred = loyalty_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.foundations.outrm, pred = loyalty_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks15.model.lm.foundations, pred = authority_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.foundations.outrm, pred = authority_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks15.model.lm.foundations, pred = sanctity_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks15.model.lm.foundations.outrm, pred = sanctity_lag, modx = tf2_15min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)
