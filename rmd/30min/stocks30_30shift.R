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

stocks30 <- read.table("data30_30shift.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # READ DATA
stocks30$dt_30min <- as.POSIXct(stocks30$dt_30min, tz="EST") # CONVERT DT TO POSIX
col60 <- c("season_workday_30min", "season_month_30min","tf1_30min", "tf2_30min", "tf3_30min", "tf4_30min", "tf5_30min", "tf6_daily") 
stocks30[col60] <- lapply(stocks30[col60], as.factor) # CONVERT CAT VARs TO FACTORs

stocks30$stocks_30min_diff = stocks30$stocks_30min %>% diff() %>% append(NA, 0) # Difference for moving averages
stocks30$stocks_30min_diff_ln = stocks30$stocks_30min %>% log() %>% diff() %>% append(NA, 0) # Log then difference for variance stabilization

stocks30$day_count = stocks30$dt_30min %>% date() %>% as.integer() - stocks30$dt_30min %>% date() %>% as.integer() %>% min() + 1 # Counting the days

stocks30$morality = 
  stocks30$care_p_30min*stocks30$care_sent_30min + 
  stocks30$fairness_p_30min*stocks30$fairness_sent_30min +
  stocks30$loyalty_p_30min*stocks30$loyalty_sent_30min +
  stocks30$authority_p_30min*stocks30$authority_sent_30min +
  stocks30$sanctity_p_30min*stocks30$sanctity_sent_30min

stocks30$morality_lag = lag(stocks30$morality) # lag morality index

stocks30$care_p_30min_lag = lag(stocks30$care_p_30min) # lag moral probabilities
stocks30$fairness_p_30min_lag = (stocks30$fairness_p_30min)
stocks30$loyalty_p_30min_lag = lag(stocks30$loyalty_p_30min)
stocks30$authority_p_30min_lag = lag(stocks30$authority_p_30min)
stocks30$sanctity_p_30min_lag = lag(stocks30$sanctity_p_30min)

stocks30$care_sent_30min_lag = lag(stocks30$care_sent_30min) # lag moral sentiments
stocks30$fairness_sent_30min_lag = lag(stocks30$fairness_sent_30min)
stocks30$loyalty_sent_30min_lag = lag(stocks30$loyalty_sent_30min)
stocks30$authority_sent_30min_lag = lag(stocks30$authority_sent_30min)
stocks30$sanctity_sent_30min_lag = lag(stocks30$sanctity_sent_30min)

stocks30$care_lag = stocks30$care_p_30min_lag * stocks30$care_sent_30min_lag # lag probability*sentiments
stocks30$fairness_lag = stocks30$fairness_p_30min_lag * stocks30$fairness_sent_30min_lag
stocks30$loyalty_lag = stocks30$loyalty_p_30min_lag * stocks30$loyalty_sent_30min_lag
stocks30$authority_lag = stocks30$authority_p_30min_lag * stocks30$authority_sent_30min_lag
stocks30$sanctity_lag = stocks30$sanctity_p_30min_lag * stocks30$sanctity_sent_30min_lag

stocks30_ordered = stocks30[, c(1, 24, 2:4, 16:21, 5, 22, 23, 25, 6:15, 27:36, 26, 37:41)] # REORDER COLUMNS
stocks30_ordered %>% colnames() # DISPLAY COL NAMES
stocks30ts = ts(stocks30_ordered) # MAKE TIME SERIES

plot(stocks30ts[,"stocks_30min"]) # PLOT INITIAL DATA

acf(stocks30_ordered$stocks_30min, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR NON-TRANSFORMED DATA
acf(stocks30_ordered$stocks_30min, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR NON-TRANSFORMED DATA

acf(stocks30_ordered$stocks_30min_diff_ln, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR TRANSFORMED DATA
acf(stocks30_ordered$stocks_30min_diff_ln, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR TRANSFORMED DATA

plot(stocks30ts[,"stocks_30min_diff_ln"]) # UNSTABLE VARIANCE IN CONTRACTION AND RECOVERY PERIOD - NEED GARCH MODEL

hist(stocks30_ordered[16:25]) # PROB & SENT
hist(stocks30_ordered[26:35]) # PROB & SENT LAG
hist(stocks30_ordered[, c(15,36)]) # MORALITY & MORALITY LAG
hist(stocks30_ordered[37:41]) # FOUNDATIONS LAG
hist(stocks30_ordered[14]) # TRANSFORMED STOCKS

# VIOLIN
plot_violin <- function(input) { 
  return(input + geom_violin(trim = FALSE) + stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "pointrange", color = "black"))}

# DOUBLE VIOLIN
plot_violin2 <- function(input) {
  return(input + geom_violin(aes(color = tf2_30min), trim = FALSE,position = position_dodge(0.9)) + geom_boxplot(aes(color = tf2_30min),width = 0.60, position = position_dodge(0.9)) + scale_color_manual(values = c("#00AFBB","#E7B800")))}

# BOXPLOT
plot_boxplot <- function(input) {
  return(input +  geom_boxplot(notch = TRUE, fill = "lightgray") +stat_summary(fun.y = mean, geom = "point",shape = 18, size = 2.5, color = "#FC4E07"))}

# SCATTERPLOT
plot_scatter <- function(input) {
  return(ggplot(stocks30_ordered, aes(input, stocks_30min_diff_ln, color=tf2_30min)) + geom_point() + geom_smooth(method=lm) +scale_color_manual(values = c('#999999','#E69F00')) + theme(legend.position=c(0,1), legend.justification=c(0,1)))}

# X DENSITY
plot_xdensity <- function(input) {
  return(ggplot(stocks30_ordered, aes(input, fill=tf2_30min)) +geom_density(alpha=.5) +scale_fill_manual(values = c('#999999','#E69F00')) + theme(legend.position = "none") )}

# Y DENSITY
plot_ydensity <- function() {return(ggplot(stocks30_ordered, aes(stocks_30min_diff_ln, fill=tf2_30min)) +geom_density(alpha=.5) +scale_fill_manual(values = c('#999999','#E69F00')) +theme(legend.position = "none") )}

# BLANK PLOT
plot_blank <- function() {
  return(ggplot() + geom_blank(aes(1,1)) +theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank()))}

e <- ggplot(stocks30, aes(x = tf2_30min, y = morality_lag)) 
plot_boxplot(e) # MORALITY BOX PLOT
plot_violin(e) # MORALITY VIOLIN PLOT

stocks30_foundations_long = melt(stocks30_ordered[, c(7,37:41)]) 
e1 <-  ggplot(stocks30_foundations_long, aes(x = variable, y = value))
plot_violin(e1) # FOUNDATIONS VIOLIN PLOT
plot_boxplot(e1) # FOUNDATIONS BOX PLOT
plot_violin2(e1) # FOUNDATIONS BY TF VIOLIN PLOT

scatterPlot <- plot_scatter(stocks30_ordered$morality_lag)
xdensity <- plot_xdensity(stocks30_ordered$morality_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # MORALITY PLOT

scatterPlot <- plot_scatter(stocks30_ordered$care_lag)
xdensity <- plot_xdensity(stocks30_ordered$care_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # CARE PLOT

scatterPlot <- plot_scatter(stocks30_ordered$fairness_lag)
xdensity <- plot_xdensity(stocks30_ordered$fairness_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # FAIRNESS PLOT

scatterPlot <- plot_scatter(stocks30_ordered$loyalty_lag)
xdensity <- plot_xdensity(stocks30_ordered$loyalty_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # LOYALTY PLOT

scatterPlot <- plot_scatter(stocks30_ordered$authority_lag)
xdensity <- plot_xdensity(stocks30_ordered$authority_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # AUTHORITY PLOT

scatterPlot <- plot_scatter(stocks30_ordered$sanctity_lag)
xdensity <- plot_xdensity(stocks30_ordered$sanctity_lag)
ydensity <- plot_ydensity()
blankPlot <- plot_blank()
grid.arrange(xdensity, blankPlot, scatterPlot, ydensity, ncol=2, nrow=2, widths=c(4, 1.4), heights=c(1.4, 4)) # SANCTITY PLOT

omtted <- na.omit(stocks30_ordered)

outlier_rm_IQR <- function(data, df_str, col_str, threshold){
  Q <- quantile(data, probs=c(.25, .75), na.rm = FALSE) # 25/75 QUANTILES AFTER REMOVING ROW 1 (NA row)
  iqr <- IQR(data) # IQR AFTER REMOVING ROW 1 (NA row)
  upper <- Q[2]+threshold*iqr # Upper Range for outliers
  lower <- Q[1]-threshold*iqr # Lower Range for outliers
  df <- get(df_str)
  column <- get(df_str)[col_str]
  a <- subset.data.frame(df, column > lower)
  b <- subset.data.frame(df, column < upper)
  return(intersect(a,b))
}

a <- outlier_rm_IQR(omtted$morality_lag[-1], "stocks30_ordered", "morality_lag", 2.5)
b <- outlier_rm_IQR(omtted$stocks_30min_diff_ln[-1], "stocks30_ordered", "stocks_30min_diff_ln", 2.5)

stocks30_outrm <- intersect(a,b)
stocks30_outrm_moralityonly <- outlier_rm_IQR(omtted$morality_lag[-1], "stocks30_ordered", "morality_lag", 2.5)

stocks30.model.lm <- lm(stocks_30min_diff_ln ~ tf2_30min * morality_lag,data = omtted)

stocks30.model.lm.foundations <- lm(stocks_30min_diff_ln ~ tf2_30min * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), data = omtted)

stocks30.model.lme.null <-lmer(stocks_30min_diff_ln ~ 1 + (1|day_count), data = omtted, REML=TRUE)

stocks30.model.lme <- lmer(stocks_30min_diff_ln ~ (1 + season_intraday_30min + tf2_30min*morality_lag + (1 + season_intraday_30min | day_count)), data = omtted, REML = TRUE)

stocks30.model.lme.foundations <- lmer(stocks_30min_diff_ln ~  (1 + season_intraday_30min + tf2_30min*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_30min | day_count)), data = omtted, REML = TRUE)

stocks30.model.lm.outrm <- lm(stocks_30min_diff_ln ~ tf2_30min * morality_lag,data = stocks30_outrm_moralityonly)

stocks30.model.lm.foundations.outrm <- lm(stocks_30min_diff_ln ~ tf2_30min * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), data = stocks30_outrm_moralityonly)

stocks30.model.lme.null.outrm <-lmer(stocks_30min_diff_ln ~ 1 + (1|day_count), data = stocks30_outrm_moralityonly, REML=TRUE)

stocks30.model.lme.outrm <- lmer(stocks_30min_diff_ln ~ (1 + season_intraday_30min + tf2_30min*morality_lag + (1 + season_intraday_30min | day_count)), data = stocks30_outrm_moralityonly, REML = TRUE)

stocks30.model.lme.foundations.outrm <- lmer(stocks_30min_diff_ln ~  (1 + season_intraday_30min + tf2_30min*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_30min | day_count)), data = stocks30_outrm_moralityonly, REML = TRUE)

Anova(stocks30.model.lm, type="III", test="F")
Anova(stocks30.model.lm.foundations, type="III", test="F")
Anova(stocks30.model.lme.null, type="III", test="F")
Anova(stocks30.model.lme, type="III", test="F")
Anova(stocks30.model.lme.foundations, type="III", test="F")

anova(stocks30.model.lm, stocks30.model.lm.foundations)
anova(stocks30.model.lme.null, stocks30.model.lme, stocks30.model.lme.foundations)
Anova(stocks30.model.lm.outrm, type="III", test="F")
Anova(stocks30.model.lm.foundations.outrm, type="III", test="F")
Anova(stocks30.model.lme.null.outrm, type="III", test="F")
Anova(stocks30.model.lme.outrm, type="III", test="F")
Anova(stocks30.model.lme.foundations.outrm, type="III", test="F")

anova(stocks30.model.lm.outrm, stocks30.model.lm.foundations.outrm)
anova(stocks30.model.lme.null.outrm, stocks30.model.lme.outrm, stocks30.model.lme.foundations.outrm)

summ(stocks30.model.lm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks30.model.lme, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks30.model.lm.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks30.model.lme.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks30.model.lm.foundations, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks30.model.lme.foundations, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

summ(stocks30.model.lm.foundations.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)
summ(stocks30.model.lme.foundations.outrm, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=3)

plot1 <- interact_plot(stocks30.model.lm, pred = morality_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.outrm, pred = morality_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks30.model.lme, pred = morality_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lme.outrm, pred = morality_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)


plot1 <- interact_plot(stocks30.model.lm.foundations, pred = care_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.foundations.outrm, pred = care_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks30.model.lm.foundations, pred = fairness_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.foundations.outrm, pred = fairness_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks30.model.lm.foundations, pred = loyalty_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.foundations.outrm, pred = loyalty_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks30.model.lm.foundations, pred = authority_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.foundations.outrm, pred = authority_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)

plot1 <- interact_plot(stocks30.model.lm.foundations, pred = sanctity_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
plot2 <- interact_plot(stocks30.model.lm.foundations.outrm, pred = sanctity_lag, modx = tf2_30min, plot.points = TRUE, linearity.check = FALSE) + ylim(-0.04,0.04)
gridExtra::grid.arrange(plot1, plot2, ncol=2)
