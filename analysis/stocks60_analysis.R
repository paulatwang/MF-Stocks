knitr::opts_chunk$set(echo = TRUE, collapse = FALSE, warning = FALSE, tidy = TRUE)
options(width=120)

setwd("~/Projects/mfstocks/code_and_data/analysis") # data path
stocks60 <- read.table("data_60.csv", header=TRUE, stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)  # READ DATA

stocks60$dt_60 <- as.POSIXct(stocks60$dt_60, tz="EST") # CONVERT DT TO POSIX
col60 <- c("season_workday_60", "season_month_60", "tf2_60") 
stocks60[col60] <- lapply(stocks60[col60], as.factor) # CONVERT CAT VARs TO FACTORs

library(RcmdrMisc)
library(dplyr) # %>% function
library(reshape2) # melt() 
library(lubridate) # date()

stocks60$morality = 
  stocks60$care_p_60*stocks60$care_sent_60 + 
  stocks60$fairness_p_60*stocks60$fairness_sent_60 +
  stocks60$loyalty_p_60*stocks60$loyalty_sent_60 +
  stocks60$authority_p_60*stocks60$authority_sent_60 +
  stocks60$sanctity_p_60*stocks60$sanctity_sent_60

# Stocks 
stocks60$stocks_60_diff = stocks60$stocks_60 %>% diff() %>% append(NA, 0) # Difference for moving averages
stocks60$stocks_60_diff_ln = stocks60$stocks_60 %>% log() %>% diff() %>% append(NA, 0) # Log then difference for variance stabilization
stocks60$day_count = stocks60$dt_60 %>% date() %>% as.integer() - stocks60$dt_60 %>% date() %>% as.integer() %>% min() + 1 # produce day_count predictor

# Morality predictors 
foundations <- c("care","fairness","loyalty","authority","sanctity")
stocks60[foundations] <- stocks60[6:10]*stocks60[11:15]

stocks60$morality_diff =  stocks60$morality %>% diff() %>% append(NA, 0) 
stocks60$care_diff = stocks60$care %>% diff() %>% append(NA, 0)
stocks60$fairness_diff = stocks60$fairness %>% diff() %>% append(NA, 0) 
stocks60$loyalty_diff = stocks60$loyalty %>% diff() %>% append(NA, 0)
stocks60$authority_diff = stocks60$authority %>% diff() %>% append(NA, 0) 
stocks60$sanctity_diff = stocks60$sanctity %>% diff() %>% append(NA, 0)

stocks60 %>% colnames()


# morality
stocks60$morality_lag = lag(stocks60$morality_diff)
stocks60$morality_lead = lead(stocks60$morality_diff)

stocks60[paste(foundations, "_lag", sep="")] = lag(stocks60[27:31])
stocks60[paste(foundations, "_lead", sep="")] = lead(stocks60[27:31])

dir.create("processed_data")
write.csv(stocks60, "processed_data/stocks60.csv")

stocks60ts = ts(stocks60) # MAKE TIME SERIES
plot(stocks60ts[,"stocks_60"]) # PLOT INITIAL DATA
plot(stocks60ts[,"stocks_60_diff"]) # PLOT DIFFERENCED DATA
plot(stocks60ts[,"stocks_60_diff_ln"]) # PLOT LOG DATA

plot(stocks60ts[,"morality_diff"])
plot(stocks60ts[,"care_diff"])
plot(stocks60ts[,"fairness_diff"])
plot(stocks60ts[,"loyalty_diff"])
plot(stocks60ts[,"authority_diff"])
plot(stocks60ts[,"sanctity_diff"])

plot(stocks60ts[,"stocks_60"]) # PLOT INITIAL DATA

acf(stocks60$stocks_60, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR NON-TRANSFORMED DATA
acf(stocks60$stocks_60, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR NON-TRANSFORMED DATA

acf(stocks60$stocks_60_diff_ln, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR TRANSFORMED DATA
acf(stocks60$stocks_60_diff_ln, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR TRANSFORMED DATA

plot(stocks60ts[,"stocks_60_diff_ln"]) # 

plot(stocks60ts[,"morality"]) # PLOT INITIAL DATA

acf(stocks60$morality, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR NON-TRANSFORMED DATA
acf(stocks60$morality, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR NON-TRANSFORMED DATA

acf(stocks60$morality_diff, lag.max = NULL, type = c("correlation"), plot = TRUE, na.action = na.pass) # ACF FOR TRANSFORMED DATA
acf(stocks60$morality_diff, lag.max = NULL, type = c("partial"), plot = TRUE, na.action = na.pass) # PACF FOR TRANSFORMED DATA

plot(stocks60ts[,"morality_diff"]) # 

library(pastecs)
cols <- c("morality","care","fairness","loyalty","authority","sanctity", "stocks_60", "stocks_60_diff","stocks_60_diff_ln")
stocks60[,cols] %>% 
  stat.desc()

library(rstatix)
outliers <- stocks60 %>% 
  identify_outliers(c(morality))
print(paste("Number of outliers: ", nrow(outliers)))

stocks60_outrm <-  stocks60 %>% dplyr::filter(!(morality %in% outliers$morality))
print(paste("Before outlier removal: ", nrow(stocks60))) 
print(paste("After outlier removal: ", nrow(stocks60_outrm))) 
print(paste("Difference: ", nrow(stocks60) - nrow(stocks60_outrm)))

plot_TF = TRUE

lag_max = 5
ylab = "ccf"

png("figs/ccf.png", width = 5, height=4, units='in', res=300)


par(mfrow=c(2,3))
ccf(stocks60_outrm$morality_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Morality Lags")
ccf(stocks60_outrm$care_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Care Lags")
ccf(stocks60_outrm$fairness_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Fairness Lags")
ccf(stocks60_outrm$loyalty_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Loyalty Lags")
ccf(stocks60_outrm$authority_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Authority Lags")
ccf(stocks60_outrm$sanctity_diff, stocks60_outrm$stocks_60_diff_ln, na.action = na.exclude, lag.max = lag_max, plot= plot_TF, ylab=ylab, main="Stocks & Sanctity Lags")
dev.off()

library(jtools) # summ()
library(lme4) # lme models
library(interactions)

stocks60_outrm <- stocks60_outrm %>% na.omit()

# Lag
## Linear regression
model.lm <- 
  lm(stocks_60_diff_ln ~ tf2_60 * morality_lag,
     data = stocks60_outrm)

model.lm.foundations <- 
  lm(stocks_60_diff_ln ~ tf2_60 * (care_lag +fairness_lag +loyalty_lag +authority_lag +sanctity_lag), 
     data = stocks60_outrm)

## Random intercept with fixed mean (null)
model.lme.null <-
  lmer(stocks_60_diff_ln ~ 1 + (1|day_count), 
       data = stocks60_outrm, REML=TRUE)

# Correlated random slopes
model.lme <- 
  lmer(stocks_60_diff_ln ~ 
         (1 + season_intraday_60 + tf2_60*morality_lag + (1 + season_intraday_60 | day_count)), 
       data = stocks60_outrm, REML = TRUE)

model.lme.foundations <- 
  lmer(stocks_60_diff_ln ~
         1 + season_intraday_60 + tf2_60*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_60 | day_count), 
       data = stocks60_outrm, REML = TRUE)

# # Lead
# 
# ## Linear regression
# model.lm <-
#   lm(stocks_60_diff_ln ~ tf2_60 * morality_lead,
#      data = stocks60_outrm)
# 
# model.lm.foundations <- 
#   lm(stocks_60_diff_ln ~ tf2_60 * (care_lead +fairness_lead +loyalty_lead +authority_lead +sanctity_lead), 
#      data = stocks60_outrm)
# 
# ## Random intercept with fixed mean (null)
# model.lme.null <-
#   lmer(stocks_60_diff_ln ~ 1 + (1|day_count), 
#        data = stocks60_outrm, REML=TRUE)
# 
# # Correlated random slopes
# model.lme <- 
#   lmer(stocks_60_diff_ln ~ 
#          (1 + season_intraday_60 + tf2_60*morality_lead + (1 + season_intraday_60 | day_count)), 
#        data = stocks60_outrm, REML = TRUE)
# 
# model.lme.foundations <- 
#   lmer(stocks_60_diff_ln ~  
#          (1 + season_intraday_60 + tf2_60*(care_lead + fairness_lead + loyalty_lead + authority_lead + sanctity_lead) + ( 1 + season_intraday_60 | day_count)), 
#        data = stocks60_outrm, REML = TRUE)

anova(model.lm, model.lm.foundations)
anova(model.lme.null, model.lme, model.lme.foundations)

anova(model.lme, model.lm, type="Chisq")
anova(model.lme.foundations, model.lm.foundations, type="Chisq")

Anova(model.lm, type="III", test="F")
Anova(model.lm.foundations, type="III", test="F")
Anova(model.lme.null, type="III", test="F")
Anova(model.lme, type="III", test="F")
Anova(model.lme.foundations, type="III", test="F")

summ(model.lme, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=2)
summ(model.lme.foundations, scale=TRUE, transform.response=TRUE, confint=TRUE, digits=2)

# define models
lag_mor <- lmer(stocks_60_diff_ln ~ (1 + season_intraday_60 + tf2_60*morality_lag + (1 + season_intraday_60 | day_count)), 
       data = stocks60_outrm, REML = TRUE)
lag_f <- lmer(stocks_60_diff_ln ~1 + season_intraday_60 + tf2_60*(care_lag + fairness_lag + loyalty_lag + authority_lag + sanctity_lag) + ( 1 + season_intraday_60 | day_count), 
       data = stocks60_outrm, REML = TRUE)
lead_mor <- lmer(stocks_60_diff_ln ~ (1 + season_intraday_60 + tf2_60*morality_lead + (1 + season_intraday_60 | day_count)), 
       data = stocks60_outrm, REML = TRUE)
lead_f <- lmer(stocks_60_diff_ln ~  (1 + season_intraday_60 + tf2_60*(care_lead + fairness_lead + loyalty_lead + authority_lead + sanctity_lead) + ( 1 + season_intraday_60 | day_count)), 
       data = stocks60_outrm, REML = TRUE)

# define plots
lag_m <- interact_plot(model = lag_mor, pred = morality_lag, modx = tf2_60, interval = TRUE, x.label="Morality", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic \n Period") + ylim(-0.02,0.02) + xlim(-0.02,0.02)  +rremove("ylab")+ theme(axis.text.x = element_text(size=8))
lag_f1 <- interact_plot(model = lag_f, pred = authority_lag, modx = tf2_60, interval = TRUE, x.label="Authority", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic Period") + ylim(-0.02,0.02)+ xlim(-0.005,0.003) + rremove("legend")+rremove("ylab")+ theme(axis.text.x=element_text(size=8), axis.text.y=element_blank())
lag_f2 <- interact_plot(model = lag_f, pred = sanctity_lag, modx = tf2_60,interval = TRUE, x.label="Sanctity", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic Period") + ylim(-0.02,0.02)+ xlim(-0.005,0.003)+ rremove("legend")+rremove("ylab")+ theme(axis.text.x=element_text(size=8),axis.text.y=element_blank())

lead_m <- interact_plot(model = lead_mor, pred = morality_lead, modx = tf2_60, interval = TRUE, x.label="Morality", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic \n Period") + ylim(-0.02,0.02) + xlim(-0.02,0.02)  +rremove("ylab")+ theme(axis.text.x = element_text(size=8))
lead_f1 <- interact_plot(model = lead_f, pred = authority_lead, modx = tf2_60, interval = TRUE, x.label="Authority", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic Period") + ylim(-0.02,0.02)+ xlim(-0.005,0.003) + rremove("legend")+rremove("ylab")+ theme(axis.text.x=element_text(size=8), axis.text.y=element_blank())
lead_f2 <- interact_plot(model = lead_f, pred = sanctity_lead, modx = tf2_60,interval = TRUE, x.label="Sanctity", y.label="Market Movement", modx.labels = c("High","Low"), legend.main="Economic Period") + ylim(-0.02,0.02)+ xlim(-0.005,0.003)+ rremove("legend")+rremove("ylab")+ theme(axis.text.x=element_text(size=8),axis.text.y=element_blank())

# define extra graphical objects
t1 <- textGrob("Lag 1")
t2 <- textGrob("Lead 1")
legend <- get_legend(lag_m)
blank <- grid.rect(gp=gpar(col="white"))

# plot interactions
png("figs/interactions.png", width = 8, height=4, units='in', res=300)
ggarrange(t1, lag_m+ rremove("legend"),lag_f1,lag_f2,blank,
          t2,lead_m+ rremove("legend"),lead_f1,lead_f2,legend, 
          ncol=5, nrow=2,widths = c(0.25,1,1,1, 0.5))
dev.off()
