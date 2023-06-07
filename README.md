# Project Goals
The goal of this project is to predict short-term stock market movement from the prevalence of moral language use in online news using 15-min interval data points across an 8-month period. 

## Theoretical Background
The Model of Intuitive Morality and Exemplars (MIME) suggest that news audiences, including investors, evaluate news based on their moral frames, and that these moral evaluations shape their ensuing behavior. According to MIME, our media environment plays a role in both shaping and being shaped by our moral intuitions. Content features in a media message can influence both the short and long-term salience of an individual’s moral intuitions. Content features include representations of behaviors in the media that either uphold or violate one or more of the five moral subdimensions (i.e., care/harm, fairness/cheating, loyalty/betrayal, authority/subversion, and sanctity/degradation). These are known as exemplars, or morally-framed behaviors in media representations that are reflective of specific moral domains. As moral exemplars draw our attention towards certain moral subdimensions, these subdimensions become more salient in both strength and valence within our moral intuitions.

## Data
- Text from 1m+ news articles computationally extracted from GDELT (Global Database of Events, Language, and Tone; Leetaru & Schrodt, 2013). After cleaning, our final dataset contained 382,185 individual news articles collected every 15 minutes over the eight-month period from 1 November 2019 to 30 June 2020.
- S&P 500 closing values collected at 1 minute intervals over the eight-month period from 1 November 2019 to 30 June 2020.
- All data available via OSF, https://osf.io/859gh/

## Process
1. Extract moral content from article text using eMFDscore (https://github.com/medianeuroscience/emfdscore)
2. Aggregate and temporally align data across lowest common time interval (15 minutes) 
3. Fit linear mixed-effects models to estimate repeated observations of hourly data points nested within business days
4. Draw conclusions about predictive (not actual) causality using Granger Causality

## Results
Likelihood-ratio tests indicated that the mixed-effects models fit the data better than a standard linear model for both morality (Model 1), χ2(4) = 157. 70, 𝑝 < 0. 00, and its subdimensions (Model 2), χ2(4) = 161. 45, 𝑝 < 0. 00. While the addition of morality in Model 1 provided a better fit for the data compared to a null mixed-effects model without any moral predictors, χ2(6) = 164. 00, 𝑝 < 0. 00, breaking morality down into its foundations in Model 2 further explained the data better than Model 1, χ2(8) = 161. 45, 𝑝 = 0. 01.

To test our hypothesis that morality positively predicts stock market movement (H1), especially during perceived bad economic times (H2), we specified fixed effects for our moral predictors and their interactions with high or low economic periods. Random effects were specified as the random intercept and slope variance for nested hours within business days. Fixed effects accounted for 1% and 3% of the outcome variance in Model 1 and 2 respectively; total effects accounted for 32% and 14% of the outcome variance in Model 1 and 2 respectively. In the parameters for Model 1, we see that while overall morality does not show significance, the interaction of morality and economic time period is positively significant, 𝛽 = 0. 15, 𝑝 = 0. 02 . An examination into Model 2 shows that the significant effect of morality given its interaction with economic period is primarily driven by the significant effects of the foundations for authority/subversion, which was negatively related to market movement, 𝛽 =− 0. 45, 𝑝 = 0. 02, and sanctity/degradation, which was positively related to market movement, 𝛽 = 0.60, 𝑝 = 0.00.

For the predictive model, a cross-correlation function using lags -5 to 5 for all moral predictors found no overall significance, but indicated that lag 1 predictors were closest in significance overall, and had greater proximity to significance compared to lead 1 predictors. We compared the model parameters and fit for lag 1 values of moral predictors with both current and lead 1 predictors. Results show that for Model 1, fixed and total effects account for the same degree of variation in our outcome variable between lag 1 and lead 1 predictors (1% fixed and 32% total for both). However, morality is not a significant predictor in the lead 1 model as it is in the lag 1 model. Model 2 indicates that lag 1 fixed effects account for triple the amount of outcome variance (3%) compared to lead 1 fixed effects (1%). Moreover, none of the foundations are significant predictors in the lead 1 model, compared to authority/subversion and sanctity/degradation foundations being significant in their interaction with economic period for the lag 1 model. Total effects for lag 1, however, explain less outcome variation compared with lead 1 (14% and 32% respectively). Overall, our results indicate that our lag 1 models better predict stock market movement compared to our lead 1 models. Going by our definition of predictive causality, we can thus infer that morality has a predictive causal effect on (i.e., Granger-causes) movement within the stock market.

## Conclusions
- Morality (and its subdimensions) was found to be a significant predictor only when the stock market was in a recession, highlighting the increased influence of news on public opinion during periods of socio-economic fragility
- Stock market movement is driven by only two of the five existing moral subdimensions: 
-   Violation of authority (i.e., subversion)
> Possible explanation: As the reliance on traditional institutional structure decreases (i.e., subversion increases), there is greater uncertainty surrounding the immediate socio-economic landscape, leading to a short-term liquidation of assets and subsequent drop in market value.
-   Upholding of sanctity
> Possible explanation: A response to serendipitous economic growth during a recession and hopefulness towards economic stability and virus eradication.

Overall our findings show support for the short-term predictions of the MIME, which describe the cause-and-effect processes between moral intuition and news evaluation. According to the MIME, exposure to highly moral/immoral exemplars within specific moral domains will heighten audience sensitivity to these domains, causing them to seek out and react more strongly to media that further exemplify these domains
