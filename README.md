# MF-Stocks
Code for moral foundations and stocks project

The goal of this project is to explore the short-term effects of moral language in news framing through an examination of activity in the financial market. 

## Theoretical Background
The Model of Intuitive Morality and Exemplars (MIME) suggest that news audiences, including investors, evaluate news based on their moral frames, and that these moral evaluations shape their ensuing behavior. According to MIME, our media environment plays a role in both shaping and being shaped by our moral intuitions. Content features in a media message can influence both the short and long-term salience of an individual’s moral intuitions. Content features include representations of behaviors in the media that either uphold or violate one or more of the five moral subdimensions (i.e., care/harm, fairness/cheating, loyalty/betrayal, authority/subversion, and sanctity/degradation). These are known as exemplars, or morally-framed behaviors in media representations that are reflective of specific moral domains. As moral exemplars draw our attention towards certain moral subdimensions, these subdimensions become more salient in both strength and valence within our moral intuitions.

## Data
- Text from 1m+ news articles computationally extracted from GDELT (Global Database of Events, Language, and Tone; Leetaru & Schrodt, 2013). After cleaning, our final dataset contained 382,185 individual news articles collected every 15 minutes over the eight-month period from 1 November 2019 to 30 June 2020.
- S&P 500 closing values collected at 1 minute intervals over the eight-month period from 1 November 2019 to 30 June 2020.

## Process
1. Extract moral content from article text using eMFDscore (https://github.com/medianeuroscience/emfdscore)
2. Aggregate and temporally align data across lowest common time interval (15 minutes) 
3. Fit linear mixed-effects models to estimate repeated observations of hourly data points nested within business days
4. Draw conclusions about predictive (not actual) causality using Granger Causality

## Conclusions
- Morality (and its subdimensions) was found to be a significant predictor only when the stock market was in a recession, highlighting the increased influence of news on public opinion during periods of socio-economic fragility
- Stock market movement is driven by only two of the five existing moral subdimensions: 
-   Violation of authority (i.e., subversion)
> Possible explanation: As the reliance on traditional institutional structure decreases (i.e., subversion increases), there is greater uncertainty surrounding the immediate socio-economic landscape, leading to a short-term liquidation of assets and subsequent drop in market value.
-   Upholding of sanctity
> Possible explanation: A response to serendipitous economic growth during a recession and hopefulness towards economic stability and virus eradication.

Overall our findings show support for the short-term predictions of the MIME, which describe the cause-and-effect processes between moral intuition and news evaluation. According to the MIME, exposure to highly moral/immoral exemplars within specific moral domains will heighten audience sensitivity to these domains, causing them to seek out and react more strongly to media that further exemplify these domains


