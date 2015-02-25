# driver-telematics-analysis-continued
driver telematics analysis for kaggle competition containing all processing performed after February 20, 2015.
Much of the code in this repository was built on a template provided by [Stephane Soulier] (http://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11299/score-0-66-with-logistic-regression) in the Kaggle Competition Forumn. 

Brief Summary of Code
- gbm\_speedhistogram\_doMC.R - gradient boosting model using doMC for parallel backend - score: 0.78
- gbm\_speedhistogram\_removeOverlap.R - gradient boosting model (not parallel), filter out target 0 data from targe 1 entries - score: 0.69
- gbm\_speedquantile.R - gradient boosting model using doMC backend - score: 0.77
- prediction\_triplength\_glm.R - logistic regression using previously compiled stop data - score: 0.56