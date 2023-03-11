# MrJerryTAO collection of custom R scripts and functions

<!--
**MrJerryTAO/MrJerryTAO** is a ✨ _special_ ✨ repository because its `README.md` (this file) appears on your GitHub profile.

Here are some ideas to get you started:

- 🔭 I’m currently working on ...
- 🌱 I’m currently learning ...
- 👯 I’m looking to collaborate on ...
- 🤔 I’m looking for help with ...
- 💬 Ask me about ...
- 📫 How to reach me: ...
- 😄 Pronouns: ...
- ⚡ Fun fact: ...
-->

Throughout substantive use of R for economics and data science consulting, I have encountered occasions necessary to develop my own functions and customize existing source codes to achieve analytical objectives. To use one or more of these functions, download the R scripts from this GitHub repository, run the scripts in R console to load the functions in the global environment, and implement the functions like other R functions. This repository includes the following functions: 

## 1. keras.cv_MrJerryTAO() 

The R package caret tunes keras deep learning models to some extent. However, all of the four associated functions, namely method = "mlpKerasDecay", "mlpKerasDecayCost", "mlpKerasDropout", and "mlpKerasDropoutCost" in caret::train() only allows one layer in the model although the method names containing "mpl", short for Multilayer Perceptron Network, suggests that they tune a newral network of multiple layers. The intrinsic pipeline usage in keras may have made the tuning coding difficult to adjust. Luckily, I found a solution to conditional pipelines used between keras components when tuning the number of layers in deep learning. My function adds an arbitrary number of dense layers according to the user specification with a dropout layer after each dense layer during cross validation. The current  implementation accommodates binary classification of tabular data. 

## 2. keras.permimp_MrJerryTAO() 

Variable importance via permutation, originally suggested by Breiman (2001), is the difference in model performance before and after randomly permuting a predictor. Permutation removes the original association between a predictor and the response. Thus permuting an important predictor should result in a substantial decrease in model performance. However, Strobl et al. (2007) demonstrated that unconditionally permutating predictor does not account for correlation between predictors and thus leads to overestimated importance of correlated predictors. Therefore, in keras.permimp_MrJerryTAO() I transferred the conditional permutation importance approach to keras platform for deep learning. The function takes account for predictor correlation by permuting a perdictor only within blocks defined by the intersection of all other predictors, where continuous variables are splitted into bins whose size is user specified. The output is a log of performance metrics both before and after permutation, as well as their difference, evaluated at a given epoch. 

Breiman, L. (2001). Random forests. Machine Learning, 45(1), 5–32. https://doi.org/10.1023/A:1010933404324
Strobl, C., Boulesteix, A.-L., Zeileis, A., & Hothorn, T. (2007). Bias in random forest variable importance measures: Illustrations, sources and a solution. BMC Bioinformatics, 8(1), 25. https://doi.org/10.1186/1471-2105-8-25
Strobl, C., Boulesteix, A.-L., Kneib, T., Augustin, T., & Zeileis, A. (2008). Conditional variable importance for random forests. BMC Bioinformatics, 9(1), 307. https://doi.org/10.1186/1471-2105-9-307


## 3. xgb.train_MrJerryTAO() 

In the latest XGBoost R package, Version 1.6.0.1 by April 2022, the fundamental function xgb.train() does not produce evaluation results of more than one metric although the help document suggests that multiple entries in the eval_metric argument are allowed. However, the function xgb.cv() enables returning multiple evaluation metrics. Therefore, after comparing the source code of these two functions, I altered xgb.train() to form a new function xgbtrain_MrJerryTAO(), which enables returning results of multiple metrics for both training and testing data sets. This function of course requires the installation of xbgoost R package. 

Chen, T., He, T., Benesty, M., Khotilovich, V., Tang, Y., Cho, H., Chen, K., Mitchell, R., Cano, I., Zhou, T., Li, M., Xie, J., Lin, M., Geng, Y., Li, Y., Yuan, J., & implementation), Xgb. contributors (base Xgb. (2022). xgboost: Extreme gradient boosting (1.6.0.1). https://CRAN.R-project.org/package=xgboost

## 4. selFreq_MrJerryTAO()

Adapted from the R package permimp designed for conditional permutation importance for random forests, this customized function enables the return of not mean but raw frequencies of individual trees splitting by each predictor. 

Hothorn, T., Hornik, K., Strobl, C., & Zeileis, A. (2021). party: A laboratory for recursive partytioning (1.3-9). https://CRAN.R-project.org/package=party

## 5. facet.scales_MrJerryTAO
An ensemble of three functions, namely scale_override(), facet_wrap_MrJerryTAO() and facet_grid_MrJerryTAO() adapted from Dunnington (2018). They extend facet_warp() and facet_grid() native to ggplot2 to accommodate different axis scales by facet. Note that facet_grid_MrJerryTAO() adjust scales by column and by row, so facets on the same column share the same x axis scale, while facets on the same row share the same y axis scale. facet_wrap_MrJerryTAO() enables adjust scales of both x- and y- axes for each facet. However, both functions allows only one title of each axis. To adjust axis scales while displaying different axis titles, consider ggpubr::ggarrange(). 

Dunnington, D. (2018, February 28). Modifying facet scales in ggplot2. Fish & Whistle. https://dewey.dunnington.ca/post/2018/modifying-facet-scales-in-ggplot2/

## 6. ci.adjusted_MrJerryTAO()

Efficient dplyr programming to generate repeated-measurements-, variance-, and difference-adjusted, range-preserving CIs for means and proportions from individual scores and group estimates, which are robust to imbalanced factorial designs, unequal group variances, different group sizes, and missing observations. 

Error bars are useful to describe data variability and inference uncertainty and should preferably represent the confidence intervals (CI), according to Cousineau (2017). A regular 95% CI is designed to contain the true population parameter in 19 out of 20 times of random sampling. However, often of interest is the difference in parameters and whether this difference is truly zero. This calls for an adjustment to the regular CI for difference inference (Goldstein & Healy, 1995). Two-tiered CIs in plots can contrast the regular CI with the difference-adjusted CI (Baguley, 2012). The U.S. Bureau of Census (Wright et al., 2019) has adapted this CI adjustment method for pairwise multiple comparisons between independent groups. The CI of a group difference can be further adjusted for heterogeneous variances between groups (Tryon, 2001). For proportions, the Wilson CI is recommended (Beaulieu-Prévost, 2006; Cumming & Fidler, 2009). This is because it has the best actual coverage and shortest width among several alternatives (Oranje, 2006). The way to adjust standard errors and degree of freedom for group differences in proportions are similar to that in means due to range-preserving properties of transformed CI through the delta method (Noguchi & Marmolejo-Ramos, 2016). The confidence interval adjustment method for group differences has been extended to the repeated measures (Afshartous & Preston, 2010). This is done by either altering the critical test value or correcting the standard error component using normalized scores in the CI formula (Cousineau, 2005; Morey, 2008; Cousineau, 2019). Both approaches are equivalent and consistent with null hypothesis significance testing (Blouin & Riopelle, 2005). CI adjustment methods under complex sampling designs were also proposed (Cousineau et al., 2021). I composed efficient programming to generate repeated-measurements-, variance-, and difference-adjusted, range-perserving CIs for means and proportions, which are robust to imbalanced factorial designs, unequal group variances, different group sizes, and missing data. Nonoverlapping between variance- and difference-adjusted CIs of two groups suggests statistical differences between the pair (Morey, 2008). For three or more groups, if at least one group’s adjusted CI lies outside of another’s, it suggests significant variation of group means among all groups (Cousineau et al., 2021). ci_MrJerryTAO() requires installation of the tidyverse package in R, tidyr and dplyr in particular. The formulae and procedures used for these CIs are partially inspired by Baguley (2011). 

Baguley, T. (2011, October 4). Calculating and graphing within-subject confidence intervals for ANOVA. R-Bloggers. https://www.r-bloggers.com/2011/10/calculating-and-graphing-within-subject-confidence-intervals-for-anova/

## 7. balance.IPW_MrJerryTAO()

The function of balance.IPW() in R package CausalGAM for examining sample balance after inverse propensity weighting via generalized additive models contains erroneous coding which falcifies the treatment indicator in the data, typically a factor or character, as well as other nonnumeric columns and leads to errors in the inferent gam() modeling. Consequently, it will never return the intended results whenever these is one or more nonnumeric columns in the supplied data. I corrected such errors, reorganized the script blocks for robustness to missingness in control variables, and enhanced the analysis by reporting asymptotic, studentized bootstrap percentile, and permutation-based p values and confidence intervals of both differences in means and ratios of standard deviations between the treatment and control group. However, this function is slow due to calculation of additional statistics. 
