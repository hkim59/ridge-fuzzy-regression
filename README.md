# ridge-fuzzy-regression

This repository contains code to the following papers:

Choi, S.H., Jung, HY. & Kim, H. Ridge Fuzzy Regression Model. Int. J. Fuzzy Syst. 21, 2077–2090 (2019). https://doi.org/10.1007/s40815-019-00692-0

Kim H, Jung H-Y. Ridge Fuzzy Regression Modelling for Solving Multicollinearity. Mathematics. 2020; 8(9):1572. https://doi.org/10.3390/math8091572

## Ridge Fuzzy Regression Model

Code is inside the `ridge-fuzzy-regression` folder. Check html files for Rmarkdown documents on the step-by-step implementation of the method. 

- *example1_fuzzy_ols.Rmd* and *example1_fuzzy_ridge.Rmd*: code to run Example 1 in paper.
- *example2_fuzzy_ridge.Rmd*: code to run Example 2 in paper.


## Ridge Fuzzy Regression Modelling for Solving Multicollinearity

- *Alpha_cut_regression.R*: performs fuzzy alpha-cut regression. 
- *choose_alpha_Alpha_cut_regression.R*: chooses optimal alpha-level set for fuzzy alpha-cut regression based on RMSE.
- *predict_Alpha_cut_regression.R*: performs fuzzy alpha-cut regression on new data.
- *plot_Alpha_cut_regression.R*: code to generate appropriate plots for simulation & empirical study.
- *simulation_study.R*: code to run simulation study in paper.
- *empirical_study.R*: code to run empirical study in paper.
- *ridge_fuzzy_regression_algorithm.R*: code to genenerate figures in paper. 


