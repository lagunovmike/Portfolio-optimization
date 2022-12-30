# Portfolio Optimization

Welcome to the Portfolio Optimization repository! This project is designed to provide tools for optimizing a portfolio of financial assets using R.

## Requirements

To use this project, you will need R and the following packages:

- tidyverse
- tidyquant
- PerformanceAnalytics
- timetk
- plotly


## Getting Started

To get started with the project, you will need to clone the repository to your local machine. 
You can do this by opening RStudio and going to the "Git" tab in the "Git" pane. 
Click the "Clone" button and enter the repository URL: `https://github.com/lagunovmike/Portfolio-optimization.git`. 
Choose a directory on your local machine to clone the repository to and click "Create Project".

Alternatively, you can use the `usethis` package to clone the repository directly from within RStudio:

```r
# Install the usethis package if you don't have it already
install.packages("usethis")

# Load the usethis package
library(usethis)

# Clone the repository
usethis::use_git("https://github.com/lagunovmike/Portfolio-optimization.git",
                 destdir = "~/path/to/local/directory")
```


## Results

`portfolio-optimization.R` script will output the optimized portfolio's expected return, standard deviation, and Sharpe ratio. It will also generate a plot showing the efficient frontier and the optimal portfolio, as well as a plot of the distribution of returns for the random portfolios generated by the Monte Carlo simulation. The `portfolio-optimization.Rmd` can be used to generate reports in html.

find_best_spb_portfolio_by_groups.R is a script that aims to find the optimal portfolio from a list of tickers from the SPB exchange. To improve computational efficiency, the analysis is divided into two steps. The first step is to identify a wide range of strong performing stocks, and the second step is to find the optimal allocation of these stocks in a portfolio.

`optimize assets allocation.R` script is using the lpSolve library to perform linear programming to find the optimal allocation of assets given certain constraints. The objective of the optimization is to maximize the return on investment, and the decision variables are the quantities of each asset to be purchased. The code sets up the constraints for the optimization, including the budget for the investment and the maximum allowed allocation for each asset. The optimization is then performed using the lp function, and the optimal asset quantities and the maximum return on investment are obtained as the solution. The code also calculates the proportion of the budget allocated to each asset. This file continues the analysis started in "find_best_spb_portfolio_by_groups.R" and uses the output of that file as the input for the analysis.

## Customization

You can customize the optimization process by modifying the functions in portfolio_optimization.R. This script provides a set of functions for loading data, calculating statistics, and optimizing portfolios.
