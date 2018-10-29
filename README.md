# ARK-Fund
FINA4353 Financial Engineering Structured Product Design Project

@ Description:
  This is a self-designed stuructured product sample for FINA4353 Financial Engineering.

@ Authors:

    BAI Yang
  
    CAO Wenxin
  
    HUANG Feiqing
  
    LIN Pingyi
  
    LU Yuqiao

@ Date: 4/27/2018

# Usage:
## Run program by command line:
    This program can be executed by command line under Linux environment.
    For R environment, we suggest using R version 3.4.4 (2018-03-15) -- "Someone to Lean On" to run this set of programs.
#### First, enter the directory of this file by command line
#### To run simulations on current stock price, execute: 

$ R <ARK.R --save
  
  Graphs will be stored under ./Simulations/Graph/ and simulation data will be stored in .csv files under ./Simulations/
#### To run backtesting with two given datasets (BacktestBear.xlsx and BacktestBull.xlsx), execute: 

$ R <Backtest.R --save
  
  Data will be stored in .csv files under ./Backtests/ and graphs of the two backtesting will be stored undner ./Backtests/Graph

## Run program using RStudio
#### First, open RStudio by GUI or execute under the directory of this file: 

$ rstudio *.R &
#### Set up working directory
 
Use setwd({the path of this file}) to change the working directory.
#### Run all the lines in ARK.R using RStudio to run simulations.
#### Run all the lines in Backtest.R using RStudion to run backtests on two given datasets.
