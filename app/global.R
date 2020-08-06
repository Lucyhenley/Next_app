library(shiny)
library(grid)
library(gridExtra)
library(reshape2)
library(png)
library(tools)
library(pracma)
library(latex2exp)



#setwd("C:/Users/lucy_/covid-recovery/app")


seat_locations <- read.csv(file="next_all_seats_first_floor_trial.csv")
shield_locations <- read.csv(file="next_all_seats_first_floor_trial.csv")
best_order <- read.csv(file= "best_order_next_trial.csv")

domain_x <- 60
domain_y <- 45
x_box <- c(0,0,       domain_x,domain_x,0,domain_x)
y_box <- c(0,domain_y,domain_y,0,       0,0)

source("ui.R")
source("server.R")
source("src.R")

shinyApp(ui = ui, server = server)
