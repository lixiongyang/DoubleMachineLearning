# Y=audit quality
# D=big4
# X=controls variables

rm(list=ls())
setwd("D:\\报表")
#install.packages("readxl")
library(readxl)
data <- read_excel("dataForML.xlsx")

y <- data$RM
d <- data$big4
x="lnA+ATURN+MKT+roa+LEV+curr"

sed <- 123  # default=123

setwd("E:\\R-econometrics\\myDML")
source("dml_boost.R")

dml_boost <- dml_boost(data=data,y,x,d,sed=123)


