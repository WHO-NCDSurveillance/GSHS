rm(list = ls())
library(readxl)
setwd("~/Documents/Transferred files/Data/data part 4/WHO/Automated scripts version 3/site level data inputs/2022 UNRWA/West Bank")

data_file = read_excel('raw_data.xlsx')

table(data_file$q3)
