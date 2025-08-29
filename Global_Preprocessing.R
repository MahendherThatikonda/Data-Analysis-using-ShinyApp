library(shiny)
library(shinyjs)
library(shinycssloaders)
library(vcd)
library(GGally)
library(RColorBrewer)
library(bslib)
library(devtools)
library(corrgram)
library(visdat)
library(ggplot2)
library(DT)
dat <- read.csv('Ass2Data.csv',header = TRUE,na.strings = c("NA","N/A"),stringsAsFactors = TRUE)

dat[dat==-1] <- NA
dat[dat==-99] <- NA
dat[dat=="--"] <- NA
dat[dat==""] <- NA
dat[dat=="na"] <- NA

dat$POLITICS  <-  as.character(dat$POLITICS)
dat$POLITICS[dat$POLITICS == "--"] <- NA
dat$POLITICS  <-  as.factor(dat$POLITICS)

# this is not applicable, rest are not available
#When the HEALTHCARE_BASIS is free, HEALTHCARE_COST becomes Not Applicable(NA)
dat$HEALTHCARE_COST[as.character(dat$HEALTHCARE_BASIS) == "FREE"] <- 0