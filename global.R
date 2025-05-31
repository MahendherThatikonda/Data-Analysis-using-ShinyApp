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
library(dplyr)

dat <- read.csv('Ass2Data.csv',header = TRUE,na.strings = c("NA","N/A"),stringsAsFactors = TRUE)

dat[dat == -1] <-  NA
dat[dat == -99.00000] <-  NA
dat[dat == ""] <-  NA
dat[dat == "--"] <-  NA
dat <- dat %>%
  mutate(ID = row_number())
dat$ID <- seq_len(nrow(dat)) 
#View(dat)
#dat[dat == "--"] <-  NA
#View(dat)