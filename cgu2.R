#######################################
# Loren Collingwood                   #
# UC Riverside                        #
# DATE: 5/22/2019                     #
# Webscraping with rvest              #
# Topic 1: Immigrant Detention Deaths #
#######################################

rm(list=ls())

library(rvest)
library(stringr)
library(gsubfn)
library(tidyverse)
library(data.table)
library(readxl)
library(descr)

# Read in death data located on capital and main website #
deaths <- read_html("https://capitalandmain.com/mapping-death")

# Read in Table-type information
deaths <- (html_table(deaths, fill=T))

# Take the first list #
deaths <- deaths[[1]]
colnames(deaths)[1]<- "Name"

############
# CLEANING #
############

# Recode for checking purposes
deaths$death_r <- ifelse(deaths[,1]== "FINAL CAUSE OF DEATH", 1, 0)
# Check the messed up pattern -- DF not quite what i want  so need to delete a few `rogue` cases
which(deaths$death_r==1)

# Drop these few here that are messing up the 1:10 flow #
deaths <- deaths[-412,]
deaths <- deaths[-1293,]

# Set up Splitter
splitter <- list()
n <- 1880

# Create splitter variable
for (i in 1:(n/10)) splitter[[i]] <- rep(i, 10)
splitter <- unlist(splitter); length(splitter)

# Subset just to the necessary columns #
tester <- deaths[1:n,c("Name", "Title", "death_r")]
tester <- data.frame(tester, splitter, stringsAsFactors = F)

# Split data into n/10 lists #
tester_list <- split(tester, tester$splitter)

# Function: death_list_split #
death_list_split <- function(x){
  # Relabels
  x[1,1] <- "Name"
  # Returns just the first 2 columns
  x[, c("Name", "Title")]
}

# Execute Function with lapply() #
out <- lapply(tester_list, death_list_split)

# Bind columns; transpose, and put into data.frame() #
out <- data.frame(t(dplyr::bind_cols(out)), stringsAsFactors=F)

# Clean the data #
colnames(out) <- out[1,]
rownames(out) <- NULL

# Drop Rows you don't need
drops <- seq(1, nrow(out), 2)
out <- out[-drops,]

# Verify Everything is good
tail(out)

# Recode a few Variables #
out$SEX[out$SEX=="SEX"] <- "M"
out$SEX[out$SEX=="W"] <- "F"
table(out$SEX)

# Date variables, year of Death #
out$dob <- lubridate::mdy(out$`DATE OF BIRTH`) # some missing data
out$yob <- lubridate::year(out$dob)
out$dod <- lubridate::mdy(out$`DATE OF DEATH`) # 1 missing data
out$yod <- lubridate::year(out$dod)
out$age_death <- with(out, yod - yob)

# Write to Disk #
write.csv(out, "~/Dropbox/sanctuary_paper2/data/detention_deaths/detention_deaths.csv", row.names=F)

###########################################
# Looking at Distribution of Age of Death #
###########################################

d <- density(out$age_death, na.rm=T) # returns the density data 
plot(d, xlab = "Age", main = "Detention Death:\nAge at Death Distribution", bty='n') 
abline(v = mean(out$age_death, na.rm=T), col="grey", lty=2)
text(75, .024, "Mean Age = 45")

# Life Expectancy Averages #
all <- 78.6
hisp <- 81.8
wh <- 78.5
blck <- 74.8
# https://www.cdc.gov/nchs/data/hus/2017/fig01.pdf

###########################################
#### Reading back in from Chase Ramos #####
###########################################

library(readxl)
death <- read_xlsx("~/Dropbox/sanctuary_paper2/data/detention_deaths/detention_deaths.xlsx",
                   sheet="detention_deaths"); dim(death)
death$latino_carrib <- car::recode(death$`COUNTRY OF BIRTH`," 
                                   'ANTIGUA‚ BARBUDA'=1; 
                                   'ARGENTINA'=1; 'BARBADOS'=1; 'BRAZIL'=1;
                                   'BRITISH VIRGIN ISLANDS'=1; 'COLOMBIA'=1;
                                   'Cuba'=1;'CUBA'=1; 'CUBAN'=1; 'DOMINICAN REPUBLIC'=1;
                                   'ECUADOR'=1; 'EL SALVADOR'=1; 'GUATEMALA'=1;
                                   'HAITI'=1; 'Honduras'=1; 'HONDURAS'=1; 
                                   'JAMAICA'=1; 'Mexico'=1; 'MEXICO'=1;'NICARAGUA'=1;
                                   'PANAMA'=1; ;else=0")
CrossTable(death$latino_carrib)
CrossTable(death$latino_carrib, death$private_detention_center,
           prop.r=F, prop.t=F, prop.chisq=F)
CrossTable(death$company)
CrossTable(death$`COUNTRY OF BIRTH`)

public <- death[death$private_detention_center==0,]; dim(public)
private <- death[death$private_detention_center==1,]; dim(private)

pub_tl <-table(public$yod)
priv_tl <- table(private$yod); 
priv_tl <- priv_tl[-length(priv_tl)]

x <- 1:length(priv_tl)

plot(x, pub_tl, type='l')
lines(x, priv_tl, col="red", lty=2)
