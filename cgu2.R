#######################################
# Loren Collingwood                   #
# UC Riverside                        #
# DATE: 5/22/2019                     #
# Webscraping with rvest              #
# Topic 1: Immigrant Detention Deaths #
#######################################

rm(list=ls())

############
# Packages # 
############

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Check to see if packages are installed, and then load them
packages<-c("rvest","stringr","striprtf", "gsubfn", "tidyverse","readxl", "quanteda", "descr",
            "ggplot2", "readxl","writexl", "topicmodels", "devtools", "lubridate", "svMisc",
            "data.table", "tm")

check.packages(packages)

# Read in death data located on capital and main website #
deaths <- read_html("https://capitalandmain.com/mapping-death")

class(deaths)

#################################################
# Read in Table-type information                #
#################################################

# Like much of webscraping, the data are unruly #
# Welcome to text anaylysis                     # 

deaths <- (html_table(deaths, fill=T))

##################################################
# Take the first list which is data.frame object #
##################################################

deaths <- deaths[[1]]

# Label column name #
colnames(deaths)[1]<- "Name"

############
# CLEANING #
############

# Recode for checking purposes
deaths$death_r <- ifelse(deaths[,1]== "FINAL CAUSE OF DEATH", 1, 0)

# Check the messed up pattern -- DF not quite what wanted, so need to delete a few `rogue` cases
which(deaths$death_r==1)

# Drop these few here that are messing up the 1:10 flow #
deaths <- deaths[-412,]
deaths <- deaths[-1293,]

#################
# Set up Parser #
#################

splitter <- list()
n <- 1880

############################
# Create splitter variable #
############################

for (i in 1:(n/10)) splitter[[i]] <- rep(i, 10)
splitter <- unlist(splitter); length(splitter)

########################################
# Subset just to the necessary columns #
########################################

tester <- deaths[1:n,c("Name", "Title", "death_r")]
tester <- data.frame(tester, splitter, stringsAsFactors = F)

##############################
# Split data into n/10 lists #
##############################

tester_list <- split(tester, tester$splitter)

# Take a peek #
tester_list

##############################
# Function: death_list_split #
##############################

death_list_split <- function(x){
  
  # Relabels
  x[1,1] <- "Name"
  
  # Returns just the first 2 columns
  x[, c("Name", "Title")]
  
}

##################################
# Execute Function with lapply() #
##################################

out <- lapply(tester_list, death_list_split)

######################################################
# Bind columns; transpose, and put into data.frame() #
######################################################

out <- data.frame(t(dplyr::bind_cols(out)), stringsAsFactors=F)

##################
# Clean the data #
##################

colnames(out) <- out[1,]
rownames(out) <- NULL

#############################
# Drop Rows you don't need  #
#############################

drops <- seq(1, nrow(out), 2)
out <- out[-drops,]

#############################
# Verify Everything is good #
#############################

tail(out)

##########################
# Recode a few Variables #
##########################

out$SEX[out$SEX=="SEX"] <- "M"
out$SEX[out$SEX=="W"] <- "F"
table(out$SEX)

#################################
# Date variables, year of Death #
#################################

out$dob <- lubridate::mdy(out$`DATE OF BIRTH`) # some missing data
out$yob <- lubridate::year(out$dob)
out$dod <- lubridate::mdy(out$`DATE OF DEATH`) # 1 missing data
out$yod <- lubridate::year(out$dod)
out$age_death <- with(out, yod - yob)

#################
# Write to Disk #
#################

write.csv(out, "detention_deaths.csv", row.names=F)

