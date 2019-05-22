#############################################
# Loren Collingwood                         #
# UC Riverside                              #
# DATE: 5/22/2019                           #
# Webscraping with rvest                    #
# Topic 2: Prison Company News Releases     #
#############################################

#####################
# Clear the Console #
#####################

rm(list=ls())

############
# Packages #
############

library(rvest)
library(readxl)
library(lubridate)
library(svMisc)
library(data.table)

#################
# Set Directory #
#################

setwd("~/Dropbox/collingwood_research/cgu_workshop/text_analysis"); list.files()

######################
# Read in Links Data #
######################

links <- read.csv("cca_links.csv", header=T, stringsAsFactors = F)

links <- links$link

# Initiate list holder/container #
cca_hold <- list()

n <- length(links)

#################
# Initiate Loop #
#################

for ( i in 1:n){

  if (i == 1) message("Start")
  if (i == round(n*.5,0)) message("50% Done")
  if (i == n) message("Done!")
  
  #############################################
  # Read in Link -- this is the main iterator #
  #############################################
  
  cca <- read_html(links[i])
  
  ################
  # Extract Date #
  ################
  
  text <- html_text(html_nodes(cca, "div div")); length(text)
  
  # Date Regular Expression and Clean #
  dates <- str_squish( str_extract(text, "[a-zA-Z]+ [0-9]+[,]+\\s+[0-9]{4}") )
  
  # Further Cleaning #
  dates <- dates[!is.na(dates)][1] # take out nas and select first date
  
  ####################
  # Extract Headline #
  ####################
  
  # Collapse Text into Vector #
  text <- paste(text, collapse=" ")
  
  text_manip <- unlist ( str_split(text, dates) )[1] # take the first split on the date
  text_manip <- unlist(str_split(text_manip, "\n\n\n")) # Assumes \n\n\n is in all releases
  text_manip <- text_manip[length(text_manip)] # assumes the headline is last
  headline <- str_trim(text_manip) # Store into headline vector
  
  rm(text) # Garbage Clean #
  
  ######################################
  # Get Paragraph Text of News Release #
  ######################################
  
  text <- html_text(html_nodes(cca, "div p")); length(par)
  text <- paste(text, collapse = " ")
  text <- gsub("\r","", text)
  text <- gsub("\n","", text)
  
  # Place data frame into List Item #
  cca_hold[[i]] <- data.frame(dates, headline, text, stringsAsFactors = F)
  
}

###########################################
# Convert List of dataframes to dataframe #
###########################################

cca_df <- rbindlist(cca_hold)

########################
# Look at Column Names #
########################

names(cca_df)

#################
# Look at Dates #
#################

cca_df$dates <- lubridate::mdy(cca_df$dates) # Convert dates #
cca_df$year <- lubridate::year(cca_df$dates) # Convert dates #
cca_df$headline

############################################
# Take a quick look at Yearly Distribution #
############################################

hist(cca_df$year)

#######################
# Write out .csv File #
#######################

write.csv(cca_df[, c("dates","year", "headline")], "cnews_release.csv", row.names=F)

