#######################################
# Loren Collingwood                   #
# UC Riverside                        #
# Functions for cleaning and          #
# manipulating CNN broadcast segments #
#######################################

#########################################
#  Function: read_rtf_single_directory  #
#########################################
# @ dir = directory/path where all the .rtf files are located
# @ filter_yes = logical. Do you want to filter the files based on key words. Default = TRUE.
# @ filter_words = vector of words to filter files through. e.g., "family separation|families separated"

read_rtf_single_directory <- function(dir, filter_yes=TRUE, filter_words){
  
  
  #############################################################
  # Create vector of .rtf file names (from working directory) #
  #############################################################
  
  rtf_files <- list.files(dir)[grep( "*.rtf", tolower(list.files()) )]
  
  # Take length #
  n <- length( rtf_files )
  
  ###########################################
  # Filler for reading in files & meta data #
  ###########################################
  
  hold <- rep(NA, n)
  hold_meta <- list()
  hold_date <- rep(NA, n)
  hold_section <- rep(NA, n)
  hold_word_length <- rep(NA, n)
  hold_byline <- rep(NA, n)
  hold_guests <- rep(NA, n)
  hold_highlight <- rep(NA, n)
  
  ##################################################
  # Loop Over files, read in and paste into vector #
  ##################################################
  
  for (i in 1:n){ # open i loop
    
    # 'Progress' Bar #
    if (i == 1) message("Start")
    if (i == round(n*.25,0)) message("25% Did!")
    if (i == round(n*.5,0)) message("50% Did!")
    if (i == round(n*.75,0)) message("75% Did!")
    if (i == n) message("Done!")
    
    ################################
    # Read file i into list object #
    ################################
    
    hold_meta[[i]] <- read_rtf( paste(dir, rtf_files[i], sep="/") )
    
    ######################################################
    # Paste into vector for later addition to data frame #
    ######################################################
    
    hold[i] <- paste(hold_meta[[i]], collapse = " ")
    
    ##########################################
    #     -----    Meta Data    -----        #
    ##########################################
    
    # Date # -- Assumes written as: January 20, 2019, e.g. 
    date_look <- str_extract(hold_meta[[i]], "[a-zA-Z]+\\s+[0-9]+[,]+\\s+[0-9]{4}") 
    hold_date[i] <- date_look[!is.na(date_look)][1] # take first date
    
    # Section #
    section_line <- grep("Section:", hold_meta[[i]], fixed=T)[1]
    hold_section[i] <- str_squish ( gsub( ".*:","", hold_meta[[i]][section_line] ) )
    
    # Word Length #
    word_length_line <- grep("Length:", hold_meta[[i]], fixed=T) [1]
    hold_word_length[i] <- str_extract ( str_squish ( gsub( ".*:","", hold_meta[[i]][word_length_line] ) ),  "[[:digit:]]+")
    
    # Byline # 
    byline_line <- grep("Byline:", hold_meta[[i]], fixed=T) [1]
    hold_byline[i] <- str_squish ( gsub( ".*:","", hold_meta[[i]][ byline_line ] ) )
    
    # Guests #
    guests_line <- grep("Guests:", hold_meta[[i]], fixed=T) [1]
    hold_guests[i] <- str_squish ( gsub( ".*:","", hold_meta[[i]][ guests_line ] ) )
    
    # Highlights #
    high_line <- grep("Highlight:", hold_meta[[i]], fixed=T) [1]
    hold_highlight[i] <- str_squish ( gsub( ".*:","", hold_meta[[i]][ high_line ] ) )  
    
  } # close i loop
  
  #################################################################
  #   ----  Create DataFrame Object + unique ids + Metadata  ---- #
  #################################################################
  
  hold_dat <- data.frame(uniq_id = 1:length(hold),
                         filename = rtf_files,
                         text = hold, 
                         highlight = hold_highlight,
                         date = hold_date, 
                         section = hold_section,
                         word_num = as.numeric(hold_word_length),
                         byline = hold_byline,
                         guests = hold_guests,
                         stringsAsFactors = F)
  
  ###########################
  # Filter the Data Further #
  ###########################
  
  if (filter_yes){
    
    hold_dat <- hold_dat[ grepl(filter_words, hold_dat$text), ] 
    
  }
  
  # Print out New Dimensions #
  print ( dim(hold_dat) )
  
  #########################################################
  # Subset List (for later filtering & text manipulation) #
  #########################################################
  
  filtered_list <- list()
  
  for (j in 1:length(hold_dat$uniq_id)){
    
    filtered_list[[j]] <- hold_meta[[ hold_dat$uniq_id[j] ]]
    
  }
  
  ######################################################
  # Return Data Frame and List (readLines-type object) #
  ######################################################
  
  return ( list(df = hold_dat, df_list = filtered_list) )
  
}

#################################
#     Function: top_words       #
#################################
# @segment: list object from rtf read in 
# @general_remove: vector of words to remove from print out list (in addition to stopwords)
# @min_termfreq: minimum number of words a term must show up, default = 3
# @min_docfreq: minimum number of documents a term must be in, default = 2

top_words <- function(segment, general_remove,
                      min_termfreq=3, min_docfreq = 2){
  
  #############################
  # Convert segment to corpus #
  #############################
  
  corp <- corpus(segment) 
  
  ###############################
  #  -- Initial DFM Creation -- #
  ###############################
  
  cnn_dfm <- dfm(corp, stem=T, 
                 remove_punct=T, 
                 remove_numbers =T, 
                 remove = c(stopwords("english"))
  )
  
  cat("\nInitial DFM Feature Size:\n")
  print ( cnn_dfm )
  
  ####################################
  #   --  General Word Removal  --   #
  ####################################
  
  cnn_dfm <- dfm(cnn_dfm, remove = general_remove)
  
  #########################
  #   -- DFM Sorting --   #
  #########################
  
  cnn_dfm <- dfm_sort(cnn_dfm)
  cnn_dfm <- dfm_trim(cnn_dfm, min_termfreq=min_termfreq, min_docfreq = min_docfreq); 
  
  cat("\nThinned DFM Feature Size:\n")
  
  print ( cnn_dfm )
  
  cat("\nNow select relevant 'topic' words\n")
  
  ###############################
  # Convert to DataFrame Object #
  ###############################
  
  cnn_dfm <- convert(cnn_dfm, "data.frame")
  
  ##############################
  # Return vector of top words #
  ##############################
  
  return ( sort( names(rev(sort ( apply(cnn_dfm[,-1] , 2, sum) ))) ) )
  
}


###########################
# Function: percent_of    #
###########################
# @original_text: character vector of original segments
# @clean_text: character vector of cleaned segments 

percent_of <- function(original_text, clean_text){
  
  # Verify the vectors are character #
  stopifnot(class(original_text)=="character")
  stopifnot(class(clean_text)=="character")
  
  # Tokenize the data #
  ot <- tokens(original_text)
  ct <- tokens(clean_text)
  
  # Return vector of percents #
  return ( sapply(ct, function(x) length(x)) / sapply(ot, function(x) length(x)) ) 
  
}

#####################
# Function: day_sum #
#####################
# @ lst = list object (i.e., split object of datasets)
# @ var = character of column name 
# @ d = logical; default = FALSE, indicator whether variable is date variable 

day_sum <- function(lst, var, d=FALSE) { # write function to sum variable by date
  if(d){ # just for date variable sum #
    nrow(lst) # can't sum date so take number of rows #
  } else {
    sum(lst[, var]) # sum variable
  }
}

######################
# Function: week_sum #
######################
# @ x = weekly dataset
# @ vars = character vector of relevant column names 

week_sum <- function(x, vars) {
  
  words <- apply(x[, vars], 2, sum)
  return( c(words))
  
}
