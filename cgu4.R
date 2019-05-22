##########################################
# Loren Collingwood                      #
# UC Riverside                           #
# DATE: 5/22/2019                        #
# Media Semi-Automated Content Analysis  #
# CNN Broadcast Segment Functions        #
# READs IN .RTF FILES                    #
# PUT INTO ONE DIRECTORY                 #
##########################################

# Clear working environment #

rm(list=ls())

##############################
#     ---- Packages ----     #
##############################

library(quanteda)
library(striprtf)
library(stringr)
library(readxl); library(writexl)
library(svMisc)

#################
# Set Directory #
#################

setwd("~/Dropbox/collingwood_research/cgu_workshop/text_analysis"); list.files()

####################################################
# Create directory with single media segment files #
# Lexis-Uni downloads single CNN media segments    #
# Each segment is a different .rtf file            #
####################################################

source ("cgu4_functions.R")

setwd("~/Dropbox/collingwood_research/cgu_workshop/text_analysis/cnn"); list.files()

####################################################
# Read in .rtf Data -- takes a little while to run #
####################################################

rtf_dat <- read_rtf_single_directory(getwd(), filter_words = "child detention|children detained|children detention|detention policy|family separation|families separated|zero tolerance")

# Look at Output --two item list: dataframe and list #
names(rtf_dat)

# Look at top 6 rows of dataframe object #
head(rtf_dat$df)

# The listed data looks like this: #
rtf_dat$df_list[[1]]

##################################
# DEVELOP CHILD SEPARATION WORDS #
##################################

# Randomly Select 10 segments (already started with 1) #
set.seed(8009174)
rand <- sort ( sample(1:length( rtf_dat$df_list), 9) )

#####################################
# Examine Random Subset of Segments #
#####################################

# You will want to look at each full segment first to determine which lines to broadly include #

segment1 <- rtf_dat$df_list[[1]][107:192]
segment2 <- rtf_dat$df_list[[rand[1]]] 
segment3 <- rtf_dat$df_list[[rand[2]]]
segment4 <- rtf_dat$df_list[[rand[3]]]
segment5 <- rtf_dat$df_list[[rand[4]]][1:105]
segment6 <- rtf_dat$df_list[[rand[5]]][c(1:74, 85:106, 127, 148:173)]
segment7 <- rtf_dat$df_list[[rand[6]]][c(4, 14, 45:123)]
segment8 <- rtf_dat$df_list[[rand[7]]][40:72]
segment10 <- rtf_dat$df_list[[rand[9]]][1:32]

#################################################################
# Filter Broadcast Transcript -- relevant Paragraphs/Lines Only #
#################################################################

# remove words that don't seem to help #
# this is an iterative process, where you look at top words, and add in words here
# that don't discriminate

general_remove <- c("abbi","abl","absolut","actual","admit","affect","ago","ahead","allow","along", 
                    "alreadi", "also","analyst","announc","anoth","answer","anyon","anyth","anywher",
                    "apart","appear","appreci","area","around","argu","argument","ask","away",
                    "back","basic","becom","begin","behind","believ","better","bit","blame","bori",
                    "break","bring","brought","call","came","camera","can","care","carri","case",
                    "caus","certain","chang", "choos","chose","clear","clip","cnn","come","comfort",
                    "comment","commerc", "compar","complet","condit","continu","controversi",
                    "countri","correspon","correspond","coupl","cours","creat","credi","crosstalk",
                    "day","deal", "debat", "decid","differ","dig","direct","discuss","distanc", "done",
                    "earlier","eighth","end","enough", "even","ever","everi","everybodi","everyon",
                    "exact","exampl", "expect", "face","fact","fair","feel","felt", "figur","find", 
                    "first","folk","follow","forget","former","forward","found","fourth","fifth",
                    "five","front", "general","get","give","go","good","got","great","group","grow",
                    "hand","happen","head","heard", "here","ignor", "imagin", "import","includ",
                    "inform", "insid","intent","intend", "interest","issu","item", "join", "just",
                    "last","learn", "led", "listen","long","lucki", "kind","know","leav","let","lie",
                    "life","like","live","longer","look","lot", "made","main", "make","man","mani",
                    "matter","mayb", "mean","meet","might", "move","movi","mr","mrs", "much","multipl",
                    "near","need","needl","new","news","newsroom", "next","ninth","norm", "now",
                    "number","obvious","ok","one","onto","old","own","part","particular","pattern",
                    "peopl", "perhap","period","person","phone","photo","pictur","place","point",
                    "potenti","practic","present","press","pressur","proceed","program","properti",
                    "put","question","quick","quit","read","reason","real","realli","receiv","rememb",
                    "report","right", "said","saw","say","second","see","seem","seen","send", "sever",
                    "show","side","situat", "someth","sometim","sort","soon","sound", "speak","spend",
                    "spokesperson","start","statement","step", "still","stop","stori", "support", 
                    "suppos","sure","take","tal","taken","talk","tell","tenth","term", "thank",
                    "thing","think","third","though","thought","time","today","told","tomorrow",
                    "took","tour","toward","tri","turn","tv","two","understand", "us","use","video",
                    "videotap","view", "wait","want","watch", "way","well","whether","without", "word",
                    "wrote","year","yes")

# Make sure only in there once #
table(general_remove)

##############################
# Execute top_words Function #
##############################

top_words(segment1, general_remove = general_remove)
top_words(segment2, general_remove = general_remove)
top_words(segment3, general_remove = general_remove)
top_words(segment4, general_remove = general_remove)
top_words(segment5, general_remove = general_remove)
top_words(segment6, general_remove = general_remove)
top_words(segment7, general_remove = general_remove)
top_words(segment8, general_remove = general_remove)
top_words(segment10, general_remove = general_remove)

####################################################
# Create Word Dictionary to select 'relevant' rows #
####################################################

broadcast_dict1 <- sort(c("children","child", "famili","separ","southern", "border","parent","immigr",
                          "facil","asylum","flee","undocu","detent","hate", "inhuman",
                          "deter","releas", "cage","center","cruel","rip", "toler", "zero",
                          "travel", "secur", "homeland", "texa", "accompani", "enforce"))
broadcast_dict2 <- sort(c("kid", "citizen", "deport", "protest", "prison", "imprison", "refuge", "march", 
                          "demonstrat", "mexico", "mexican", "moral", "rally", "rallies", "compassion",
                          "reunify", "reunification", "crowd", "voice", "wall", "ice", "dhs", "agenci",
                          "agency", "abolish", "civic", "activism"))
broadcast_dict4 <- sort(c("deterr", "process", "fence", "unaccompani", "minor", "pen", "mother", 
                          "guatemala", "customs", "spanish", "cross", "smuggler", "coyote", "split"))
broadcast_dict6 <- sort(c("screen","reunif","reunit", "toddler", "crate", "doll"))
broadcast_dict7 <- sort(c("detain", "migrant","toler"))
broadcast_dict8 <- sort(c("dna"))

# Combine 'key words' together, then sort, for viewing pleasure #
broadcast_full <- sort ( unique(c(broadcast_dict1, broadcast_dict2, broadcast_dict4, broadcast_dict6,
                           broadcast_dict7,broadcast_dict8)) )
# Look at words #
broadcast_full

# Verify Filter Works (via research assistant)

#############################
# Set up Holder: text_clean #
#############################

text_clean <- rep(NA, length(rtf_dat$df_list))

#####################
# Initiate for loop #
#####################

for (i in 1:length(rtf_dat$df_list)){ # initiate i loop
  
  ##########
  # Corpus #
  ##########
  
  corp <- corpus(rtf_dat$df_list[[i]]) 
  
  ###################################################
  # First Doc Freq Matrix (so can stem/clean first) #
  ###################################################
  
  cdfm <- dfm(corp, 
              stem=T, 
              remove_punct=T, 
              remove_numbers =T, 
              remove = stopwords("english"))
  
  print(cdfm)
  
  #############################
  # Sort the columns real nas #
  #############################
  
  cdfm <- dfm_sort(cdfm) 
  
  ########################
  # Apply the dictionary #
  ########################
  
  # Creates 1 column with count of dictionary words appearing in text document (line) #
  cdfm <- dfm(cdfm, dictionary = dictionary(list(broadcast = broadcast_full)))
  
  ############################################
  # Convert DFM to matrix and add on uniq_id #
  ############################################
  
  cdfm_mat <- data.frame( as.matrix(cdfm), uniq_id = 1:nrow(as.matrix(cdfm)), stringsAsFactors = F)
  
  # Subset, take only relevant lines/paragraphs; but still in DFM matrix #
  relevant_lines <- cdfm_mat[cdfm_mat$broadcast>0,]
  
  # Take only the relevant lines-- in the actual text #
  relevant_file <- rtf_dat$df_list[[i]][relevant_lines$uniq_id]
  
  # Paste into one file and sl
  text_clean[i] <- paste ( relevant_file, collapse = " ")
  
} # close i joop

##########################################
# Combine with original DataFrame Object #
##########################################

rtf_final <- data.frame(rtf_dat$df, text_clean = text_clean, stringsAsFactors = F)

###############################
# Execute percent_of Function #
###############################

rtf_final$percent_of_r <- percent_of(rtf_final$text, rtf_final$text_clean)

##########################################
# Place final segment vector into Corpus #
##########################################

corp <- corpus(rtf_final$text_clean) # Corpus
docvars(corp) <- rtf_final$date # Add date real good! 

##############################
# Create Thematic Dictionary #
##############################

theme_dict <- dictionary(list(fambelong=c("families belong together"),
                          empathy=c("torn", "apart","cry", "tears", "crying", "cried", "hug", "rip", 
                                    "ripped"),
                          protest = c("protest", "march", "protesters", "marchers", "marched"),
                          democrat = c("democrat", "democrats", "democratic"),
                          republican = c("republican", "republicans"),
                          partisan = c("democrat", "democrats", "democratic", "republican", "republicans", "aisl"),
                          trump = "trump"))

################################
# Convert to DFM w/ Dictionary #
################################

cnn_dfm <- dfm(corp, dictionary=theme_dict)

# Convert to Data frame Object #
cnn_dfm <- convert(cnn_dfm, "data.frame")

# Sum columns -- just to look at #
apply(cnn_dfm[,-1], 2, sum)

# Place into Dataframe #
cnn_dfm <- data.frame(cnn_dfm, date = lubridate::mdy(rtf_final$date), stringsAsFactors = F)

# Order Dataframe #
cnn_dfm <- cnn_dfm[order(cnn_dfm$date),]

# Split the Data by Day #
cnn_split <- split(cnn_dfm, cnn_dfm$date) # Split by date

#######################################################
# Apply day_sum Function to the list for each 'theme' #
#######################################################

prot <- plyr::ldply(cnn_split, day_sum, "protest")
emp <- plyr::ldply(cnn_split, day_sum, "empathy")
dem <- plyr::ldply(cnn_split, day_sum, "democrat")
rep <- plyr::ldply(cnn_split, day_sum, "republican")
partisan <- plyr::ldply(cnn_split, day_sum, "partisan")
trump <- plyr::ldply(cnn_split, day_sum, "trump")
n <-  plyr::ldply(cnn_split, day_sum, "date", d=T)

################
# Combine Data #
################

comb <- data.frame(prot,
                   Empathy = emp$V1, 
                   Democrat=dem$V1, 
                   Republican=rep$V1, 
                   Partisan = partisan$V1,
                   Trump = trump$V1, 
                   n=n$V1, 
                   week = lubridate::week(prot$.id))

# Label Columns #

colnames(comb)[1:2] <- c("Date", "Protest") 

##############################################
# Convert to Week (depends on what you want) #
##############################################

# Split comb dataset by week variable #
comb_s <- split(comb, comb$week)

####################################
# Column names to send to week_sum #
####################################

vars <- c("Protest","Empathy","Democrat","Republican",
          "Partisan", "Trump", "n")

################################
# Sum: Apply week_sum Function #
################################

comb_w <- plyr::ldply(comb_s, week_sum, vars)

# Adjust Column Name
colnames(comb_w)[1] <- "Date"

############
# Plotting #
############

# Create x,y's #
x <- comb_w$Date; x
y <- comb_w$Protest; y

#################
# Initiate Plot #
#################

plot(x, y, type="l",
     lwd=2, bty="n", ylim =c(0,320),
     ylab="Count of Theme in News Segments",
     xlab = "Week of Year (Date)",
     col="black", 
     main = "CNN Broadcasts\n(#FamiliesBelongTogether)")
lines(x, comb_w$Empathy, lty=2, lwd = 2, col="brown")
lines(x, comb_w$Partisan, lty=4, lwd = 2, col="red")
lines(x, comb_w$Trump, lty=5, lwd = 2, col="pink")

points(x, comb_w$n*2, pch=21)

legend ("topright", legend=c("Protest", "Empathy","Partisan", "Trump", "#Segments*2"),
        col=c("black", "brown", "red", "pink", "black"),
        lty=c(1:4, NA), lwd=c(2,2,2,2, NA), pch= c(rep(NA,4), 21),
        bty="n", cex=.5, title="Theme in Text")
abline(v=26, col="gray", lty=2, lwd=2)
arrows(25, 260,26, 260)
text(24.5, 260, "Protest", cex=.75)

