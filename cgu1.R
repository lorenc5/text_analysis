#########################
# Loren Collingwood     #
# UC Riverside          #
# CGU Text Presentation #
# Text Manipulation     #
# Date: 5/22/2019       #
#########################

library(stringr)

shopping_list <- c("apples x45!", "loaf of bread", "Bag of sugar", "milk x2 or x3")

#extract the first number in a string; remember: NA in R denotes a missing value.
str_extract(shopping_list, "\\d")

#extract first lower case character in a string
str_extract(shopping_list, "[a-z]")

#extract lower case characters one or more times (note the "+" symbol after "[a-z]")
str_extract(shopping_list, "[a-z]+")

#extract up to four lower case letters
str_extract(shopping_list, "[a-z]{1,4}")

#extract up to four upper or lower case letters
str_extract(shopping_list, "[A-z]{1,4}")

#extract words smaller than or equal to four letters
str_extract(shopping_list, "\\b[a-z]{1,4}\\b")

#extract all matches
str_extract_all(shopping_list, "[A-z]+")

str_extract_all(shopping_list, "\\d")

#note that str_extract_all has a list of character strings as output. 
#This can be simplified into a character matrix using the simplify command
str_extract_all(shopping_list, "\\b[a-z]+\\b", simplify = TRUE)

str_extract_all(shopping_list, "\\d", simplify = TRUE)

unlist(str_extract_all(shopping_list, "\\b[a-z]+\\b"))

#replace first match
str_replace(shopping_list, "[aeiou]", "-")

#replace all matches
str_replace_all(shopping_list, "[aeiou]", "-")

#upper case
str_to_upper(shopping_list)

#lower case
str_to_lower(shopping_list)


#In R, you write regular expressions as strings, sequences of characters surrounded by quotes ("“) or single
#quotes (”). Characters like +, ?, ˆ, and . have a special meaning in regular expressions and cannot be
#represented directly in an R string (see the RegEx cheat sheet for more examples). In order to match them
#literally, they need to be preceded by two backslashes: “\”.

name_list <- c("Jo.hn", "Anna.", "Si.+si")

#compare the output of these two calls
str_replace(name_list, ".", "-")

str_replace(name_list, "\\.", "-")

#compare the output of these two calls
str_replace(name_list, ".+", "-")

str_replace(name_list, "\\.\\+", "-")

