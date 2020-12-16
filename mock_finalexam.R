print ("INSTALLLING / IMPORTING LIBRARIES...")
# define used libraries
libraries_used <- c("plyr", "tidyverse", "caret", "httr")

# check missing libraries
libraries_missing <- libraries_used[!(libraries_used %in% installed.packages()[,"Package"])]
# install missing libraries
if(length(libraries_missing)) install.packages(libraries_missing)

library(plyr)
library(tidyverse)
library(caret)


# Visit the following link to download the train dataset: "http://manoelutad.pythonanywhere.com/static/uploads/mfalonso__6aQ6IxU7Va__train.csv"
print ("LOADING DATASETS...")
path <- ""
dev <- readr::read_csv("http://manoelutad.pythonanywhere.com/static/uploads/mfalonso__6aQ6IxU7Va__train.csv")
dev <- dev[,2:83]
#dev <- readr::read_csv("train.csv")
head(dev)

# Visit the following link to download the test dataset: "http://manoelutad.pythonanywhere.com/static/uploads/mfalonso__6aQ6IxU7Va__test.csv"
path <- ""
oot <- readr::read_csv("http://manoelutad.pythonanywhere.com/static/uploads/mfalonso__6aQ6IxU7Va__test.csv")
oot <- oot[,2:83]
head(oot)

# Replacing na with 0 for all columns and rows after the work is done!
dev %>% replace(is.na(.), 1) -> dev
oot %>% replace(is.na(.), 1) -> oot

print ("STEP 1: DOING MY TRANSFORMATIONS...")
# 01) Replace 0 with 1 for variable icn_var_22
dev$ib_var_3[dev$icn_var_22== 0] <- 1
oot$ib_var_3[oot$icn_var_22== 0] <- 1

# 02) Do the negation of "ib_var_1" and "ib_var_2" , if 0 becomes 1 and vice-versa
dev %>% mutate(ib_var_1=1-ib_var_1) %>% mutate(ib_var_2=1-ib_var_2) -> dev
oot %>% mutate(ib_var_1=1-ib_var_1) %>% mutate(ib_var_2=1-ib_var_2) -> oot

# 03) For variables  "ib_var_3", "ib_var_4", "ib_var_5", "ib_var_6", change 1 for 5 and 0 for -5
dev$ib_var_6[dev$ib_var_6 == 1] <- 5
oot$ib_var_6[oot$ib_var_6 == 1] <- 5


# 04) Drop variables "ib_var_7"   "ib_var_8"   "ib_var_9"   "ib_var_10"

# XX) Calculate the square of all categorial nominal variables - "icn_var_22" "icn_var_23" "icn_var_24" 

# YY) Divide column "ico_var_59" "ico_var_60" "ico_var_61" by ("ico_var_62"-1) 
dev$ico_var_59 <- dev$ico_var_59/(dev$ico_var_62-1)

# than replace Inf with the maximum value of the given column without taking the inf into account, hint: use is.infinite


### HOW TO STUDY FOR THE FINAL EXAM!
# 1 - Make sure you revise the group project!
# 2 - The final exam is only about data frames, but it is recommended you
#     revise the IPE to make sure you are familiar with functions 
#     and if/else statements. 
# 3 - The previous should be enough, but it is recommended you revise:
#      Data frame subsetting
#      Dropping variables
#      Data Cleaning
#      Change values with conditions
#      Change types
#      Applying functions to data frames
#      Calculate statistics like mean, etc...
#      Calculate basic operations like +, -, *, / and the remainder



# DO NOT TOUCH ANYTHING FROM HERE!
# Replacing na with 0 for all columns and rows after the work is done!
dev %>% replace(is.na(.), 0) -> dev
oot %>% replace(is.na(.), 0) -> oot

dev$row.sum <- apply(as.matrix(dev[2:length(dev)]),1,sum)
dev$ob_target <- 1*(dev$row.sum/max(dev$row.sum))
dev <- dev[,!(names(dev) %in% c("row.sum"))]

# Developing a model with all variables except the ones you get a NA as P-VALUE
# lm(target ~ . -var1 -var2, data=df) # https://stackoverflow.com/questions/22580379/how-do-i-exclude-specific-variables-from-a-glm-in-r
print ("STEP 3: DEVELOPING THE MODEL...")
model_lm_fit <- lm(ob_target ~ . -id -ib_var_12 -if_var_77 -if_var_78, data=dev)


print ("STEP 4: ASSESSING THE MODEL...")
summary(model_lm_fit)
dev_probs <- data.frame(dev,pred = predict(model_lm_fit,
                                           newdata = dev,
                                           type="response"))
#dev_probs[c("ob_target","pred")]


print ("STEP 5: SUBMITTING THE RESULTS...")

oot_probs <- data.frame(oot,pred = predict(model_lm_fit,
                                           newdata = oot,
                                           type="response"))


write.csv(oot_probs,"test_pred.csv", row.names = FALSE)

library(httr)
url = 'http://mfalonso.pythonanywhere.com/rcourse_finalexam_uploadpredictions/mock'


# MAKE SURE YOU CHANGE user AND password BELOW TO YOUR CREDENTIALS TO http://mfalonso.pythonanywhere.com

response <- POST(url, body = list(file = upload_file("test_pred.csv")),
                 authenticate("user", "password"))


content(response, "text")


