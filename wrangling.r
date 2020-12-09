#TODO: intro
load("data-wrangling.RData")
library(readxl)
library(tidyverse)
library(modelr)
library(reshape2)
# TODO: research rocr
library(ROCR)
# Alt + - for <- 
# Ctrl + Shift + M for %>% 

# import excel dataset and adjust column types
# TODO: change to csv (maybe not)
# accounts <- read_excel("data.xlsx",
#                        col_types = c("text", "text", "text", 
#                                      "text", "numeric", "text", "text", 
#                                      "numeric", "text", "text", "text", 
#                                      "text", "numeric", "numeric", "text", 
#                                      "text", "text", "text", "text", "text", 
#                                      "numeric", "text", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "numeric", 
#                                      "numeric", "numeric", "text", "text", 
#                                      "text", "text", "numeric", "numeric"))
# "takes a while, we won't bother with making you wait"

glimpse(accounts)
# column type conversion
accounts.recast <- accounts %>%
  mutate(customerType = as.numeric(customerType), # convert customerType to numeric
         creationDate = parse_date(creationDate), # convert creationDate to date
         dateOfBirth = parse_date(dateOfBirth),   # convert dateOfBirth to date
         date_of_assessment = parse_date(date_of_assessment)) %>% # convert date_of_assessment to date
  mutate_if(is.character, list(~na_if(.,"NULL"))) # convert string "NULL" to NA datatype in character columns

# in column birthPlace, convert any string containing a "?" to an NA datatype
accounts.cleaned <- accounts.recast %>%
  mutate(birthPlace = str_replace(birthPlace, "[?]", replacement=NA_character_)) 

# calculate percentage of missing NAs
(percentage.na <- accounts.cleaned %>% summarise(across(everything(), ~ sum(is.na(.x))/length(.x))))


# discarding columns with more than 50% of missing values
accounts.tidy <- select(accounts.cleaned,
                        -c(extraNationality,
                           birthCountry,
                           birthPlace,
                           profession,
                           giinCode,
                           lastUpdate,
                           GENDER, # removing gender although we could later decide to analyze it in depth (questions of gender inequality?)
                           legal_Stat_desc,
                           score_card_Desc,
                           companyType
                        ))


# discarding dateOfBirth because it gives no extra information over Age
accounts.tidy <- select(accounts.tidy,
                        -c(dateOfBirth))

# discarding ledgerCode because it is just a random number
accounts.tidy <- select(accounts.tidy,
                        -c(ledgerCode))
# TODO: move unique_counter and na_counter to beginning and clarify
map_dbl(accounts.tidy, unique_counter)

# discarded because the columns are the same for everybody
accounts.tidy <- select(accounts.tidy,
                        -c(IsBlackListed,
                           org_code,
                           status,
                           date_of_assessment))

# encoding rbagradedesc to 0 if low or 1 if medium or high
accounts.tidy$rba_grade_desc[accounts.tidy$rba_grade_desc != "Low"] <- 1
accounts.tidy$rba_grade_desc[accounts.tidy$rba_grade_desc == "Low"] <- 0
accounts.tidy$rba_grade_desc = as.numeric(accounts.tidy$rba_grade_desc)





# we are converting columns with levels to factors
accounts.tidy.factored <- accounts.tidy %>%  mutate_at(c('customerType', 
                              'onboarding', 
                              'residentStatus',
                              'jointAccount',
                              'LEGAL_STA_CODE',
                              'rbaGradeAbrv',
                              'score_card'), as.factor)

# imputing NA values with median value (na.rm is TRUE to ignore NA values during median calculation)
accounts.tidy.factored.replaced_nas <- accounts.tidy.factored %>%  
    replace_na(list(age_in_year=median(accounts.tidy.factored$age_in_year, na.rm = TRUE)))

na_counter <- function(x) sum(is.na(x))

map_dbl(accounts.tidy.factored.replaced_nas, na_counter)

# dropping observations for other columns that have NA values since the NA ratio is low
accounts.tidy.factored.replaced_nas.dropped <- accounts.tidy.factored.replaced_nas %>% 
  filter(!is.na(nationalityOriginal)) %>% 
  filter(!is.na(residentCountry)) %>% 
  filter(!is.na(LEGAL_STA_CODE)) 
map_dbl(accounts.tidy.factored.replaced_nas.dropped, na_counter)
rows_dropped <- nrow(accounts.tidy.factored.replaced_nas) - nrow(accounts.tidy.factored.replaced_nas.dropped)

print(paste("Dropped", rows_dropped, "rows out of", nrow(accounts.tidy.factored.replaced_nas)))

# Number of NAs per column
# sapply(accounts.tidy.factored.replaced_nas.dropped, function(x) sum(is.na(x)))
map_dbl(accounts.tidy.factored.replaced_nas.dropped, na_counter)

unique_counter <- function(x) length(unique(x))
map_dbl(accounts.tidy.factored.replaced_nas.dropped, unique_counter)

# showing unique value for each column
unique.values <- accounts.tidy.factored.replaced_nas %>% summarise(across(everything(), ~ length(unique(.x))))

# showing the percentage of NA values in each column
percentage.na <- accounts.tidy.factored.replaced_nas.dropped %>% summarise(across(everything(), ~ sum(is.na(.x))/length(.x)))

# view of our final data set to use in the logit regression model
View(accounts.tidy.factored.replaced_nas.dropped)
# TODO: encode categorical variable as flag


is.numeric(c(1,2))
# select numeric only since it is the only valid way to do a model
numeric.accounts.tidy <- accounts.tidy.factored.replaced_nas.dropped %>% 
  select_if(is.numeric)

map_lgl(accounts.tidy.factored.replaced_nas.dropped, is.numeric)

result <- round(cor(numeric.accounts.tidy), 2)
melted_cormat <- melt(result)
head(melted_cormat)


ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

# TODO: print labels diagonally

# we find that avg_last_90, 30 and 10 are highly correlated
# we could do PCA but we will discard 2 of them

numeric.accounts.tidy.removed.cor <- numeric.accounts.tidy %>% 
  select(-c(avg_last_30_days, avg_last_10_days))

View(accounts.tidy.factored.replaced_nas.dropped)


# modeling

# create another data set name accounts.logit
# remove rbaValue from the dataset
accounts.logit <- numeric.accounts.tidy.removed.cor %>% select(-c(rbaValue))

# split data for training
## 75% of the sample size
smp_size <- floor(0.75 * nrow(accounts.logit))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(accounts.logit)), size = smp_size)

# resulting train and test splits
train <- accounts.logit[train_ind, ]
test <- accounts.logit[-train_ind, ]

# count the number of NA values in the train data set
sapply(train, function(x) sum(is.na(x)))

# build the logit regression model
logit.model <- glm(rba_grade_desc ~ ., family=binomial(link='logit'), maxit = 100, data=train)
# TODO: give more info on glm function
summary(logit.model)
# could remove insignificant variables

#calculate AUC 

p <- predict(logit.model,newdata=train,type='response')
pr <- prediction(p, train$rba_grade_desc)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# test on test dataset to see if it's overfitted or not
p <- predict(logit.model,newdata=test,type='response')
pr <- prediction(p, test$rba_grade_desc)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# train: 0.6144642
# test: 0.6146259
# similar




##liner model

accounts.linear <- numeric.accounts.tidy.removed.cor %>% select(-c(rba_grade_desc))

# split data for training
## 75% of the sample size
smp_size <- floor(0.75 * nrow(accounts.linear))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(accounts.linear)), size = smp_size)

# resulting train and test splits
train <- accounts.linear[train_ind, ]
test <- accounts.linear[-train_ind, ]

# count the number of NA values in the train data set
sapply(train, function(x) sum(is.na(x)))

#variables <- accounts.recast %>% select_if(is.numeric)   
#variables %>% head(5)
#create the linear regression model
lm.model <- lm(rbaValue ~ ., data = train)
# predictions <- predict(lm.model, test)

#summarize the model
summary(lm.model)

#calculate root means squared error
rmse(lm.model, train)
rmse(lm.model, test)


