#TODO: intro

# library(readxl)
library(tidyverse)
# import excel dataset
# accounts <- read_delim("data.csv", delim=";",   escape_double = FALSE, trim_ws=TRUE,
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

accounts <- read_excel("data.xlsx",
                       col_types = c("text", "text", "text", 
                                     "text", "numeric", "text", "text", 
                                     "numeric", "text", "text", "text", 
                                     "text", "numeric", "numeric", "text", 
                                     "text", "text", "text", "text", "text", 
                                     "numeric", "text", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "text", "text", 
                                     "text", "text", "numeric", "numeric"))

accounts.recast <- accounts %>%
  mutate(customerType = as.numeric(customerType), # convert customerType to numeric
         creationDate = parse_date(creationDate), # convert creationDate to date
         dateOfBirth = parse_date(dateOfBirth),   # convert dateOfBirth to date
         date_of_assessment = parse_date(date_of_assessment)) %>% # convert date_of_assessment to date
  mutate_if(is.character, list(~na_if(.,"NULL"))) # convert string "NULL" to NA datatype in character columns

accounts.cleaned <- accounts.recast %>%
  mutate(birthPlace = str_replace(birthPlace, "[?]", replacement=NA_character_)) # in column birthPlace, convert any string containing a "?" to an NA datatype

# calculate percentage of missing NAs
percentage.na <- accounts.cleaned %>% summarise(across(everything(), ~ sum(is.na(.x))/length(.x)))
unique.values <- accounts.cleaned %>% summarise(across(everything(), ~ length(unique(.x))))
# discard columns listed below - we are gonna have to change this line
# accounts.tidy <- select(accounts, 
#                         -c(birthPlace,
#                            extraNationality,
#                            birthCountry, 
#                            profession, 
#                            companyType, 
#                            giinCode, 
#                            lastUpdate, 
#                            ledgerCode, 
#                            legal_Stat_desc, 
#                            score_card_Desc))  

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
                           score_card_Desc
                           ))

# discarded because the columns are the same for everybody
accounts.tidy <- select(accounts.tidy,
                        -c(IsBlackListed,
                           org_code, 
                           ))
# 


# View(accounts.recast)
View(accounts.tidy)
# date columns are imported as char, here we convert them to date format



# generate dummy variables for "score_card" and "rba_grade_desc"
accounts.encoded <- accounts.recast %>%
  mutate(dummy.acc = 1) %>% # column with single value
  spread(key = score_card, # column to spread
         value = dummy.acc,
         fill = 0,
         sep = "_"
  ) %>%
  mutate(dummy.rba_grade=1) %>%
  spread(key=rba_grade_desc,
         value=dummy.rba_grade,
         fill=0,
         sep="_")

View(accounts.encoded)

sum(is.na(accounts$birthCountry))

#accounts$birthCountry %>% replace_na('Empty')3

#temp <- recode(accounts$birthPlace, "?????" = "NULL")

# sum(accounts$birthPlace=="?????")
# 
# colnames(accounts.recast)
# 
# View(account.logit.data )
accounts.logit <- accounts.tidy
accounts.logit$rba_grade_desc[accounts.logit$rba_grade_desc != "Low"] <- 1
accounts.logit$rba_grade_desc[accounts.logit$rba_grade_desc == "Low"] <- 0
accounts.logit$rba_grade_desc = as.numeric(accounts.logit$rba_grade_desc)

train <- accounts.logit[5001:224866,]
test <- accounts.logit[1:5000,]

sapply(train, function(x) sum(is.na(x)))

#create the model
logit.model <- glm(rba_grade_desc ~ avg_cash_deposit_90_days, family=binomial(link='logit'), maxit = 100, data=test)

##liner model

#variables <- accounts.recast %>% select_if(is.numeric)   
#variables %>% head(5)
# account.logit.data.subset <- select(account.logit.data, c(avg_last_90_days, avg_last_30_days, avg_cash_deposit_90_days, rba_grade_desc))

#calculate auc 
library(ROCR)
p <- predict(logit.model,newdata=train,type='response')
pr <- prediction(p, train$rba_grade_desc)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#print the number of na values in every column


#create the logit regression model
predictions <- predict(logit.model, test)

#summarize the model
summary(logit.model)


train %>% mutate(risk_value = factor(rba_grade_desc, levels = c( 0, 1))) -> train
#test %>% mutate(gender = factor(gender, levels = c("Male","Female"))) -> test



library(modelr)

##liner model

#variables <- accounts.recast %>% select_if(is.numeric)   
#variables %>% head(5)
accounts.subset <- select(accounts.recast, c(avg_last_90_days, avg_last_30_days, avg_cash_deposit_90_days, rbaValue, rba_grade_desc))

train <- accounts.subset[5001:224866,]
test <- accounts.subset[1:5000,]

#print the number of na values in every column
sapply(train,function(x) sum(is.na(x)))

#create the linear regression model
lm.model <- lm(rbaValue ~ c(avg_last_90_days), data = train)
predictions <- predict(lm.model, test)

#summarize the model
summary(lm.model)

#calculate root means squared error
rmse(lm.model, test)


