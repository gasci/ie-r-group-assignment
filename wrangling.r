library(readxl)
library(tidyverse)
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


# discard columns listed below - we are gonna have to change this line
accounts.tidy <- select(accounts, 
                        -c(birthPlace,
                           extraNationality,
                           birthCountry, 
                           profession, 
                           companyType, 
                           giinCode, 
                           lastUpdate, 
                           ledgerCode, 
                           legal_Stat_desc, 
                           score_card_Desc)) %>%  na_if("NULL")

View(accounts.recast)

# date columns are imported as char, here we convert them to date format
accounts.recast <- accounts.tidy %>%
  mutate(customerType = as.numeric(customerType),
         creationDate = parse_date(creationDate),
         dateOfBirth = parse_date(dateOfBirth),
         date_of_assessment = parse_date(date_of_assessment))


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

sum(accounts$birthPlace=="?????")

colnames(accounts.recast)

#train %>% mutate(gender = factor(gender, levels = c("Male","Female"))) -> train
#test %>% mutate(gender = factor(gender, levels = c("Male","Female"))) -> test

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
