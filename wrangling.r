#TODO: intro

library(readxl)
library(tidyverse)
library(reshape2)
# import excel dataset and adjust column types
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
percentage.na <- accounts.cleaned %>% summarise(across(everything(), ~ sum(is.na(.x))/length(.x)))

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


# discarding dateOfBirth because it gives no extra information
accounts.tidy <- select(accounts.tidy,
                        -c(dateOfBirth))

# discarding ledgerCode because it is just a random number
accounts.tidy <- select(accounts.tidy,
                        -c(ledgerCode))

# discarded because the columns are the same for everybody
accounts.tidy <- select(accounts.tidy,
                        -c(IsBlackListed,
                           org_code,
                           status,
                           date_of_assessment
                        ))


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

# dropping rows for other columns that have NA values since the NA ratio is low
accounts.tidy.factored.replaced_nas.dropped <- accounts.tidy.factored.replaced_nas %>% filter(!is.na(nationalityOriginal)) %>% 
  filter(!is.na(residentCountry)) %>% filter(!is.na(LEGAL_STA_CODE)) 

# showing unique value for each column
unique.values <- accounts.tidy.factored.replaced_nas %>% summarise(across(everything(), ~ length(unique(.x))))

# showing the percentage of NA values in each column
percentage.na <- accounts.tidy.factored.replaced_nas.dropped %>% summarise(across(everything(), ~ sum(is.na(.x))/length(.x)))

# view of our final data set to use in the logit regression model
View(accounts.tidy.factored.replaced_nas.dropped)

# create another data set name accounts.logit
accounts.logit <- accounts.tidy.factored.replaced_nas.dropped

View(accounts.tidy.factored.replaced_nas.dropped)

# following your instructions - 0 if low, 1 if medium or high
accounts.logit$rba_grade_desc[accounts.logit$rba_grade_desc != "Low"] <- 1
accounts.logit$rba_grade_desc[accounts.logit$rba_grade_desc == "Low"] <- 0
accounts.logit$rba_grade_desc = as.numeric(accounts.logit$rba_grade_desc)

# split data for training
## 75% of the sample size
smp_size <- floor(0.75 * nrow(accounts.logit))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(accounts.logit)), size = smp_size)

# resulting train and test splits
train <- accounts.logit[train_ind, ]
test <- accounts.logit[-train_ind, ]

View(train$rba_grade_desc)
# count the number of NA values in the train data set
sapply(train, function(x) sum(is.na(x)))

# build the logit regression model
logit.model <- glm(rba_grade_desc ~ avg_cash_deposit_90_days, family=binomial(link='logit'), maxit = 100, data=test)
numeric.train <- train %>% select_if(is.numeric)
result <- round(cor(numeric.train), 2)

melted_cormat <- melt(result)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

result

#calculate AUC 
library(ROCR)
p <- predict(logit.model,newdata=train,type='response')
pr <- prediction(p, train$rba_grade_desc)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


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


