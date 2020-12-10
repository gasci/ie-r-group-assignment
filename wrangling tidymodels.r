load("data-wrangling.RData")
library(tidymodels)
library(tidyverse)
library(readxl)
library(reshape2)
# library(modelr)


#library(workflows)

# Alt + - for <- 
# Ctrl + Shift + M for %>% 

# import excel dataset and adjust column types
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
  mutate(creationDate = parse_date(creationDate), # convert creationDate to date
         dateOfBirth = parse_date(dateOfBirth),   # convert dateOfBirth to date
         date_of_assessment = parse_date(date_of_assessment)) %>% # convert date_of_assessment to date
  mutate_if(is.character, list(~na_if(.,"NULL"))) # convert string "NULL" to NA datatype in character columns

# in column birthPlace, convert any string containing a "?" to an NA datatype
accounts.cleaned <- accounts.recast %>%
  mutate(birthPlace = str_replace(birthPlace, "[?]", replacement=NA_character_)) 

# helper functions for inspecting the data
unique_counter <- function(x) length(unique(x))
na_counter <- function(x) sum(is.na(x))
na_proportion <- function(x) mean(is.na(x))

# calculate percentage of missing NAs
map_dbl(accounts.cleaned, na_proportion)

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
                           companyType #49% of missing values
                        ))


# discarding dateOfBirth because it gives no extra information over Age
accounts.tidy <- select(accounts.tidy,
                        -c(dateOfBirth))

# discarding ledgerCode because it is just a random number
accounts.tidy <- select(accounts.tidy,
                        -c(ledgerCode))

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
accounts.tidy$rba_grade_desc = as.factor(accounts.tidy$rba_grade_desc)





# we are converting columns with levels to factors
accounts.tidy.factored <- accounts.tidy %>%  mutate_at(c('customerType', 
                              'onboarding', 
                              'residentStatus',
                              'jointAccount',
                              'LEGAL_STA_CODE',
                              'rbaGradeAbrv',
                              'score_card'), as.factor)

# imputing NA values with median value (na.rm is TRUE to ignore NA values during median calculation)


map_dbl(accounts.tidy.factored, na_counter)

accounts.tidy.factored.replaced_nas <- accounts.tidy.factored %>%  
    replace_na(list(age_in_year=median(accounts.tidy.factored$age_in_year, na.rm = TRUE)))


map_dbl(accounts.tidy.factored.replaced_nas, na_counter)

# dropping observations for other columns that have NA values since the NA ratio is low
accounts.tidy.factored.replaced_nas.dropped <- accounts.tidy.factored.replaced_nas %>% 
  filter(!is.na(nationalityOriginal)) %>% 
  filter(!is.na(residentCountry)) %>% 
  filter(!is.na(LEGAL_STA_CODE)) 
map_dbl(accounts.tidy.factored.replaced_nas.dropped, na_counter)
rows_dropped <- nrow(accounts.tidy.factored.replaced_nas) - nrow(accounts.tidy.factored.replaced_nas.dropped)

print(paste("Dropped", rows_dropped, "rows out of", nrow(accounts.tidy.factored.replaced_nas)))

# view of our final data set to use in the logit regression model
# View(accounts.tidy.factored.replaced_nas.dropped)

# select numeric only since it is the only valid way to do a model
numeric.accounts.tidy <- accounts.tidy.factored.replaced_nas.dropped %>% 
  select_if(function (col) !is.character(col))

numeric.accounts.tidy %>% select_if(is.numeric) %>% gather() %>% ggplot(aes(value)) +
  geom_histogram(bins = 500) + 
  facet_wrap(~key, scales = 'free_x', ncol=2) + scale_y_continuous(trans='log10')


summary(numeric.accounts.tidy)



result <- round(cor(select_if(numeric.accounts.tidy, is.numeric)), 2)
melted_cormat <- melt(result)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1)  )


# we find that avg_last_90, 30 and 10 are highly correlated
# we could do PCA but we will discard 2 of them



numeric.accounts.tidy.removed.cor <- numeric.accounts.tidy %>% 
  select(-c(avg_last_30_days, avg_last_10_days))

set.seed(123)

kyc_data <- numeric.accounts.tidy
map_dbl(numeric.accounts.tidy, na_proportion)

# setting up train-test split

kyc_split <- initial_split(kyc_data, prop = 0.8)
kyc_train <- training(kyc_split)
kyc_test <- testing(kyc_split)


# LOGISTIC CLASSIFICATION

kyc_logit_recipe <- 
  recipe(rba_grade_desc ~ ., data=kyc_data) %>% 
  step_rm(rbaValue) %>% 
  step_rm(all_predictors(), -all_numeric()) %>% 
  step_normalize(all_predictors())
  

lr_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") # %>%
  # set_args(threshold=0.6)

lr_workflow <- workflow() %>% 
  add_recipe(kyc_logit_recipe) %>% 
  add_model(lr_model)

lr_fit <- lr_workflow %>% last_fit(kyc_split)

lr_performance <- lr_fit %>% collect_metrics()
lr_performance




# LINEAR REGRESSION

kyc_lm_recipe <- 
  recipe(rbaValue ~ ., data=kyc_data) %>% 
  step_rm(rba_grade_desc) %>% 
  step_rm(all_predictors(), -all_numeric()) %>% 
  step_normalize(all_predictors()) 



lm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

lm_workflow <- workflow() %>% 
  add_recipe(kyc_lm_recipe) %>% 
  add_model(lm_model)

lm_fit <- lm_workflow %>% last_fit(kyc_split)

lm_performance <- lm_fit %>% collect_metrics()

lm_performance


