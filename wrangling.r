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


# discard columns listed below
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



glimpse(accounts.tidy) 

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
