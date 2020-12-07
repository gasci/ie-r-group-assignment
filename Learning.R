#install.packages("tidyverse")
#install.packages("nycflights13")
library(tidyverse)
library(nycflights13)

#graphics
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue", show.legend = TRUE) + 
  geom_smooth(mapping = aes(x = displ, y = hwy), color = "red", show.legend = TRUE)

# table-like vector
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#range in python
y <- seq(1, 10, length.out = 5)
y
#> [1]  1.00  3.25  5.50  7.75 10.00

#data filtering and manipulation
flights

View(flights)
glimpse(flights)

#get colnames
colnames(flights)

#filter
filter(flights, month == 1, day == 1)

near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)
#> [1] TRUE

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
#> # A tibble: 1 x 1
#>       x
#>   <dbl>
#> 1     3
filter(df, is.na(x) | x > 1)
#> # A tibble: 2 x 1
#>       x
#>   <dbl>
#> 1    NA
#> 2     3

#arrange
arrange(flights, desc(year), month, day)

#select columns by name
select(flights, year, month, day)
