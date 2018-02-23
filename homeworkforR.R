#Step 1, attach file to R#
homework.df <- readRDS("blpw.all.RDS", refhook = NULL)

View(homework.df)

#step 2, install the packages I need#

library(tidyverse)
library(dplyr)
library(lubridate)

#step 3, filter out the birds that were recaptured#

recaptured.df <-
  filter (homework.df, recap == "R")

View(recaptured.df)

#take out the band numbers with only 1 weight#

recap.df <- recaptured.df %>%
  group_by(band) %>%
  filter(n() >= 2)

View(recap.df)
#create a date column for graphing#

dateadded.df <- recap.df %>% 
  mutate(recapdate = make_date(year, month, day))

View(dateadded.df)

#im going to facet by location #
# im going to need a date column for my x axis#
# i need to take out the bands with only 1 weight#
# im going to need to group it by band and by year#
#i need some kind of delta weight for my y axis#

