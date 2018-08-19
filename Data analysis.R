library(tidyverse)
library(tidytext)
library(ggthemes)

##Read in the data

mydata <- read_csv("~/Documents/Data driven baby bath/Data.csv")

##Start by plotting the stars

ggplot(mydata, aes(stars)) +
  geom_bar(stat='count') +
  ggthemes::theme_fivethirtyeight()

##so, that's really interesting, the vast majority are five star reviews.

##Okay, let's now identity the keywords

check <- c("month$", "month")

keywords <- mydata %>%
  unnest_tokens(word, comments, token="ngrams", n = 2) %>%
  filter(str_detect(word, "month$|months$")) %>%
  mutate(duration = sub(" .*", "", word))

##Wanted to see what they say when its a few months

few <- keywords %>%
  filter(duration == "few") %>%
  select(author)

fewdata <- mydata %>%
  filter(author %in% few$author)

##Okay, looking at the data we've decided to put 'few' as 3 months

numberdata <- data.frame(
  duration = c("few", "second","couple", "fifth", "first", "five", "four",
               "six", "three","two","one"),
  time = c(3,2,2,5,1,5,4,6,3,2,1)
)

keywords <- keywords %>%
  left_join(numberdata, by = 'duration') %>%
  mutate(time = as.character(time)) %>%
  mutate(time = case_when(
    is.na(time) ~ duration,
    TRUE ~ time
  )) %>%
  mutate(time = as.numeric(time)) %>%
  filter(!is.na(time))

##Next step is to identify the duplicated reviews becuase we don't want them
##To skew the data

duplicated <- keywords %>%
  group_by(author) %>%
  filter(n()>1)

keywords <- keywords %>%
  filter(!(author %in% duplicated$author))

##Okay, finally let's plot it

ggplot(keywords, aes(time)) +
  geom_bar(stat='count', fill="steelblue") +
  theme_minimal() +
  ylab("") +
  xlab("Months")




  
  
