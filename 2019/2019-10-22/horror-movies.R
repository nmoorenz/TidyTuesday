# load libraries
library(tidyverse)

# load data
horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv", 
                                 col_types = cols(
                                   title = col_character(),
                                   genres = col_character(),
                                   release_date = col_date("%d-%b-%y"),
                                   release_country = col_character(),
                                   movie_rating = col_character(),
                                   review_rating = col_double(),
                                   movie_run_time = col_character(),
                                   plot = col_character(),
                                   cast = col_character(),
                                   language = col_character(),
                                   filming_locations = col_character(),
                                   budget = col_character()
                                 ))

# counts of things
horror_movies %>% 
  count(genres)

horror_movies %>% 
  count(release_country) %>% 
  arrange(desc(n))

horror_movies %>% 
  count(movie_rating)

# create new variables
horror_df <- horror_movies %>% 
  mutate(release_year = lubridate::year(release_date), 
         budget_num = parse_number(budget), 
         movie_run_min = parse_number(movie_run_time), 
         genre_list = str_split(genres, "\\|"), 
         genre_count = lengths(genre_list))

# plot for ratings
ggplot(horror_df, aes(release_date, review_rating)) + 
  geom_point() +
  geom_smooth() + 
  labs(title = "ratings increase in last two years", 
       subtitle = "Is there a recency bias?")

# testing out budget numbers
# so, maybe converting these into the same currency really does matter
ggplot(horror_df, aes(release_date, budget_num)) + 
  geom_point() + 
  scale_y_log10(breaks = c(100, 1000, 10000, 100000, 10^6, 10^7, 10^8, 10^9), labels = scales::comma) + 
  labs(title = "little change in budget over time")

# check the highest budget films - top is korean
horror_df %>% 
  select(title, budget_num, everything()) %>% 
  arrange(desc(budget_num))

# check ratings by number of genre
horror_df %>% 
  ggplot(aes(as.factor(genre_count), review_rating)) + 
  geom_boxplot()

# not many films with many genres, but there is an upward trend
horror_df %>% 
  count(genre_count)

# New Zealand! 
horror_df %>% 
  filter(release_country == "New Zealand" ) %>% 
  select(title, genres, release_year, cast)

horror_df %>% 
  filter(str_detect(filming_locations, "Zealand")) %>% 
  select(title, genres, release_year, cast)

# Can money buy you ratings? 
horror_df %>% 
  ggplot(aes(budget_num, review_rating)) + 
  geom_point() + 
  scale_x_log10(breaks = c(100, 1000, 10000, 100000, 10^6,   10^9), labels = scales::comma)
