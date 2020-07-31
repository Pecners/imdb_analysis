library(tidyverse)

files <- list.files("./data")

dt <- list()

data <- map(files, function(x) {
  item <- read_tsv(gzfile(paste("data", x, sep = "/")))
  
  dt <- list(dt, item)
})

titles <- data[[1]][[2]]
episodes <- data[[2]][[2]]
ratings <- data[[3]][[2]]
rm(data, dt)


titles_tv <- titles %>%
  filter(titleType == "tvEpisode")

te <- left_join(titles_tv, episodes, by = "tconst")
tem <- left_join(te, ratings, by = "tconst")

tem1 <- tem %>%
  modify_at(c("seasonNumber", "episodeNumber"), as.numeric) %>%
  filter(!is.na(averageRating) & !is.na(seasonNumber) & !is.na(episodeNumber)) %>%
  modify_at("seasonNumber", factor)

tem_cleaned <- tem %>%
  modify_at(c("seasonNumber", "episodeNumber"), as.numeric) %>%
  filter(!is.na(averageRating) & !is.na(seasonNumber) & !is.na(episodeNumber)) %>%
  group_by(parentTconst) %>%
  mutate(max_season = max(seasonNumber)) %>%
  ungroup() %>%
  filter(max_season > 4 & seasonNumber < 6) %>%
  modify_at("seasonNumber", factor)

tem_cleaned %>%
  ggplot(aes(seasonNumber, avg)) +
  geom_boxplot() +
  theme_minimal()


tem_cleaned %>%
  ggplot(aes(seasonNumber, n)) +
  geom_col() +
  theme_minimal()
