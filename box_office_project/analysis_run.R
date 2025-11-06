install.packages(c('tidyverse','lubridate','janitor','tidytext','scales','broom','caret','randomForest','corrplot','GGally','rmarkdown','knitr','kableExtra'))
library(tidyverse)
library(lubridate)
library(janitor)
library(tidytext)
library(scales)
library(corrplot)
library(GGally)
library(broom)

#parameters we will use in the project
INPUT_CSV<- "tmdb_movies.csv"
OUTPUT_DIR<- "outputs"
USD_TO_KSH<- 130

if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

movies_raw <- read_csv(INPUT_CSV, show_col_types = FALSE)
print(glimpse(movies_raw))

#Clean and Preparation
movies <- movies_raw %>%
  clean_names() %>%
  mutate(
    release_date = as_date(release_date, format = '%Y-%m-%d'),
    year = year(release_date),
    month = month(release_date),
    release_month = month(release_date, label = TRUE, abbr = TRUE)
  )

#keep the key variables
movies <- movies %>% select(id, title, original_title, budget, revenue, runtime, genres, release_date, year, release_month, vote_average, vote_count, popularity)


#zeros mean missing so we will replace all zeros(if any)in budget and revenue with  NA
movies <- movies %>%
  mutate(
    budget = na_if(budget, 0),
    revenue = na_if(revenue, 0)
  )


#Conversion of USD to KSH
movies <- movies %>%
  mutate(
    budget_ksh = round(budget * USD_TO_KSH, 0),
    revenue_ksh = round(revenue * USD_TO_KSH, 0)
  )


#Simplifying the primary genre in common format 
extract_primary_genre <- function(genre_field){
  if(is.na(genre_field)) return(NA_character_)
  g <- str_extract(genre_field, "'name'/s*:/s*'[^']+'")
  if(is.na(g)) g <- str_extract(genre_field, '"name"/s*:/s*"[^"]+"')
  if(is.na(g)) return(NA_character_)
  g <- str_replace_all(g, "('name'|\"name\"|:|\")", "")
  g <- str_replace_all(g, "^/s+|/s+$|/'", "")
g <- str_replace(g, "name/s*:/s*", "")
g <- str_replace_all(g, "^[:space:]+|[:space:]+$", "")
return(str_trim(g))
}


movies <- movies %>%
  rowwise() %>%
  mutate(primary_genre = extract_primary_genre(genres)) %>%
  ungroup()  

movies <- movies %>% mutate(primary_genre = if_else(primary_genre == "", NA_character_, primary_genre))

#season feature: mostly Kenyan holidays
movies <- movies %>%
  mutate(
    release_date = as.Date(release_date),
    month = lubridate::month(release_date),
    release_season = case_when(
      month %in% c(12) ~ "Christmas",
      month %in% c(6, 7, 8) ~ "MidYear/Holidays",
      month %in% c(3, 4) ~ "Easter/Spring",
      TRUE ~ "Other"
    )
  )

#drop rows that are missing crucial variables that we kept earlier
movies_clean <- movies %>% filter(!is.na(revenue_ksh) & !is.na(budget_ksh) & !is.na(runtime))


write_csv(movies_clean, file.path(OUTPUT_DIR, 'movies_clean.csv'))

# Create a simulated local_popularity score influenced by popularity, vote_average and genre-weights based on presumed Kenyan tastes
set.seed(123)
genre_weights <- tibble(
  primary_genre = c('Drama','Comedy','Action','Romance','Horror','Animation','Documentary'),
  weight = c(1.2,1.3,1.1,1.0,0.8,1.0,0.7)
)

movies_sim <- movies_clean %>%
  left_join(genre_weights, by = 'primary_genre') %>%
  mutate(weight = coalesce(weight, 1.0)) %>%
  mutate(
    simulated_local_interest = (popularity + vote_average * 2 + runif(n(), 0, 10)) * weight,
    simulated_expected_revenue_ksh = round(revenue_ksh * (simulated_local_interest / (simulated_local_interest + 50)), 0)
  )

write_csv(movies_sim, file.path(OUTPUT_DIR, 'movies_simulated.csv'))



#EXPLORATORY DATA ANALYSIS(EDA)
summary_table <- movies_sim %>%
  summarise(
    n_movies = n(),
    mean_budget_ksh = mean(budget_ksh, na.rm = TRUE),
    median_budget_ksh = median(budget_ksh, na.rm = TRUE),
    mean_revenue_ksh = mean(revenue_ksh, na.rm = TRUE),
    median_revenue_ksh = median(revenue_ksh, na.rm = TRUE),
    mean_runtime = mean(runtime, na.rm = TRUE)
  )
write_csv(summary_table, file.path(OUTPUT_DIR, 'summary_table.csv'))


#Budget vs revenue scatter 
p1 <- movies_sim %>%
  ggplot(aes(x = budget_ksh, y = revenue_ksh)) +
  geom_point(alpha = 0.5) +
  scale_x_log10(labels = scales::label_number(scale_cut = scales::cut_si("K"))) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("K"))) +
  labs(
    title = "Budget vs Revenue (KSh) — log scale",
    x = "Budget (KSh) [log10]",
    y = "Revenue (KSh) [log10]"
  ) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR,'budget_vs_revenue_log.png'), p1, width = 8, height = 6)

#Revenue by primary genre
p2 <- movies_sim %>%
  filter(!is.na(primary_genre)) %>%
  group_by(primary_genre) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(primary_genre, revenue_ksh, FUN = median), y = revenue_ksh)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("K"))) +
  coord_flip() +
  labs(
    title = "Revenue by Primary Genre (KSh)",
    x = "Genre",
    y = "Revenue (KSh)"
  ) +
  theme_minimal()

ggsave(file.path(OUTPUT_DIR, "revenue_by_genre_boxplot.png"), p2, width = 8, height = 6)


#Release season effect on revenue
p3 <- movies_sim %>%
  ggplot(aes(x = release_season, y = revenue_ksh)) +
  geom_violin(trim = TRUE) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("K"))) +
  labs(
    title = "Revenue distribution by release season",
    x = "Release Season",
    y = "Revenue (KSh)"
  ) +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, 'revenue_by_season_violin.png'), p3, width=8, height=6)


#correlation matrix for numeric features
num_vars <- movies_sim %>% select(budget_ksh, revenue_ksh, runtime, vote_average, vote_count, popularity, simulated_local_interest)
num_mat <- cor(na.omit(num_vars))
png(file.path(OUTPUT_DIR,'correlation_matrix.png'), width = 800, height = 800)
corrplot::corrplot(num_mat, method='number', tl.cex = 0.8)
dev.off()

#top 10 movies by simulated expected revenue
top10 <- movies_sim %>% arrange(desc(simulated_expected_revenue_ksh)) %>% slice(1:10) %>%
  select(title, year, primary_genre, budget_ksh, revenue_ksh, simulated_expected_revenue_ksh)

write_csv(top10, file.path(OUTPUT_DIR, 'top10_simulated_revenue.csv'))


#Save plots and data
saveRDS(movies_sim, file.path(OUTPUT_DIR, 'movies_sim.rds'))

#print brief results
print(summary_table)
print(head(top10, 10))

