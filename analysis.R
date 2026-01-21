# ============================================
# IMDb Movie Analysis using Tidyverse
# ============================================

# 1. Load Libraries
library(tidyverse)

# 2. Load and Clean Data
movies <- read_csv("movies.csv")

# Simplify: Keep only essential columns and split genres
movies_clean <- movies %>%
  select(movie_title, genres, imdb_score, gross, budget, year) %>%
  drop_na() %>%
  separate_rows(genres, sep = "\\|")  # Splits "Action|Comedy" into separate rows

# Preview cleaned data
head(movies_clean)

# 3. Genre Analysis - Find "Money Makers"
genre_analysis <- movies_clean %>%
  group_by(genres) %>%
  summarise(
    avg_revenue = mean(gross) / 1e6,      # Convert to millions
    avg_rating = mean(imdb_score),
    count = n(),
    total_revenue = sum(gross) / 1e6
  ) %>%
  filter(count > 10) %>%                  # Remove rare genres
  arrange(desc(avg_revenue))

# View results
print(genre_analysis)

# 4. Visualize: Rating vs Revenue (The Sweet Spot)
ggplot(genre_analysis, aes(x = avg_rating, y = avg_revenue, label = genres)) +
  geom_point(aes(size = count, color = genres), alpha = 0.7, show.legend = FALSE) +
  geom_text(vjust = -1, check_overlap = TRUE, size = 3) +
  labs(
    title = "Movie Genres: Rating vs. Revenue",
    subtitle = "Size represents number of movies in each genre",
    x = "Average IMDb Rating",
    y = "Average Revenue (Millions $)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16))

# 5. Top 10 Highest Grossing Genres
top_genres <- genre_analysis %>%
  top_n(10, avg_revenue) %>%
  ggplot(aes(x = reorder(genres, avg_revenue), y = avg_revenue, fill = genres)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 Highest Grossing Genres",
    x = "Genre",
    y = "Average Revenue (Millions $)"
  ) +
  theme_minimal()

print(top_genres)

# 6. Revenue Trends Over Time
yearly_trends <- movies_clean %>%
  group_by(year) %>%
  summarise(
    avg_revenue = mean(gross) / 1e6,
    avg_rating = mean(imdb_score),
    movie_count = n()
  )

ggplot(yearly_trends, aes(x = year)) +
  geom_line(aes(y = avg_revenue, color = "Revenue"), size = 1.2) +
  geom_line(aes(y = avg_rating * 30, color = "Rating"), size = 1.2) +  # Scale rating for dual axis
  scale_y_continuous(
    name = "Average Revenue (Millions $)",
    sec.axis = sec_axis(~./30, name = "Average IMDb Rating")
  ) +
  scale_color_manual(values = c("Revenue" = "#E74C3C", "Rating" = "#3498DB")) +
  labs(
    title = "Movie Trends: Revenue vs Rating Over Time",
    x = "Year",
    color = "Metric"
  ) +
  theme_minimal()

# 7. ROI Analysis (Return on Investment)
roi_analysis <- movies %>%
  filter(!is.na(gross) & !is.na(budget) & budget > 0) %>%
  mutate(
    roi = (gross - budget) / budget * 100,  # ROI as percentage
    profit = (gross - budget) / 1e6         # Profit in millions
  ) %>%
  arrange(desc(roi)) %>%
  select(movie_title, imdb_score, roi, profit, budget, gross)

# Top 10 Best ROI Movies
print("Top 10 Movies by ROI:")
print(head(roi_analysis, 10))

# 8. Budget vs Gross Scatter Plot
ggplot(movies, aes(x = budget/1e6, y = gross/1e6)) +
  geom_point(aes(color = imdb_score, size = imdb_score), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  scale_color_gradient(low = "orange", high = "darkgreen") +
  labs(
    title = "Budget vs Gross Revenue",
    x = "Budget (Millions $)",
    y = "Gross Revenue (Millions $)",
    color = "IMDb Score",
    size = "IMDb Score"
  ) +
  theme_minimal()

# 9. Word Cloud of Genres (Optional)
# Uncomment if you want to use wordcloud
# library(wordcloud)
# library(RColorBrewer)
# 
# wordcloud(
#   words = genre_analysis$genres,
#   freq = genre_analysis$count,
#   colors = brewer.pal(8, "Dark2"),
#   scale = c(3, 0.5),
#   random.order = FALSE
# )

# 10. Summary Statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Total Movies:", nrow(movies), "\n")
cat("Date Range:", min(movies$year), "-", max(movies$year), "\n")
cat("Average IMDb Score:", round(mean(movies$imdb_score), 2), "\n")
cat("Average Gross:", paste0("$", round(mean(movies$gross, na.rm = TRUE)/1e6, 2), "M"), "\n")
cat("Highest Rated:", movies$movie_title[which.max(movies$imdb_score)], 
    "(", max(movies$imdb_score), ")\n")
cat("Highest Grossing:", movies$movie_title[which.max(movies$gross)], 
    paste0("($", round(max(movies$gross)/1e6, 2), "M)\n"))
