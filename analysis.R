library(tidyverse)
library(arrow)

tweets <- read_csv('tweets.csv')
results <- read_csv('results.csv') %>%
	select(tweetid, cluster)


tweets
results

df <- inner_join(results, tweets) %>%
	mutate(
		cluster = factor(cluster, levels = c(0, 1, 2, 3, 4, 5, 6), labels = c('joy', 'disgust_surprise', 'neutral', 'fear', 'anger', 'sadness', 'mixed_emotions'))
	)
df
extractTexts <- function(inputString) {
  # Use a regular expression to match all occurrences of 'text': 'YourText'
  matches <- gregexpr("'text': '([^']*)'", inputString, perl=TRUE)

  # Extract the matched groups
  texts <- regmatches(inputString, matches)[[1]]

  # Remove the 'text': ' part from each match
  texts <- gsub("'text': '", "", texts)

  # Remove the closing ' from each match
  texts <- gsub("'", "", texts)

  # Collapse the vector of texts into a single comma-separated string
  result <- paste(texts, collapse = ",")

  return(result)
}

df <-
	df %>%
		# head(300) %>%
		rowwise() %>%
		mutate(hashtags = extractTexts(hashtags))

pivoted <-
	df %>%
		group_by(userid) %>%
		mutate(n = n()) %>%
		group_by(userid, cluster, n) %>%
		summarise(nc = n()) %>%
		mutate(pct = nc / n) %>%
		select(-nc) %>%
		pivot_wider(names_from = cluster, values_from = pct, values_fill = 0) %>%
		ungroup() %>%
		mutate(n = scales::rescale(n, c(0, 1)))

library(tidyverse)
library(cluster)
library(factoextra)

# Assuming your data is in a tibble called 'pivoted'
data_for_clustering <- select(pivoted, -userid) # Remove userid column
df
# Scale the data
scaled_data <- data_for_clustering
scaled_data
# Elbow Method to find optimal number of clusters
wss <- map_dbl(1:10, function(k) {
  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
})

# Plotting the Elbow Plot
elbow_plot <- tibble(k = 1:10, wss = wss)
ggplot(elbow_plot, aes(x = k, y = wss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method for Determining Optimal K",
       x = "Number of Clusters K",
       y = "Total Within-Cluster Sum of Squares")

# Perform K-means clustering with the determined number of clusters
# (Replace 'optimal_k' with the chosen number of clusters)
optimal_k <- 6 # Example, replace with the value you find optimal
set.seed(42) # For reproducibility
kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart = 25)

# Add cluster assignments to the original data
pivoted$cluster <- kmeans_result$cluster

cluster_composition <-
	pivoted %>%
		select(-userid) %>%
		group_by(cluster) %>%
		summarise(across(everything(), mean, na.rm = TRUE))

# Print the result
print(cluster_composition)

users <-
	pivoted %>%
		select(userid, cluster) %>%
		rename(user_type = cluster)

results %>%
	group_by(language) %>%
	summarise()

df %>%
		inner_join(users) %>%
	group_by(as.Date(tweetcreatedts)) %>%
	count() %>%
	print(n = 35)
df %>%
	inner_join(users) %>%
	write_csv('tweets_cleaned.csv')