install.packages("readxl")
install.packages("deplyr")

library(dplyr)
library(readxl)
excel_file <- "C:/Users/kaush/OneDrive/Desktop/Kguragain_R_Project/datsets/US_youtube_trending_data.xlsx"
data <- read_xlsx(excel_file, sheet = 1)

View(data)

# Extracting year and month from publishedAt
data$published_year <- substr(data$publishedAt, 1, 4)
data$published_month <- substr(data$publishedAt, 6, 7)

# Extracting year and month from trending_date
data$trending_year <- substr(data$trending_date, 1, 4)
data$trending_month <- substr(data$trending_date, 6, 7)


#after extracting the necessary data from the published date and trending date we can saftely remove the columns from the dataset 
# Remove the publishedAt column
data <- data[, -which(names(data) == "publishedAt")]
# Remove the trending_date column
data <- data[, -which(names(data) == "trending_date")]

#Removing the thumbnail link as a column as it will not be used for analysis
# Remove the thumbnail_link column by name
data <- data[, -which(names(data) == "thumbnail_link")]


#now using logical indexing to change true to 1 and false to 0 in columns columns disabled and rating disabled
# Convert TRUE and FALSE to 1 and 0 in the comments_disabled column
data$comments_disabled <- as.integer(data$comments_disabled)

# Convert TRUE and FALSE to 1 and 0 in the ratings_disabled column
data$ratings_disabled <- as.integer(data$ratings_disabled)

# Count the number of characters in each row of the description column and replace the values
data$description <- apply(data["description"], 1, function(x) nchar(as.character(x)))

# Show the modified data frame
View(data)

install.packages("openxlsx")
library(openxlsx)
write.xlsx(data, "changed_data.xlsx")


# Count the number of 1's and 0's in comments_disabled and ratings_disabled columns
count_summary <- data %>%
  summarise(
    "comments_disabled_True" = sum(comments_disabled == 1, na.rm = TRUE),
    "comments_disabled_False" = sum(comments_disabled == 0, na.rm = TRUE),
    "ratings_disabled_True" = sum(ratings_disabled == 1, na.rm = TRUE),
    "ratings_disabled_False"  = sum(ratings_disabled == 0, na.rm = TRUE)
  )

# Show the count summary
print(count_summary)


#dividing tags column to find data and comparing the number of times the tags are repaded in the column tags
# Step 1: Split the tags column into separate strings using both delimiters
tags_split <- strsplit(data$tags, "[|#]")

# Step 2: Unlist the resulting list to get a vector of individual strings
tags_unlisted <- unlist(tags_split)

# Step 3: Remove unwanted tags
tags_unlisted <- tags_unlisted[grepl("^[a-zA-Z0-9-]+$", tags_unlisted)]

# Step 4: Count the frequency of each string
tags_counts <- table(tags_unlisted)

# Step 5: Sort the tag frequencies in descending order
tags_counts_sorted <- sort(tags_counts, decreasing = TRUE)


# Convert the tags_counts_sorted object to a data frame
tags_frequency <- data.frame(
  tags = names(tags_counts_sorted),
  frequency = as.numeric(tags_counts_sorted)
)

# Show the frequency of each string
print(tags_frequency)


#NOT FOR PRESENTATION
#SKIP FOR PRESENTATION

#FINDING ABOVE AVERAGE TAGS
# Step 1: Calculate the average occurrence of tags
#average_occurrence <- mean(tags_counts)

# Step 2: Filter tags that are above average
#tags_above_average <- tags_counts[tags_counts > average_occurrence]

# Step 3: Display tags above average
#print(tags_above_average)
#NOT FOR PRESENTATION



# Plot the frequency distribution of top tags
top_n <- 20  # Choose the top N tags to display
top_tags <- names(tags_counts_sorted)[1:top_n]
top_tags_freq <- as.numeric(tags_counts_sorted)[1:top_n]

# Create a bar plot of the top tags
library(ggplot2)
ggplot(data = data.frame(Tag = top_tags, Freq = top_tags_freq), aes(x = reorder(Tag, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Tags by Frequency", x = "Tags", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


View(data)
install.packages("tidyr")
library(tidyr)
# Step 1: Split the Tags Column
split_tags <- separate_rows(data, tags, sep = "\\|")

# Step 2: Calculate the Average View Count
transformed_data <- split_tags %>%
  group_by(tags) %>%
  summarise(average_view_count = mean(view_count))
unique_average_data <- unique(transformed_data)
# Print the transformed data
ranked_data <- unique_average_data %>%
  arrange(desc(average_view_count))

# Print the ranked data
print(ranked_data)

# Merge the datasets
combined_data <- merge(ranked_data, tags_frequency, by = "tags", all = TRUE)

# Remove rows with NA values
combined_data <- na.omit(combined_data)

# Print the combined data without NA values
print(combined_data)


#Testing using Spearman's
#resons
#assuming the data is not linear and not normally distributed 

#create a scatter plot to check before testing 

# Subset the combined_data dataframe to include only the first 20,000 rows

# Filter the combined_data dataframe to include only rows where frequency is less than 10,000
filtered_data <- combined_data[combined_data$frequency < 10000, ]

# Create a scatter plot
#chances for monotonic relationship 
#As one value increase the other value cannot decrease 
#As one value increase the other value cannot increase 
scatter_plot <- ggplot(filtered_data, aes(x = frequency, y = average_view_count)) +
  geom_point() +
  labs(title = "Frequency vs. Average View Count (Frequency < 10,000)",
       x = "Frequency",
       y = "Average View Count")

# Print the scatter plot
print(scatter_plot)

# Perform Spearman's rank correlation test
#If threshhold is 0.05 accept null hypothesis(there is no correlation rs = 0 ) 
#if p > 0.05 accept alternative hypothesis(there is corrrelation  rs is not = 0 ) if p < 0.05




correlation_test <- cor.test(combined_data$frequency, combined_data$average_view_count, method = "spearman")

# Print the results
print(correlation_test)


#testing distributuion of data 
# Create a Q-Q plot
qq_plot <- qqplot(combined_data$frequency, combined_data$average_view_count,
                  main = "Q-Q Plot",
                  xlab = "Frequency",
                  ylab = "Average View Count")

# Add a diagonal line to the Q-Q plot
abline(0, 1, col = "red")

# Print the Q-Q plot
print(qq_plot)









