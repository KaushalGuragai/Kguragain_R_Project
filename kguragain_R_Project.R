######################installing and loading packages###################

install.packages("readxl")
install.packages("deplyr")
install.packages("openxlsx")
install.packages("tidyr")
install.packages("caret")

library(openxlsx)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(caret)

excel_file <- "C:/Users/kaush/OneDrive/Desktop/Kguragain_R_Project/datsets/US_youtube_trending_data.xlsx"
data <- read_xlsx(excel_file, sheet = 1)
View(data)
#######################installing and loading packages and the source file ends###############


###############Changing data set to fit requirements################
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
#Remove the 
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
###################Changing dataset ends#################


###############Writing changed data to xlsx#####################
write.xlsx(data, "changed_data.xlsx")
############################Writing xlsx file ends ##############



########################spliting Tags and getting the top 10 tags based on frequency############################

#dividing tags column to find data and comparing the number of times the tags are repeated in the column tags
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
#top 10 tags for analysis
top_tags <- head(tags_frequency$tags, 10)
top_tags_freq <- head(tags_frequency$frequency, 10)


# Create a bar plot of the top tags
ggplot(data = data.frame(Tag = top_tags, Freq = top_tags_freq), aes(x = reorder(Tag, -Freq), y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Top Tags by Frequency", x = "Tags", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


View(data)
##################################Visualization of Tag Ends######################




###########################combining tags with frequency and average view counts################
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
# Sort the combined data by frequency in descending order
top_10_combined <- combined_data[order(combined_data$frequency, decreasing = TRUE), ]
# Select the top 10 rows
top_10_combined <- head(top_10_combined, 10)
# Print the top 10 combined data
print(top_10_combined)
View(top_10_combined)

###########################combining tags with frequency and average view counts Ends################



##################Speakmans test to see the distribution####################
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
#################################spearmans test and visualization ends######################


##########################Start of Final Presentation######################## 

#preparing data for regression model. 

View(data)
selected_data <- data[, c("video_id", "title", "categoryId", "view_count", "tags")]

View(selected_data)


########################generating weights for each tag###################
# Create weights for top 10 tags
top_10_weights <- top_10_combined$frequency / sum(top_10_combined$frequency)



####################Generating random sample 100,000######################
# Set the desired sample size
desired_sample_size <- 100000

# Sample indices randomly
sampled_indices <- sample(1:nrow(selected_data), size = desired_sample_size, replace = FALSE)

# Subset the selected data using the sampled indices
sampled_data <- selected_data[sampled_indices, ]

View(sampled_data)
############################Filtering data with no tags and with tags###############
# Filter rows with no tags
rows_with_none_tags <- sampled_data[sampled_data$tags == "[None]", ]

# Create a separate dataframe with rows ### WITH NO ###tags
empty_tags <- sampled_data[sampled_data$tags == "[None]", ]

# Create a separate dataframe with rows ###WITH ###tags
with_tags <- sampled_data[sampled_data$tags != "[None]", ]

#check if na exists in empty_tags
empty_tags %>% na.omit()

#calculated median  of the view_count column
median_view_count_empty_tags <- median(empty_tags$view_count)

# Print the median view count
print(median_view_count_empty_tags)

View(empty_tags)
View(with_tags)
##################################seperating tags ends ##############



#####################Getting weights of each tags based on frequency of its occurance#########
# Step 1: Split the tags column into separate strings
split_tags <- separate_rows(with_tags, tags, sep = "\\|")

# Calculate total frequency of all tags
total_frequency <- sum(top_10_combined$frequency)

# Calculate weight for each tag
top_10_combined$weight <- top_10_combined$frequency / total_frequency

# Print the combined data with the new weight column
print(top_10_combined)



##################setting up weights to the merged_data join#########################
# Merge split_tags with top_10_combined
merged_data <- inner_join(split_tags, top_10_combined, by = "tags")

# Select relevant columns and rename the weight column
filtered_data <- merged_data %>%
  select(video_id, title, categoryId, view_count, tags, weight = weight)

# Print the filtered dataset
print(filtered_data)



#############grouping tags for same video id row and filtering based on weight of tags###########
# Group by video_id and filter for the row with the highest weight
final_data_prepared <- filtered_data %>%
  group_by(video_id) %>%
  filter(weight == max(weight)) %>%
  ungroup()

# Filter out rows with NA values in any column
final_data_filtered <- final_data_prepared %>%
  na.omit()

# Print the final dataset
unique_tags_with_weights <- final_data_filtered

unique_tags_with_weights <- unique_tags_with_weights %>%
  arrange(weight)

###############filtering based on tag ends##########################



#################visualizing the tags based on average view count and weights of tags ##############
# Group by tags and calculate average view count
tag_view_count <- final_data_filtered %>%
  group_by(tags,weight) %>%
  summarise(average_view_count = mean(view_count)) 

# Create a scatterplot
scatter_plot <- ggplot(tag_view_count, aes(x = reorder(tags, weight), y = average_view_count, size = weight)) +
  geom_point() +
  labs(title = "Average View Count for Tags arranged by Weight",
       x = "Tags",
       y = "Average View Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Print the scatterplot
print(scatter_plot)
#####Visualization Ends#####################




#######################Preparing for regression analysis generating dummy variables###############

# Identify the tag with the lowest weight
tag_with_lowest_weight <- unique_tags_with_weights[which.min(unique_tags_with_weights$weight), "tags"]

# Convert 'tags' column to character type
final_data_filtered$tags <- as.character(final_data_filtered$tags)

# Create dummy variables for 'tags' column
dummy_data <- predict(dummyVars(~ tags, data = final_data_filtered), 
                      newdata = final_data_filtered)

# Remove the dummy variable for the tag with the lowest weight
dummy_data <- dummy_data[, !(grepl(tag_with_lowest_weight, colnames(dummy_data)))]

# Combine the dummy variables with the original dataset
final_data_with_dummies <- cbind(final_data_filtered, dummy_data)

# Print the dataset with dummy variables
print(final_data_with_dummies)
########################generating dummy variables ends#########################






####################preparing for regression model, ANOVA TEST##################
dependent_variable = "view_count"
independent_variable = c("tagsanimation", "tagschallenge", "tagscomedy", "tagsfortnite", 
                      "tagsfunny", "tagsgaming", "tagsminecraft", "tagsnews", "tagsvlog")


#ANOVA test for analysis of variance 

# Fit Multiple Linear Regression Model
model <- lm(view_count ~ ., data = final_data_with_dummies[, c(dependent_variable, independent_variable)])

# Perform ANOVA Test
anova_result <- anova(model)
print(anova_result)

############Anova test ends###############################


##################creating a regression model##############################

#Excluding the independed variables with p > 0.05  we remove, animation, gaming, minecraft 

independent_variable_after_ANOVA = c("tagschallenge", "tagscomedy", "tagsfortnite", 
                         "tagsfunny","tagsnews", "tagsvlog")

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(final_data_with_dummies), 0.7 * nrow(final_data_with_dummies))
train_data <- final_data_with_dummies[train_index, ]
test_data <- final_data_with_dummies[-train_index, ]

# Build Regression Model with Custom Intercept
model_custom_intercept <- lm(view_count ~ . - 1, data = train_data[, c("view_count", independent_variable_after_ANOVA)])

# Set the intercept to the median view count for videos with no tags
model_custom_intercept$coefficients[1] <- median_view_count_empty_tags
# Step 4: Evaluate Model
# Predict view count using the testing set
predictions_custom_intercept <- predict(model_custom_intercept, newdata = test_data)

# Step 5: Interpret Results
# Print coefficients of the regression model
summary(model_custom_intercept)
####################regression model ends#####################
#END of Presentation 








#For shiny.

#creating a bar chart 
# Extract coefficients and standard errors
coefficients <- coef(model_custom_intercept)
std_errors <- summary(model_custom_intercept)$coef[, "Std. Error"]

# Calculate upper and lower bounds of confidence intervals
upper_bound <- coefficients + 1.96 * std_errors
lower_bound <- coefficients - 1.96 * std_errors

# Create a bar plot
barplot(coefficients, ylim = range(c(lower_bound, upper_bound)), 
        names.arg = names(coefficients), 
        xlab = "Independent Variables", ylab = "Coefficients", 
        main = "Coefficient Plot with 95% Confidence Intervals", 
        col = ifelse((lower_bound > 0 & upper_bound > 0) | (lower_bound < 0 & upper_bound < 0), "blue", "red"))

# Add error bars
arrows(1:length(coefficients), lower_bound, 1:length(coefficients), upper_bound, 
       angle = 90, code = 3, length = 0.05, col = "black", lwd = 1.5)

