library(ggplot2)
library(reshape2)
library(dplyr)

df = read.csv("C:/Users/Eli/Desktop/tv_shows.csv")

#Let me do some basic exploration of the data.

table(df$Age)

ggplot(data = df, aes(x = Age)) +
  geom_bar()

#The vast majority of TV shows with ratings are rated at 16+. 

ggplot(data = df, aes(x = Year)) +
  geom_histogram(binwidth = 5)

#And it looks like the majority of TV shows are released after the year 2000.

###############################

#First Question: Which streaming platform has the highest rated TV shows overall?

df = df[complete.cases(df$IMDb, df$Rotten.Tomatoes), ]

#We make sure any NA values in our data are not being used in our analysis.

#We also want our raw scores, as the columns are formatted "x/10" for IMDb and "x/100" for Rotten Tomatoes.
#I am only interested in the x score value before the / for each column.

df$IMDb = as.numeric(sub("/.*", "", df$IMDb))
df$Rotten.Tomatoes = as.numeric(sapply(strsplit(as.character(df$Rotten.Tomatoes), "/"), "[[", 1))

#Two different techniques were used. For IMDb, I utilized regular expressions to remove any character after the /. 
#We now have raw score for both columns to use for the analysis. 

df = df[!is.na(df$IMDb) & df$IMDb >= 0 & df$IMDb <= 10, ]

#This bit was added after a lot of attempting to debug why my plots would occasionally be empty. All plots should display properly
#as they are on the written report.

#I also wanted to see the relationship between the scores from the two sites. 

ggplot(data = df, aes(x = IMDb)) +
  geom_histogram(binwidth = 1, fill = "deepskyblue", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "aliceblue") +
  labs(x = "IMDb Rating", y = "Density",
       title = "IMDb Rating Distribution")

ggplot(data = df, aes(x = Rotten.Tomatoes)) +
  geom_histogram(binwidth = 10, fill = "brown3", color = "black", aes(y = ..density..)) +
  geom_density(alpha = 0.5, fill = "aliceblue") +
  labs(x = "Rotten Tomatoes Rating", y = "Density",
       title = "Rotten Tomatoes Rating Distribution")

#When observing our distributions, it looks like. It looks like IMBd are generally higher, as it is left skewed.

correlation_coefficient = cor(df$IMDb, df$Rotten.Tomatoes, use = "complete.obs")
correlation_coefficient

#A 0.4618722 correlation seems to indicate a positive relationship between the two ratings, so it seems that while a higher
#IMDb score usually results in a higher RT score, there are a couple of factors at play that show that both scores aren't 100%
#consistent with each other. If I had to guess personally, the harsher RT ratings might be due to the platform having a 
#"critic score" component.

avg_imdb = data.frame(
  Platform = c("Netflix", "Hulu", "Prime Video", "Disney+"),
  Average = c(
    mean(df$IMDb[df$Netflix == 1]),
    mean(df$IMDb[df$Hulu == 1]),
    mean(df$IMDb[df$Prime.Video == 1]),
    mean(df$IMDb[df$Disney. == 1])
  )
)


avg_rt = data.frame(
  Platform = c("Netflix", "Hulu", "Prime Video", "Disney+"),
  Average = c(
    mean(df$Rotten.Tomatoes[df$Netflix == 1]),
    mean(df$Rotten.Tomatoes[df$Hulu == 1]),
    mean(df$Rotten.Tomatoes[df$Prime.Video == 1]),
    mean(df$Rotten.Tomatoes[df$Disney. == 1])
  )
)

#To aid with making my visualization, I created two new dataframes to hold the average scores for each platform. Since
#each platform column is a binary, we can find what is on a specific platform by seting a filter to only return rows that
#have "1". Then, we can perform the average.

ggplot(avg_imdb, aes(x = Platform, y = Average, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(x = "Platform", y = "Average IMDb Rating", title = "Average IMDb Ratings by Platform") +
  ylim(0, 10) +  
  theme_minimal()

ggplot(avg_rt, aes(x = Platform, y = Average, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(x = "Platform", y = "Average Rotten Tomatoes Rating", title = "Average Rotten Tomatoes Ratings by Platform") +
  ylim(0, 100) +  
  theme_minimal()

#With all of  this in mind, it seems that if you prefer to go by RT score, Hulu is a clear winner, with Prime Video being the
#biggest loser. Conversely, the IMDb ratings say the opposite. By IMDb, prime video *just barely* squeezes ahead to take 1st place.

#Now we have the scores for the individual rating sites, but I finally want to view overall score.
#To do this, we will need to put the IMDb scores on the same scale as the RT ones. I will then combine
#them into one dataframe and use that for my visualization.

df$IMDb_scaled =df$IMDb * 10

df$Overall_Score = (df$IMDb_scaled + df$Rotten.Tomatoes) / 2

avg_overall = data.frame(
  Platform = c("Netflix", "Hulu", "Prime Video", "Disney+"),
  Average_Overall_Score = c(
    mean(df$Overall_Score[df$Netflix == 1]),
    mean(df$Overall_Score[df$Hulu == 1]),
    mean(df$Overall_Score[df$Prime.Video == 1]),
    mean(df$Overall_Score[df$Disney. == 1])
  )
)

ggplot(avg_overall, aes(x = Platform, y = Average_Overall_Score, fill = Platform)) +
  geom_bar(stat = "identity") +
  labs(x = "Platform", y = "Average Overall Score", title = "Average Overall Scores by Platform") +
  ylim(0, 100) +
  theme_minimal()

#So it looks like the winner is Hulu has the highest rated shows overall!

############################

#Question 2:

#The first thing we are going to do, is create an age group filter. 

age_groups = c("7+", "13+", "16+", "18+")

#I made specific age groups because there is an "all" group, but I find that
#a bit too vague and want to restrict it only to specified age ratings.
#This is also necessary for the next step to properly order the age ranges for the x axis.

df_filtered = df %>%
  filter(Age %in% age_groups) %>%
  mutate(Age = factor(Age, levels = age_groups))

# Now we are going to calculate the average overall scores for each platform,
# grouping the data by age. We will use the `summarize` function to calculate
# the mean of the overall score column for each age group within each platform.

avg_overall_netflix = df_filtered %>%
  filter(Netflix == 1) %>%
  group_by(Age) %>%
  summarize(Average_Overall_Score = mean(Overall_Score)) %>%
  mutate(Platform = "Netflix")

avg_overall_hulu = df_filtered %>%
  filter(Hulu == 1) %>%
  group_by(Age) %>%
  summarize(Average_Overall_Score = mean(Overall_Score)) %>%
  mutate(Platform = "Hulu")

avg_overall_prime = df_filtered %>%
  filter(Prime.Video == 1) %>%
  group_by(Age) %>%
  summarize(Average_Overall_Score = mean(Overall_Score)) %>%
  mutate(Platform = "Prime Video")

avg_overall_disney = df_filtered %>%
  filter(Disney. == 1) %>%
  group_by(Age) %>%
  summarize(Average_Overall_Score = mean(Overall_Score)) %>%
  mutate(Platform = "Disney+")

avg_overall_all = bind_rows(avg_overall_netflix, avg_overall_hulu, avg_overall_prime, avg_overall_disney)

#Now we commit those results to their own dataframe, and use that dataframe to plot our visualization.

ggplot(avg_overall_all, aes(x = Age, y = Average_Overall_Score, fill = Age, label = round(Average_Overall_Score, 2))) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), color = "aliceblue") + 
  facet_wrap(~ Platform) +
  labs(x = "Age Group", y = "Average Overall Score", 
       title = "Average Overall Scores per Age Group and Platform") +
  ylim(0, 100) +
  theme_minimal()

#This looks a bit odd because there are no 13+ rated shows on Disney+.
#Observing the visualization, it looks like there aren't any real major gaps and that overall scores for most age ranges are
#between 62-75%. Oddly enough, Disney+ has the best 18+ show ratings, as well as 7+. Prime video is awful for the 13+ bracket.

#################

#QUESTION 3: Find if there is a noticeable relationship between TV show age rating and Rotten Tomatoes/IMDB score.

df$Age_numeric = as.numeric(factor(df$Age, levels = c("7+", "13+", "16+", "18+")))

#We convert our age groups to numeric, so we can perform our correlation correctly. 

age_rating_correlation = cor(df$Age_numeric, df$Overall_Score, use = "complete.obs")

age_rating_correlation

#It seems there is a minor positive correlation of 0.20 between age and overall score. While it does seem score
#goes up as age goes up, the relationship is loose.

df$Age = factor(df$Age, levels = c("7+", "13+", "16+", "18+"))

ggplot(df, aes(x = Age, y = Overall_Score)) +
  geom_boxplot() +
  labs(title = "Distribution of Overall Scores by Age Rating",
       x = "Age Rating",
       y = "Overall Score")

#Observing the plot, we can see that overall, 18+ shows tend to have the higher scores as they have the highest median.
#16+ Tend to have the most outliers besides "NA", and it looks like its for the worst, as they are very lowly rated.

################
#QUESTION 4:

#Find out which years have the best shows, and if there is a trend to show quality over time. This process should be a self explanatory
#by now.

avg_score_by_year = df %>%
  group_by(Year) %>%
  summarize(Average_Overall_Score = mean(Overall_Score, na.rm = TRUE))

ggplot(avg_score_by_year, aes(x = Year, y = Average_Overall_Score)) +
  geom_line(color = "darksalmon") +
  geom_point(color = "brown") +
  geom_smooth(method = "lm", se = FALSE, color = "brown1") +
  labs(title = "Average Overall Score by Year with Trend Line",
       x = "Year",
       y = "Average Overall Score") +
  theme_minimal()

linear_model = lm(Average_Overall_Score ~ Year, data = avg_score_by_year)
summary(linear_model)

#Interestingly the trend line does go down indicating TV show quality has gone down over time. It's important to note, however,
#that during our first exploration of the data, we saw there were WAY more shows with ratings in the 2000s onwards.
#Therefor, the real conclusion should be that more ratings = more low outliers dragging the score down for 2000s+ shows.
