############################ library, load, and clean data ############################
library(openxlsx)
library(dplyr)
library(ggplot2)
library(car)
library(onewaytests)
library(rstatix)
library(lubridate)
library(scales)
library(tidyverse)

filtered_df <- df %>%
  filter(!is.na(final_category)) %>%
  filter(!is.na(min_date)) %>%
  filter(is.na(sub_categories) | sub_categories != "bots")

mean <- filtered_df %>%
  group_by(final_category) %>%
  mutate(blm_percentage_mean = mean(blm_percentage, na.rm=T),
         blm_related_count_mean = mean(blm_related_count, na.rm=T),
         unique_day_mean = mean(unique_day, na.rm=T),
         days_active_range = mean(days_active_range, na.rm=T),
         count = n()) %>%
  select(final_category, 
         blm_related_count_mean, 
         blm_percentage_mean, unique_day_mean, days_active_range, count) %>%
  unique()

# Assuming your data is named 'df'
# Calculate the mean and standard error for each 'final_category'
df_summary <- filtered_df %>%
  group_by(final_category) %>%
  summarise(mean_blm = mean(days_active_range, na.rm = TRUE),
            se_blm = sd(days_active_range, na.rm = TRUE)/sqrt(n()),
            .groups = "drop")  # avoid the 'ungroup()' step

# Make a bar plot
ggplot(df_summary, aes(x = reorder(final_category, mean_blm), y = mean_blm)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  geom_errorbar(aes(ymin = mean_blm - se_blm, ymax = mean_blm + se_blm), 
                width = 0.2, size = 0.5, colour = "black") +
  theme_minimal() +
  theme(text = element_text(size=12, family="Times New Roman"), 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="Account Type", y="Average Length of Participation") +
  coord_flip()



############################ blm percentage ############################
leveneTest(blm_percentage ~ final_category, data = filtered_df)
bartlett.test(blm_percentage ~ final_category, data = filtered_df)
# strong evidence to believe that the variances of your groups are not equal

# Welch's ANOVA
oneway.test(blm_percentage ~ final_category, data = filtered_df, var.equal = FALSE)
# post hoc
# reference: https://stackoverflow.com/questions/28587498/post-hoc-tests-for-one-way-anova-with-welchs-correction-in-r
# https://rpubs.com/aaronsc32/games-howell-test
# Grouped data
games_howell_result <- filtered_df %>%
  games_howell_test(blm_percentage ~ final_category)
games_howell_result


############################ unique days ############################
leveneTest(unique_day ~ final_category, data = filtered_df)
bartlett.test(unique_day ~ final_category, data = filtered_df)
# strong evidence to believe that the variances of your groups are not equal

# Welch's ANOVA
oneway.test(unique_day ~ final_category, data = filtered_df, var.equal = FALSE)
# post hoc
games_howell_result <- filtered_df %>%
  games_howell_test(unique_day ~ final_category)
games_howell_result


############################ range of active days ############################
leveneTest(days_active_range ~ final_category, data = filtered_df)
bartlett.test(days_active_range ~ final_category, data = filtered_df)
# strong evidence to believe that the variances of your groups are not equal

# Welch's ANOVA
oneway.test(days_active_range ~ final_category, data = filtered_df, var.equal = FALSE)
# post hoc
games_howell_result <- filtered_df %>%
  games_howell_test(days_active_range ~ final_category)
games_howell_result


############################ ability to attract new users ############################
filtered_df <- df %>%
  filter(!is.na(final_category) & sub_categories != "bots") %>%
  filter(!is.na(min_date))

library(arrow)
users <- read_parquet('unique_users_first_appearance.parquet')

users_retweeted <- users %>%
  filter(!is.na(inReplyToUser_username)) %>%
  filter(inReplyToUser_username %in% filtered_df$account)

tomerge <- filtered_df %>% 
  select(account, final_category) %>%
  rename(inReplyToUser_username = account) %>%
  group_by(inReplyToUser_username) %>%
  slice(1)

new_users <- inner_join(users_retweeted, tomerge, by='inReplyToUser_username')
new_users$date <- as.Date(new_users$date)

avg <- new_users %>%
  group_by(final_category) %>%
  summarise(new_accounts = n())
tomerg2 <- mean %>%
  select(final_category, count)
df1 <- merge(avg, tomerg2, by='final_category') %>%
  mutate(avg = new_accounts/count)

df_toplot <- new_users %>%
  group_by(final_category, date) %>%
  summarise(new_accounts = n())

# Make a bar plot
ggplot(df1, aes(x = reorder(final_category, avg), y = avg)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size=12, family="Times New Roman"), 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="Account Type", y="Average Number of New Participants") +
  coord_flip()

leveneTest(new_accounts ~ final_category, data = df_toplot)
bartlett.test(new_accounts ~ final_category, data = df_toplot)
# Welch's ANOVA
oneway.test(new_accounts ~ final_category, data = df_toplot, var.equal = FALSE)
# post hoc
games_howell_result <- df_toplot %>%
  ungroup() %>%
  games_howell_test(new_accounts ~ final_category)
games_howell_result

p <- ggplot(df_toplot, aes(x=date, y=count, group=final_category, color=final_category)) +
  geom_line() +
  labs(x = "Date", y = "Number of Accounts Recurited", title = "Number of Accounts by Category Over Time") +
  theme_minimal() +
  scale_x_date(labels = date_format("%m-%Y"),
               limits = as.Date(c('2013-01-01','2021-12-01')))
p

############################ ability to attract new users new ############################
filtered_df <- df %>%
  filter(!is.na(final_category) & sub_categories != "bots") %>%
  filter(!is.na(min_date))

library(arrow)
users <- read_parquet('unique_users_first_appearance.parquet')

users_retweeted <- users %>%
  filter(!is.na(inReplyToUser_username)) %>%
  filter(inReplyToUser_username %in% filtered_df$account) %>%
  unique() %>%
  group_by(inReplyToUser_username) %>%
  count() %>%
  rename("account" = "inReplyToUser_username")

new_users <- inner_join(filtered_df, users_retweeted, by=c('account'))
df_summary <- new_users %>%
  group_by(final_category) %>%
  summarise(mean_blm = mean(n, na.rm = TRUE),
            se_blm = sd(n, na.rm = TRUE)/sqrt(n()),
            .groups = "drop") 

# Make a bar plot
ggplot(df_summary, aes(x = reorder(final_category, mean_blm), y = mean_blm)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  geom_errorbar(aes(ymin = mean_blm - se_blm, ymax = mean_blm + se_blm), 
                width = 0.2, size = 0.5, colour = "black") +
  theme_minimal() +
  theme(text = element_text(size=12, family="Times New Roman"), 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="Account Type", y="Average Length of Participation") +
  coord_flip()

leveneTest(n ~ final_category, data = new_users)
bartlett.test(n ~ final_category, data = new_users)
# Welch's ANOVA
oneway.test(n ~ final_category, data = new_users, var.equal = FALSE)
# post hoc
games_howell_result <- new_users %>%
  games_howell_test(n ~ final_category)
games_howell_result


p <- ggplot(df_toplot, aes(x=date, y=count, group=final_category, color=final_category)) +
  geom_line() +
  labs(x = "Date", y = "Number of Accounts Recurited", title = "Number of Accounts by Category Over Time") +
  theme_minimal() +
  scale_x_date(labels = date_format("%m-%Y"),
               limits = as.Date(c('2013-01-01','2021-12-01')))
p

df1 <-  df_toplot %>%
  group_by(final_category) %>%
  summarise(mean_blm = mean(new_accounts, na.rm = TRUE),
            se_blm = sd(new_accounts, na.rm = TRUE)/sqrt(n()),
            .groups = "drop") 

ggplot(df1, aes(x = reorder(final_category, mean_blm), y = mean_blm)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  geom_errorbar(aes(ymin = mean_blm - se_blm, ymax = mean_blm + se_blm), 
                width = 0.2, size = 0.5, colour = "black") +
  theme_minimal() +
  theme(text = element_text(size=12, family="Times New Roman"), 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="Account Type", y="Average Daily Number of New Participates in Conversation") +
  coord_flip()


############################ influence ############################
library(arrow)
user_influence <- read_parquet('reply_retweet_like.parquet')

user_influence_agg <- user_influence %>%
  select(username, replyCount, retweetCount, likeCount) %>%
  group_by(username) %>%
  mutate(total_like = log(sum(likeCount, na.rm=T)+1),
         total_retweet = log(sum(retweetCount, na.rm=T)+1),
         total_reply = log(sum(replyCount, na.rm=T)+1),
         count = n(),
         avg_like = total_like/count,
         avg_retweet = total_retweet/count,
         avg_reply = total_reply/count) %>%
  select(username, total_like, total_retweet, total_reply, 
         count, avg_like, avg_retweet, avg_reply) %>% 
  unique()

tomerge <- filtered_df %>% 
  select(account, final_category) %>%
  rename(username = account) %>%
  group_by(username) %>%
  slice(1)

influence <- inner_join(user_influence_agg, tomerge, by='username')
influence$total_score <- (0.1*influence$total_like + 0.5*influence$total_reply + 1*influence$total_retweet)
influence$avg_score <- (0.1*influence$avg_like + 0.5*influence$avg_retweet + 1*influence$avg_reply)

stats <- influence %>%
  group_by(final_category) %>%
  summarise(sum_like = sum(total_like, na.rm=T),
         sum_reply = sum(total_reply, na.rm=T),
         sum_retweet = sum(total_retweet, na.rm=T),
         count = n(),
         mean_like = mean(total_like, na.rm=T),
         mean_reply = mean(total_reply, na.rm=T),
         mean_retweet = mean(total_retweet, na.rm=T),
         mean_total = mean(total_score, na.rm=T))


leveneTest(total_score ~ final_category, data = influence)
bartlett.test(total_score ~ final_category, data = influence)
# Welch's ANOVA
oneway.test(total_score ~ final_category, data = influence, var.equal = FALSE)
# post hoc
games_howell_result <- influence %>%
  ungroup() %>%
  games_howell_test(total_score ~ final_category)
games_howell_result

# other weights
influence$total_score <- (1*influence$total_like + 1*influence$total_reply + 1*influence$total_retweet)

# Assuming your data is named 'df'
# Calculate the mean and standard error for each 'final_category'
df_summary <- influence %>%
  group_by(final_category) %>%
  summarise(mean_blm = mean(total_score, na.rm = TRUE),
            se_blm = sd(total_score, na.rm = TRUE)/sqrt(n()),
            .groups = "drop")  # avoid the 'ungroup()' step

# Make a bar plot
ggplot(df_summary, aes(x = reorder(final_category, mean_blm), y = mean_blm)) +
  geom_bar(stat = "identity", fill = "skyblue", colour = "black") +
  geom_errorbar(aes(ymin = mean_blm - se_blm, ymax = mean_blm + se_blm), 
                width = 0.2, size = 0.5, colour = "black") +
  theme_minimal() +
  theme(text = element_text(size=12, family="Times New Roman"), 
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
  labs(x="Account Type", y="Average Influence Score") +
  coord_flip()
