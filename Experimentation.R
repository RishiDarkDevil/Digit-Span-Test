library(viridis)
library(ggpubr)
library(GGally)

user_data <- read_csv("user_data.csv")
user_dig_seq <- read_csv("user_dig_seq.csv")
user_restart_wrong <- read_csv("user_restart_wrong.csv")
user_digit_click_time <- read_csv("user_digit_click_time.csv")

user_data
print(user_dig_seq, n = 50)
print(user_digit_click_time, n = 150)
print(user_restart_wrong, n = 100)

# Per Individual User Analytics

# Hypothesis: For bigger numbers on an average we take more time to retrieve larger digit sequences(i.e. later rounds) from our memory
# Plot for mean click time for each round
avg_time_per_round_per_ID <- user_digit_click_time %>%
  group_by(ID, rounds) %>%
  summarise(mean_time_diff = mean(time_diff))
avg_time_per_round_per_ID  

avg_time_per_round_per_ID %>%
  filter(ID == 3) %>%
  mutate(rounds = parse_number(rounds)) %>%
  mutate(ID = as.character(ID)) %>%
  ggplot() +
  geom_line(aes(rounds, mean_time_diff, color = ID), size = 2)


# Hypothesis: We take more time near the middle of a digit sequence to guess its digits
time_per_round_per_ID <- user_digit_click_time %>%
  group_by(ID, rounds, clicks) %>%
  summarise(mean_time_diff = mean(time_diff))
time_per_round_per_ID  

time_per_round_per_ID %>%
  filter(ID == 39) %>%
  mutate(ID = as.character(ID), clicks = parse_number(clicks)) %>%
  ggplot() +
  geom_line(aes(clicks, mean_time_diff, color = rounds), size = 1.5) +
  coord_cartesian(ylim = c(0, 3))


# Group Analytics

digit_span_per_ID <- user_dig_seq %>%
  group_by(ID) %>%
  summarise(dig_span = max(parse_number(rounds))+1)
digit_span_per_ID

user_data_dig_span <- left_join(user_data, digit_span_per_ID, by = "ID")
user_data_dig_span

# Digit Span Histogram
p1 <- digit_span_per_ID %>%
  mutate(dig_span = as.factor(dig_span)) %>%
  ggplot() +
  geom_bar(aes(dig_span, fill = dig_span)) +
  labs(
    x = "Digit Span Score",
    y = "Proportion"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_viridis(discrete = TRUE)

t1 <- digit_span_per_ID %>%
  summarise('Mean' = mean(dig_span), 'S.D.' = sd(dig_span)) %>%
  mutate(across(where(is.numeric), ~round(., digits = 2)))

t1 <- ggtexttable(t1, theme = ttheme("mCyan"), rows = NULL)

ggarrange(p1, t1, nrow = 2, heights = c(11,1))

# Age Wise Performance
user_dig_span_age <- user_data_dig_span %>%
  mutate(age = as.factor(ifelse(age < 10, 1, ifelse(age < 20, 2, ifelse(age < 30, 3, ifelse(age < 40, 4, ifelse(age < 50, 5, ifelse(age < 60, 6, ifelse(age < 70, 7, 8)))))))))
user_dig_span_age

user_dig_span_age %>%
  group_by(age) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(age, mean_dig_span, fill = age), stat = "identity") +
  labs(
    x = "Age Groups",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Below 10","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "Above 70"), c(1, 2, 3, 4, 5, 6, 7, 8))) +
  scale_fill_viridis(discrete = TRUE)

user_dig_span_age %>%
  group_by(age) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))
  
# Gender wise Performance
user_data_dig_span %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(sex) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(sex, mean_dig_span, fill = sex), stat = "identity") +
  labs(
    x = "Gender",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Male", "Female"), c(0, 1))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  group_by(sex) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Education wise Performance
user_data_dig_span %>%
  mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
  group_by(educat) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(educat, mean_dig_span, fill = educat), stat = "identity") +
  labs(
    x = "Education Level",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("School-High School", "Undergraduate", "Graduate-Post Graduate"), c(1, 2, 3))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
  group_by(educat) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Job wise Performance
user_data_dig_span %>%
  mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
  group_by(job) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(job, mean_dig_span, fill = job), stat = "identity") +
  labs(
    x = "Profession",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Academia", "Industry, Bussiness or Other"), c(1, 2))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
  group_by(job) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Academic Performance wise Performance
user_data_dig_span %>%
  mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
  group_by(academic) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(academic, mean_dig_span, fill = academic), stat = "identity") +
  labs(
    x = "Academic Performance",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Below Average", "Average", "Above Average"), c(1, 2, 3))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
  group_by(academic) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Maths wise Performance
user_data_dig_span %>%
  mutate(maths = as.factor(maths)) %>%
  group_by(maths) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(maths, mean_dig_span, fill = maths), stat = "identity") +
  labs(
    x = "Have Maths?",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(maths = as.factor(maths)) %>%
  group_by(maths) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Music wise Performance
user_data_dig_span %>%
  mutate(music = as.factor(music)) %>%
  group_by(music) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(music, mean_dig_span, fill = music), stat = "identity") +
  labs(
    x = "Play Musical Instrument?",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Yes", "No"), c(1, 0))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(music = as.factor(music)) %>%
  group_by(music) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Env wise Performance
user_data_dig_span %>%
  mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
  group_by(env) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(env, mean_dig_span, fill = env), stat = "identity") +
  labs(
    x = "Environment",
    y = "Average Digit Span Score"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Silent or Normal", "Noisy or Very Noisy"), c(1, 2))) +
  scale_fill_viridis(discrete = TRUE)

user_data_dig_span %>%
  mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
  group_by(env) %>%
  summarise(mean_dig_span = mean(dig_span), sd_dig_span = sd(dig_span))

# Avg Time taken Age wise round wise
user_dig_span_age_click_time <- full_join(user_digit_click_time, user_dig_span_age)
user_dig_span_age_click_time

user_dig_span_age_click_time %>%
  group_by(age, rounds) %>%
  summarise(mean_time_diff = mean(time_diff)) %>%
  mutate(age = as.numeric(age)) %>%
  ggplot() +
  geom_line(aes(age, mean_time_diff, color = rounds), size = 1.5)

# Calculating DSP Ranking 
digit_span_per_ID <- user_dig_seq %>%
  group_by(ID) %>%
  summarise(dig_span = max(parse_number(rounds))+1)
digit_span_per_ID

user_data_dig_span <- left_join(user_data, digit_span_per_ID, by = "ID")
user_data_dig_span

user_data_dig_span <- user_data_dig_span %>%
  arrange(dig_span, age, educat, academic, maths, music, job, env) %>%
  add_column(ranking = 1:nrow(user_data_dig_span))

user_data_dig_span

curr_user_rank <- user_data_dig_span %>%
  filter(ID == 3)
rankpercent <- (nrow(user_data_dig_span) - curr_user_rank$ranking)*100 / nrow(user_data_dig_span)
ranktext <- paste0(round(rankpercent), "%")
ranktext

# Calculating Mean Time Ranking with same DSP Rank
avg_time_per_round_per_ID <- user_digit_click_time %>%
  group_by(ID) %>%
  summarise(mean_time_diff = mean(time_diff))

user_data_time <- full_join(full_join(user_data, avg_time_per_round_per_ID, by = "ID"), user_data_dig_span)
print(user_data_time, n = 100)

user_data_time <- user_data_time %>%
  filter(dig_span == 9) %>%
  arrange(mean_time_diff, age, educat, academic, maths, music, job, env) %>%
  add_column(ranking = 1:nrow(user_data_time %>% filter(dig_span == 9)))
user_data_time

curr_user_rank <- user_data_time %>%
  filter(ID == 3)
rankpercent <- (curr_user_rank$ranking)*100 / nrow(user_data_time)
ranktext <- paste0(round(rankpercent), "%")
ranktext

# Calculating Total Time Ranking with same DSP Rank
avg_time_per_round_per_ID <- user_digit_click_time %>%
  group_by(ID) %>%
  summarise(mean_time_diff = sum(time_diff))

user_data_time <- full_join(full_join(user_data, avg_time_per_round_per_ID, by = "ID"), user_data_dig_span)
user_data_time

user_data_time <- user_data_time %>%
  filter(dig_span == 8) %>%
  arrange(mean_time_diff, age, educat, academic, maths, music, job, env) %>%
  add_column(ranking = 1:nrow(user_data_time %>% filter(dig_span == 8)))
user_data_time

curr_user_rank <- user_data_time %>%
  filter(ID == 59)
rankpercent <- (curr_user_rank$ranking)*100 / nrow(user_data_time)
ranktext <- paste0(round(rankpercent), "%")
ranktext

# Calculating Net Ranking
user_data_dig_span_time <- user_digit_click_time %>%
  group_by(ID) %>%
  summarise(mean_time_diff = mean(time_diff), sum_time_diff = sum(time_diff)) 
user_data_dig_span_time

user_data_dig_span_time <- full_join(user_data_dig_span_time, user_data_dig_span)
print(user_data_dig_span_time, n = 100)

user_data_dig_span_time <- user_data_dig_span_time %>%
  group_by(dig_span) %>%
  arrange(dig_span, sum_time_diff, mean_time_diff, .by_group = TRUE) %>%
  ungroup() %>%
  mutate(ranking = 1:nrow(user_data_dig_span_time))
user_data_dig_span_time

curr_user_rank <- user_data_dig_span_time %>%
  filter(ID == 3)
rankpercent <- (nrow(user_data_dig_span_time) - curr_user_rank$ranking)*100 / nrow(user_data_dig_span_time)
ranktext <- paste0("Top ", round(rankpercent,2), "%")
ranktext
rankpercent


# Time Based Group Analytics
user_data_dig_time <- full_join(user_digit_click_time, user_data_dig_span)
user_data_dig_time

user_data_dig_time <- user_data_dig_time %>%
  mutate(age = as.factor(ifelse(age < 10, 1, ifelse(age < 20, 2, ifelse(age < 30, 3, ifelse(age < 40, 4, ifelse(age < 50, 5, ifelse(age < 60, 6, ifelse(age < 70, 7, 8))))))))) %>%
  group_by(age) %>%
  summarise(mean_time_diff = mean(time_diff))
user_data_dig_time

user_data_dig_time %>%
  ggplot() +
  geom_bar(aes(age, mean_time_diff, fill = age), stat = "identity") +
  labs(
    x = "Age",
    y = "Mean Time Difference between Clicks"
  ) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.background = element_blank(),
    legend.position = "none"
  ) +
  scale_x_discrete(labels = setNames(c("Below 10","10-19","20-29", "30-39", "40-49", "50-59", "60-69", "Above 70"), c(1, 2, 3, 4, 5, 6, 7, 8))) +
  scale_fill_viridis(discrete = TRUE)
