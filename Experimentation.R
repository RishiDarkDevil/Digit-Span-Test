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
  filter(ID == 3) %>%
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

# Age Wise Performance
user_dig_span_age <- user_data_dig_span %>%
  mutate(age = as.factor(ifelse(age < 10, 1, ifelse(age < 20, 2, ifelse(age < 30, 3, ifelse(age < 40, 4, ifelse(age < 50, 5, ifelse(age < 60, 6, ifelse(age < 70, 7, 8)))))))))
user_dig_span_age

user_dig_span_age %>%
  group_by(age) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(age, mean_dig_span), stat = "identity")

# Gender wise Performance
user_data_dig_span %>%
  mutate(sex = as.factor(sex)) %>%
  group_by(sex) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(sex, mean_dig_span), stat = "identity")

# Education wise Performance
user_data_dig_span %>%
  mutate(educat = as.factor(ifelse(educat < 2, 1, ifelse(educat == 2, 2, 3)))) %>%
  group_by(educat) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(educat, mean_dig_span), stat = "identity")

# Job wise Performance
user_data_dig_span %>%
  mutate(job = as.factor(ifelse(job == 0, 1, 2))) %>%
  group_by(job) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(job, mean_dig_span), stat = "identity")

# Academic Performance wise Performance
user_data_dig_span %>%
  mutate(academic = as.factor(ifelse(academic <= 2, 1, ifelse(academic >= 4, 3, 2)))) %>%
  group_by(academic) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(academic, mean_dig_span), stat = "identity")

# Maths wise Performance
user_data_dig_span %>%
  mutate(maths = as.factor(maths)) %>%
  group_by(maths) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(maths, mean_dig_span), stat = "identity")

# Music wise Performance
user_data_dig_span %>%
  mutate(music = as.factor(music)) %>%
  group_by(music) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(music, mean_dig_span), stat = "identity")

# Env wise Performance
user_data_dig_span %>%
  mutate(env = as.factor(ifelse(env <= 1, 1, 2))) %>%
  group_by(env) %>%
  summarise(mean_dig_span = mean(dig_span)) %>%
  ggplot() +
  geom_bar(aes(env, mean_dig_span), stat = "identity")

# Avg Time taken Age wise round wise
user_dig_span_age_click_time <- full_join(user_digit_click_time, user_dig_span_age)
user_dig_span_age_click_time

user_dig_span_age_click_time %>%
  group_by(age, rounds) %>%
  summarise(mean_time_diff = mean(time_diff)) %>%
  mutate(age = as.numeric(age)) %>%
  ggplot() +
  geom_line(aes(age, mean_time_diff, color = rounds), size = 1.5)

#   
