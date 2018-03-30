# Loading packages

library(tidyverse)
library(haven)
library(nlme)
library(patchwork)
library(broom.mixed)
library(broom)

# Loading data

recode_df <- read_csv('yday.csv')
vars_df <- read_sav("archive/data2/all_three_semesters.sav")
IDs_df <- read_csv("archive/ss15_key.csv")
all_three_semesters <- read_sav("archive/data2/all_three_semesters.sav")
all_three_semesters$ID <- as.integer(all_three_semesters$ID)

# Processing / preparing data

IDs_df <- select(IDs_df, ID, username = Username)
vars_df$ID <- as.integer(vars_df$ID)

demo_df <- left_join(IDs_df, vars_df, by = "ID")

recode_df <- recode_df %>% 
  mutate(ydaya = yday + 14)

recode_df$date <-strptime(paste("2015", as.integer(recode_df$ydaya)), format="%Y %j")
recode_df$date <- lubridate::ymd(recode_df$date)

# Descriptive plots 

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

# > 273/2
# [1] 136.5
# 
# > 273/4
# [1] 68.25

p1 <- recode_df %>%
  group_by(date, student_ID) %>%
  summarize(sum_tw = sum(time_watched_minutes)) %>% 
  ungroup() %>% 
  group_by(student_ID) %>%  
  sample_n_groups(137) %>% 
  ggplot(aes(x = date, y = sum_tw, group = student_ID)) +
  geom_line(alpha = .5) +
  geom_point(alpha = .5) +
  theme_bw() +
  ylab("Minutes Viewed") +
  xlab(NULL) +
  theme(text = element_text(family = "Times")) +
  ggtitle("Minutes viewed by student per day (for sample of students)") +
  labs(subtitle = "Note. Interval not used in this analysis is in gray.") +
  geom_rect(aes(xmin = lubridate::ymd("2015-01-13"), xmax = lubridate::ymd("2015-02-03"), ymin = -2, ymax = 350), alpha = .025, fill = "lightgray") +
  geom_rect(aes(xmin = lubridate::ymd("2015-03-02"), xmax = lubridate::ymd("2015-05-07"), ymin = -2, ymax = 350), alpha = .025, fill = "lightgray") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-02-03"))) +
  geom_text(aes(x = lubridate::ymd("2015-01-27"), y = -12.5), label = "Exam 1", family = "Times") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-03-03"))) +
  geom_text(aes(x = lubridate::ymd("2015-02-23"), y = -12.5), label = "Exam 2", family = "Times") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-04-14"))) +
  geom_text(aes(x = lubridate::ymd("2015-04-07"), y = -12.5), label = "Exam 3", family = "Times") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-05-07"))) +
  geom_text(aes(x = lubridate::ymd("2015-05-01"), y = -12.5), label = "Final", family = "Times")

uid <- pp$data$student_ID

p2 <- recode_df %>%
  filter(date >= lubridate::ymd("2015-02-04") & date <= lubridate::ymd("2015-03-02")) %>% 
  filter(student_ID %in% uid) %>% 
  group_by(date, student_ID) %>% 
  summarize(sum_tw = sum(time_watched_minutes)) %>%
  ggplot(aes(x = date, y = sum_tw, group = student_ID)) +
  geom_line(color = "black", alpha = .5) +
  geom_point(alpha = .5) +
  theme_bw() +
  ylab("Minutes Viewed") +
  xlab(NULL) +
  theme(text = element_text(family = "Times")) +
  ggtitle("Minutes viewed between Exam 1 and 2 (for sample of students)") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-02-03"))) +
  geom_text(aes(x = lubridate::ymd("2015-02-05"), y = -10), label = "Exam 1", family = "Times") +
  geom_vline(aes(xintercept = lubridate::ymd("2015-03-02"))) +
  geom_text(aes(x = lubridate::ymd("2015-02-28"), y = -10), label = "Exam 2", family = "Times") +
  theme(text= element_text(size = 12))

pp <- p1 + p2 + plot_layout(ncol = 1)

ggsave("descriptive-engagement-plot.png", pp, width = 6, height = 8)

# facet plots

recode <- filter(recode_df, date >= lubridate::ymd("2015-02-04") & date <= lubridate::ymd("2015-03-02")) 

wave_plotter <- function(n_waves) {
  seq_by <- 26/n_waves
  var <- seq(21, 47, seq_by)
  recode$wave <- cut(recode$yday, breaks=var, labels = 1:n_waves)
  recode$wave <- ifelse(is.na(recode$wave), 1, recode$wave)
  recode %>% 
    group_by(student_ID, wave) %>% 
    summarize(stwm = sum(time_watched_minutes)) %>% 
    ggplot(aes(x = wave, y = stwm, group = student_ID)) +
    geom_line() +
    geom_point() +
    theme_bw() +
    xlab("Wave") + 
    ylab("Minutes viewed") +
    ggtitle(str_c(n_waves, " Waves")) +
    scale_x_continuous(breaks= scales::pretty_breaks(n = n_waves - 1)) +
    theme(text = element_text(size = 12))
}

w2 <- wave_plotter(2)
w3 <- wave_plotter(3)
w4 <- wave_plotter(4)
w5 <- wave_plotter(5)
w6 <- wave_plotter(6)
w7 <- wave_plotter(7)
w8 <- wave_plotter(8)
w9 <- wave_plotter(9)
w10 <- wave_plotter(10)

pdes <- w2 + w3 + w4 + w5 + w6 + w7 + w8 +w9 + w10 + plot_layout(ncol = 3)

ggsave("descriptive-wave-plot.png", pdes, width = 6.5, height = 8)

# Processing data into five waves

recode <- filter(recode_df, date >= lubridate::ymd("2015-02-04") & date <= lubridate::ymd("2015-03-02")) 

n_waves <- 5
seq_by <- 26/n_waves
var <- seq(21, 47, seq_by)
recode$wave <- as.integer(cut(recode$yday, breaks=var, labels = 1:n_waves))
recode$wave <- ifelse(is.na(recode$wave), 1, recode$wave)

d <- recode %>%
  group_by(student_ID, wave) %>%
  summarize(stwm = sum(time_watched_minutes, na.rm = T)) %>%
  ungroup() %>% 
  mutate(wave = as.integer(wave))

# Growth modeling

recode.grouped <- groupedData(stwm ~ wave|student_ID, data = d, order.groups = F)
ctrl <- lmeControl(opt='optim', maxIter=1e8, msMaxIter = 1e8)

ma1 <- lme(stwm ~ wave + I(wave^2),
           random = ~ wave + I(wave^2), method = "REML",
           data = recode.grouped, na.action = na.omit, control = ctrl)


# no random
mb1 <- lme(stwm ~ wave + I(wave^2),
           random = ~ wave, method = "REML",
           data = recode.grouped, na.action = na.omit, control = ctrl)

anova(ma1, mb1)

ma2 <- lme(stwm ~ wave + I(wave^2),
           random = ~ wave + I(wave^2), method = "REML",
           correlation = corAR1(form = ~ wave|student_ID),
           data = recode.grouped, na.action = na.omit, control = ctrl)

anova(ma1, ma2)
summary(ma2)

# Adding predicted values

recode.grouped$p1l0 <- predict(ma2, newdata = recode.grouped, level = 0)
recode.grouped$p1l1 <- predict(ma2, newdata = recode.grouped, level = 1)

p5 <- ggplot(recode.grouped, aes(x=wave, y = p1l1, group= student_ID)) +
  # geom_point(aes(x = wave, y = stwm)) +
  geom_line(aes(x=wave, y = p1l0), stat = "smooth", method = "lm", formula = y ~ x + I(x^2), se = F, color = "black", size = 1.5) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x + I(x^2), se = F, color = "gray", alpha = .1) +
  theme_bw() +
  ylab("Minutes Viewed") +
  xlab("Wave") +
  theme(text = element_text(family = "Times")) +
  ggtitle("Fitted quadratic growth model (black) and student trajectories (gray)")

ggsave("fitted-engagement-plot-ma2-5-waves.png", p4, width = 7, height = 5)

# Preparing other dataset

d1 <- nlme::random.effects(ma2) %>% 
  rownames_to_column("student_ID") %>% 
  mutate(intercept = `(Intercept)` + nlme::fixed.effects(ma2)[1],
         wave = wave + nlme::fixed.effects(ma2)[2],
         quad =  `I(wave^2)`+ nlme::fixed.effects(ma2)[3]) %>% 
  select(student_ID, intercept, wave, quad, -`(Intercept)`, -`I(wave^2)`) %>% 
  as_tibble()

#cost value
psych::alpha(jmRtools::composite_matrix_maker(demo_df, T1Q003, T1Q017, T1Q042, T1Q034, T1Q025))
# perceptions of competence
psych::alpha(jmRtools::composite_matrix_maker(demo_df, T1Q016, T1Q007, T1Q028, T1Q035, T1Q022))

demo_df$cost_value <- jmRtools::composite_mean_maker(demo_df, T1Q003, T1Q017, T1Q042, T1Q034, T1Q025)
demo_df$perceived_competence <- jmRtools::composite_mean_maker(demo_df, T1Q016, T1Q007, T1Q028, T1Q035, T1Q022)
demo_df$utility_value <- jmRtools::composite_mean_maker(demo_df, T1Q038, T1Q014, T1Q026, T1Q005, T1Q043)
demo_df$interest_value <- jmRtools::composite_mean_maker(demo_df, T1Q036, T1Q019, T1Q001, T1Q032, T1Q041)
demo_df$attainment_value <- jmRtools::composite_mean_maker(demo_df, T1Q024, T1Q009, T1Q045, T1Q030, T1Q012)
demo_df$task_value <- jmRtools::composite_mean_maker(demo_df, T1Q038, T1Q014, T1Q026, T1Q005, T1Q043, T1Q036, T1Q019, T1Q001, T1Q032, T1Q041, T1Q024, T1Q009, T1Q045, T1Q030, T1Q012)
demo_df$mastery_approach <- jmRtools::composite_mean_maker(demo_df, T1Q003, T1Q012, T1Q020, T1Q024, T1Q027)
demo_df$mastery_avoid <- jmRtools::composite_mean_maker(demo_df, T1Q036, T1Q015, T1Q006, T1Q022)
demo_df$performance_approach <- jmRtools::composite_mean_maker(demo_df, T1Q034, T1Q008, T1Q033, T1Q025, T1Q039)
demo_df$performance_avoid <- jmRtools::composite_mean_maker(demo_df, T1Q041, T1Q030, T1Q018, T1Q004)
demo_df$watch_videos <- jmRtools::composite_mean_maker(demo_df, T1Q139, T1Q140, T1Q141)
demo_df$taken_online <- ifelse(demo_df$T1Q142 == 1, 1,
                               ifelse(demo_df$T1Q142 == 2, 0, NA))
demo_df$have_watched_video <- ifelse(demo_df$T1Q143 == 1, 1,
                                     ifelse(demo_df$T1Q142 == 2, 0, NA))
demo_df <- rename(demo_df, student_ID = ID)
demo_df <- select(demo_df, student_ID, cost_value:have_watched_video, Exam1, Exam2, FinalExam, FinalGrade)

dd <- left_join(mutate(d1, student_ID = as.integer(student_ID)), demo_df)

## Descriptives

tm <- d %>% spread(wave, stwm)
names(tm) <-c(c("student_ID"), str_c("wave", 1:5))

top <- dd %>% 
  select(student_ID, cost_value, perceived_competence, Exam2, FinalExam, FinalGrade) %>% 
  left_join(tm) %>% 
  psych::describe() %>% 
  as_data_frame() %>% 
  rownames_to_column() %>% 
  filter(rowname!="student_ID") %>% 
  select(variable = rowname, everything(), -vars, -mad, -range, -se)

top %>% mutate_if(is.double, round, digits = 3) %>% 
  mutate(mean_sd = str_c(mean, " (", sd, ")")) %>% 
  clipr::write_clip()

top %>% mutate_if(is.double, round, digits = 3) %>% 
  mutate(mean_sd = str_c(mean, " (", sd, ")")) %>% 
  select(mean_sd)

toc <- dd %>% 
  select(student_ID, cost_value, perceived_competence, Exam2, FinalExam, FinalGrade) %>% 
  left_join(tm) %>% 
  select(-student_ID) %>% 
  corrr::correlate() %>% 
  corrr::shave() %>% 
  corrr::fashion() 

toc %>% clipr::write_clip()

fc <- dd %>% 
  select(student_ID, cost_value, perceived_competence, Exam1, FinalExam, FinalGrade) %>% 
  left_join(tm) %>% 
  select(-student_ID)

# Final modeling

# for antecedents
summary(lm(intercept ~ cost_value + perceived_competence, data = dd))
summary(lm(wave ~ cost_value + perceived_competence, data = dd))
summary(lm(quad ~ cost_value + perceived_competence, data = dd))

# predicting outcomes
summary(lm(scale(Exam2) ~intercept+wave+quad, data = dd))
summary(lm(scale(FinalExam) ~intercept+wave+quad, data = dd))
summary(lm(scale(FinalGrade) ~intercept+wave+quad, data = dd))
