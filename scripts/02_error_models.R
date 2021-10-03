'../data/reaction_times_subject_basis_df.tsv'

# hol daten
data_err <- read.table('../data/rt_err_fractions_subject_basis_df.tsv',
                       sep = '\t', header = T)


# erstelle "neue" variablen
require(dplyr)
data_err <- data_err %>%
  arrange(subj, target, condition) %>%
  # ändere die types
  mutate(condition = factor(condition, levels = c('test', 'baseline', 'positive', 'negative')),
         target = factor(target)) %>%
  # füge time on task variable
  mutate(time_on_task = ifelse(condition == 'test', 0,
                               ifelse(condition == 'baseline', 5,
                                      ifelse(condition == 'positive' & order == 'positive first', 20,
                                             ifelse(condition == 'negative' & order == 'positive first', 35,
                                                    ifelse(condition == 'positive' & order == 'negative first', 35, 20)))))) %>%
  # kicke misses und test trials
  filter(condition != 'test')

# plote die means und cis für error fraction
require(ggplot2)
require(Hmisc)
ggplot(data = data_err, aes(x = time_on_task,
                           y = err_fract,
                           color = target,
                           shape = condition)) +
  stat_summary(fun = mean, geom = 'point') +
  stat_summary(fun.data = mean_cl_boot, geom = 'linerange') +
  facet_wrap(~ order, ncol = 2)


hist(log(data_err$err_fract))

# setze die modelle auf
require(lme4)
require(lmerTest)

mod_err_0 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task + target + condition + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_0)
summary(mod_err_0)