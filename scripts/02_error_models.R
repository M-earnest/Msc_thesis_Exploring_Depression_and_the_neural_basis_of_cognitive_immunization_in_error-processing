# /home/michael/git/master_thesis/data/rt_err_fractions_subject_basis_df.tsv
# ToDo add order(group) as factor
# hol daten
data_err <- read.table('/home/michael/git/master_thesis/data/rt_err_fractions_subject_basis_df.tsv',
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
                 log(err_fract) ~ time_on_task + order + target + condition + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
#anova(mod_err_0)
car::Anova(mod_err_0, test = 'F', type = '3')

#summary(mod_err_0)

plot_model(mod_err_0,
           axis.title = 'Predicted value of error rate',
           title = 'log(err_fract) ~ time_on_task + order + target + condition + (1|Id)',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4', 'thistle4'))

mod_err_1 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task * condition + order + target + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_1)
summary(mod_err_1)

plot_model(mod_err_1,
           axis.title = 'Predicted value of error rate', type='int',
           title = 'log(err_fract) ~ time_on_task + order + target + condition + (1|Id)',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4', 'thistle4'))


mod_err_2 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task * condition + order * target + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_2)
summary(mod_err_2)


mod_err_3 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task + condition * order + target + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_3)
summary(mod_err_3)


mod_err_4 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task * condition * order + target + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_4)
summary(mod_err_4)


mod_err_5 <- lmer(data = data_err,
                 log(err_fract) ~ time_on_task + condition * order * target + (1|subj),
                 contrasts = list(target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_err_5)
summary(mod_err_5)

# compare models
require(performance)
compare_performance(mod_err_0, mod_err_1, mod_err_2, mod_err_3, mod_err_4, mod_err_5,  rank = T)
