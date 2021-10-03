##'../data/reaction_times_subject_basis_df.tsv'

# hol daten
data_rt <- read.table('/home/michael/git/master_thesis/data/reaction_times_subject_basis_df.tsv',
                        sep = '\t', header = T)


# erstelle "neue" variablen
require(dplyr)
data_rt <- data_rt %>%
  arrange(subj, target, answer, condition) %>%
  # ändere die types
  mutate(mean_rt = as.numeric(mean_rt),
         sem = as.numeric(sem),
         amount.trial_num = as.numeric(fract_amount_trial_num),
         amount.trial_num_per_block = as.numeric(fract_amount_trial_num_per_block),
         condition = factor(condition, levels = c('test', 'baseline', 'positive', 'negative')),
         target = factor(target),
         answer = factor(answer)) %>%
  # füge time on task variable
  mutate(time_on_task = ifelse(condition == 'test', 0,
                               ifelse(condition == 'baseline', 5,
                                      ifelse(condition == 'positive' & order == 'positive first', 20,
                                             ifelse(condition == 'negative' & order == 'positive first', 35,
                                                    ifelse(condition == 'positive' & order == 'negative first', 35, 20)))))) %>%
  # kicke misses und test trials
  filter(answer != 'miss' & condition != 'test')


# plote die means und cis für mean rt
require(ggplot2)
require(Hmisc)
ggplot(data = data_rt, aes(x = time_on_task,
                           y = mean_rt,
                           color = target,
                           shape = condition)) +
  stat_summary(fun = mean, geom = 'point') +
  stat_summary(fun.data = mean_cl_boot, geom = 'linerange') +
  facet_wrap(answer ~ order, ncol = 2)


# setze die modelle auf
require(lme4)
require(lmerTest)

mod_rt_0 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer + target + condition + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_0)
summary(mod_rt_0)

mod_rt_1 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer * target + condition + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_1)
summary(mod_rt_1)
require(sjPlot)
plot_model(mod_rt_1, 'int')
plot_model(mod_rt_1, 'pred')

mod_rt_2 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer * target * condition + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_2)
summary(mod_rt_2)
plot_model(mod_rt_2, 'int')

mod_rt_3 <- lmer(data = data_rt,
                 mean_rt ~
                   time_on_task +
                   answer * target +
                   answer * condition + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_3)
summary(mod_rt_3)
plot_model(mod_rt_3, 'int')

# compare models
require(performance)
compare_performance(mod_rt_0, mod_rt_1, mod_rt_2, mod_rt_3, rank = T)

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
require(emmeans)
answer_means <- emmeans(mod_rt_3, ~ answer | target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_3, ~ answer)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_3, ~ answer | target | condition)
contrast(answer_means, 'tukey', adjust = 'fdr')