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

# setze die modelle auf
require(lme4)
require(lmerTest)

mod_rt_0 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer + target + condition + order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'),)
anova(mod_rt_0)
summary(mod_rt_0)

mod_rt_1 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer * target + condition + order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
#anova(mod_rt_1)
car::Anova(mod_rt_1, test = 'F', type = 'III')

summary(mod_rt_1)
require(sjPlot)
#plot_model(mod_rt_1, 'int')

plot_model(mod_rt_1, 'int',
           axis.title = 'Mean raction time',
           title = 'Mean reaction time by congruency of flanker and answer modality',
           base_size = 11,
           color= c('darkgoldenrod', 'navy'))

plot_model(mod_rt_1, 'pred')

mod_rt_2 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + answer * target * condition * order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_2)
summary(mod_rt_2)
plot_model(mod_rt_2, 'int')

#mod_rt_3 <- lmer(data = data_rt,
#                 mean_rt ~
#                   time_on_task +
#                   answer * target +
#                   answer * condition + order + (1|subj),
#                 contrasts = list(answer = 'contr.sum',
#                                  target = 'contr.sum',
#                                  condition = 'contr.sum'))
#anova(mod_rt_3)
#summary(mod_rt_3)
#plot_model(mod_rt_3, 'int')

mod_rt_4 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task * condition + answer * target + order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_4)
summary(mod_rt_4)
plot_model(mod_rt_4, 'int')

mod_rt_5 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task * condition * order + answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_5)
summary(mod_rt_5)
plot_model(mod_rt_5, 'int')

mod_rt_6 <- lmer(data = data_rt,
                 mean_rt ~ time_on_task + condition * order + answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_6)
summary(mod_rt_6)
plot_model(mod_rt_6, 'int')

set_theme(base=theme_bw(),
          axis.angle.x = 90,)

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ condition * answer + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  condition = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ time_on_task * answer + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target * condition  + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ time_on_task * answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target * time  + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ answer * target * order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  order = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target * condition  + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ answer * target * condition * order + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target * condition * order + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_rt_plot <- lmer(data = data_rt,
                 mean_rt ~ answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum'))
anova(mod_rt_plot)
#summary(mod_rt_plot)
plot_model(mod_rt_plot, 'int',title = 'model: mean_rt ~ answer * target * condition * order + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

answer_means <- emmeans(mod_rt_plot, ~ answer | target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_plot, ~ target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_plot, ~ answer)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_plot, ~ target | answer)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_plot, ~  target | answer)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_plot, ~ answer | target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

mod_rt_post_hoc <- lmer(data = data_rt,
                 mean_rt ~ answer * target + (1|subj),
                 contrasts = list(answer = 'contr.sum',
                                  target = 'contr.sum',
                                  condition = 'contr.sum'),)
anova(mod_rt_post_hoc)
#summary(mod_rt_0)
# compare models
require(performance)
> compare_performance(mod_rt_0, mod_rt_1, mod_rt_2, mod_rt_4, mod_rt_5, mod_rt_6,  rank = T)
> compare_performance(mod_rt_post_hoc, mod_rt_0, mod_rt_1, mod_rt_2, mod_rt_4, mod_rt_5, mod_rt_6,  rank = T)

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
require(emmeans)
answer_means <- emmeans(mod_rt_1, ~ answer | target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_1, ~  target | answer)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')


answer_means <- emmeans(mod_rt_1, ~ answer)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_1, ~ target)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_1, ~ answer | target | condition)
contrast(answer_means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
require(emmeans)
answer_means <- emmeans(mod_rt_3, ~ answer | target)
# teste die gegeneinander (i.e., contraste)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_3, ~ answer)
contrast(answer_means, 'tukey', adjust = 'fdr')

answer_means <- emmeans(mod_rt_3, ~ answer | target | condition)
contrast(answer_means, 'tukey', adjust = 'fdr')