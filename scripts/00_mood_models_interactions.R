#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2')
#install.packages('Hmisc')
#install.packages('lme4')
#install.packages('sjPlot')
#install.packages('performance')
#install.packages('see')

require(sjPlot)
require(dplyr)
require(ggplot2)
require(Hmisc)
require(lme4)
require(performance)
require(partR2)
require(emmeans)


# hol daten
data_mood <- read.table('/home/michael/git/master_thesis/data/mood_&_questionnaire_data_long_format.tsv',
                        sep = '\t', header = T)

# erstellt daten für model
# erstell data_model aus data_mood
data_model <- data_mood %>%
  # und zwar mach neuen variable aus oder, wenn 1 dann pos first, asonsten neg fist
  mutate(group = ifelse(order == 1, 'positive first', 'negative first')) %>%
  # behalte nur folgenede emotionen
  #filter(emotion %in% c('Wut', 'froh_freudig',
  #                      'erfolg_stolz', 'Niedergeschlagenheit')) %>%
  # group_by(group) %>%
  # mach neue variable, bdi - mittelwertzentrierung von bdi etc
  mutate(bdi = bdi_sum_score - mean(bdi_sum_score),
         des = des_sum_score - mean(des_sum_score),
         sps = sp_sum_score - mean(sp_sum_score)) %>%
  # mach grupen weg
  ungroup() %>%

  # mach neue variblen mit folgenden type
  mutate(value = as.integer(value),
         emotion = factor(emotion),
         valence = factor(valence),
         group = factor(group),
         condition = factor(condition),
         id = factor(id))

# plotte
ggplot(data = data_model, aes(x = group, y = bdi)) +
  # plote den mean von y
  stat_summary(fun = mean, geom = 'point') +
  # plote den CI von y
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange')

# mood + questionnaires
mod_mood_3 <- lmer(data = data_model,
                   value ~ bdi * group +  valence + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_3, test = 'F', type = 'III')
summary(mod_mood_3)
plot_model(mod_mood_3, 'int')

mod_mood_4 <- lmer(data = data_model,
                   value ~ bdi * group * valence + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_4, test = 'F', type = 'III')
summary(mod_mood_4)
plot_model(mod_mood_4, 'int')

mod_mood_5 <- lmer(data = data_model,
                   value ~ bdi * des + valence * group + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_5, test = 'F', type = 'III')
summary(mod_mood_5)
plot_model(mod_mood_5, 'int')

mod_mood_6 <- lmer(data = data_model,
                   value ~ bdi * des * group + valence + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_6, test = 'F', type = 'III')
summary(mod_mood_6)
plot_model(mod_mood_6, 'int')

mod_mood_7 <- lmer(data = data_model,
                   value ~
                   des * group + valence +  condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_7, test = 'F', type = 'III')
summary(mod_mood_7)
plot_model(mod_mood_7, 'int')
check_model(mod_mood_7)


mod_mood_8 <- lmer(data = data_model,
                   value ~
                   bdi + sps + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_8, test = 'F', type = 'III')
summary(mod_mood_8)
check_model(mod_mood_8)
plot_model(mod_mood_8, 'int')


mod_mood_9 <- lmer(data = data_model,
                   value ~ bdi * sps * group + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_9, test = 'F', type = 'III')
summary(mod_mood_9)
check_model(mod_mood_9)
plot_model(mod_mood_9, 'int')


mod_mood_10 <- lmer(data = data_model,
                   value ~ sps * des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_10, test = 'F', type = 'III')
summary(mod_mood_10)
check_model(mod_mood_10)
plot_model(mod_mood_10, 'int')

mod_mood_11 <- lmer(data = data_model,
                   value ~ bdi * group + sps + valence  + condition +  (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_11, test = 'F', type = 'III')
summary(mod_mood_11)
check_model(mod_mood_11)
plot_model(mod_mood_11, 'int')

mod_mood_11 <- lmer(data = data_model,
                   value ~ bdi * des + sps + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_12, test = 'F', type = 'III')
summary(mod_mood_12)
check_model(mod_mood_12)
plot_model(mod_mood_12, 'int')


mod_mood_13 <- lmer(data = data_model,
                   value ~ bdi * des * sps * group + valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_13, test = 'F', type = 'III')
summary(mod_mood_13)
check_model(mod_mood_13)
plot_model(mod_mood_13, 'int')


mod_mood_14 <- lmer(data = data_model,
                   value ~ bdi * sps * des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_14, test = 'F', type = 'III')
summary(mod_mood_14)
check_model(mod_mood_14)
plot_model(mod_mood_14, 'int')

mod_mood_15 <- lmer(data = data_model,
                   value ~ bdi * sps * des * group + valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_15, test = 'F', type = 'III')
summary(mod_mood_15)
check_model(mod_mood_15)
plot_model(mod_mood_15, 'int')

mod_mood_16 <- lmer(data = data_model,
                   value ~ bdi + group * valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_16, test = 'F', type = 'III')
summary(mod_mood_16)
check_model(mod_mood_16)
plot_model(mod_mood_16, 'int')

mod_mood_17 <- lmer(data = data_model,
                   value ~ bdi * valence + group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_17, test = 'F', type = 'III')
summary(mod_mood_17)
check_model(mod_mood_17)
plot_model(mod_mood_17, 'int')

mod_mood_18 <- lmer(data = data_model,
                   value ~ bdi * group * valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_18, test = 'F', type = 'III')
summary(mod_mood_18)
check_model(mod_mood_18)
plot_model(mod_mood_18, 'int')

mod_mood_19 <- lmer(data = data_model,
                   value ~ des * group * valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_19, test = 'F', type = 'III')
summary(mod_mood_19)
check_model(mod_mood_19)
plot_model(mod_mood_19, 'int')

mod_mood_20 <- lmer(data = data_model,
                   value ~ des * valence + group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_20, test = 'F', type = 'III')
summary(mod_mood_20)
check_model(mod_mood_20)
plot_model(mod_mood_20, 'int')

mod_mood_21 <- lmer(data = data_model,
                   value ~
                   sps * group * valence + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_21, test = 'F', type = 'III')
summary(mod_mood_21)
check_model(mod_mood_21)
plot_model(mod_mood_21, 'int')

mod_mood_22 <- lmer(data = data_model,
                   value ~ bdi * des * valence + group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_22, test = 'F', type = 'III')
summary(mod_mood_22)
check_model(mod_mood_22)
plot_model(mod_mood_22, 'int')

compare_performance(mod_mood_4, mod_mood_5, mod_mood_6, mod_mood_7, mod_mood_8,
                    mod_mood_9, mod_mood_10, mod_mood_11, mod_mood_12, mod_mood_13,
                    mod_mood_14, mod_mood_15, mod_mood_16, mod_mood_17, mod_mood_17,
                    mod_mood_19, mod_mood_20, mod_mood_21, mod_mood_22,
                    rank = TRUE)
plot(compare_performance(mod_mood_4, mod_mood_5, mod_mood_6, mod_mood_7, mod_mood_8,
                    mod_mood_9, mod_mood_10, mod_mood_11, mod_mood_12, mod_mood_13,
                    mod_mood_14, mod_mood_15, mod_mood_16, mod_mood_17, mod_mood_17,
                    mod_mood_19, mod_mood_20, mod_mood_21, mod_mood_22,
                    rank = TRUE))


#partR2(mod_mood_6, R2_type='conditional', partvars = c('sps:valence'))


#mean_mod_5 <- emmeans(mod_mood_5, ~ valence)
#contrast(mean_mod_5, 'tukey')
#confint(contrast(mean_mod_5, 'tukey'))

#mean_mod_5 <- emmeans(mod_mood_5, ~ condition | valence)
#contrast(mean_mod_5, 'tukey')
#confint(contrast(mean_mod_5, 'tukey'))

#ggplot(data = data_model, aes(x = condition, y = value, color = valence)) +
#  facet_wrap(~ group, ncol = 2) +
#  stat_summary(fun = mean, geom = 'point') +
#  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange')
