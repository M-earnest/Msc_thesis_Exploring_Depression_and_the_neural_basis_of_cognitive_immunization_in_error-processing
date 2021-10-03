'/home/michael/git/master_thesis/data/mood_&_questionnaire_data_long_format.tsv'
install.packages('dplyr', dependencies = TRUE)
install.packages('ggplot2')
install.packages('Hmisc')
install.packages('lme4')
install.packages('sjplot')
install.packages('performance')
install.packages('sjplot')
install.packages('performance')

# hol daten
data_mood <- read.table('/home/michael/git/master_thesis/data/mood_&_questionnaire_data_long_format.tsv',
                        sep = '\t', header = T)

# erstellt daten für model
require(dplyr)
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
require(ggplot2)
require(Hmisc)
ggplot(data = data_model, aes(x = group, y = bdi)) +
  # plote den mean von y
  stat_summary(fun = mean, geom = 'point') +
  # plote den CI von y
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange')

# modele (formel = abhängig ~ unabhängig + (random)

# mood only
require(lme4)
mod_mood_0 <- lmer(data = data_model,
                   value ~ sps + valence + group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_0, test = 'F', type = 'III')
summary(mod_mood_0)

require(lme4)
mod_mood_1 <- lmer(data = data_model,
                   value ~ valence + group * condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_1, test = 'F', type = 'III')
summary(mod_mood_1)

require(sjPlot)
plot_model(mod_mood_1, 'int')

mod_mood_2 <- lmer(data = data_model,
                   value ~ valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_2, test = 'F', type = 'III')
summary(mod_mood_2)

require(sjPlot)
plot_model(mod_mood_2, 'int')

require(lme4)
mod_mood_3 <- lmer(data = data_model,
                   value ~ valence + condition + group +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_3, test = 'F', type = 'III')
summary(mod_mood_3)
require(sjPlot)
plot_model(mod_mood_3, 'int')

require(lme4)
mod_mood_4 <- lmer(data = data_model,
                   value ~ valence * condition * group +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_4, test = 'F', type = 'III')
summary(mod_mood_4)
require(sjPlot)
plot_model(mod_mood_4, 'int')

require(lme4)
mod_mood_5 <- lmer(data = data_model,
                   value ~ sps + valence * condition * group +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_5, test = 'F', type = 'III')
summary(mod_mood_5)
require(sjPlot)
plot_model(mod_mood_5, 'int')


require(lme4)
mod_mood_6 <- lmer(data = data_model,
                   value ~
                     group +
                     sps * valence +
                     valence * condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_6, test = 'F', type = 'III')
summary(mod_mood_6)
require(sjPlot)
plot_model(mod_mood_6, 'int')
require(performance)
check_model(mod_mood_6)


mod_mood_7 <- lmer(data = data_model,
                   value ~ des + sps * valence * condition * group + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_7, test = 'F', type = 'III')

require(partR2)
partR2(mod_mood_6, R2_type='conditional', partvars = c('sps:valence'))


require(emmeans)
mean_mod_5 <- emmeans(mod_mood_5, ~ valence)
contrast(mean_mod_5, 'tukey')
confint(contrast(mean_mod_5, 'tukey'))

mean_mod_5 <- emmeans(mod_mood_5, ~ condition | valence)
contrast(mean_mod_5, 'tukey')
confint(contrast(mean_mod_5, 'tukey'))

require(performance)
compare_performance(mod_mood_0, mod_mood_1, mod_mood_3, mod_mood_4, mod_mood_6, rank = TRUE)
plot(compare_performance(mod_mood_0, mod_mood_1, mod_mood_3, mod_mood_4, mod_mood_6, rank = TRUE))

require(ggplot2)
require(Hmisc)
ggplot(data = data_model, aes(x = condition, y = value, color = valence)) +
  facet_wrap(~ group, ncol = 2) +
  stat_summary(fun = mean, geom = 'point') +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange')
