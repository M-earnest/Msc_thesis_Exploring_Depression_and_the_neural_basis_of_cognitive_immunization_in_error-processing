#install.packages('dplyr', dependencies = TRUE)
#install.packages('ggplot2')
#install.packages('Hmisc')
#install.packages('lme4')
#install.packages('sjPlot')
#install.packages('performance')
#install.packages('see')
#install.packages('patchwork')

require(sjPlot)
require(dplyr)
require(ggplot2)
require(Hmisc)
require(lme4)
require(performance)
require(partR2)
require(emmeans)

#set_theme(
 # geom.outline.color = "antiquewhite4",
#  geom.outline.size = 1,
#  geom.label.size = 2,
#  geom.label.color = "grey50",
#  color.background = 'white',
#  title.size = 1.5)
theme_sjplot()
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
    # füge time on task variable
  mutate(time_on_task = ifelse(condition == 'test', 0,
                               ifelse(condition == 'baseline', 5,
                                      ifelse(condition == 'positive' & group == 'positive first', 20,
                                             ifelse(condition == 'negative' & group == 'positive first', 35,
                                                    ifelse(condition == 'positive' & group == 'negative first', 35, 20)))))) %>%

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

# modele (formel = abhängig ~ unabhängig + (random)

# mood only
mod_mood_0 <- lmer(data = data_model,
                   value ~ valence + group + (1|id))
car::Anova(mod_mood_0, test = 'F', type = 'III')
summary(mod_mood_0)

mod_mood_1 <- lmer(data = data_model,
                   value ~ valence + group * condition + (1|id))
car::Anova(mod_mood_1, test = 'F', type = 'III')
summary(mod_mood_1)
plot_model(mod_mood_1, 'int',
           axis.title = 'Predicted value of mood',
           title = 'Linear mixed model: Mood',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4', 'thistle4'))

plot_model(mod_mood_1, 'int')

mod_mood_2 <- lmer(data = data_model,
                   value ~ valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_2, test = 'F', type = 'III')
summary(mod_mood_2)
plot_model(mod_mood_2, 'int')


## Modell zur Veranschaulichung
mod_mood_3 <- lmer(data = data_model,
                   value ~ valence * condition * group +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_3, test = 'F', type = 'III')
summary(mod_mood_3)
#plot_model(mod_mood_3, 'int')
plot_model(mod_mood_3, 'int',
           axis.title = 'Predicted value of mood',
           title = 'Linear mixed model: Mood ~ valence * time_on_task * group +(1|id)',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4', 'thistle4'))

mod_mood_4 <- lmer(data = data_model,
                   value ~ valence + condition + (1|id))
car::Anova(mod_mood_4, test = 'F', type = 'III')
summary(mod_mood_4)
#plot_model(mod_mood_4, 'int')


mod_mood_5 <- lmer(data = data_model,
                   value ~ valence * condition + group + (1|id))
car::Anova(mod_mood_5, test = 'F', type = 'III')
summary(mod_mood_5)
plot_model(mod_mood_5, 'int')


compare_performance(mod_mood_0, mod_mood_1, mod_mood_2, mod_mood_3, mod_mood_4, mod_mood_5, rank = TRUE)
plot(compare_performance(mod_mood_0, mod_mood_1, mod_mood_2, mod_mood_3, mod_mood_4, mod_mood_5, rank = TRUE))

# use time factor instead of conditions

mod_mood_t_1 <- lmer(data = data_model,
                   value ~ valence + group * time_on_task + (1|id))
car::Anova(mod_mood_t_1, test = 'F', type = 'III')
summary(mod_mood_t_1)

plot_model(mod_mood_t_1, 'int')

mod_mood_t_2 <- lmer(data = data_model,
                   value ~ valence * group + time_on_task + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum'))
car::Anova(mod_mood_t_2, test = 'F', type = 'III')
summary(mod_mood_t_2)
plot_model(mod_mood_t_2, 'int')


## Modell zur Veranschaulichung
mod_mood_t_3 <- lmer(data = data_model,
                   value ~ valence * time_on_task * group +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_t_3, test = 'F', type = 'III')
summary(mod_mood_t_3)
#plot_model(mod_mood_t_3, 'int')
plot_model(mod_mood_t_3, 'int',
           axis.title = 'Predicted value of mood',
           title = 'Linear mixed model: Mood ~ valence * time_on_task * group +(1|id)',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4', 'thistle4'))

mod_mood_t_4 <- lmer(data = data_model,
                   value ~ valence + time_on_task + (1|id))
car::Anova(mod_mood_t_4, test = 'F', type = 'III')
summary(mod_mood_t_4)
#plot_model(mod_mood_t_4, 'int')


mod_mood_t_5 <- lmer(data = data_model,
                   value ~ valence * time_on_task + group + (1|id))
car::Anova(mod_mood_t_5, test = 'F', type = 'III')
summary(mod_mood_t_5)
plot_model(mod_mood_t_5, 'int')

mod_mood_t_6 <- lmer(data = data_model,
                   value ~ valence * time_on_task * condition + group + (1|id))
car::Anova(mod_mood_t_6, test = 'F', type = 'III')
summary(mod_mood_t_6)
plot_model(mod_mood_t_6, 'int')

mod_mood_t_7 <- lmer(data = data_model,
                   value ~ valence * condition +  time_on_task + group + (1|id))
car::Anova(mod_mood_t_7, test = 'F', type = 'III')
summary(mod_mood_t_7)
plot_model(mod_mood_t_7, 'int')

mod_mood_t_8 <- lmer(data = data_model,
                   value ~ valence * group + condition * time_on_task + (1|id))
car::Anova(mod_mood_t_8, test = 'F', type = 'III')
summary(mod_mood_t_8)
plot_model(mod_mood_t_8, 'int')

mod_mood_t_9 <- lmer(data = data_model,
                   value ~ valence + group + condition *  time_on_task + (1|id))
car::Anova(mod_mood_t_9, test = 'F', type = 'III')
summary(mod_mood_t_9)
plot_model(mod_mood_t_9, 'int')

mod_mood_t_10 <- lmer(data = data_model,
                   value ~ valence * group + condition + time_on_task + (1|id))
car::Anova(mod_mood_t_10, test = 'F', type = 'III')
summary(mod_mood_t_10)
plot_model(mod_mood_t_10, 'int')

mod_mood_t_11 <- lmer(data = data_model,
                   value ~ valence * group * condition + time_on_task + (1|id))
car::Anova(mod_mood_t_11, test = 'F', type = 'III')
summary(mod_mood_t_11)
set_theme(base = theme_light())
plot_model(mod_mood_t_11, 'int',
           axis.title = 'Predicted value of mood',
           title = 'Linear mixed model: Mood',
           base_size = 11,
           color= c('darkgoldenrod', 'seagreen4'))


compare_performance(mod_mood_0, mod_mood_1, mod_mood_2, mod_mood_3, mod_mood_4, mod_mood_5,
                    mod_mood_t_1, mod_mood_t_2, mod_mood_t_3, mod_mood_t_4, mod_mood_t_5,
                    mod_mood_t_6, mod_mood_t_7, mod_mood_t_8, mod_mood_t_9, mod_mood_t_10,
                    mod_mood_t_11, rank = TRUE)
plot(compare_performance(mod_mood_0, mod_mood_1, mod_mood_2, mod_mood_3, mod_mood_4, mod_mood_5,
                    mod_mood_t_1, mod_mood_t_2, mod_mood_t_3, mod_mood_t_4, mod_mood_t_5,
                    mod_mood_t_6, mod_mood_t_7, mod_mood_t_8, mod_mood_t_9, mod_mood_t_10,
                    mod_mood_t_11, rank = TRUE))

# mood + questionnaires
mod_mood_6 <- lmer(data = data_model,
                   value ~ bdi + valence * group + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_6, test = 'F', type = 'III')
summary(mod_mood_6)
plot_model(mod_mood_6, 'int')

mod_mood_7 <- lmer(data = data_model,
                   value ~ sps + valence * group + condition +(1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_7, test = 'F', type = 'III')
summary(mod_mood_7)
plot_model(mod_mood_7, 'int')

mod_mood_8 <- lmer(data = data_model,
                   value ~
                   des + valence * group +  condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_8, test = 'F', type = 'III')
summary(mod_mood_8)
plot_model(mod_mood_8, 'int')
check_model(mod_mood_8)


mod_mood_9 <- lmer(data = data_model,
                   value ~ bdi + sps + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_9, test = 'F', type = 'III')
summary(mod_mood_9)
check_model(mod_mood_9)
plot_model(mod_mood_9, 'int')


mod_mood_10 <- lmer(data = data_model,
                   value ~ bdi + des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_10, test = 'F', type = 'III')
summary(mod_mood_10)
check_model(mod_mood_10)
plot_model(mod_mood_10, 'int')


mod_mood_11 <- lmer(data = data_model,
                   value ~ sps + des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_11, test = 'F', type = 'III')
summary(mod_mood_11)
check_model(mod_mood_11)
plot_model(mod_mood_11, 'int')

mod_mood_12 <- lmer(data = data_model,
                   value ~ bdi + sps + des + valence * group + condition +  (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_12, test = 'F', type = 'III')
summary(mod_mood_12)
check_model(mod_mood_12)
plot_model(mod_mood_12, 'int')

mod_mood_13 <- lmer(data = data_model,
                   value ~ bdi * sps + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_13, test = 'F', type = 'III')
summary(mod_mood_13)
check_model(mod_mood_13)
plot_model(mod_mood_13, 'int')

############################### temp models
model_interaction <- lmer(data = data_model,
                   value ~ sps + bdi * valence * group * condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(model_interaction, test = 'F', type = 'III')
summary(model_interaction)
check_model(model_interaction)
model_interaction_plot <- plot_model(model_interaction, 'int')[11]

model_interaction_plot + labs(y = 'predicted values for emotions')



###################################

mod_mood_14 <- lmer(data = data_model,
                   value ~ bdi * des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_14, test = 'F', type = 'III')
summary(mod_mood_14)
check_model(mod_mood_14)
plot_model(mod_mood_14, 'int')


mod_mood_15 <- lmer(data = data_model,
                   value ~ sps * des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_15, test = 'F', type = 'III')
summary(mod_mood_15)
check_model(mod_mood_15)
plot_model(mod_mood_15, 'int')

mod_mood_16 <- lmer(data = data_model,
                   value ~ bdi * sps * des + valence * group + condition + (1|id),
                   contrasts = list(valence = 'contr.sum',
                                    group = 'contr.sum',
                                    condition = 'contr.sum'))
car::Anova(mod_mood_16, test = 'F', type = 'III')
summary(mod_mood_16)
check_model(mod_mood_16)
plot_model(mod_mood_16, 'int')


compare_performance(mod_mood_6, mod_mood_7, mod_mood_8,
                    mod_mood_9, mod_mood_10, mod_mood_11, mod_mood_12,
                    mod_mood_13, mod_mood_14, mod_mood_15, mod_mood_16,
                    rank = TRUE)
plot(compare_performance(mod_mood_6, mod_mood_7, mod_mood_8,
                          mod_mood_9, mod_mood_10, mod_mood_11, mod_mood_12,
                          mod_mood_13, mod_mood_14, mod_mood_15, mod_mood_16,
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
