require(dplyr)
require(ggplot2)
require(Hmisc)
require(lme4)
require(lmerTest)
require(sjPlot)
require(performance)
require(emmeans)


data_ern <- read.csv('/home/michael/data/derivatives/results/erp/erns.tsv', header = T, sep = '\t')
data_crn <- read.csv('/home/michael/data/derivatives/results/erp/crns.tsv', header = T, sep = '\t')

data_ern <- data_ern %>%
  arrange(subject, epoch, group, condition, exp_condition,
          bdi_sum_score, des_sum_score, sps_sum_score,
          channel, time) %>%
  group_by(subject, epoch, group, condition, exp_condition,
          bdi_sum_score, des_sum_score, sps_sum_score,
          channel) %>%
  summarise(amplitude = mean(value)) %>%
  mutate(acc = 'incorrect')


data_crn <- data_crn %>% 
  arrange(subject, epoch, group, condition, exp_condition,
          bdi_sum_score, des_sum_score, sps_sum_score,
          channel, time) %>%
  group_by(subject, epoch, group, condition, exp_condition,
           bdi_sum_score, des_sum_score, sps_sum_score,
           channel) %>%
  summarise(amplitude = mean(value)) %>%
  mutate(acc = 'correct')


data_erps <- rbind(data_ern, data_crn)
data_erps <- data_erps %>%
  mutate(soc_condition = ifelse(group == 'negative_first' & exp_condition == 2, 'negative', 
                                ifelse(group == 'negative_first' & exp_condition == 3, 'positive',
                                       ifelse(group == 'positive_first' & exp_condition == 2, 'positive',
                                              ifelse(group == 'positive_first' & exp_condition == 3, 'negative',
                                                     'baseline')))))

ggplot(data = data_erps, aes(x = soc_condition,
                             y = amplitude, 
                             color = acc, shape = group)) +
  stat_summary(fun = mean, position = position_dodge(0.25)) + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange', 
               position = position_dodge(0.25)) + 
  # geom_point(alpha = 0.8, size = 1.25) +
  facet_wrap(~ channel) +
  coord_cartesian(ylim = c(8, -8))

rm(data_crn, data_ern)

# datensatz aaufbauen
# mittlere amplitude für condition (factor epochs raus)
data_erps_sum <- data_erps %>%
  group_by(subject, acc, group, soc_condition, channel) %>%
  summarise(amp = mean(amplitude))
  
mod_ern_0 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_0)
anova(mod_ern_0)
plot_model(mod_ern_0, 'int', title = 'model 0: amplitude ~ acc + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_0_int <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_0_int)
anova(mod_ern_0_int)
plot_model(mod_ern_0_int, 'int', , title = 'model 0_int: amplitude ~ acc * group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_1 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ acc * group * soc_condition * bdi_sum_score +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_1)
anova(mod_ern_1)
plot_model(mod_ern_1, 'int', , title = 'model 1: amplitude ~ acc * group * soc_condition * bdi_sum_score + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_2 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ acc * group * soc_condition * des_sum_score +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_2)
anova(mod_ern_2)
plot_model(mod_ern_2, 'int', title = 'model 2: amplitude ~ acc * group * soc_condition * des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_3 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ acc * group * soc_condition * bdi_sum_score * des_sum_score +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_3)
anova(mod_ern_3)
plot_model(mod_ern_3, 'int', , title = 'model 3: amplitude ~ acc * group * soc_condition * bdi_sum_score * des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_5 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ acc * group * soc_condition + bdi_sum_score + des_sum_score +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_5)
anova(mod_ern_5)
plot_model(mod_ern_5, 'int', , title = 'model 5: amplitude ~ acc * group * soc_condition + bdi_sum_score + des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_6 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ acc * group * soc_condition * bdi_sum_score + des_sum_score +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_6)
anova(mod_ern_6)
plot_model(mod_ern_6, 'int', , title = 'model 6: amplitude ~ acc * group * soc_condition * bdi_sum_score + des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_7 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ des_sum_score * group * soc_condition * acc +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_7)
anova(mod_ern_7)
plot_model(mod_ern_7, 'int', , title = 'model 7: amplitude ~ des_sum_score * group * soc_condition * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_4 <- lmer(data = filter(data_erps, channel == 'Cz'),
                amplitude ~ bdi_sum_score * group * soc_condition * acc +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_4)
anova(mod_ern_4)
plot_model(mod_ern_4, 'int', , title = 'model 4: amplitude ~ bdi_sum_score * group * soc_condition * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))
# compare models
compare_performance(mod_ern_0, mod_ern_0_int, mod_ern_1, mod_ern_2,
                    mod_ern_3, mod_ern_4, mod_ern_5, mod_ern_6,  rank = T)
plot(compare_performance(mod_ern_0, mod_ern_0_int, mod_ern_1, mod_ern_2,
                    mod_ern_3, mod_ern_4, mod_ern_5, mod_ern_6, rank = TRUE))

#### Contrasts model 3
# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)

means <- emmeans(mod_ern_3, ~ acc)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_ern_3, ~ des_sum_score)
#contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_ern_3, ~ bdi_sum_score)
#contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ group | acc)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ acc | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ acc | soc_condition)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ soc_condition | acc)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ group | soc_condition)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ soc_condition | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ group | des_sum_score)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ soc_condition | des_sum_score)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_3, ~ acc | bdi_sum_score)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ group | bdi_sum_score)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
means <- emmeans(mod_ern_3, ~ soc_condition | bdi_sum_score)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')
