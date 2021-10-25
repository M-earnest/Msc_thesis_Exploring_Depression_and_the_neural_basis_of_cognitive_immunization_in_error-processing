require(dplyr)
require(ggplot2)
require(Hmisc)
require(lme4)
require(lmerTest)
require(sjPlot)
require(performance)
require(emmeans)

set_theme(base=theme_bw())

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
                                                     'baseline'))))) %>%
  ungroup() %>%
  mutate(bdi_z = scale(bdi_sum_score),
         des_z = scale(des_sum_score),
         sps_z = scale(sps_sum_score))

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
                amplitude ~ acc * soc_condition + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
anova(mod_ern_0)
plot_model(mod_ern_0, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_0_int <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_0_int)
anova(mod_ern_0_int)
plot_model(mod_ern_0_int, 'int', , title = 'model 0_int: amplitude ~ acc * condition * group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_1 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition + bdi_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_1)
anova(mod_ern_1)
plot_model(mod_ern_1, 'int', , title = 'model 1: amplitude ~ acc * condition * group * soc_condition * bdi_sum_score + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_2 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_2)
anova(mod_ern_2)
plot_model(mod_ern_2, 'int', title = 'model 2: amplitude ~ acc * condition * group * soc_condition * des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_3 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_3)
anova(mod_ern_3)
plot_model(mod_ern_3, 'int', , title = 'model 3: amplitude ~ acc * condition * group * soc_condition * bdi_sum_score * des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_3 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition * bdi_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_3)
anova(mod_ern_3)
plot_model(mod_ern_3, 'int', , title = 'model 3: amplitude ~ acc * condition * group * soc_condition * bdi_sum_score * des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_3 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc  * group * des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_3)
anova(mod_ern_3)

mod_ern_5 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_5)
anova(mod_ern_5)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_5, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

bdi_mean = mean(data_erps$bdi_z)
bdi_sd = sd(data_erps$bdi_z)
#bdi_min = min(data_erps$bdi_sum_score)
#bdi_max = max(data_erps$bdi_sum_score)
means <- emmeans(mod_ern_5, ~ acc)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | soc_condition)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ soc_condition | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

#rm(data_erps)
#rm(data_erps_sum)

#emm_options(pbkrtest.limit = 36172)

means <- emmeans(mod_ern_5, ~ bdi_z | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ bdi_z | soc_condition, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

mod_ern_6 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition * des_z + bdi_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_6)
anova(mod_ern_6)
plot_model(mod_ern_6, 'int', , title = 'model 6: amplitude ~ acc * condition * group * soc_condition * bdi_sum_score + des_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_7 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * bdi_z + soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_7)
anova(mod_ern_7)
plot_model(mod_ern_7, 'int', , title = 'model 7: amplitude ~ des_sum_score * condition * group * soc_condition * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_8 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * des_z + soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_8)
anova(mod_ern_8)
plot_model(mod_ern_8, 'int', , title = 'model 7: amplitude ~ des_sum_score * condition * group * soc_condition * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_9 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition * bdi_z + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_9)
anova(mod_ern_9)
plot_model(mod_ern_9, 'int', , title = 'model 9: amplitude ~ acc * soc_condition + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_10 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition * des_z + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum'))
summary(mod_ern_10)
anova(mod_ern_10)
plot_model(mod_ern_10, 'int', , title = 'model 10: amplitude ~ acc * group + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_11 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition * bdi_z + des_z + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_11)
anova(mod_ern_11)
plot_model(mod_ern_11, 'int', , title = 'model 11: amplitude ~ acc * group + soc_condition + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_13 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition * des_z + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_13)
anova(mod_ern_13)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_13, 'int', title = 'model: amplitude ~ acc * group * soc_condition * bdi_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_13 <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition * des_z * des_z + group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_13)
anova(mod_ern_13)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_13, 'int', title = 'model: amplitude ~ acc * group * soc_condition * bdi_sum_score + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'thistle4'))

#mod_ern_14 <- lmer(data = filter(data_erps, channel == 'FCz'),
#                amplitude ~ acc + group * bdi * des_z + soc_condition +
#                  (1|subject),
#                  contrasts = list(acc = 'contr.sum',
#                                    group = 'contr.sum',
#                                    soc_condition = 'contr.sum'))
#summary(mod_ern_14)
#anova(mod_ern_14)
#set_theme(base=theme_bw(),
#          axis.angle.x = 90,)
#plot_model(mod_ern_14, 'int', title = 'model: amplitude ~ acc * group * soc_condition * bdi_sum_score + (1|subject)',
#           base_size = 11,
#           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~  soc_condition * group  * acc + (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude ~  soc_condition * group  * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~  soc_condition * group  * acc * bdi_z + (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude ~  soc_condition * group  * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~  soc_condition   * acc * bdi_z + group + (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude ~  soc_condition * group  * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ group * acc * bdi_z + soc_condition + (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude ~  soc_condition * group  * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ group  * soc_condition * bdi_z + acc + (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude ~  soc_condition * group  * acc + (1|subject)',
           base_size = 11,
           color= c('darkgoldenrod', 'navy', 'red'))

# compare models
compare_performance(mod_ern_0, mod_ern_0_int, mod_ern_1, mod_ern_2,
                    mod_ern_3, mod_ern_5, mod_ern_6, mod_ern_8,
                    mod_ern_9, mod_ern_10, mod_ern_11,
                    mod_ern_13, rank = T)
plot(compare_performance(mod_ern_0, mod_ern_0_int, mod_ern_1, mod_ern_2,
                    mod_ern_3, mod_ern_5, mod_ern_6, mod_ern_8,
                    mod_ern_9, mod_ern_10, mod_ern_11,
                    mod_ern_13, rank = T))

#### Contrasts model 5 ~ highest R2 + hypothesis; acc + group * soc_condition * bdi_z + des_z + condition + (1/ID)
# berechne die estimated marginal means (i.e., vom model vorhergesagte werte)
anova(mod_ern_5)

bdi_mean = mean(data_erps$bdi_z)
bdi_sd = sd(data_erps$bdi_z)

means <- emmeans(mod_ern_5, ~ bdi_z | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')
#bdi_min = min(data_erps$bdi_sum_score)
#bdi_max = max(data_erps$bdi_sum_score)
means <- emmeans(mod_ern_5, ~ acc)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_ern_5, ~ condition)
#contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | soc_condition)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')


means <- emmeans(mod_ern_5, ~ bdi_z | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ bdi_z | soc_condition, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')


means <- emmeans(mod_ern_5, ~ soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_5, ~ group | soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')


means <- emmeans(mod_ern_5, ~ soc_condition | group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')



mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition + bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition * group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition + bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition + bdi_z * group + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc +  soc_condition + group * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition + group * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc +  soc_condition + group * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           ylim = c(-5, 5))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition + group * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc +  soc_condition + group * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           axis.lim = c(-5, 5))



mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group + soc_condition * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc +  soc_condition + group * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           axis.lim = c(-2, 2))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group +  bdi_z * soc_condition + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
#anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group + soc_condition * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition + bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * soc_condition * bdi_z + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * soc_condition * bdi_z + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + group * bdi_z + soc_condition + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + group * bdi_z + soc_condition + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc + soc_condition * bdi_z + group + des_z +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model 5: amplitude ~ acc + soc_condition * bdi_z + group + des_z + (1|subject)',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

ggplot(data = data_erps, aes(x = soc_condition,
                             y = amplitude,
                             color = soc_condition)) +
  stat_summary(fun = mean, position = position_dodge(0.25)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
               position = position_dodge(0.25)) +
  scale_color_manual(values = c('darkgoldenrod', 'navy', 'thistle4')) #+
  # geom_point(alpha = 0.8, size = 1.25) +
  #facet_wrap(~ channel) +
  #coord_cartesian(ylim = c(0, -3))


ggplot(data = data_erps, aes(x = acc,
                             y = amplitude,
                             color = acc)) +
  stat_summary(fun = mean, position = position_dodge(0.25)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
               position = position_dodge(0.25)) +
  scale_color_manual(values = c('darkgoldenrod', 'navy', 'thistle4')) #+
  # geom_point(alpha = 0.8, size = 1.25) +
  #facet_wrap(~ channel) +
  #coord_cartesian(ylim = c(0, -3))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
#summary(mod_ern_plot)
plot_model(mod_ern_plot, type = "pred", title = 'estimated marginal means: soc_condition',
           base_size = 11, terms = c("soc_condition"))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc +
                  (1|subject))
#summary(mod_ern_plot)
plot_model(mod_ern_plot, type = "pred", title = 'estimated marginal means: correct/incorrect reaction',
           base_size = 11)

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ soc_condition * group +
                  (1|subject))
#summary(mod_ern_plot)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group + soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

means <- emmeans(mod_ern_plot, ~ acc | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_plot, ~ group | acc)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

means <- emmeans(mod_ern_plot, ~ acc )
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')


means <- emmeans(mod_ern_plot, ~ soc_condition )
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')



mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ acc * group * soc_condition +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



means <- emmeans(mod_ern_plot, ~ group | soc_condition)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_plot, ~ soc_condition | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_plot, ~ acc | soc_condition | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_plot, ~ group | soc_condition | acc)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~ soc_condition * group * acc +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~  soc_condition* acc * group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_ern_plot <- lmer(data = filter(data_erps, channel == 'FCz'),
                amplitude ~  acc * group +
                  (1|subject),
                  contrasts = list(acc = 'contr.sum',
                                    group = 'contr.sum',
                                    soc_condition = 'contr.sum'))
summary(mod_ern_plot)
anova(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.angle.x = 90,)
plot_model(mod_ern_plot, 'int', , title = 'model: amplitude acc * group * soc_condition + (1|ID))',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

means <- emmeans(mod_ern_plot, ~ group | acc)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_ern_plot, ~ acc | group)
# teste die gegeneinander (i.e., contraste)
contrast(means, 'tukey', adjust = 'fdr')

