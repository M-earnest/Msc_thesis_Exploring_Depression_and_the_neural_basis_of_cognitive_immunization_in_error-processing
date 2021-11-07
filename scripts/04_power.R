require(dplyr)
require(ggplot2)
require(Hmisc)
require(lme4)
require(lmerTest)
require(sjPlot)
require(performance)
require(emmeans)

# plots timeframe = -100/-.5 - 150/200
# adapt timeframe

set_theme(base=theme_bw())

#data_ern <- read.csv('/home/michael/data/derivatives/results/erp/erns.tsv', header = T, sep = '\t')
#data_power <- read.csv('/home/michael/git/master_thesis/data/average_power_itc_by_condition_group__long_format.csv', header = T, sep = '\t')
data_power <- read.csv('/home/michael/git/master_thesis/data/average_power_itc_by_condition_group_standardized_long_format.csv', header = T, sep = '\t')

df_power <- data_power %>%
  arrange(subject, FCz_avg, FCz_itc, time, freq,
          soc_condition, group, BDI_sum_score, DES_sum_score) %>%
  #summarise(amplitude = mean(value)) %>%
  mutate(freq = factor(freq)) %>%
  mutate(BDI_sum_score = as.numeric(BDI_sum_score)) %>%
  mutate(DES_sum_score = as.numeric(DES_sum_score)) %>%
  group_by(subject, FCz_avg, FCz_itc, time, freq,
          soc_condition, group, BDI_sum_score, DES_sum_score, bdi_z, des_z)

df_power_avg <- df_power %>%
  mutate(freq= as.numeric(as.character(freq))) %>%
  mutate(theta_band = ifelse(freq<=6.0, "low_theta","high_theta")) %>%
  mutate(theta_band = as.factor(theta_band)) %>%
  group_by(soc_condition, group, time, subject, theta_band, BDI_sum_score, DES_sum_score, bdi_z, des_z) %>%
  summarise(total_power = sum(FCz_avg, na.rm =TRUE))

df_itc_avg <- df_power %>%
  mutate(freq= as.numeric(as.character(freq))) %>%
  mutate(theta_band = ifelse(freq<=6.0, "low_theta","high_theta")) %>%
  mutate(theta_band = as.factor(theta_band)) %>%
  group_by(soc_condition, group, time, subject, theta_band, BDI_sum_score, DES_sum_score, bdi_z, des_z) %>%
  summarise(mean_itc = mean(FCz_itc, na.rm =TRUE))


ggplot(data = df_power_avg, aes(x = time,
                             y = total_power,
                             color = theta_band, shape = group)) +
  stat_summary(fun = mean, position = position_dodge(0.5)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
               position = position_dodge(0.5)) +
  facet_wrap(~ soc_condition) #+


ggplot(data = df_itc_avg, aes(x = time,
                             y = mean_itc,
                             color = theta_band, shape = group)) +
  stat_summary(fun = mean, position = position_dodge(0.5)) +
  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
               position = position_dodge(0.5)) +
  facet_wrap(~ soc_condition) #+

#ggplot(data = df_power, aes(x = time,
#                             y = FCz_avg,
#                             color = freq, shape = group)) +
#  stat_summary(fun = mean, position = position_dodge(0.25)) +
#  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
#               position = position_dodge(0.25)) #+
  # geom_point(alpha = 0.8, size = 1.25) +
  #facet_wrap(~ soc_condtion) #+
  #coord_cartesian(ylim = c(8, -8))


mod_power <- lmer(data = df_power_avg,
                total_power ~ group * soc_condition + time + theta_band + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum',
                                    theta_band = 'contr.sum'))
anova(mod_power)
summary(mod_power)
plot_model(mod_power, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_0 <- lmer(data = df_power_avg,
                total_power ~ soc_condition + time + theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_0)
summary(mod_power_0)
#plot_model(mod_power_0, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
#           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_1 <- lmer(data = df_power_avg,
                total_power ~ soc_condition * time + theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_1)
summary(mod_power_1)
plot_model(mod_power_1, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_2 <- lmer(data = df_power_avg,
                total_power ~ soc_condition + time * theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_2)
summary(mod_power_2)
plot_model(mod_power_2, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_power_3 <- lmer(data = df_power_avg,
                total_power ~ soc_condition + time + theta_band * group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_3)
summary(mod_power_3)
plot_model(mod_power_3, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_power_4 <- lmer(data = df_power_avg,
                total_power ~ soc_condition * group * theta_band + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_4)
summary(mod_power_4)
plot_model(mod_power_4, 'int', title = 'total_power ~ soc_condition * group * theta_band + time +',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_power_5 <- lmer(data = df_power_avg,
                total_power ~ time * group * theta_band + soc_condition + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_5)
summary(mod_power_5)
plot_model(mod_power_5, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_6 <- lmer(data = df_power_avg,
                total_power ~ group * theta_band + soc_condition + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_6)
summary(mod_power_6)
plot_model(mod_power_6, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_7 <- lmer(data = df_power_avg,
                total_power ~ group + theta_band * soc_condition + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_7)
summary(mod_power_7)
plot_model(mod_power_7, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_power_8 <- lmer(data = df_power_avg,
                total_power ~ group + theta_band * soc_condition * time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_8)
summary(mod_power_8)
plot_model(mod_power_8, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_9 <- lmer(data = df_power_avg,
                total_power ~ group + theta_band + soc_condition * time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_9)
summary(mod_power_9)
plot_model(mod_power_9, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_10 <- lmer(data = df_power_avg,
                total_power ~ theta_band + soc_condition + time * group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_10)
summary(mod_power_10)
plot_model(mod_power_10, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_11 <- lmer(data = df_power_avg,
                total_power ~ theta_band + soc_condition * group * bdi_z + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_11)
summary(mod_power_11)
plot_model(mod_power_11, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_12 <- lmer(data = df_power_avg,
                total_power ~ theta_band * soc_condition * des_z + bdi_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_12)
summary(mod_power_12)
plot_model(mod_power_12, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_13 <- lmer(data = df_power_avg,
                total_power ~ theta_band * soc_condition * bdi_z + des_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_13)
summary(mod_power_13)
plot_model(mod_power_13, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_14 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * bdi_z * des_z + time + group + soc_condition +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_14)
summary(mod_power_14)
plot_model(mod_power_14, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_15 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * bdi_z * time + group + soc_condition + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_15)
summary(mod_power_15)
plot_model(mod_power_15, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_16 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * des_z * time + group + soc_condition + bdi_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_16)
summary(mod_power_16)
plot_model(mod_power_16, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_17 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * bdi_z * group + soc_condition + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_17)
summary(mod_power_17)
plot_model(mod_power_17, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_18 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * des_z * group + soc_condition + bdi_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_18)
summary(mod_power_18)
plot_model(mod_power_18, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_19 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * des_z * soc_condition + bdi_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_19)
summary(mod_power_19)
plot_model(mod_power_19, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_20 <- lmer(data = df_power_avg,
                total_power ~ theta_band  * bdi_z * soc_condition + des_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_20)
summary(mod_power_20)
plot_model(mod_power_20, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


# MODEL COMPARISON

compare_performance(mod_power, mod_power_1, mod_power_2, mod_power_3,
                    mod_power_4, mod_power_5, mod_power_6, mod_power_7,
                    mod_power_8, mod_power_9, mod_power_10,mod_power_11,
                    mod_power_12, mod_power_13, mod_power_14,
                    mod_power_15, mod_power_16, mod_power_17, mod_power_18,
                    mod_power_19, mod_power_20,  rank = T)
plot(compare_performance(mod_power, mod_power_1, mod_power_2, mod_power_3,
                    mod_power_4, mod_power_5, mod_power_6, mod_power_7,
                    mod_power_8, mod_power_9, mod_power_10,mod_power_11,
                    mod_power_12, mod_power_13, mod_power_14,
                    mod_power_15, mod_power_16, mod_power_17, mod_power_18,
                    mod_power_19, mod_power_20,  rank = T))


# Plotting winnning model
set_theme(base=theme_bw(),
          axis.textsize.x = 0.9,
          axis.textsize.y = 0.8,
          axis.textsize = 1,
          axis.title.size = 1.2,
          axis.angle.x = 90,
          legend.title.size = 1,
          title.size = 1.1,)


mod_power_4 <- lmer(data = df_power_avg,
                total_power ~ soc_condition * group * theta_band + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_4)
summary(mod_power_4)
plot_model(mod_power_4, 'int', title = 'total_power ~ soc_condition * group * theta_band + time +',
           base_size = 11,
           mdrt.values = 'meansd',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_power_11 <- lmer(data = df_power_avg,
                total_power ~ theta_band + bdi_z * group * soc_condition + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power_11)
summary(mod_power_11)
set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3)
plot_model(mod_power_11, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_power_11 <- lmer(data = df_power_avg,
                total_power ~ theta_band + bdi_z * group * soc_condition  + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
#anova(mod_power_11)
#summary(mod_power_11)
set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3)
plot_model(mod_power_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("bdi_z", "soc_condition", "group"))

plot_model(mod_power_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("bdi_z", "soc_condition"))

plot_model(mod_power_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("group", "soc_condition"))

plot_model(mod_power_4, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("soc_condition", "theta_band"))


plot_model(mod_power_4, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("group", "theta_band"))


set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3,
          axis.angle.x = 90,)

plot_model(mod_power_4, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("group", "theta_band", "soc_condition"))

#CONTRASTS

bdi_mean = mean(df_power_avg$bdi_z)
bdi_sd = sd(df_power_avg$bdi_z)
bdi_min = min(df_power_avg$bdi_z)
bdi_max = max(df_power_avg$bdi_z)

means <- emmeans(mod_power_4, ~ theta_band)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_power_4, ~ time)
#contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ soc_condition | group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ group | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ theta_band | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ soc_condition | theta_band)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ theta_band | group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ group | theta_band)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ theta_band | group | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ group | theta_band | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_4, ~ soc_condition | theta_band | group)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_power_4, ~ group | soc_condition | theta_band)
#contrast(means, 'tukey', adjust = 'fdr')

# contrast second best model

means <- emmeans(mod_power_11, ~ theta_band)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_power_11, ~ time)
#contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ bdi_z | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ bdi_z | soc_condition, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ soc_condition | group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_power_11, ~ group | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')


#summary(mod_ern_plot)
set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3)

# plot contrast condition
plot_model(mod_power_11, type = "pred", title = 'estimated marginal means: soc_condition',
           base_size = 11,
           terms = c("soc_condition"))

# plot contrast theta band
plot_model(mod_power_11, type = "pred", title = 'estimated marginal means: theta_band',
           base_size = 11,
           terms = c("theta_band"))




#means <- emmeans(mod_ern_3, ~ soc_condition | group | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
# teste die gegeneinander (i.e., contraste)
#contrast(means, 'tukey', adjust = 'fdr')



### itc

mod_itc <- lmer(data = df_itc_avg,
                mean_itc ~ group * soc_condition + time + theta_band + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc)
summary(mod_itc)
plot_model(mod_itc, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_0 <- lmer(data = df_itc_avg,
                mean_itc ~ soc_condition + time + theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_0)
summary(mod_itc_0)
#plot_model(mod_power_0, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
#           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_1 <- lmer(data = df_itc_avg,
                mean_itc ~ soc_condition * time + theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_1)
summary(mod_itc_1)
plot_model(mod_itc_1, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_2 <- lmer(data = df_itc_avg,
                mean_itc ~ soc_condition + time * theta_band + group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_2)
summary(mod_itc_2)
plot_model(mod_itc_2, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_itc_3 <- lmer(data = df_itc_avg,
                mean_itc ~ soc_condition + time + theta_band * group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_3)
summary(mod_itc_3)
plot_model(mod_itc_3, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_itc_4 <- lmer(data = df_itc_avg,
                mean_itc ~ soc_condition * group * theta_band + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_4)
summary(mod_itc_4)
plot_model(mod_itc_4, 'int', title = 'total_power ~ soc_condition * group * theta_band + time +',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_itc_5 <- lmer(data = df_itc_avg,
                mean_itc ~ time * group * theta_band + soc_condition + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_5)
summary(mod_itc_5)
plot_model(mod_itc_5, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_6 <- lmer(data = df_itc_avg,
                mean_itc ~ group * theta_band + soc_condition + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_6)
summary(mod_itc_6)
plot_model(mod_itc_6, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_7 <- lmer(data = df_itc_avg,
                mean_itc ~ group + theta_band * soc_condition + time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_7)
summary(mod_itc_7)
plot_model(mod_itc_7, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



mod_itc_8 <- lmer(data = df_itc_avg,
                mean_itc ~ group + theta_band * soc_condition * time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_8)
summary(mod_itc_8)
plot_model(mod_itc_8, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_9 <- lmer(data = df_itc_avg,
                mean_itc ~ group + theta_band + soc_condition * time + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_9)
summary(mod_itc_9)
plot_model(mod_itc_9, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_10 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band + soc_condition + time * group + bdi_z + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_10)
summary(mod_itc_10)
plot_model(mod_itc_10, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_11 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band + soc_condition * group * bdi_z + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_11)
summary(mod_itc_11)
plot_model(mod_itc_11, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_12 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band * soc_condition * des_z + bdi_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_12)
summary(mod_itc_12)
plot_model(mod_itc_12, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_13 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band * soc_condition * bdi_z + des_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_13)
summary(mod_itc_13)
plot_model(mod_itc_13, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_14 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * bdi_z * des_z + time + group + soc_condition +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_14)
summary(mod_itc_14)
plot_model(mod_itc_14, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_15 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * bdi_z * time + group + soc_condition + des_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_15)
summary(mod_itc_15)
plot_model(mod_itc_15, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_16 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * des_z * time + group + soc_condition + bdi_z +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_16)
summary(mod_itc_16)
plot_model(mod_itc_16, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_17 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * bdi_z * group + soc_condition + des_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_17)
summary(mod_itc_17)
plot_model(mod_itc_17, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))


mod_itc_18 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * des_z * group + soc_condition + bdi_z + time +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_18)
summary(mod_itc_18)
plot_model(mod_itc_18, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_19 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * des_z * soc_condition + bdi_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_19)
summary(mod_itc_19)
plot_model(mod_itc_19, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

mod_itc_20 <- lmer(data = df_itc_avg,
                mean_itc ~ theta_band  * bdi_z * soc_condition + des_z + time + group +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc_20)
summary(mod_itc_20)
plot_model(mod_itc_20, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

compare_performance(mod_itc, mod_itc_0, mod_itc_1, mod_itc_2,
                    mod_itc_3, mod_itc_4, mod_itc_5, mod_itc_6,
                    mod_itc_7, mod_itc_8, mod_itc_9,mod_itc_10,
                    mod_itc_11, mod_itc_12, mod_itc_13, mod_itc_14,
                    mod_itc_15, mod_itc_16, mod_itc_17, mod_itc_18,
                    mod_itc_19, mod_itc_20,  rank = T)
plot(compare_performance(mod_itc, mod_itc_0, mod_itc_1, mod_itc_2,
                    mod_itc_3, mod_itc_4, mod_itc_5, mod_itc_6,
                    mod_itc_7, mod_itc_8, mod_itc_9,mod_itc_10,
                    mod_itc_11, mod_itc_12, mod_itc_13, mod_itc_14,
                    mod_itc_15, mod_itc_16, mod_itc_17, mod_itc_18,
                    mod_itc_19, mod_itc_20,  rank = T))


# explore best model
anova(mod_itc_11)
summary(mod_itc_11)
set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3)

plot_model(mod_itc_11, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))

# plot contrast theta band
plot_model(mod_itc_11, type = "pred", title = 'estimated marginal means: theta_band',
           base_size = 11,
           terms = c("theta_band"))


plot_model(mod_itc_11, type = "pred", title = 'estimated marginal means: theta_band',
           base_size = 11,
           terms = c("soc_condition"))


#anova(mod_power_11)
#summary(mod_power_11)
set_theme(base=theme_bw(),
          axis.textsize.x = 1.1,
          axis.textsize.y = 1.1,
          axis.textsize = 1.1,
          axis.title.size = 1.3)
plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("bdi_z", "soc_condition", "group"))

plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
             color= c('darkgoldenrod', 'navy', 'thistle4'),
             terms = c("bdi_z", "soc_condition"))

plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
             color= c('darkgoldenrod', 'navy', 'thistle4'),
             terms = c("bdi_z", "soc_condition"))

plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("group", "soc_condition"))

plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("soc_condition", "theta_band"))


plot_model(mod_itc_11, type = "pred", title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'),
           terms = c("group", "theta_band"))


#CONTRASTS

bdi_mean = mean(df_itc_avg$bdi_z)
bdi_sd = sd(df_itc_avg$bdi_z)
bdi_min = min(df_itc_avg$bdi_z)
bdi_max = max(df_itc_avg$bdi_z)


means <- emmeans(mod_itc_11, ~ theta_band)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ soc_condition)
contrast(means, 'tukey', adjust = 'fdr')

#means <- emmeans(mod_power_11, ~ time)
#contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ soc_condition | group)
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ group | soc_condition)
contrast(means, 'tukey', adjust = 'fdr')



means <- emmeans(mod_itc_11, ~ soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ bdi_z | soc_condition, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')


means <- emmeans(mod_itc_11, ~ soc_condition | bdi_z | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ bdi_z | soc_condition | group, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')

means <- emmeans(mod_itc_11, ~ group | soc_condition | bdi_z, at=list(bdi_z=c(bdi_mean-(bdi_sd), bdi_mean+(bdi_sd))))
contrast(means, 'tukey', adjust = 'fdr')
