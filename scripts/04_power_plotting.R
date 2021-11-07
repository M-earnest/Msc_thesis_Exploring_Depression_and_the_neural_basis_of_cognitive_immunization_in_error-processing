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

## this doesn't seem to work
#ggplot(data = df_power, aes(x = time,
#                             y = FCz_avg,
#                             color = soc_condtion, shape = group)) +
#  stat_summary(fun = mean, position = position_dodge(0.25)) +
#  stat_summary(fun.data = 'mean_cl_boot', geom = 'linerange',
#               position = position_dodge(0.25)) +
  # geom_point(alpha = 0.8, size = 1.25) +
#  facet_wrap(~ freq) #+
  #coord_cartesian(ylim = c(8, -8))


mod_power <- lmer(data = df_power_avg,
                total_power ~ group * soc_condition + time + theta_band +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_power)
plot_model(mod_power, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))



### itc

mod_itc <- lmer(data = df_itc_avg,
                mean_itc ~ group * soc_condition + time + theta_band +
                  (1|subject),
                  contrasts = list(group = 'contr.sum',
                                    soc_condition = 'contr.sum', theta_band = 'contr.sum'))
anova(mod_itc)
plot_model(mod_itc, 'int', title = 'model 0: amplitude ~ acc * condition + group * soc_condition + (1|subject)',
           color= c('darkgoldenrod', 'navy', 'thistle4'))
