# Author: Kevin See
# Purpose: Make some presentation plots
# Created: 2/27/2020
# Last Modified: 3/2/2020
# Notes: This is a collaboration with Claire McGrath

#----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(lme4)
library(MuMIn)
# library(caret)

theme_set(theme_bw())
# theme_set(theme_gray())
theme_update(panel.grid = element_blank(),
             panel.background = element_rect(fill = 'gray90'),
             text = element_text(family = "Arial",
                                      face = "bold"),
             strip.background = element_rect(fill = 'white'))

#----------------------------------------------------------------
# load data and model results
load('analysis/code/modelResults/fits_validation.rda')

mod_fits
mod_sel


#----------------------------------------------------------------
# plots for aerial surveys
#----------------------------------------------------------------
# pull out the model for air surveys
mod = mod_sel %>%
  ungroup() %>%
  filter(type == 'full',
         Survey == 'Air',
         Resp == 'Net') %>%
  pull(model) %>%
  extract2(1)

pred_seq = seq(-2, 2, by = 0.05)

coef_nms = names(fixef(mod))[-1]
coef_nms = coef_nms[!grepl('^I\\(', coef_nms)]
coef_nms = coef_nms[!grepl('\\:', coef_nms)]

pred_covar = c('AveOverlap',
               'ANNDist_log',
               'redd_dens',
               'AveContrast',
               'AvgAge',
               'AveSunny',
               'AveCanopy')

pred_df = covar_center %>%
  group_by(variable) %>%
  nest() %>%
  mutate(pred_seq = map(data,
                        .f = function(x) {
                          tibble(value = seq(x$min,
                                             x$max,
                                             length.out = 200))
                        })) %>%
  select(-data) %>%
  unnest(cols = pred_seq) %>%
  left_join(covar_center) %>%
  mutate(value_z = (value - mu) / sd) %>%
  select(-(mu:max)) %>%
  filter(variable %in% pred_covar) %>%
  crossing(tibble(covar = coef_nms,
                  value = 0) %>%
             spread(covar, value)) %>%
  # mutate_at(vars(ANNDist),
  #           list(exp)) %>%
  gather(covar, covar_value, -variable, -value, -value_z) %>%
  mutate(covar_value = if_else(variable == covar,
                               value_z,
                               covar_value)) %>%
  spread(covar, covar_value) %>%
  mutate(pred = predict(mod,
                          newdata = .,
                          re.form = ~ 0,
                          type = 'response'))

pred_df %>%
  mutate(variable = recode(variable,
                           'redd_dens' = 'Redd Density')) %>%
  mutate(variable = recode(variable,
                           'ANNDist_log' = 'ANNDist')) %>%
  mutate_at(vars(value),
            list(~ if_else(variable == 'ANNDist',
                           exp(.), .))) %>%
  # select(variable:value_z, pred) %>%
  ggplot(aes(x = value,
             y = pred)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1,
             linetype = 2,
             color = 'darkgray') +
  facet_wrap(~ variable,
             scales = 'free_x') +
  labs(x = 'Covariate',
       y = 'Predicted Net Error Rate',
       title = 'Air Surveys')




air_list = pred_df %>%
  mutate(variable = recode(variable,
                           'redd_dens' = 'Redd Density')) %>%
  mutate(variable = recode(variable,
                           'ANNDist_log' = 'ANNDist')) %>%
  mutate_at(vars(value),
            list(~ if_else(variable == 'ANNDist',
                           exp(.), .))) %>%
  split(list(.$variable)) %>%
  map(.f = function(z) {
    z %>%
      ggplot(aes(x = value,
                 y = pred)) +
      geom_line(size = 1) +
      geom_hline(yintercept = 1,
                 linetype = 2,
                 color = 'darkgray') +
      facet_wrap(~ variable,
                 scales = 'free_x') +
      theme(axis.title.x = element_blank()) +
      labs(y = 'Predicted Net Error Rate')
  })

air_list[[1]]

pdf('figures/validation/Covariate_Effects_Air.pdf',
    width = 5,
    height = 5)
for(i in 1:length(air_list)) {
  plot(air_list[[i]])
}
dev.off()

for(i in 1:length(air_list)) {
  ggsave(paste0('figures/validation/Covariate_Effects_Air_', i, '.png'),
         plot = air_list[[i]],
         width = 5,
         height = 5)
}

#----------------------------------------------------------------
# plots for ground surveys
#----------------------------------------------------------------
# pull out the model for air surveys
mod = mod_sel %>%
  ungroup() %>%
  filter(type == 'full',
         Survey == 'Ground',
         Resp == 'Net') %>%
  pull(model) %>%
  extract2(1)

pred_seq = seq(-2, 2, by = 0.05)

coef_nms = names(fixef(mod))[-1]
coef_nms = coef_nms[!grepl('^I\\(', coef_nms)]
coef_nms = coef_nms[!grepl('\\:', coef_nms)]
coef_nms = coef_nms[!grepl('Experience', coef_nms)]

pred_covar = c('AveOverlap',
               'ANNDist_log',
               'redd_dens')

pred_df = covar_center %>%
  group_by(variable) %>%
  nest() %>%
  mutate(pred_seq = map(data,
                        .f = function(x) {
                          tibble(value = seq(x$min,
                                             x$max,
                                             length.out = 200))
                        })) %>%
  select(-data) %>%
  unnest(cols = pred_seq) %>%
  left_join(covar_center) %>%
  mutate(value_z = (value - mu) / sd) %>%
  select(-(mu:max)) %>%
  filter(variable %in% pred_covar) %>%
  crossing(tibble(covar = coef_nms,
                  value = 0) %>%
             spread(covar, value)) %>%
  # mutate_at(vars(ANNDist),
  #           list(exp)) %>%
  gather(covar, covar_value, -variable, -value, -value_z) %>%
  mutate(covar_value = if_else(variable == covar,
                               value_z,
                               as.numeric(covar_value))) %>%
  spread(covar, covar_value) %>%
  crossing(ExperienceCat = levels(mod_data$ExperienceCat)) %>%
  # mutate(ExperienceCat = "B") %>%
  bind_rows(tibble(variable = "ExperienceCat",
                   value = levels(mod_data$ExperienceCat)) %>%
              crossing(tibble(covar = coef_nms,
                              value = 0) %>%
                         spread(covar, value)) %>%
              mutate(covar = variable,
                     covar_value = value) %>%
              spread(covar, covar_value) %>%
              select(-value)) %>%
  mutate(pred = predict(mod,
                        newdata = .,
                        re.form = ~ 0,
                        type = 'response'))

pred_df %>%
  mutate(variable = recode(variable,
                           'redd_dens' = 'Redd Density')) %>%
  mutate(variable = recode(variable,
                           'ANNDist_log' = 'ANNDist')) %>%
  mutate_at(vars(value),
            list(~ if_else(variable == 'ANNDist',
                           exp(.), .))) %>%
  ggplot(aes(x = value,
             y = pred,
             color = ExperienceCat)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1,
             linetype = 2,
             color = 'gray') +
  scale_color_brewer(palette = 'Set1',
                     name = 'Experience') +
  facet_wrap(~ variable,
             scales = 'free_x') +
  labs(x = 'Covariate',
       y = 'Predicted Net Error Rate',
       title = 'Ground Surveys')




grnd_list = pred_df %>%
  mutate(variable = recode(variable,
                           'redd_dens' = 'Redd Density')) %>%
  mutate(variable = recode(variable,
                           'ANNDist_log' = 'ANNDist')) %>%
  mutate_at(vars(value),
            list(~ if_else(variable == 'ANNDist',
                           exp(.), .))) %>%
  split(list(.$variable)) %>%
  map(.f = function(z) {
    if(unique(z$variable) == 'ExperienceCat') {
      z %>%
        mutate_at(vars(value, value_z),
                  list(~ ExperienceCat)) %>%
        mutate(value = recode(value,
                              'A' = 'Novice',
                              'B' = 'Pro',
                              'C' = 'Veteran'),
               variable = 'Experience') %>%
        ggplot(aes(x = value,
                   y = pred,
                   ymax = pred,
                   ymin = pred,
                   color = ExperienceCat)) +
        geom_crossbar(size = 1) +
        geom_hline(yintercept = 1,
                   linetype = 2,
                   color = 'gray') +
        scale_color_brewer(palette = 'Set1',
                          name = 'Experience',
                          guide = "none") +
        facet_wrap(~ variable,
                   scales = 'free_x') +
        labs(x = 'Covariate',
             y = 'Predicted Net Error Rate')
    } else {
      z %>%
        mutate(ExperienceCat = recode(ExperienceCat,
                                      'A' = 'Novice',
                                      'B' = 'Pro',
                                      'C' = 'Veteran')) %>%
               ggplot(aes(x = value,
                          y = pred,
                          color = ExperienceCat)) +
                 geom_line(size = 1) +
                 geom_hline(yintercept = 1,
                   linetype = 2,
                   color = 'gray') +
        scale_color_brewer(palette = 'Set1',
                           name = 'Experience') +
        facet_wrap(~ variable,
                   scales = 'free_x') +
        labs(x = 'Covariate',
             y = 'Predicted Net Error Rate')
    }
  })

grnd_list[[1]]

mod_data %>%
  ggplot(aes(x = ExperienceCat,
             y = NetError)) +
  geom_boxplot()


pdf('analysis/figures/validation/Covariate_Effects_Grnd.pdf',
    width = 5,
    height = 5)
for(i in 1:length(grnd_list)) {
  plot(grnd_list[[i]])
}
dev.off()

for(i in 1:length(grnd_list)) {
  ggsave(paste0('analysis/figures/validation/Covariate_Effects_Grnd_', i, '.png'),
         plot = grnd_list[[i]],
         width = 5,
         height = 5)
}

