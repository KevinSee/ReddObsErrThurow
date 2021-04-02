# Author: Kevin See
# Purpose: Exploratory data analysis for errors of ommission, commission, and net error for redd survey study
# Created: 4/21/2016
# Last Modified: 4/22/2016
# Notes: This is a collaboration with Claire McGrath

#----------------------------------------------------------------
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lme4)
library(MuMIn)
library(ggplot2)
library(GGally)

theme_set(theme_bw())
#----------------------------------------------------------------
# read in data
raw_data = read_csv('Data/reach_data.csv') %>%
  mutate_each(funs(as.factor(.)), Year, Reach, Survey, Surveyor, ExperienceCat) %>%
  mutate(ExperienceCat = ordered(ExperienceCat, levels = c('B', 'A', 'C')),
         Experience3 = ordered(Experience3)) %>%
  mutate(obs_cnt = redd_correct + CommitReachCt,
         OmisRate = OmitReachCt / TrueReachCt,
         CommisRate = CommitReachCt / obs_cnt)

xtabs(~ ExperienceCat + Experience3 + Survey, raw_data)

# pull out air surveys
air_data = filter(raw_data,
                  Survey == 'Air')
# pull out ground surveys
grnd_data = filter(raw_data,
                   Survey == 'Ground')

#----------------------------------------------------------------
# Make some plots
#----------------------------------------------------------------

ggplot(raw_data,
       aes(x = TrueReachCt,
           y = obs_cnt)) +
  geom_point(aes(color = Year),
             size = 5) +
  scale_color_brewer(palette = 'Set1') +
  geom_abline(slope = 1,
              intercept = 0,
              color = 'black',
              linetype = 2) +
  facet_wrap(~ Survey) +
  labs(x = 'True # of Redds',
       y = 'Observed # of Redds')


ggplot(raw_data,
       aes(x = TrueReachCt,
           y = obs_cnt)) +
  geom_point(aes(color = log(frxn_true)),
             size = 5) +
  scale_color_gradient2(low = 'red', high = 'blue', midpoint = 0) +
  theme_gray() +
  geom_abline(slope = 1,
              intercept = 0,
              color = 'black',
              linetype = 2) +
  facet_wrap(~ Survey) +
  labs(x = 'True # of Redds',
       y = 'Observed # of Redds')

ggplot(raw_data,
       aes(x = OmisRate)) +
  geom_density(aes(color = Year,
                   fill = Year),
               alpha = 0.2) +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~ Survey)

ggplot(raw_data,
       aes(x = frxn_true)) +
  geom_density(aes(color = Year,
                   fill = Year),
               alpha = 0.2) +
  scale_color_brewer(palette = 'Set1') +
  scale_fill_brewer(palette = 'Set1') +
  facet_wrap(~ Survey)


mod_data_all %>%
  select(one_of(fix_covar_num)) %>%
  ggpairs()

ggplot(raw_data,
       aes(x = Year,
           y = OmisRate)) +
  geom_boxplot() +
  facet_wrap(~ Survey)

ggplot(raw_data,
       aes(x = Experience3,
           y = OmisRate)) +
  geom_boxplot() +
  facet_wrap(~ Survey)


ggplot(raw_data,
       aes(x = Year,
           y = CommisRate)) +
  geom_boxplot() +
  facet_wrap(~ Survey)

ggplot(raw_data,
       aes(x = TrueReachCt / Km,
           y = CommisRate)) +
  geom_point() +
  geom_smooth(method = lm) + 
  geom_smooth(method = lm, 
              formula = y ~ exp(-x),
              color = 'red',
              fill = 'pink') + 
  facet_wrap(~ Survey)

ggplot(raw_data,
       aes(x = TrueReachCt / Km,
           y = OmisRate)) +
  geom_point() +
  geom_smooth(method = lm) + 
  facet_wrap(~ Survey)



ggplot(raw_data,
       aes(x = Experience3,
           y = CommisRate)) +
  geom_boxplot() +
  facet_wrap(~ Survey)

ggplot(raw_data,
       aes(x = Year,
           y = frxn_true)) +
  geom_boxplot() +
  geom_hline(yintercept = 1,
             linetype = 2,
             color = 2) +
  facet_wrap(~ Survey) +
  labs(y = 'Net Error (Obs Redds / True Count)')

ggplot(raw_data,
       aes(x = Reach,
           y = frxn_true)) +
  geom_boxplot() +
  geom_hline(yintercept = 1,
             linetype = 2,
             color = 2) +
  facet_wrap(~ Survey) +
  labs(y = 'Net Error (Obs Redds / True Count)')

raw_data %>%
  filter(Survey == 'Ground') %>%
  ggplot(aes(x = Experience3,
             y = frxn_true)) +
  geom_boxplot() +
  geom_hline(yintercept = 1,
             linetype = 2,
             color = 2) +
  facet_wrap(~ Survey)


raw_data %>%
  select(Year:TrueReachCt, OmisRate, CommisRate) %>%
  gather(ErrType, value, -(Year:TrueReachCt)) %>%
  ggplot(aes(x = TrueReachCt,
             y = value)) +
  geom_point() +
  geom_smooth() +
  facet_grid(ErrType ~ Survey)


air_data %>%
  ggplot(aes(x = AveSunny,
             y = log(frxn_true))) +
  geom_point(aes(color = Reach)) +
  geom_smooth(method = lm)

grnd_data %>%
  ggplot(aes(x = Experience3,
             y = frxn_true)) +
  geom_boxplot()

grnd_data %>%
  ggplot(aes(x = Reach,
             y = log(frxn_true))) +
  geom_boxplot(aes(fill = Experience3)) +
  geom_hline(yintercept = 0,
             linetype = 2,
             color = 'darkblue')



raw_data %>%
  select(Year:Surveyor, OmisRate, CommisRate, NetError = frxn_true) %>%
  gather(error, value, -(Year:Surveyor)) %>%
  ggplot(aes(x = Year,
             y = value)) +
  geom_boxplot() +
  facet_grid(error ~ Survey, scales = 'free_y')

raw_data %>%
  select(Year:Surveyor, OmisRate, CommisRate, NetError = frxn_true) %>%
  gather(error, value, -(Year:Surveyor)) %>%
  mutate(error = revalue(error,
                         c('OmisRate' = 'Omission',
                           'CommisRate' = 'Commission',
                           'NetError' = 'Net Error'))) %>%
  ggplot(aes(x = Survey,
             y = value)) +
  geom_boxplot() +
  facet_wrap(~ error, scales = 'free_y') +
  labs(y = 'Rate')