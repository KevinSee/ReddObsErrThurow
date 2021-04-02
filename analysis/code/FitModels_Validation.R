# Author: Kevin See
# Purpose: Fit a variety of models for redd validation study
# Created: 4/29/2016
# Last Modified: 3/2/2020
# Notes: This is a collaboration with Claire McGrath

#----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(lme4)
library(MuMIn)
library(caret)

theme_set(theme_bw())

#----------------------------------------------------------------
# read in data
raw_data = read_csv('analysis/data/raw_data/reach_data.csv') %>%
  select(-X1) %>%
  mutate(id = 1:n()) %>%
  select(Year, Reach, Survey, id, everything()) %>%
  mutate_at(vars(Year, Reach, Survey, Surveyor, ExperienceCat),
            list(as.factor)) %>%
  mutate(Experience3 = ordered(Experience3)) %>%
  mutate(ExperienceCat = recode(Experience3,
                                "0" = "A",
                                "1" = "B",
                                "2" = "C")) %>%
  mutate(ANNDist_log = log(ANNDist)) %>%
  mutate(obs_cnt = redd_correct + CommitReachCt,
         OmisRate = OmitReachCt / TrueReachCt,
         CommisRate = CommitReachCt / obs_cnt,
         # NetError = (CommitReachCt - OmitReachCt) / TrueReachCt,
         NetError = obs_cnt / TrueReachCt,
         log_NetError = log(NetError))

#----------------------------------------------------------------
# model for errors of ommision (what proportion of redds were missed by surveyor?)
# Ground surveys
#----------------------------------------------------------------
# pull in possible model covariates
all_mod_specs = read_csv('analysis/data/raw_data/V_A1_specifications.csv') %>%
  mutate(Resp = 'Omi') %>%
  bind_rows(read_csv('analysis/data/raw_data/V_A2_specifications.csv') %>%
              mutate(Resp = 'Com')) %>%
  bind_rows(read_csv('analysis/data/raw_data/V_A3_specifications.csv') %>%
              mutate(Resp = 'Net')) %>%
  select(EffectType, Resp, VarName, matches('^M')) %>%
  gather(Model, Incl, -(EffectType:VarName)) %>%
  mutate(VarName = recode(VarName,
                          'AveDepth*AveDepth' = 'I(AveDepth^2)',
                          'LYabund*PeakQ' = 'LYabund:PeakQ',
                          'ExperienceCat' = 'Experience3')) %>%
  filter(!is.na(Incl)) %>%
  mutate(Survey = 'Air') %>%
  bind_rows(read_csv('analysis/data/raw_data/V_G2_specifications.csv') %>%
              mutate(Resp = 'Omi') %>%
              bind_rows(read_csv('analysis/data/raw_data/V_G3_specifications.csv') %>%
                          mutate(Resp = 'Com')) %>%
              bind_rows(read_csv('analysis/data/raw_data/V_G4_specifications.csv') %>%
                          mutate(Resp = 'Net')) %>%
              select(EffectType, Resp, VarName, matches('^M')) %>%
              gather(Model, Incl, -(EffectType:VarName)) %>%
              mutate(VarName = recode(VarName,
                                      'AveDepth*AveDepth' = 'I(AveDepth^2)',
                                      'LYabund*PeakQ' = 'LYabund:PeakQ',
                                      'ExperienceCat' = 'Experience3')) %>%
              filter(!is.na(Incl)) %>%
              mutate(Survey = 'Ground')) %>%
  select(-EffectType) %>%
  spread(VarName, Incl) %>%
  arrange(Survey, Resp, Model) %>%
  mutate(Year = 1) %>%
  gather(VarName, Incl, -(Resp:Survey)) %>%
  filter(!is.na(Incl)) %>%
  mutate(EffectType = ifelse(VarName %in% c('Reach', 'Year', 'Surveyor'), 'Random', 'Fixed')) %>%
  mutate(Model = factor(Model, levels = paste0('M', 1:20))) %>%
  select(Survey, Resp, EffectType, Model, VarName, Incl) %>%
  arrange(Survey, Resp, EffectType, VarName, Model) %>%
  # can't have surveyor as random effect for air surveys, only one surveyor
  filter(!(Survey == 'Air' & VarName == 'Surveyor')) %>%
  # rename log of ANNDist variable
  mutate(VarName = recode(VarName,
                          'log(ANNDist)' = 'ANNDist_log')) %>%
  # rename experience variable
  mutate(VarName = recode(VarName,
                          'Experience3' = 'ExperienceCat'))

# # reproduce last version (delete redd_dens_obs)
# all_mod_specs %<>%
#   filter(VarName != 'redd_dens_obs',
#          Model != "M21")

# do any models have both observed redd density and escapement est?
all_mod_specs %>%
  group_by(Survey, Resp, Model) %>%
  filter(VarName %in% c('EscapeEst', 'redd_dens_obs')) %>%
  summarise(n_vars = n_distinct(VarName)) %>%
  filter(n_vars > 1)

# how correlated are those covariates?
raw_data %>%
  select(Year, Survey, EscapeEst, redd_dens_obs) %>%
  distinct() %>%
  # group_by(Year, Survey) %>%
  # filter(redd_dens_obs == max(redd_dens_obs)) %>%
  ggplot(aes(x = as_factor(EscapeEst),
             y = redd_dens_obs)) +
  geom_boxplot() +
  facet_wrap(~ Survey) +
  geom_smooth()

raw_data %>%
  group_by(Survey) %>%
  summarise(cor = cor(EscapeEst, redd_dens_obs))

#------------------------------
# get means and standard deviations of each covariate
covar_center = raw_data %>%
  select(one_of(unique(all_mod_specs$VarName[all_mod_specs$EffectType == 'Fixed'])),
         ANNDist,
         -starts_with("Experience")) %>%
  gather(variable, value) %>%
  group_by(variable) %>%
  summarise_at(vars(value),
               list(mu = mean,
                    sd = sd,
                    min = min,
                    max = max)) %>%
  ungroup()

# create model dataframe, z-scoring all covariates
mod_data = raw_data %>%
  gather(variable, value, one_of(covar_center$variable)) %>%
  left_join(covar_center) %>%
  mutate(value = (value - mu) / sd) %>%
  select(-(mu:max)) %>%
  spread(variable, value)

# fit all the various models
set.seed(8)
mod_fits = all_mod_specs %>%
  select(-Incl) %>%
  nest(covars = c(EffectType, VarName)) %>%
  arrange(Survey, Resp, Model) %>%
  mutate(n_fix = map_dbl(covars,
                         .f = function(x) {
                           x %>%
                             filter(EffectType == 'Fixed') %>%
                             nrow()
                         }),
         n_ran = map_dbl(covars,
                         .f = function(x) {
                           x %>%
                             filter(EffectType == 'Random') %>%
                             nrow()
                         })) %>%
  mutate(mod_form = map2(Resp,
                         covars,
                         .f = function(x, y) {
                           resp = if_else(x == 'Com',
                                          "cbind(CommitReachCt, redd_correct)",
                                          if_else(x == "Omi",
                                                  "cbind(TrueReachCt - redd_correct, redd_correct)",
                                                  "NetError"))

                           fix_eff = y %>%
                             filter(EffectType == 'Fixed') %>%
                             pull(VarName) %>%
                             paste(collapse = ' + ')

                           rand_eff = y %>%
                             filter(EffectType == 'Random') %>%
                             mutate(var_form = paste("(1 |", VarName, ")")) %>%
                             pull(var_form) %>%
                             paste(collapse = ' + ')


                           mod_form = if_else(fix_eff == "",
                                              paste(resp, "~ +", rand_eff),
                                              paste(resp, "~ +", rand_eff, "+", fix_eff)) %>%
                             as.formula()

                           return(mod_form)
                         })) %>%
  left_join(mod_data %>%
              group_by(Survey) %>%
              nest()) %>%
  mutate(fit = map2(mod_form,
                    data,
                    .f = function(x, y) {
                      if(grepl("NetError", all.vars(x)[1])) {
                        # mod = try(lmer(x,
                        #                data = y))
                        mod = try(glmer(x,
                                        data = y,
                                        family = gaussian(link = 'log')))
                      }else{
                        mod = try(glmer(x,
                                        data = y,
                                        family = "binomial"))
                      }

                      return(mod)
                    })) %>%
  mutate(AICc = map_dbl(fit,
                        .f = AICc)) %>%
  mutate(r2 = map2(mod_form,
                   data,
                   .f = function(x, y) {
                     if(grepl("NetError", all.vars(x)[1])) {
                       # mod = try(lmer(x,
                       #                data = y))
                       mod = try(glmer(x,
                                       data = y,
                                       family = gaussian(link = 'log')))
                     }else{
                       mod = try(glmer(x,
                                       data = y,
                                       family = "binomial"))
                     }

                     r2_df = r.squaredGLMM(mod)
                     if(is.null(rownames(r2_df))) rownames(r2_df) = c('delta')

                     r2_df %>%
                       as_tibble(rownames = 'type') %>%
                       filter(type == 'delta') %>%
                       select(-type) %>%
                       return()
                   })) %>%
  group_by(Survey, Resp) %>%
  mutate(delta = AICc - min(AICc)) %>%
  arrange(Survey, Resp, delta) %>%
  ungroup()

# pull out some model selection stuff: best model, naive model, full model, model averaging
mod_sel = mod_fits %>%
  group_by(Survey, Resp) %>%
  summarise(full = fit[n_fix == max(n_fix)],
            naive = fit[n_fix == 0],
            best = fit[delta == 0]) %>%
  left_join(mod_fits %>%
              select(Survey, Resp, fit) %>%
              nest(mod_list = c(fit)) %>%
              mutate(avg = map(mod_list,
                               .f = function(x) {
                                 model.avg(unlist(x))
                               })) %>%
              select(-mod_list)) %>%
  gather(type, model, -Survey, -Resp) %>%
  arrange(Survey, Resp, type) %>%
  left_join(mod_fits %>%
              group_by(Survey, Resp) %>%
              summarise(full = r2[n_fix == max(n_fix)],
                        naive = r2[n_fix == 0],
                        best = r2[delta == 0]) %>%
              gather(type, r2, -Survey, -Resp) %>%
              arrange(Survey, Resp, type) %>%
              unnest(cols = r2))


# mod_fits %>%
#   group_by(Survey, Resp) %>%
#   summarise(full = r2[n_fix == max(n_fix)]) %>%
#   unnest(cols = full) %>%
#   rename(full_R2m = R2m,
#          full_R2c = R2c) %>%
#   left_join(mod_fits %>%
#               group_by(Survey, Resp) %>%
#               summarise(naive = r2[n_fix == 0]) %>%
#               unnest(cols = naive) %>%
#               rename(naive_R2m = R2m,
#                      naive_R2c = R2c)) %>%
#   left_join(mod_fits %>%
#               group_by(Survey, Resp) %>%
#               summarise(best = r2[delta == 0]) %>%
#               unnest(cols = best) %>%
#               rename(best_R2m = R2m,
#                      best_R2c = R2c))

#----------------------------------------------------------------
# leave-one-out cross validation
# leave out each year one at a time
#----------------------------------------------------------------
# set.seed(6)
# loocv_df = mod_data %>%
#   # group_by(Survey, Year) %>%
#   group_by(Survey, Year, id) %>%
#   # group_by(Survey, Year, Surveyor) %>%
#   nest() %>%
#   rename(loocv = data) %>%
#   mutate(loocv = map2(loocv,
#                       Year,
#                       .f = function(x, y) {
#                         x %>%
#                           mutate(Year = y) %>%
#                           select(Year, everything())
#                       })) %>%
#   left_join(mod_data %>%
#               group_by(Survey) %>%
#               nest()) %>%
#   mutate(data = map2(data,
#                      loocv,
#                      .f = anti_join)) %>%
#   full_join(mod_sel %>%
#               filter(type %in% c('best', 'naive')) %>%
#               select(-starts_with("R2")) %>%
#               mutate(form = map(model,
#                                 .f = formula)) %>%
#               select(-model) %>%
#               rename(mod_type = type)) %>%
#   mutate(fit = map2(form,
#                     data,
#                     .f = function(x, y) {
#                       fam = if_else(all.vars(x)[1] == 'log_NetError',
#                                     "gaussian",
#                                     "binomial")
#
#                       if(fam == 'gaussian') {
#                         mod = try(lmer(x,
#                                        data = y))
#                       }else{
#                         mod = try(glmer(x,
#                                         data = y,
#                                         family = fam))
#                       }
#                       return(mod)
#                     })) %>%
#   mutate(obs_rate = map2(loocv, Resp,
#                          .f = function(x, y) {
#                            col_nm = if_else(y == 'Com',
#                                             "CommisRate",
#                                             if_else(y == 'Omi',
#                                                     "OmisRate",
#                                                     "log_NetError"))
#
#                            x %>%
#                              pull(col_nm) %>%
#                              return()
#                          })) %>%
#   mutate(pred_rate = map2(fit,
#                           loocv,
#                           .f = function(x, y) {
#                             predict(x,
#                                     newdata = y,
#                                     type = 'response',
#                                     allow.new.levels = T)
#                           }))



# how many cross validation folds (training datasets) shall we make?
n_folds = 5
set.seed(6)
loocv_df = mod_data %>%
  group_by(Survey) %>%
  nest() %>%
  mutate(test_rows = map(data,
                         .f = function(x) {
                           createFolds(x$id,
                                       k = n_folds)
                         })) %>%
  unnest(cols = test_rows) %>%
  mutate(loocv = map2(data,
                      test_rows,
                      .f = function(x, y) {
                        x %>%
                          slice(y)
                      }),
         data = map2(data,
                     test_rows,
                     .f = function(x, y) {
                       x %>%
                         slice(-y)
                     })) %>%
  full_join(mod_sel %>%
              filter(type %in% c('best', 'naive')) %>%
              select(-starts_with("R2")) %>%
              mutate(form = map(model,
                                .f = formula)) %>%
              select(-model) %>%
              rename(mod_type = type)) %>%
  mutate(fit = map2(form,
                    data,
                    .f = function(x, y) {
                      fam = if_else(grepl("NetError", all.vars(x)[1]),
                                    "gaussian",
                                    "binomial")

                      if(fam == 'gaussian') {
                        # mod = try(lmer(x,
                        #                data = y))
                        mod = try(glmer(x,
                                        data = y,
                                        family = gaussian(link = 'log')))
                      }else{
                        mod = try(glmer(x,
                                        data = y,
                                        family = fam))
                      }
                      return(mod)
                    })) %>%
  mutate(obs_rate = map2(loocv, Resp,
                         .f = function(x, y) {
                           col_nm = if_else(y == 'Com',
                                            "CommisRate",
                                            if_else(y == 'Omi',
                                                    "OmisRate",
                                                    "NetError"))

                           x %>%
                             pull(col_nm) %>%
                             return()
                         })) %>%
  mutate(pred_rate = map2(fit,
                          loocv,
                          .f = function(x, y) {
                            predict(x,
                                    newdata = y,
                                    type = 'response',
                                    allow.new.levels = T)
                          }))



#----------------------------------------------------------------
# save some stuff
#----------------------------------------------------------------
save(raw_data,
     mod_data,
     all_mod_specs,
     covar_center,
     mod_fits,
     mod_sel,
     n_folds,
     loocv_df,
     file = 'analysis/modelResults/fits_validation.rda')
