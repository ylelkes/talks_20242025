anes <- rio::import("data/anes_timeseries_2012_sav/anes_timeseries_2012.sav")
anes %>% mutate(iwr_pre_pid = case_when(iwrdesc_pre_pid_lean==1~'R',
                                    iwrdesc_pre_pid_lean==2~'D',
                                    iwrdesc_pre_pid_lean == 3~'I',
                                    iwrdesc_pre_pid==1~'D',
                                    iwrdesc_pre_pid==2~'R'),
                iwr_pre_pid7 = case_when(iwrdesc_pre_pid_repstr==1~1,
                                         iwrdesc_pre_pid_repstr==2~2,
                                         iwrdesc_pre_pid_lean==1~3,
                                         iwrdesc_pre_pid_lean==3~4,
                                         iwrdesc_pre_pid_lean==2~5,
                                         iwrdesc_pre_pid_demstr==2~6,
                                         iwrdesc_pre_pid_demstr==1~7),
                iwr_post_pid = case_when(iwrdesc_post_pid_lean==1~'R',
                                    iwrdesc_post_pid_lean==2~'D',
                                    iwrdesc_post_pid_lean==3~'I',
                                    iwrdesc_post_pid==1~'D',
                                    iwrdesc_post_pid==2~'R'),
                pid3 = case_when(pid_x %in% c(1:3)~'D',
                                 pid_x %in% c(5:7)~'R',
                                 pid_x %in% c(4,-2)~'I'),
                d_angry = case_when(candaff_angdpc==2 ~ 1,
                                    candaff_angdpcoft<0~NA,
                                    TRUE~6-candaff_angdpcoft),
                r_angry = case_when(candaff_angrpc==2 ~ 1,
                                    candaff_angrpcoft<0~NA,
                                    TRUE~6-candaff_angdpcoft)) -> anes

library(sjlabelled)
anes$ft_dem[anes$ft_dem<0] = NA
anes$ft_rep[anes$ft_rep<0] = NA
anes$ft_dpc[anes$ft_dpc<0] = NA
anes$ft_rpc[anes$ft_rpc<0] = NA

anes$cses_dptylike[anes$cses_dptylike<0] = NA
anes$cses_rptylike[anes$cses_rptylike<0] = NA
library(fixest)
anes$cses_dptylike
anes %>% mutate(infeels = case_when(pid3=='D'~ft_dem,
                                    pid3=='R'~ft_rep,
                                    TRUE~NA_real_),
                outfeels = case_when(pid3=='D'~ft_rep,
                                    pid3=='R'~ft_dem,
                                    TRUE~NA)) -> anes

anes$iwrobspre_gender
anes %>% mutate(iwrobspre_gender = case_when(iwrobspre_gender==1~'Male',
                                             iwrobspre_gender==2~'Female',
                                             TRUE~NA),
                iwr_pre_educgrp = case_when(iwrdesc_pre_educgrp==1~'HS',
                                                iwrdesc_pre_educgrp==2~'College',
                                                 TRUE~NA),
                iwr_pre_race = case_when(iwrdesc_pre_race_white==1 ~ 'White',
                                             iwrdesc_pre_race_black==1 ~ 'Black',
                                             iwrdesc_pre_hisp==1 ~ 'Hispanic',
                                             TRUE ~ 'Other'),
                rgender = case_when(gender_respondent_x==1~'Male',
                                    gender_respondent_x==2~'Female'),
                rrace = case_when(dem_raceeth_x==1~'White',
                                  dem_raceeth_x==2~'Black',
                                  dem_raceeth_x==5~'Hispanic',
                                  TRUE~'Other')) -> anes


anes$dem_raceeth_x
anes$race_match_pre <- anes$iwr_pre_race==anes$rrace
anes$gender_match_pre <- anes$iwrobspre_gender==anes$rgender
anes$pid_match_pre <- anes$iwr_pre_pid==anes$pid3
anes$ftgr_liberals

feols(infeels ~ pid_match_pre + race_match_pre+gender_match_pre,fixef ="iwrobspre_iwrid",anes)
feols(outfeels ~ pid_match_pre + race_match_pre+gender_match_pre,fixef ="iwrobspre_iwrid",anes)
anes$iwr_pre_pid
d <- feols(infeels ~ pid3*iwr_pre_pid, data = anes)
plot_predictions(d,condition =  c("pid3","iwr_pre_pid"))

summary(d)
anes$iwr
table(anes$iwr_pre_pid7,anes$iwr_pre_pid)
library(estimatr)
d <- (lm_lin(ft_dem ~ iwr_pre_pid, data = anes))
summary(d)
library(marginaleffects)
a %>% facet_wrap(~pid3)
plot_predictions(d,variables = "iwr_pre_pid",by = "pid3")
