library(anesr)
library(tidyverse)
data("timeseries_2012")
timeseries_2012$iwrdesc_pre_pid_demstr
timeseries_2012 <- timeseries_2012 %>% mutate(
  iwr_pid7 = case_when(
    iwrdesc_pre_pid_repstr == 1 ~ 7,
    iwrdesc_pre_pid_repstr ==
      2 ~ 6,
    iwrdesc_pre_pid_lean %in% c(1) ~ 5,
    iwrdesc_pre_pid_lean %in% c(3) ~ 4,
    iwrdesc_pre_pid_lean %in% c(2) ~ 3 ,

    iwrdesc_pre_pid_demstr == 1 ~ 2,
    iwrdesc_pre_pid_demstr ==
      2 ~ 1,
  ),
  iwr_pid3 = case_when(
    iwr_pid7 %in% c(1, 2, 3) ~ 'Democrat',
    iwr_pid7 %in% c(5, 6, 7) ~ 'Republican',
    iwr_pid7 %in% c(4) ~ 'Independent'
  ),
  iwr_pid7post = case_when(
    iwrdesc_post_pid_repstr == 1 ~ 7,
    iwrdesc_post_pid_repstr ==
      2 ~ 6,
    iwrdesc_post_pid_lean %in% c(1) ~ 5,
    iwrdesc_post_pid_lean %in% c(3) ~ 4,
    iwrdesc_post_pid_lean %in% c(2) ~ 3 ,

    iwrdesc_post_pid_demstr ==
      1 ~ 2,
    iwrdesc_post_pid_demstr ==
      2 ~ 1,
  ),
  iwr_pid3post = case_when(
    iwr_pid7post %in% c(1, 2, 3) ~ 'Democrat',
    iwr_pid7post %in% c(5, 6, 7) ~ 'Republican',
    iwr_pid7post %in% c(4) ~ 'Independent',
    TRUE ~ NA_character_
  ),
  iwr_race = case_when(iwrdesc_pre_race_white==1~'White',
                       iwrdesc_pre_race_black==1~'Black',
                       iwrdesc_pre_hisp==1~'Hispanic',
                       TRUE~'Other'),
  iwr_race_post = case_when(iwrdesc_post_race_white==1~'White',
                       iwrdesc_post_race_black==1~'Black',
                       iwrdesc_post_hisp==1~'Hispanic',
                       TRUE~'Other'),
  iwr_gender = case_when(iwrdesc_pre_gender==1~'Male',
                         iwrdesc_pre_gender==2 ~ 'Female',
                         TRUE~NA_character_),
  iwr_gender_post = case_when(iwrdesc_post_gender==1~'Male',
                         iwrdesc_post_gender==2 ~ 'Female',
                         TRUE~NA_character_),
  gender = case_when(gender_respondent_x==1~'Male',
                     gender_respondent_x==2~'Female',
                     TRUE~NA_character_),
  race = case_when(dem_raceeth_x==1~'White',
                   dem_raceeth_x==2~'Black',
                   dem_raceeth_x==5~'Hispanic',
                   TRUE~'Other'))








table(timeseries_2012$iwr_pid3,timeseries_2012$iwr_pid3post)
timeseries_2012$pid_x <- as.numeric(timeseries_2012$pid_x)
timeseries_2012$ft_dem <- as.numeric(timeseries_2012$ft_dem)
timeseries_2012$ft_rep <- as.numeric(timeseries_2012$ft_rep)
timeseries_2012$ft_rpc <- as.numeric(timeseries_2012$ft_rpc)
timeseries_2012$ft_dpc <- as.numeric(timeseries_2012$ft_dpc)
timeseries_2012$cses_dptylike <- as.numeric(timeseries_2012$cses_dptylike)
timeseries_2012$cses_rptylike <- as.numeric(timeseries_2012$cses_rptylike)
timeseries_2012$ft_rep[timeseries_2012$ft_rep <0 ]=NA
timeseries_2012$ft_dem[timeseries_2012$ft_dem <0 ]=NA

timeseries_2012$ft_rpc[timeseries_2012$ft_rpc <0 ]=NA
timeseries_2012$ft_dpc[timeseries_2012$ft_dpc <0 ]=NA


timeseries_2012$cses_dptylike[timeseries_2012$cses_dptylike<0]=NA
timeseries_2012$cses_rptylike[timeseries_2012$cses_rptylike<0]=NA

timeseries_2012 <- timeseries_2012 %>% mutate(infeels =case_when(pid_x %in% c(1:3) ~ ft_dem,
                                                                 pid_x %in% c(5:7) ~ ft_rep,
                                                                 TRUE ~ NA),
                                              outfeels =case_when(pid_x %in% c(1:3) ~ ft_rep,
                                                                  pid_x %in% c(5:7) ~ ft_dem,
                                                                  TRUE ~ NA),
                                              infeels_pc = case_when(pid_x %in% c(1:3) ~ ft_dpc,
                                                                     pid_x %in% c(5:7) ~ ft_rpc,
                                                                     TRUE ~ NA),
                                              outfeels_pc = case_when(pid_x %in% c(1:3) ~ ft_rpc,
                                                                      pid_x %in% c(5:7) ~ ft_dpc,
                                                                      TRUE ~ NA),
                                              indislike = case_when(pid_x %in% c(1:3) ~ cses_dptylike,
                                                                    pid_x %in% c(5:7) ~ cses_rptylike,
                                                                    TRUE ~ NA),
                                              outdislike = case_when(pid_x %in% c(1:3) ~ cses_rptylike,
                                                                     pid_x %in% c(5:7) ~ cses_dptylike,
                                                                     TRUE ~ NA))

timeseries_2012$pid_x[timeseries_2012$pid_x<0]=NA
timeseries_2012 <- timeseries_2012 %>% mutate(pid3 = case_when(pid_x %in% c(1:3) ~ 'Democrat',
                                                               pid_x %in% c(5:7) ~ 'Republican',
                                                               TRUE ~ 'Independent'))

timeseries_2012 <- timeseries_2012 %>% mutate(match_pid = pid3 == iwr_pid3,
                                              match_pid_post = pid3 == iwr_pid3post,
                                              match_race = race == iwr_race,
                                              match_race_post = race == iwr_race_post,
                                              match_gender = gender == iwr_gender,
                                              match_gender_post = gender ==iwr_gender_post)

timeseries_2012 <-timeseries_2012 %>% mutate(match_pid3 = case_when(pid3 == 'Democrat' & iwr_pid3 == 'Democrat' ~ 1,
                                                  pid3 == 'Republican' & iwr_pid3 == 'Republican' ~ 1,
                                                  pid3 == 'Independent' & iwr_pid3 == 'Independent' ~ 1,
                                                  pid3=='Republican' & iwr_pid3=='Democrat' ~ -1,
                                                  pid3=='Democrat' & iwr_pid3=='Republican' ~ -1,
                                                  pid3 != 'Independent' & iwr_pid3 == 'Independent' ~ 0))



timeseries_2012$infeels <- scales::rescale(timeseries_2012$infeels, to = c(0, 1))
timeseries_2012$outfeels <- scales::rescale(timeseries_2012$outfeels, to = c(0, 1))

timeseries_2012$infeels_pc <- scales::rescale(timeseries_2012$infeels_pc, to = c(0, 1))
timeseries_2012$outfeels_pc <- scales::rescale(timeseries_2012$outfeels_pc, to = c(0, 1))

timeseries_2012$outdislike <- scales::rescale(timeseries_2012$outdislike, to = c(0, 1))
timeseries_2012$indislike <- scales::rescale(timeseries_2012$indislike, to = c(0, 1))



timeseries_2012 %>% group_by(pid3,iwr_pid7) %>% summarise(demfeels_m= mean(ft_dem,na.rm=T),repfeels_m=mean(ft_rep,na.rm=T))  %>% drop_na() %>% pivot_longer(
    cols = -(1:2),
    names_to = c("var", "type"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>% drop_na() %>% pivot_wider(names_from = type, values_from = value) %>% mutate(dv='Party FTs') -> feels

timeseries_2012 %>% group_by(pid3,iwr_pid3=iwr_pid3) %>% summarise(infeels_mean= mean(infeels,na.rm=T),outfeels_mean=mean(outfeels_pc,na.rm=T)) %>%  drop_na() %>% pivot_longer(
  cols = -(1:2),
  names_to = c("var", "type"),
  names_pattern = "(.*)_(.*)",
  values_to = "value"
) %>% drop_na() %>%  pivot_wider(names_from = type, values_from = value) %>% mutate(dv='Candidate FTs') -> pc

range(timeseries_2012$outfeels,na.rm = T)
timeseries_2012$iwr_id_pre <- timeseries_2012$iwrdesc_pre_iwrid
timeseries_2012$iwr_id_post <- timeseries_2012$iwrdesc_post_iwrid

timeseries_2012$ft_black <- as.numeric(timeseries_2012$ft_black)

a <- fixest::feols(outfeels~as.factor(match_pid3),data=timeseries_2012,weights = timeseries_2012$weight_ftf )
timeseries_2012$iwr_pid7 <- factor(timeseries_2012$iwr_pid7,labels= c("Strong Democrat","Not very strong Democrat","Lean Democrat",'Independent',"Lean Republican","Not very strong Republican","Strong Republican"))
timeseries_2012$pid7 <- factor(timeseries_2012$pid_x,labels= c("Strong Democrat","Not very strong Democrat","Lean Democrat",'Independent',"Lean Republican","Not very strong Republican","Strong Republican"))
timeseries_2012$ftcasi_black[timeseries_2012$ftcasi_black<.]
timeseries_2012$iwr_pid3post
a <- fixest::feols(cses_dptylike~iwr_pid3post*pid3,data=timeseries_2012 )
dems <- plot_predictions(a,condition =  c("iwr_pid3post","pid3"))+facet_wrap(~pid3)
a <- fixest::feols(cses_rptylike~pid3*iwr_pid3post,data=timeseries_2012 )
reps <- plot_predictions(a,condition =  c("iwr_pid3post","pid3"))+facet_wrap(~pid3)

timeseries_2012 %>% group_by(iwr_pid7,pid3) %>% summarise(rep_ft= mean(ft_rep,na.rm=T),dem_ft=mean(ft_dem,na.rm=T)) %>% drop_na() %>% ggplot(aes(x=pid3,y=rep_ft,color=as.factor(iwr_pid7))) + geom_point() + geom_line() + labs(title = "ANES 2012",x="Interviewer PID",y="Feeling Thermometer",color='PID')
library(patchwork)
dems|reps
timeseries_2012$ftcasi_white[timeseries_2012$ftcasi_white<0]=NA
timeseries_2012$ftcasi_hisp[timeseries_2012$ftcasi_hisp<0]=NA
timeseries_2012$ftcasi_black[timeseries_2012$ftcasi_black<0]=NA

timeseries_2012 <- timeseries_2012 %>% mutate(out_race = case_when(race=='Black'~ftcasi_white,
                                                race=='White'~ftcasi_black,
                                                race=='Hispanic'~ftcasi_black),
                                              in_race = case_when(race=='Black'~ftcasi_black,
                                                                  race=='White'~ftcasi_white,
                                                                  race=='Hispanic'~ftcasi_hisp))

timeseries_2012$match_pid3 <- as.factor(timeseries_2012$match_pid3)
a <- fixest::feols(outfeels~match_pid+match_gender+match_race, fixef = "iwr_id_pre",data=timeseries_2012)

b <- fixest::feols(infeels~match_pid+match_gender+match_race, fixef = "iwr_id_pre",data=timeseries_2012)
summary(a)
c <- fixest::feols(outfeels_pc~match_pid+match_gender+match_race, fixef = "iwr_id_pre",data=timeseries_2012)
d <- fixest::feols(infeels_pc~match_pid+match_gender+match_race, fixef = "iwr_id_pre",data=timeseries_2012)
e <- fixest::feols(outdislike~match_pid_post+match_gender_post+match_race_post, fixef = "iwr_id_post",data=timeseries_2012)
f <- fixest::feols(indislike~match_pid_post+match_gender_post+match_race_post, fixef = "iwr_id_post",data=timeseries_2012)

timeseries_2012$pid_xf <- as.factor(timeseries_2012$pid_x)
timeseries_2012$iwr_p
e <- fixest::feols(outdislike~pid3*iwr_pid3post,data=timeseries_2012)
e
marginaleffects::plot_predictions(e,condition  = c("pid3","iwr_pid3post")) + theme_bw() + scale_color_manual(values = c("blue","green","red")) + labs(title = "Difference-in-Difference Estimates",x="R's PID",y="Outparty Like",color='IWR PID')
ggsave("figures/anes_fts_iwr.png",width=10,height=5)
marginaleffects::plot_comparisons(c,variables = c("match_pid"),by = "pid3")
%>% as_data_frame()  %>% ggplot(aes(x=pid3,y=estimate,color=match_pid))+ geom_pointrange(aes(ymin=conf.low,ymax=conf.high),position = position_dodge(.3))+  theme_minimal() + labs(title = "ANES 2012",x="R's PID",y="Feeling Thermometer",color='Matched PID')
names(a$coefficients) =c("Matched PID","Matched Gender","Matched Race")
names(b$coefficients) =c("Matched PID","Matched Gender","Matched Race")
names(c$coefficients) =c("Matched PID","Matched Gender","Matched Race")
names(d$coefficients) =c("Matched PID","Matched Gender","Matched Race")
names(e$coefficients) =c("Matched PID","Matched Gender","Matched Race")
names(f$coefficients) =c("Matched PID","Matched Gender","Matched Race")

a$fixef_vars = "IWR ID"
b$fixef_vars = "IWR ID"
c$fixef_vars = "IWR ID"
d$fixef_vars = "IWR ID"
e$fixef_vars = "IWR ID"
f$fixef_vars = "IWR ID"
library(modelsummary)
models <- list("Out Party FT"=a,"In Party FT"=b,"Out Cand"=c,"In Cand"=d,"Out Like"=e,"In Like"=f)
modelsummary(models, fmt=2,stars = TRUE, add_n = TRUE,gof_map = c("nobs","FE: IWR ID"),output = "figures/anes_fts_iwrs.md")
modelsummary(models)
