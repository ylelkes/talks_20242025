library(tidyverse)

d <- read.csv("~/Dropbox/DIAD - Study 1 - Data - Anonymized.csv")
d %>% mutate(infeels = case_when(gen_ps ==2|ind_ps==3~SNPA_Dem_1,
                                 gen_ps==1|ind_ps==1~SNPA_Rep_1,
                                 TRUE~NA),
             outfeels = case_when(gen_ps ==2|ind_ps==3~SNPA_Rep_1,
                                 gen_ps==1|ind_ps==1~SNPA_Dem_1,
                                 TRUE~NA)) -> d

lm(d$BPS_1~d$infeels) -> lm1
cor(data.frame(d$BPS_1,d$infeels),use="complete.obs") -> cor1
ggplot(d,aes(y=BPS_1,x=infeels)) + geom_smooth()+theme_bw() + labs(title="Feelings towards the outparty and the BPS") + labs(x="Feelings towards the outparty",y="BPS") + scale_color_viridis_d() -> a
library(ggpubr)
ggplot(d,aes(y=BNPA_1,x=outfeels)) + geom_smooth()+theme_bw() + labs(title="Feelings towards the outparty and allocations") + labs(x="Feelings towards the outparty",y="Cents allocated") + scale_color_viridis_d() +   stat_regline_equation(label.x = 3, label.y = 32, aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"))) -> b


cor(data.frame(d$BNPA_1,d$infeels),use="complete.obs") -> cor1
hist(d$BNPA_1)
