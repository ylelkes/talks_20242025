df <- read.csv("data/Justifications_classified.csv")
library(tidyverse)
source("~/Dropbox/recode.r")

df <- recoder(df)

df  %>% pivot_longer(cols = c(Disagree_Premise,Institutional_Rot,Necessary_Evil,Partisanship_Positive,Partisanship_Negative,Blaming_Out_Party)) %>% mutate(name = str_replace_all(name,"_"," "), chosen_norm_question = str_to_title(str_replace(chosen_norm_question,"norm_",""))) -> norms_long
norms_long <- norms_long %>% mutate(vio_sum= rowSums(data.frame(violence2re,violence3re,violence4re,violence5re,violence6re)))

norms_long %>% filter(value==1) %>% group_by(chosen_norm_question,name) %>% tally() %>% mutate(percent=n/sum(n))%>% ggplot(aes(x=name,y=percent)) + geom_bar(stat = "identity") + theme_bw(base_size = 10) + labs(y = "Percent",x = "Justification",fill = "Norm Question") + theme(legend.position = "none")+facet_wrap(~chosen_norm_question,nrow=1) + geom_text(aes(label = scales::percent(percent,accuracy = 1)),vjust = -0.5)+scale_y_continuous(labels = scales::percent)+ scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+ theme(axis.text.x = element_text(angle = 90,vjust=0))
ggsave(plot = r_all,filename  = "figures/justifications_distributions.png",width = 10,height = 5,units = "in",dpi = 300)

norms_long %>% group_by(chosen_norm_question,name,value) %>% summarise(mean = mean(norm_total,na.rm=T),se=sd(norm_total,na.rm=T)/sqrt(n()),.groups = "drop" ) -> norms_means

norms_long %>% group_by(chosen_norm_question,name,value) %>% summarise(mean = mean(vio_sum,na.rm=T),se=sd(vio_sum,na.rm=T)/sqrt(n()),.groups = "drop" ) -> vio_means

library(ggfx)
r_all <- ggplot(norms_means %>% filter(value==1),aes(x = name,y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean-1.96*se,ymax = mean+1.96*se),width = 0.2) + facet_wrap(~chosen_norm_question,nrow=1) + theme_bw(base_size = 10) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + theme(axis.text.x = element_text(angle = 90,vjust=0)) + labs(y = "Mean",x = "Justification",title = "Number of Illiberal Attitudes by Justification") + theme(legend.position = "none")
ggsave(plot = r_all,filename  = "figures/norms_justifications_all1.png",width = 10,height = 5,units = "in",dpi = 300)
r_all$data %>% mutate(mean = ifelse(name == "Partisanship Positive",NA,mean)) -> r_all$data
ggsave(plot = r_all,filename  = "figures/norms_justifications_all0.png",width = 10,height = 5,units = "in",dpi = 300)

r_all <- ggplot(vio_means %>% filter(value==1),aes(x = name,y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean-1.96*se,ymax = mean+1.96*se),width = 0.2) + facet_wrap(~chosen_norm_question,nrow=1) + theme_bw(base_size = 10) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + theme(axis.text.x = element_text(angle = 90,vjust=0)) + labs(y = "Mean",x = "Justification",title = "Support for Violence by Justification") + theme(legend.position = "none")
r_all
ggsave(plot = r_all,filename  = "figures/violence_justifications_all1.png",width = 10,height = 5,units = "in",dpi = 300)

r_all$data %>% mutate(mean = ifelse(name == "Partisanship Positive",NA,mean)) -> r_all$data
ggsave(plot = r_all,filename  = "figures/violence_justifications_all0.png",width = 10,height = 5,units = "in",dpi = 300)

norms_long %>% group_by(chosen_norm_question,name,value) %>% summarise(mean = mean(outparty,na.rm=T),se=sd(outparty,na.rm=T)/sqrt(n()),.groups = "drop" ) -> vio_means

r_all <- ggplot(vio_means %>% filter(value==1),aes(x = name,y = mean)) + geom_bar(stat = "identity") + geom_errorbar(aes(ymin = mean-1.96*se,ymax = mean+1.96*se),width = 0.2) + facet_wrap(~chosen_norm_question,nrow=1) + theme_bw(base_size = 10) + scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) + theme(axis.text.x = element_text(angle = 90,vjust=0)) + labs(y = "Mean",x = "Justification",title = "Support for Violence by Justification") + theme(legend.position = "none")
