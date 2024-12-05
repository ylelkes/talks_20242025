library(tidyverse)
anes <- rio::import("~/Downloads/anes_timeseries_cdf_spss_20220916/anes_timeseries_cdf_spss_20220916.sav")
anes %>% group_by(VCF0004) %>% count(VCF0001)
anes$demrep <- as.factor(car::recode(as.numeric(anes$VCF0301),"1:3='Democrat';5:7='Republican';else=NA"))
anes$VCF0218[anes$VCF0218>97]=NA
anes$VCF0224[anes$VCF0224>97]=NA
anes$outaffect <- NA
anes$outaffect[which(anes$demrep=="Republican")] <- anes$VCF0218[which(anes$demrep=="Republican")]
anes$outaffect[which(anes$demrep=="Democrat")] <- anes$VCF0224[which(anes$demrep=="Democrat")]
anes$inaffect <- NA
anes$inaffect[which(anes$demrep=="Democrat")] <- anes$VCF0218[which(anes$demrep=="Democrat")]
anes$inaffect[which(anes$demrep=="Republican")] <- anes$VCF0224[which(anes$demrep=="Republican")]
anes$inaffect[anes$inaffect>97]=NA
anes$outaffect[anes$outaffect>97]=NA
anes$affpol <- (anes$inaffect-anes$outaffect)
anes$year <- anes$VCF0004
library(ggrepel)
library(directlabels)
# Transform the dataset
anes_long <- anes %>%
  pivot_longer(cols = c(outaffect, inaffect)) %>%
  drop_na(value) %>%
  mutate(name = case_when(
    name == "outaffect" ~ 'Outparty Affect',
    name == "inaffect" ~ 'Inparty Affect'
  ))

# Create the plot
ggplot(anes_long, aes(x = year, y = value, color = name)) +
  stat_summary(fun = mean, geom = "line",size=2) +
  stat_summary(fun = mean, geom = "point",cex=5,shape=10) +
  theme_bw(base_size = 20) +
  ylab("Feelings") +
  xlab("Year") + labs(title = "Feelings towards the inparty and outparty over time") + labs(color="Target Party") + scale_color_viridis_d() -> a
  ggsave(a,file="figures/affpol.png",width=15,height=10)
