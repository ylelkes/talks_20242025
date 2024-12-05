
# Overwrite alpha function
alpha <- psych::alpha

# Load data
data2 <- readRDS("data/exp2.rds") %>% mutate(exp="Experiment 2")
data3 <- readRDS("data/exp3.rds") %>% mutate(exp="Experiment 3")

intersect(names(data2),names(data3))
data2 %>% rbind(data3) -> data

data2 %>% group_by(id) %>% count()
library(marginaleffects)


data <- data %>% mutate(fact = case_when(str_detect(fact_text, "executive orders") ~ "executive orders",
                                        str_detect(fact_text, "loyal to the") ~ "loyalty",
                                        str_detect(fact_text, "polling stations") ~ "polling stations",
                                        str_detect(fact_text, "court decisions") ~ "court decisions",
                                        str_detect(fact_text, "censor media") ~ "censor media"))


# Check scale reliabilities
data %>%
    select(matches("^norm_.*_post$")) %>%
    alpha()

# Re-scale some variables
data <- data %>%
    mutate(
        across(
            c(norms_post, norms_pre, violence_post, violence_pre),
            ~ . / max(.) * 100
        )
    )

#####################
### MANIP. CHECKS ###
#####################

# Run model
manip_model <- lm_robust(
    fact_post ~ fact_dose * fact_type * inparty_in_treatment * voter_or_politician +
        fact_pre,
    clusters = id,
    data = data
)

# Calculate slopes
## Pooled
exp2_manip_pool_slopes <- avg_slopes(
    manip_model,
    variables = "fact_dose"
) %>%
    as.data.frame() %>%
    mutate(dv = "fact_post", Study = "Experiment 2")

## Not pooled
exp2_manip_slopes <- avg_slopes(
    manip_model,
    by = c("fact_type", "inparty_in_treatment", "voter_or_politician"),
    variables = "fact_dose"
) %>%
    as.data.frame() %>%
    mutate(dv = "fact_post", Study = "Experiment 2")

data %>% group_by(inparty_in_treatment,fact_type) %>% summarise(mean(fact_post,na.rm = T))
library(fixest)
data$pid2
data$fact_stereo_dose <- as.factor(ntile(data$fact_stereo_dose,5))
feols(norms_post ~  1, data %>% filter(inparty_in_treatment==1))
r
summary(r)
table(data$pid2,data$)
data %>% select(starts_with("norm_") & ends_with("post")) %>% names()
[1] "norm_censorship_post" "norm_executive_post"  "norm_judges_post"
[4] "norm_loyalty_post"    "norm_polling_post"
table(data$pid7)
# Lean Democrat Lean Republican Not very strong Democrat Not very strong Republican Strong Democrat Strong Republican
data <- data %>% mutate(pidstrength =case_when(pid7 %in% c("Lean Democrat","Lean Republican") ~ 'Leaner',
                                            pid7 %in% c("Not very strong Republican","Not very strong Democrat") ~ 'Not very strong',
                                            pid7 %in% c("Strong Democrat","Strong Republican") ~ 'Strong'))
feols(norms_post ~norms_pre  + pidstrength:fact_dose:fact_type+fact_type,data = data %>% filter(inparty_in_treatment==0)) -> r
summary(r)
b <- feols(norm_loyalty_post ~norm_loyalty_pre  + voter_or_politician +fact:fact_stereo_dose:exp , data = data %>% filter(inparty_in_treatment==1))
c <- feols(norm_executive_post ~norm_executive_pre  + voter_or_politician +fact:fact_stereo_dose:exp , data = data %>% filter(inparty_in_treatment==1))
d <- feols(norm_judges_post ~norm_judges_pre  + voter_or_politician +fact:fact_stereo_dose:exp , data = data %>% filter(inparty_in_treatment==1))

feols(norm_cens ~norm_loyalty_pre  + voter_or_politician +fact:fact_stereo_dose:exp , data = data %>% filter(inparty_in_treatment==1))


feols(norm_loyalty_post ~norms_pre + fact_type + voter_or_politician  | fact_post~fact_stereo_dose, data = data %>% filter(inparty_in_treatment==0))

######################
### PRIMARY MODELS ###
######################

# Stash formulae components
DV_prefixes <- c(
    "out_therm_", "in_therm_", "norms_", "violence_"
)

# Iterate over DVs
for (i in 1:length(DV_prefixes)) {
    # Stash DV
    dv_prefix <- DV_prefixes[i]

    # Generate regression formula
    formula <- paste0(
        dv_prefix, "post", " ~ ",
        "fact_dose * fact_type * inparty_in_treatment * voter_or_politician + fact_pre",
        " + ", dv_prefix, "pre"
    ) %>%
        as.formula()

    # Run regression and stash output
    regression <- lm_robust(
        formula = formula, clusters = id, data = data
    )

    # Copy regression to identifying name
    paste0("exp2_", dv_prefix, "model") %>%
        assign(., regression, envir = .GlobalEnv)

    # Generate and stash slopes
    ## Pooled
    paste0("exp2_", dv_prefix, "model") %>%
        get() %>%
        avg_slopes(
            by = "fact_type",
            variables = "fact_dose"
        ) %>%
        as.data.frame() %>%
        mutate(
            dv = paste0(dv_prefix, "post"),
            Study = "Experiment 2"
        ) %>%
        assign(
            paste0("exp2_", dv_prefix, "pool_slopes"),
            .,
            envir = .GlobalEnv
        )

    ## Not pooled
    paste0("exp2_", dv_prefix, "model") %>%
        get() %>%
        avg_slopes(
            by = c("fact_type", "inparty_in_treatment", "voter_or_politician"),
            variables = "fact_dose"
        ) %>%
        as.data.frame() %>%
        mutate(
            dv = paste0(dv_prefix, "post"),
            Study = "Experiment 2"
        ) %>%
        assign(
            paste0("exp2_", dv_prefix, "slopes"),
            .,
            envir = .GlobalEnv
        )
}

###################
### SAVE SLOPES ###
###################

save(
    exp2_manip_pool_slopes,
    exp2_manip_slopes,
    exp2_out_therm_pool_slopes,
    exp2_out_therm_slopes,
    exp2_in_therm_pool_slopes,
    exp2_in_therm_slopes,
    exp2_norms_pool_slopes,
    exp2_norms_slopes,
    exp2_violence_pool_slopes,
    exp2_violence_slopes,
    file = "Data/exp_2/slopes.Rdata"
)

#############
### PLOTS ###
#############

# Combine un-pooled slopes data from primary models
slopes <- rbind(
    exp2_out_therm_slopes,
    exp2_in_therm_slopes,
    exp2_norms_slopes,
    exp2_violence_slopes
)

# Clean up slopes data
slopes <- slopes %>%
    mutate(
        `Statistic Type` = case_when(
            fact_type == "policy" ~ "Policy Preferences",
            fact_type == "dem_norm" ~ "Support for\nUndemocratic Practices",
            fact_type == "demo" ~ "Demographics"
        ),
        `Poll Subject's Role` = case_when(
            voter_or_politician == "voters" ~ "Voters",
            voter_or_politician == "politicians" ~ "Politicians"
        ),
        `Dependent Variable` = case_when(
            dv == "out_therm_post" ~ "Out-Party Warmth",
            dv == "in_therm_post" ~ "In-Party Warmth",
            dv == "norms_post" ~ "Support for\nUndemocratic Practices",
            dv == "violence_post" ~ "Support for\nPartisan Violence"
        ),
        `Poll Party` = case_when(
            inparty_in_treatment == 1 ~ "In-Party",
            inparty_in_treatment == 0 ~ "Out-Party"
        ),
        `Poll Subject` = case_when(
            voter_or_politician == "voters" & inparty_in_treatment == 1 ~ "In-Party Voter",
            voter_or_politician == "voters" & inparty_in_treatment == 0 ~ "Out-Party Voter",
            voter_or_politician == "politicians" & inparty_in_treatment == 1 ~ "In-Party Politician",
            voter_or_politician == "politicians" & inparty_in_treatment == 0 ~ "Out-Party Politician"
        )
    )

# Set visual parameters
source("Code/ggplot2_theme.R")

prl_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7"
)

# Plot
exp2_plot <- slopes %>%
    subset(Study == "Experiment 2") %>%
    ggplot(aes(
        x = estimate, y = `Statistic Type`, color = `Poll Subject's Role`,
        group = `Poll Subject`
    )) +
    geom_pointrange(
        aes(
            x = estimate,
            xmin = conf.low,
            xmax = conf.high
        ),
        position = position_dodge(.5)
    ) +
    scale_color_manual(
        values = prl_palette
    ) +
    geom_vline(xintercept = 0, linetype = "dotted") +
    facet_grid(
        cols = vars(`Dependent Variable`),
        rows = vars(`Poll Party`),
        scales = "free_x"
    ) +
    ggtitle("Experiment 2") +
    theme_prl() +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x)) +
    theme(
        legend.box = "horizontal",
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-10, -10, -10, -10),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 7)
    ) +
    labs(
        x = "\n",
        y = "Type of Statistic Shown\n"
    )

## Save plot
save(
    exp2_plot,
    file = "Data/exp_2/plot.Rdata"
)
