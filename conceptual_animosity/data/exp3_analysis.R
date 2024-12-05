# Overwrite alpha function
alpha <- psych::alpha
library(estimatr)
# Load data
data <- readRDS("data/")
levels(as.factor(data$fact_text))
d_s <- data %>% mutate(fact = case_when(str_detect(fact_text, "executive orders") ~ "executive orders",
                                 str_detect(fact_text, "loyal to the") ~ "loyalty",
                                 str_detect(fact_text, "polling stations") ~ "polling stations",
                                 str_detect(fact_text, "court decisions") ~ "court decisions",
                                 str_detect(fact_text, "censor media") ~ "censor media")) %>% drop_na(fact)
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
exp3_manip_pool_slopes <- avg_slopes(
    manip_model,
    variables = "fact_dose"
) %>%
    as.data.frame() %>%
    mutate(dv = "fact_post", Study = "Experiment 3")

## Not pooled
exp3_manip_slopes <- avg_slopes(
    manip_model,
    by = c("fact_type", "inparty_in_treatment", "voter_or_politician"),
    variables = "fact_dose"
) %>%
    as.data.frame() %>%
    mutate(dv = "fact_post", Study = "Experiment 3")

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
    paste0("exp3_", dv_prefix, "model") %>%
        assign(., regression, envir = .GlobalEnv)

    # Generate and stash slopes
    ## Pooled
    paste0("exp3_", dv_prefix, "model") %>%
        get() %>%
        avg_slopes(
            by = "fact_type",
            variables = "fact_dose"
        ) %>%
        as.data.frame() %>%
        mutate(
            dv = paste0(dv_prefix, "post"),
            Study = "Experiment 3"
        ) %>%
        assign(
            paste0("exp3_", dv_prefix, "pool_slopes"),
            .,
            envir = .GlobalEnv
        )

    ## Not pooled
    paste0("exp3_", dv_prefix, "model") %>%
        get() %>%
        avg_slopes(
            by = c("fact_type", "inparty_in_treatment", "voter_or_politician"),
            variables = "fact_dose"
        ) %>%
        as.data.frame() %>%
        mutate(
            dv = paste0(dv_prefix, "post"),
            Study = "Experiment 3"
        ) %>%
        assign(
            paste0("exp3_", dv_prefix, "slopes"),
            .,
            envir = .GlobalEnv
        )
}

###################
### SAVE SLOPES ###
###################

save(
    exp3_manip_pool_slopes,
    exp3_manip_slopes,
    exp3_out_therm_pool_slopes,
    exp3_out_therm_slopes,
    exp3_in_therm_pool_slopes,
    exp3_in_therm_slopes,
    exp3_norms_pool_slopes,
    exp3_norms_slopes,
    exp3_violence_pool_slopes,
    exp3_violence_slopes,
    file = "Data/exp_3/slopes.Rdata"
)

#############
### PLOTS ###
#############

# Combine un-pooled slopes data from primary models
slopes <- rbind(
    exp3_out_therm_slopes,
    exp3_in_therm_slopes,
    exp3_norms_slopes,
    exp3_violence_slopes
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
source("~/Google Drive/Shared drives/PRL/Research/published/Null Effects/Code/ggplot2_theme.R")

prl_palette <- c(
    "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7"
)

# Plot
exp3_plot <- slopes %>%
    subset(Study == "Experiment 3") %>%
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
        rows=vars(`Poll Party`),
        scales = "free_x"
    ) +
    ggtitle("Experiment 3") +
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
        x = "\nEffect of Moving Statistic One Percentage Point in Direction of Party Stereotype",
        y = "Type of Statistic Shown\n"
    )

## Save plot
save(
    exp3_plot,
    file = "Data/exp_3/plot.Rdata"
)
