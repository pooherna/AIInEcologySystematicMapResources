library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)
library(scales)

# the data collection  -----
# We extracted bibliographic data from Scopus on April 1, 2025. We used Boolean-style search strings targeting original research and review articles related to animal cognition and general life sciences (see below for full search strings). The search was restricted to documents published between 1975 and 2024. R (R Core Team, 2024) and the ggplot2 package (Wickham, 2016) were used to illustrate trends

#' [Animal cognition research]
#' *Original papers*:
#   (TITLE-ABS-KEY(("cognition" OR "cognitive" OR "learn*" OR "memor*" OR "decision making" OR "decision-making" OR "intelligence*") AND (animal* OR mammal* OR bird* OR Ave* OR "fish*" OR "reptile*" OR "amphibian*" OR "invertebrate*" OR "primate*" OR "ape*" OR "cephalopod*")) AND ( LIMIT-TO ( DOCTYPE,"ar" ) ) )

#' *Review papers*:
#   (TITLE-ABS-KEY(("cognition" OR "cognitive" OR "learn*" OR "memor*" OR "decision making" OR "decision-making" OR "intelligence*") AND (animal* OR mammal* OR bird* OR Ave* OR "fish*" OR "reptile*" OR "amphibian*" OR "invertebrate*" OR "primate*" OR "ape*" OR "cephalopod*")) AND ( LIMIT-TO ( DOCTYPE,"re" ) ) )


#' [Life sciences]
#' *Original papers*:
#   (SUBJAREA(AGRI OR BIOC OR IMMU OR NEUR OR PHAR) AND ( LIMIT-TO ( DOCTYPE,"ar" ) ) )
#' *Review papers*:
#   (SUBJAREA(AGRI OR BIOC OR IMMU OR NEUR OR PHAR) AND ( LIMIT-TO ( DOCTYPE,"re" ) ) )


# load data -----

# read yearly publication counts used in Figure 1
#dat_fig1 <- read.csv(here("PostDocProjects/SystematicMap/RCode", "fig1_trend.csv"))
dat_fig1 <- read.csv(here("PostDocProjects/SystematicMap/RCode", "ai_trend.csv"))

# ensure rows are ordered chronologically
dat_fig1 <- dat_fig1 %>%
  arrange(year)

# prepare long-format data for plotting ----

dat_fig1_plot <- dat_fig1 %>%
  pivot_longer(
    cols = c(biol_review,ai_review),
    names_to = "type",
    values_to = "count"
  ) %>%
  mutate(
    field = case_when(
      str_detect(type, "ai") ~ "reviews on AI usage",
      str_detect(type, "biol") ~ "reviews in Life science"
    ),
    paper_type = case_when(
      str_detect(type, "review") ~ "Review"
    )
  )

head(dat_fig1_plot)

# figure 1: temporal trends in publication counts

fig1 <- ggplot(dat_fig1_plot, aes(x = year, y = count)) +
  geom_point(aes(shape = paper_type, color = field), alpha = 0.7) +
  geom_smooth(
    aes(linetype = paper_type, color = field),
    method = "lm",
    se = TRUE
  ) +
  scale_y_log10(labels = label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2017, 2025, 5)) +
  labs(
    x = "Year",
    y = "Count of papers",
    shape = "Paper type",
    linetype = "Paper type",
    color = "Field"
  ) +
  theme_classic()

fig1 <- ggplot(dat_fig1_plot, aes(x = year, y = count)) +
  geom_point(aes(color = field), alpha = 0.7) +
  geom_smooth(
    aes(color = field),
    method = "lm",
    se = TRUE
  ) +
  scale_y_log10(labels = label_log(digits = 2)) +
  scale_x_continuous(breaks = seq(2017, 2025, 5)) +
  labs(
    x = "Year",
    y = "Count of papers",
    color = "Field"
  ) +
  theme_classic()

fig1
 
# calculate annual growth rates and CAGR -----

# function to calculate year-to-year proportional growth
# growth_t = (x_t - x_{t-1}) / x_{t-1}
calc_annual_growth <- function(x) {
  previous <- head(x, -1)
  current <- tail(x, -1)
  c(NA, (current - previous) / previous)
}

# function to calculate compound annual growth rate
calc_cagr <- function(x, n_years) {
  (tail(x, 1) / x[1])^(1 / n_years) - 1
}

# add annual growth-rate columns
dat_fig1_growth <- dat_fig1 %>%
  mutate(
    growth_review_biol      = calc_annual_growth(biol_review),
    growth_review_ai      = calc_annual_growth(ai_review)
  )

# number of years across the full observation period
n_years <- max(dat_fig1$year) - min(dat_fig1$year)

# calculate CAGR for each publication series
cagr_results <- tibble(
  series = c(
    "Review_biol",
    "Review_ai"
  ),
  cagr = c(
    calc_cagr(dat_fig1$biol_review, n_years),
    calc_cagr(dat_fig1$ai_review, n_years)
  )
) %>%
  mutate(cagr_percent = cagr * 100)

cagr_results

# compare temporal trends between fields -----

# convert data to long format for model fitting
dat_fig1_model <- dat_fig1 %>%
  pivot_longer(
    cols = c(biol_review,ai_review),
    names_to = "group",
    values_to = "pub"
  )

## original research papers -----

#df_research <- dat_fig1_model %>%
#  filter(group %in% c("animcog_research", "biol_research"))

## poisson GLM for count data
#model_research_pois <- glm(
#  pub ~ year * group,
#  data = df_research,
#  family = poisson(link = "log")
#)

#summary(model_research_pois)

# ### linear model on log-transformed counts
# model_research_lm <- lm(
#   log(pub) ~ year * group,
#   data = df_research
# )
# summary(model_research_lm)

## review papers -----
df_review <- dat_fig1_model %>%
  filter(group %in% c("biol_review","ai_review"))

## poisson GLM for count data 
model_review_pois <- glm(
  pub ~ year * group,
  data = df_review,
  family = poisson(link = "log")
)
summary(model_review_pois)

# linear model on log-transformed counts
# model_review_lm <- lm(
#   log(pub) ~ year * group,
#   data = df_review
# )
# summary(model_review_lm)

