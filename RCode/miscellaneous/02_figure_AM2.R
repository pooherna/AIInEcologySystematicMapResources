library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)
library(ggalluvial)
library(treemapify)

map_data <- read_excel(file.path(data_dir, "map_20260416.xlsx"))

col_id <- "Study ID (format: first author_year_letterIfNeeded )"
col_domain <- "Primary biological domain(s) of focus"
col_task <- "Broad category of the task or goal the AI models are used for"
col_data <- "Categories of the data used for the AI models"
col_model <- "Broad category of AI models discussed"
col_review <- "The main type of secondary review"

split_multi <- function(x) {
  x |>
    str_replace_all(";", ",") |>
    str_squish()
}

extract_year <- function(x) {
  as.integer(str_extract(x, "\\d{4}"))
}

prep_two_way_counts <- function(dat, var1, var2, split1 = TRUE, split2 = TRUE) {
  out <- dat |>
    select(all_of(c(col_id, var1, var2))) |>
    rename(
      StudyID = all_of(col_id),
      Var1 = all_of(var1),
      Var2 = all_of(var2)
    ) |>
    filter(
      !is.na(Var1), Var1 != "",
      !is.na(Var2), Var2 != ""
    ) |>
    mutate(
      Var1 = split_multi(Var1),
      Var2 = split_multi(Var2)
    )
  
  if (split1) {
    out <- out |> separate_rows(Var1, sep = ",\\s*")
  }
  if (split2) {
    out <- out |> separate_rows(Var2, sep = ",\\s*")
  }
  
  out |>
    mutate(
      Var1 = str_squish(Var1),
      Var2 = str_squish(Var2)
    ) |>
    filter(Var1 != "", Var2 != "") |>
    count(Var1, Var2, name = "n") |>
    ungroup()
}

prep_alluvial_counts <- function(dat, vars, split_flags = NULL) {
  tmp <- dat |>
    select(all_of(c(col_id, vars)))
  
  names(tmp) <- c("StudyID", paste0("V", seq_along(vars)))
  
  for (i in seq_along(vars)) {
    tmp[[paste0("V", i)]] <- split_multi(tmp[[paste0("V", i)]])
  }
  
  keep <- rep(TRUE, nrow(tmp))
  for (i in seq_along(vars)) {
    keep <- keep & !is.na(tmp[[paste0("V", i)]]) & tmp[[paste0("V", i)]] != ""
  }
  tmp <- tmp[keep, , drop = FALSE]
  
  if (is.null(split_flags)) {
    split_flags <- rep(TRUE, length(vars))
  }
  
  for (i in seq_along(vars)) {
    if (split_flags[i]) {
      tmp <- tmp |> separate_rows(all_of(paste0("V", i)), sep = ",\\s*")
    }
  }
  
  for (i in seq_along(vars)) {
    tmp[[paste0("V", i)]] <- str_squish(tmp[[paste0("V", i)]])
  }
  
  tmp |>
    filter(if_all(starts_with("V"), ~ .x != "")) |>
    count(across(starts_with("V")), name = "Freq") |>
    ungroup()
}



# 1)  domain(s) of focus × Broad category of the task or goal the AI models are used for ----
dat_1 <- prep_two_way_counts(
  map_data,
  var1 = col_domain,
  var2 = col_task,
  split1 = TRUE,
  split2 = TRUE
)

domain_order_1 <- dat_1 |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

task_order_1 <- dat_1 |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

dat_1 <- dat_1 |>
  mutate(
    Var1 = factor(Var1, levels = domain_order_1),
    Var2 = factor(Var2, levels = task_order_1)
  )

fig_heat_domain_task <- ggplot(dat_1, aes(x = Var2, y = Var1, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "#F4F4F4", high = "#3B6EA8") +
  labs(
    x = "Task category",
    y = "Primary biological domain",
    fill = "Count"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

fig_heat_domain_task

# 2) domain(s) of focus × Categories of the data used for the AI models ----
dat_2 <- prep_two_way_counts(
  map_data,
  var1 = col_domain,
  var2 = col_data,
  split1 = TRUE,
  split2 = TRUE
)

domain_order_2 <- dat_2 |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

data_order_2 <- dat_2 |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

dat_2 <- dat_2 |>
  mutate(
    Var1 = factor(Var1, levels = domain_order_2),
    Var2 = factor(Var2, levels = data_order_2)
  )

fig_stack_domain_data <- ggplot(dat_2, aes(x = Var1, y = n, fill = Var2)) +
  geom_col(width = 0.8) +
  labs(
    x = "Primary biological domain",
    y = "Number of studies",
    fill = "Data category"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

fig_stack_domain_data

# 3) Broad category of the task or goal the AI models are used for × Categories of the data used for the AI models ----

dat_3 <- prep_two_way_counts(
  map_data,
  var1 = col_task,
  var2 = col_data,
  split1 = TRUE,
  split2 = TRUE
)

task_order_3 <- dat_3 |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

data_order_3 <- dat_3 |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

dat_3 <- dat_3 |>
  mutate(
    Var1 = factor(Var1, levels = task_order_3),
    Var2 = factor(Var2, levels = data_order_3)
  )

fig_bubble_task_data <- ggplot(dat_3, aes(x = Var1, y = Var2, size = n)) +
  geom_point(alpha = 0.75) +
  scale_size_area(max_size = 14) +
  labs(
    x = "Task category",
    y = "Data category",
    size = "Count"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "#EAEAEA"),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

fig_bubble_task_data

# 4) Broad category of the task or goal the AI models are used for × Broad category of AI models discussed ----

dat_4 <- prep_two_way_counts(
  map_data,
  var1 = col_task,
  var2 = col_model,
  split1 = TRUE,
  split2 = TRUE
)

task_order_4 <- dat_4 |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

model_order_4 <- dat_4 |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

dat_4 <- dat_4 |>
  mutate(
    Var1 = factor(Var1, levels = task_order_4),
    Var2 = factor(Var2, levels = model_order_4)
  )

fig_heat_task_model <- ggplot(dat_4, aes(x = Var2, y = Var1, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), size = 3) +
  scale_fill_gradient(low = "#F4F4F4", high = "#3B6EA8") +
  labs(
    x = "AI model category",
    y = "Task category",
    fill = "Count"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

fig_heat_task_model


# 5) Primary biological domain(s) of focus × data type × Broad category of AI models discussed ----

dat_5 <- prep_alluvial_counts(
  map_data,
  vars = c(col_domain, col_data, col_model),
  split_flags = c(TRUE, TRUE, TRUE)
) |>
  rename(
    Domain = V1,
    DataType = V2,
    Model = V3
  )

dat_5 <- dat_5 |>
  mutate(
    Domain = fct_reorder(Domain, Freq, .fun = sum, .desc = TRUE),
    DataType = fct_reorder(DataType, Freq, .fun = sum, .desc = TRUE),
    Model = fct_reorder(Model, Freq, .fun = sum, .desc = TRUE)
  )

fig_sankey_domain_data_model <- ggplot(
  dat_5,
  aes(axis1 = Domain, axis2 = DataType, axis3 = Model, y = Freq)
) +
  geom_alluvium(aes(fill = Domain), width = 0.16, alpha = 0.75) +
  geom_stratum(width = 0.16, fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  scale_x_discrete(
    limits = c("Biological domain", "Data type", "AI model category"),
    expand = c(0.06, 0.06)
  ) +
  labs(
    x = NULL,
    y = "Number of records",
    fill = "Biological domain"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

fig_sankey_domain_data_model

# 6) Year × Primary biological domain(s) of focus ----

dat_6 <- map_data |>
  select(all_of(c(col_id, col_domain))) |>
  rename(
    StudyID = all_of(col_id),
    Domain = all_of(col_domain)
  ) |>
  mutate(
    Year = extract_year(StudyID),
    Domain = split_multi(Domain)
  ) |>
  filter(!is.na(Year), !is.na(Domain), Domain != "") |>
  separate_rows(Domain, sep = ",\\s*") |>
  mutate(Domain = str_squish(Domain)) |>
  count(Year, Domain, name = "n") |>
  ungroup()

domain_order_6 <- dat_6 |>
  group_by(Domain) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Domain)

dat_6 <- dat_6 |>
  mutate(Domain = factor(Domain, levels = domain_order_7))

fig_year_domain <- ggplot(dat_6, aes(x = Year, y = n, fill = Domain)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = sort(unique(dat_7$Year))) +
  labs(
    x = "Year",
    y = "Count",
    fill = "Primary biological domain"
  ) +
  theme_classic(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

fig_year_domain

# 7) Year × Broad category of AI models discussed ----

dat_7 <- map_data |>
  select(all_of(c(col_id, col_model))) |>
  rename(
    StudyID = all_of(col_id),
    Model = all_of(col_model)
  ) |>
  mutate(
    Year = extract_year(StudyID),
    Model = split_multi(Model)
  ) |>
  filter(!is.na(Year), !is.na(Model), Model != "") |>
  separate_rows(Model, sep = ",\\s*") |>
  mutate(Model = str_squish(Model)) |>
  count(Year, Model, name = "n") |>
  ungroup()

model_order_7 <- dat_7 |>
  group_by(Model) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Model)

dat_7 <- dat_7 |>
  mutate(Model = factor(Model, levels = model_order_7))

fig_year_model <- ggplot(dat_7, aes(x = Year, y = n, fill = Model)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = sort(unique(dat_8$Year))) +
  labs(
    x = "Year",
    y = "Count",
    fill = "AI model category"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

fig_year_model


# 8) The main type of secondary review ----
dat_8 <- map_data |>
  select(all_of(col_review)) |>
  rename(ReviewType = all_of(col_review)) |>
  filter(!is.na(ReviewType), ReviewType != "") |>
  mutate(ReviewType = str_squish(ReviewType)) |>
  count(ReviewType, name = "n") |>
  arrange(desc(n))

fig_treemap_review <- ggplot(dat_8, aes(area = n, fill = ReviewType, label = paste0(ReviewType, "\n(n = ", n, ")"))) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    reflow = TRUE,
    min.size = 3.5
  ) +
  labs(fill = "Review type") +
  theme(legend.position = "none")

fig_treemap_review
