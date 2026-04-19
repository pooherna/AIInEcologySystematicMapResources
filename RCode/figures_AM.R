library(data.table)
library(readxl)
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)
library(scales)
library(ComplexUpset)
library(ggalluvial)
library(treemapify)
library(UpSetR)
library(bibliometrix)
library(circlize)
library(countrycode)
library(sf)
library(rnaturalearth)
library(cowplot)
library(png)
library(purrr)

rm(list = ls()) 
gc()  

data_dir <- "data_AM"
out_dir <- "figures_AM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
pdf_dir <- file.path(out_dir, "pdf")
png_dir <- file.path(out_dir, "png")
dir.create(pdf_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(png_dir, showWarnings = FALSE, recursive = TRUE)

message("Writing figures to: ", normalizePath(out_dir))


####
# > functions ----
####

theme_am <- function(base_size = 9) {
  theme_classic(base_size = base_size) +
    theme(
      text = element_text(color = "#222222"),
      plot.title = element_text(face = "bold", size = base_size + 1, margin = margin(b = 5)),
      plot.subtitle = element_text(size = base_size - 1, color = "#555555", margin = margin(b = 5)),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, color = "#333333"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = base_size - 1),
      legend.text = element_text(size = base_size - 1),
      legend.key.height = unit(0.45, "lines"),
      plot.margin = margin(5, 6, 5, 6)
    )
}

save_figure <- function(plot, name, width, height, dpi = 600) {
  pdf_file <- file.path(pdf_dir, paste0(name, ".pdf"))
  png_file <- file.path(png_dir, paste0(name, ".png"))

  if (inherits(plot, "upset")) {
    cairo_pdf(pdf_file, width = width, height = height)
    print(plot)
    dev.off()

    png(png_file, width = width, height = height, units = "in", res = dpi, bg = "white")
    print(plot)
    dev.off()
  } else {
    ggsave(pdf_file, plot, width = width, height = height, device = cairo_pdf)
    ggsave(png_file, plot, width = width, height = height, dpi = dpi, bg = "white")
  }

  message("Saved ", pdf_file)
  message("Saved ", png_file)
}

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 14),
  plot.tag.position = c(0, 1)
)

clean_title <- function(x) {
  x |>
    str_replace_all("_", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

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

make_discrete_palette <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values)]
  values <- unique(values)
  if (length(values) == 0) {
    return(character())
  }
  setNames(colorRampPalette(unname(pal_main))(length(values)), values)
}

plot_bar <- function(dat, category, title, fill = pal_main[["blue"]], n_total = 72) {
  category <- rlang::ensym(category)
  dat |>
    distinct(FileID, !!category) |>
    count(!!category, name = "n_studies") |>
    filter(!is.na(!!category), !!category != "") |>
    mutate(
      pct = n_studies / n_total,
      label = paste0(n_studies, " (", percent(pct, accuracy = 1), ")"),
      category_label = fct_reorder(as.factor(!!category), n_studies)
    ) |>
    ggplot(aes(x = category_label, y = n_studies)) +
    geom_col(fill = fill, width = 0.72) +
    geom_text(aes(label = label), hjust = -0.08, size = 2.7, color = "#333333") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
    labs(x = NULL, y = "Reviews", title = title) +
    theme_am() +
    theme(plot.title = element_text(size = 9.5))
}

check_rows <- function(dat, expected, label) {
  if (nrow(dat) != expected) {
    warning(label, " has ", nrow(dat), " rows; expected ", expected, call. = FALSE)
  }
}


# > colours ----
pal_main <- c(
  blue = "#3B6EA8",
  teal = "#4B9A8D",
  green = "#7FA35B",
  gold = "#D6A24A",
  rose = "#B76D68",
  slate = "#6D7783",
  plum = "#8A6FA8"
)

pal_era <- c(
  "Classic" = "#4B9A8D",
  "Modern" = "#3B6EA8",
  "Contemporary" = "#D6A24A",
  "not specified" = "#A7A7A7"
)

pal_appraisal <- c(
  "Yes" = "#49a79a",
  "Partially" = "#E9C46A",
  "No" = "#de5a53",
  "NA" = "#9C9C9C"
)

####
# > data ----
####

map_data <- read_excel(file.path(data_dir, "map_20260416.xlsx"))
setDT(map_data)
appraisal_data <- read_excel(file.path(data_dir, "appraisal_20260312.xlsx"))
setDT(appraisal_data)

dat_algorithms <- fread(file.path(data_dir, "algorithmsLong.csv"))
dat_modality <- fread(file.path(data_dir, "dataModalityLong.csv"))
dat_source <- fread(file.path(data_dir, "dataSourceLong.csv"))
dat_task <- fread(file.path(data_dir, "taskLong.csv"))
dat_training <- fread(file.path(data_dir, "trainingLong.csv"))
scopus <- fread(file.path(data_dir, "scopusData.csv"))

check_rows(map_data, 72, "map_20260416.xlsx")
check_rows(appraisal_data, 72, "appraisal_20260312.xlsx")

message("Map rows: ", nrow(map_data))
message("Appraisal rows: ", nrow(appraisal_data))
message("Long CSV study coverage:")
for (nm in c("algorithms", "task", "modality", "source", "training")) {
  x <- get(paste0("dat_", nm))
  message("  ", nm, ": ", uniqueN(x$FileID), " studies, ", nrow(x), " rows")
}
message(
  "Note: data_AM/scopusData.csv contains publication years ",
  paste(sort(unique(scopus$Year)), collapse = ", "),
  ". This latest bibliometric dataset is used as-is."
)

####
# > upset, bar, and sanky ----
####

####
## > upset -----
####
algo_upset <- as.data.table(dat_algorithms)[
  , .(present = 1L), by = .(FileID, Algorithm)] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Algorithm,
    values_from = present,
    values_fill = 0
  )

fig_algorithm <- UpSetR::upset(as.data.frame(algo_upset),
  sets = setdiff(names(algo_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq"
)

era_upset <- as.data.table(dat_algorithms)[
  , .(present = 1L), by = .(FileID, Era)] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Era,
    values_from = present,
    values_fill = 0
  )

fig_era_upset <- UpSetR::upset(as.data.frame(era_upset),
  sets = setdiff(names(era_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq"
  )

task_upset <- as.data.table(dat_task)[
  , .(present = 1L), by = .(FileID, Task)] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Task,
    values_from = present,
    values_fill = 0
  )

fig_task_upset <- UpSetR::upset(as.data.frame(task_upset),
  sets = setdiff(names(task_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq"
  )

modality_upset <- as.data.table(dat_modality)[
  , .(present = 1L), by = .(FileID, `Data Modality`)] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = `Data Modality`,
    values_from = present,
    values_fill = 0
    )

fig_modality_upset <- UpSetR::upset(as.data.frame(modality_upset),
  sets = setdiff(names(modality_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq"
)

source_upset <- as.data.table(dat_source)[
  , .(present = 1L), by = .(FileID, `Source of Data`)] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = `Source of Data`,
    values_from = present,
    values_fill = 0
  )

fig_source_upset <- UpSetR::upset(as.data.frame(source_upset),
  sets = setdiff(names(source_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq"
)

####
## > bar -----
####

#' *the total number of papers is not consistent among long format data…*
# -> Sergio did not include NA??
# n_reviews <- nrow(map_data) 

algo_bar <- as.data.table(dat_algorithms)[
  , .(used = 1L), by = .(FileID, Algorithm)][
    , .(n_studies = .N), by = Algorithm][order(n_studies)]

bar_algorithm <- ggplot(
  algo_bar,
  aes(x = reorder(Algorithm, n_studies), y = n_studies)
) +
  geom_col(fill = pal_main[["blue"]]) +
  coord_flip() +
  labs(x = NULL, y = "Number of studies") +
  theme_am()

algo_counts <- dat_algorithms |>
  distinct(FileID, Algorithm, Era) |>
  count(Algorithm, Era, name = "n_studies") |>
  group_by(Algorithm) |>
  mutate(total = sum(n_studies)) |>
  ungroup() |>
  mutate(Algorithm = fct_reorder(Algorithm, total))

bar_algo_era <- ggplot(algo_counts, aes(x = Algorithm, y = n_studies, fill = Era)) +
  geom_col(width = 0.72) +
  geom_text(
    data = algo_counts |> group_by(Algorithm) |> summarise(total = sum(n_studies), .groups = "drop"),
    aes(x = Algorithm, y = total, label = total),
    inherit.aes = FALSE,
    hjust = -0.15,
    size = 2.7,
    color = "#333333"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  scale_fill_manual(values = pal_era, drop = FALSE) +
  labs(x = NULL, y = "number of studies", fill = "AI era", title = "AI algorithms mentioned") +
  theme_am() +
  theme(legend.position = "bottom", plot.title = element_text(size = 9.5))

bar_task <- plot_bar(dat_task, Task, "Task or goal", pal_main[["teal"]], uniqueN(dat_task$FileID))
bar_modality <- plot_bar(dat_modality, `Data Modality`, "Data modality", pal_main[["green"]], uniqueN(dat_modality$FileID))
bar_source <- plot_bar(dat_source, `Source of Data`, "Data source", pal_main[["gold"]], uniqueN(dat_source$FileID))
bar_training <- plot_bar(dat_training, `Training paradigm`, "Training paradigm", pal_main[["rose"]], uniqueN(dat_training$FileID))

bar_era <- dat_algorithms |>
  distinct(FileID, Era) |>
  count(Era, name = "n_studies") |>
  mutate(
    pct = n_studies / uniqueN(dat_algorithms$FileID),
    label = paste0(n_studies, " (", percent(pct, accuracy = 1), ")"),
    Era = fct_reorder(Era, n_studies)
  ) |>
  ggplot(aes(x = Era, y = n_studies, fill = Era)) +
  geom_col(width = 0.72, show.legend = FALSE) +
  geom_text(aes(label = label), hjust = -0.08, size = 2.7, color = "#333333") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.24))) +
  scale_fill_manual(values = pal_era, drop = FALSE) +
  labs(x = NULL, y = "Reviews", title = "AI era") +
  theme_am() +
  theme(plot.title = element_text(size = 9.5))


####
## > sankey ----
####

alg <- as.data.table(dat_algorithms)[, .(FileID, Era)] |> unique()
tsk <- as.data.table(dat_task)[, .(FileID, Task)] |> unique()
mod <- as.data.table(dat_modality)[, .(FileID, `Data Modality`)] |> unique()
src <- as.data.table(dat_source)[, .(FileID, `Source of Data`)] |> unique()

dat_alluvial_original <- alg[tsk, on = "FileID", allow.cartesian = TRUE][
  mod, on = "FileID", allow.cartesian = TRUE][
    src, on = "FileID", allow.cartesian = TRUE] |>
  unique() |>
  as.data.frame() |>
  rename(
    Modality = `Data Modality`,
    Source = `Source of Data`
  ) |>
  count(Era, Task, Modality, Source, name = "Freq")

fig_sankey_original <- ggplot(
  dat_alluvial_original,
  aes(axis1 = Era, axis2 = Task, axis3 = Modality, axis4 = Source, y = Freq)) +
  geom_alluvium(aes(fill = Era), width = 0.2, alpha = 0.8) +
  geom_stratum(width = 0.2, fill = "grey85", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  scale_x_discrete(
    limits = c("Era", "Task", "Data Modality", "Source of Data"),
    expand = c(0.08, 0.08)
  ) +
  scale_fill_manual(values = pal_era, drop = FALSE) +
  labs(y = "Count", x = NULL) +
  theme_am(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

## ##

col_id <- "Study ID (format: first author_year_letterIfNeeded )"
col_domain <- "Primary biological domain(s) of focus"
col_task <- "Broad category of the task or goal the AI models are used for"
col_data   <- "Categories of the data used for the AI models"
col_model <- "Broad category of AI models discussed"
col_review <- "The main type of secondary review"

dat_3var <- map_data |>
  select(all_of(c(col_id, col_domain, col_task, col_data))) |>
  rename(
    StudyID = all_of(col_id),
    Domain  = all_of(col_domain),
    Task    = all_of(col_task),
    Data    = all_of(col_data)
  ) |>
  filter(
    !is.na(Domain), Domain != "",
    !is.na(Task),   Task   != "",
    !is.na(Data),   Data   != ""
  ) |>
  mutate(
    across(c(Domain, Task, Data), str_squish)
  ) |>
  separate_rows(Task, sep = ",\\s*") |>
  separate_rows(Data, sep = ",\\s*") |>
  count(Domain, Task, Data, name = "Freq") |>
  mutate(
    Domain = fct_reorder(Domain, Freq, .fun = sum, .desc = TRUE),
    Task = fct_reorder(Task,   Freq, .fun = sum, .desc = TRUE),
    Data  = fct_reorder(Data,   Freq, .fun = sum, .desc = TRUE)
  )

fig_sankey_3var <- ggplot(
  dat_3var,
  aes(axis1 = Domain, axis2 = Task, axis3 = Data, y = Freq)
) +
  geom_alluvium(aes(fill = Domain), width = 0.16, alpha = 0.75) +
  geom_stratum(width = 0.16, fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)),
    size = 3
  ) +
  scale_x_discrete(
    limits = c("Biological domain", "Task category", "Data category"),
    expand = c(0.06, 0.06)
  ) +
  labs(
    x = NULL,
    y = "Number of records",
    fill = "Biological domain"
  ) +
  scale_fill_manual(values = make_discrete_palette(levels(dat_3var$Domain)), drop = FALSE) +
  theme_am(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

## ##

domain_data_model_counts <- prep_alluvial_counts(
  map_data,
  vars = c(col_domain, col_data, col_model),
  split_flags = c(TRUE, TRUE, TRUE)
) |>
  rename(
    Domain = V1,
    DataType = V2,
    Model = V3
  ) |>
  mutate(
    Domain = fct_reorder(Domain, Freq, .fun = sum, .desc = TRUE),
    DataType = fct_reorder(DataType, Freq, .fun = sum, .desc = TRUE),
    Model = fct_reorder(Model, Freq, .fun = sum, .desc = TRUE)
  )

fig_sankey_domain_data_model <- ggplot(
  domain_data_model_counts,
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
  scale_fill_manual(values = make_discrete_palette(levels(domain_data_model_counts$Domain)), drop = FALSE) +
  labs(
    x = NULL,
    y = "Number of records",
    fill = "Biological domain"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom"
  )

## > others  ----

### > domain by task heatmap ----
domain_task_counts <- prep_two_way_counts(
  map_data,
  var1 = col_domain,
  var2 = col_task,
  split1 = TRUE,
  split2 = TRUE
)

domain_levels_by_task_total <- domain_task_counts |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

task_levels_by_domain_total <- domain_task_counts |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

domain_task_counts <- domain_task_counts |>
  mutate(
    Var1 = factor(Var1, levels = domain_levels_by_task_total),
    Var2 = factor(Var2, levels = task_levels_by_domain_total)
  )

fig_heat_domain_task <- ggplot(domain_task_counts, aes(x = Var2, y = Var1, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), size = 2.7, color = "#222222") +
  scale_fill_gradient(low = "#F4F4F4", high = pal_main[["blue"]]) +
  labs(
    x = "Task category",
    y = "Primary biological domain",
    fill = "Count"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

### > domain by data stacked bar ----
domain_data_counts <- prep_two_way_counts(
  map_data,
  var1 = col_domain,
  var2 = col_data,
  split1 = TRUE,
  split2 = TRUE
)

domain_levels_by_data_total <- domain_data_counts |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

data_levels_by_domain_total <- domain_data_counts |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

domain_data_counts <- domain_data_counts |>
  mutate(
    Var1 = factor(Var1, levels = domain_levels_by_data_total),
    Var2 = factor(Var2, levels = data_levels_by_domain_total)
  )

fig_stack_domain_data <- ggplot(domain_data_counts, aes(x = Var1, y = n, fill = Var2)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = make_discrete_palette(data_levels_by_domain_total), drop = FALSE) +
  labs(
    x = "Primary biological domain",
    y = "Number of studies",
    fill = "Data category"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "bottom"
  )

### > task by data bubble plot ----
task_data_counts <- prep_two_way_counts(
  map_data,
  var1 = col_task,
  var2 = col_data,
  split1 = TRUE,
  split2 = TRUE
)

task_levels_by_data_total <- task_data_counts |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

data_levels_by_task_total <- task_data_counts |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

task_data_counts <- task_data_counts |>
  mutate(
    Var1 = factor(Var1, levels = task_levels_by_data_total),
    Var2 = factor(Var2, levels = data_levels_by_task_total)
  )

fig_bubble_task_data <- ggplot(task_data_counts, aes(x = Var1, y = Var2, size = n)) +
  geom_point(color = pal_main[["blue"]], alpha = 0.75) +
  scale_size_area(max_size = 14) +
  labs(
    x = "Task category",
    y = "Data category",
    size = "Count"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "#EAEAEA", linewidth = 0.25),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

### > task by model heatmap ----
task_model_counts <- prep_two_way_counts(
  map_data,
  var1 = col_task,
  var2 = col_model,
  split1 = TRUE,
  split2 = TRUE
)

task_levels_by_model_total <- task_model_counts |>
  group_by(Var1) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var1)

model_levels_by_task_total <- task_model_counts |>
  group_by(Var2) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Var2)

task_model_counts <- task_model_counts |>
  mutate(
    Var1 = factor(Var1, levels = task_levels_by_model_total),
    Var2 = factor(Var2, levels = model_levels_by_task_total)
  )

fig_heat_task_model <- ggplot(task_model_counts, aes(x = Var2, y = Var1, fill = n)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = n), size = 2.7, color = "#222222") +
  scale_fill_gradient(low = "#F4F4F4", high = pal_main[["blue"]]) +
  labs(
    x = "AI model category",
    y = "Task category",
    fill = "Count"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "right"
  )

### > year by domain ----
year_domain_counts <- map_data |>
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

domain_levels_by_year_total <- year_domain_counts |>
  group_by(Domain) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Domain)

year_domain_counts <- year_domain_counts |>
  mutate(Domain = factor(Domain, levels = domain_levels_by_year_total))

fig_year_domain <- ggplot(year_domain_counts, aes(x = Year, y = n, fill = Domain)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = sort(unique(year_domain_counts$Year))) +
  scale_fill_manual(values = make_discrete_palette(domain_levels_by_year_total), drop = FALSE) +
  labs(
    x = "Year",
    y = "Count",
    fill = "Primary biological domain"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

### > year by model ----
year_model_counts <- map_data |>
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

model_levels_by_year_total <- year_model_counts |>
  group_by(Model) |>
  summarise(total = sum(n), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(Model)

year_model_counts <- year_model_counts |>
  mutate(Model = factor(Model, levels = model_levels_by_year_total))

fig_year_model <- ggplot(year_model_counts, aes(x = Year, y = n, fill = Model)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = sort(unique(year_model_counts$Year))) +
  scale_fill_manual(values = make_discrete_palette(model_levels_by_year_total), drop = FALSE) +
  labs(
    x = "Year",
    y = "Count",
    fill = "AI model category"
  ) +
  theme_am(base_size = 9) +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom"
  )

### > review type treemap ----
review_type_counts <- map_data |>
  select(all_of(col_review)) |>
  rename(ReviewType = all_of(col_review)) |>
  filter(!is.na(ReviewType), ReviewType != "") |>
  mutate(ReviewType = str_squish(ReviewType)) |>
  count(ReviewType, name = "n") |>
  arrange(desc(n))

fig_treemap_review <- ggplot(
  review_type_counts,
  aes(area = n, fill = ReviewType, label = paste0(ReviewType, "\n(n = ", n, ")"))
) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    reflow = TRUE,
    min.size = 3.5
  ) +
  scale_fill_manual(values = make_discrete_palette(review_type_counts$ReviewType), drop = FALSE) +
  labs(fill = "Review type") +
  theme_am(base_size = 9) +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    panel.grid = element_blank()
  )

####
## > plot map variables ----
####

### > upset ----
fig_algorithm 
fig_era_upset
fig_task_upset 
fig_modality_upset
fig_source_upset

### > bar ------

bar_algorithm
bar_algo_era
bar_task
bar_modality
bar_source
bar_training
bar_era

### > sankey -----

fig_sankey_original
fig_sankey_3var
fig_sankey_domain_data_model

### > others ----

fig_heat_domain_task
fig_stack_domain_data

fig_bubble_task_data

fig_heat_task_model

fig_year_domain
fig_year_model

fig_treemap_review


####
# > Critical appraisal -----
####

n_reviews <- nrow(map_data)
study_col <- "Study ID (format: first author_year_letterIfNeeded )"
item_cols <- names(appraisal_data)[5:ncol(appraisal_data)]

# organise labels 
label_map <- c(
  "Provided citation, DOI or open access to published protocol" =
    "Protocol available",
  "Provided Boolean-style full search string and state the platform for which the string is formatted (e.g., Web of Science format)" =
    "Full search string reported",
  "The authors indicated whether they considered non-English studies during the screening process" =
    "Non-English reviews considered",
  "Described the process by which the comprehensiveness of the search strategy was assessed (i.e., list of benchmark articles)" =
    "Search comprehensiveness assessed",
  "Described the methodology for screening articles/studies for relevance. Methods for consistency of screening decisions (at title, abstract, and full texts levels) checking must be described." =
    "Screening method described",
  "Provided the number of articles retained following full text screening" =
    "Full-text inclusion counts provided",
  "Provided a file and/or table containing raw extracted quantitative or qualitative data (study findings) from included studies" =
    "Raw extracted data provided",
  "Provided metadata (i.e. detailed description of extracted raw variables) in a separate table, list and/or file for included studies" =
    "Metadata provided",
  "Described any financial or non-financial competing interests that the review authors may have or provided a statement of the absence of any potential competing interests" =
    "Competing interests reported",
  "Provided any supplemental files" =
    "Supplemental files provided",
  "The authors reported which languages were included in the literature search" =
    "Search languages reported",
  "If the authors explicitly stated that they intended to include any studies in languages other than English in their final dataset, select “Yes”." =
    "Intended non-English inclusion",
  "The authors reported using non-English search terms or provided search strings formatted for non-English databases as part of their search strategy." =
    "Non-English search terms reported",
  "The authors described the data extraction process (who extracted, double-checking, and consistency)" =
    "Data extraction process described",
  "The authors reported and visualised the review flow and the number of studies retained at each stage (e.g., in a PRISMA/ROSES diagram)." =
    "PRISMA/ROSES diagram provided",
  "The authors provided bibliographic information (for example, author, year, title, DOI) for all studies included in the synthesis." =
    "Included studies listed",
  "The authors provided bibliographic information for studies excluded at the full-text screening stage, including reasons for exclusion." =
    "Excluded full texts listed",
  "The authors provided analysis and/or figure-generation computer code/scripts." =
    "Analysis or figure code provided"
  )

short_labels <- unname(label_map[item_cols])
short_labels[is.na(short_labels)] <- clean_title(item_cols[is.na(short_labels)])

classify_appraisal <- function(x) {
  out <- case_when(
    is.na(x) ~ "NA",
    str_detect(str_to_lower(as.character(x)), "^yes") ~ "Yes",
    str_detect(str_to_lower(as.character(x)), "^part") ~ "Partially",
    str_detect(str_to_lower(as.character(x)), "^no") ~ "No",
    TRUE ~ "NA"
  )
  factor(out, levels = c("NA", "No", "Partially", "Yes"))
}

appraisal_long <- appraisal_data |>
  mutate(
    Year = as.integer(str_extract(.data[[study_col]], "\\d{4}")),
    Group_pre2025 = if_else(Year < 2025, "Reviews before 2025", "Reviews published in 2025")
  ) |>
  select(all_of(study_col), Year, Group_pre2025, all_of(item_cols)) |>
  pivot_longer(all_of(item_cols), names_to = "item_raw", values_to = "response_raw") |>
  mutate(
    item = factor(short_labels[match(item_raw, item_cols)], levels = rev(short_labels)),
    response = classify_appraisal(response_raw)
  )

summarise_appraisal <- function(dat, group_label) {
  n_group <- n_distinct(dat[[study_col]])
  dat |>
    count(item, response, name = "n") |>
    complete(item, response, fill = list(n = 0)) |>
    group_by(item) |>
    mutate(prop = n / sum(n)) |>
    ungroup() |>
    mutate(Group = paste0(group_label, "\n(n = ", n_group, ")"))
}

appraisal_summary <- bind_rows(
  summarise_appraisal(appraisal_long, "All reviews"),
  summarise_appraisal(filter(appraisal_long, Year < 2025), "Reviews before 2025"),
  summarise_appraisal(filter(appraisal_long, Year == 2025), "Reviews published in 2025")
  ) |>
  mutate(Group = factor(
    Group,
    levels = c(
      paste0("All reviews\n(n = ", n_reviews, ")"),
      paste0("Reviews before 2025\n(n = ", n_distinct(filter(appraisal_long, Year < 2025)[[study_col]]), ")"),
      paste0("Reviews published in 2025\n(n = ", n_distinct(filter(appraisal_long, Year == 2025)[[study_col]]), ")")
    )
  ))

make_appraisal_plot <- function(dat, group_name) {

  dat_g <- dat |> 
    filter(Group == group_name)
  
  item_order <- dat_g |>
    filter(response == "Yes") |>
    arrange(desc(prop), item) |>
    pull(item)
  
  dat_g <- dat_g |>
    mutate(
      item = factor(item, levels = rev(item_order))
    )
  
  ggplot(dat_g, aes(x = prop, y = item, fill = response)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.15) +
    scale_x_continuous(
      labels = percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_fill_manual(values = pal_appraisal, drop = FALSE) +
    labs(
      x = "Proportion of studies",
      y = NULL,
      fill = "Assessment",
      title = group_name) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE, reverse = TRUE)) +
    
    theme_am(base_size = 8.5) +
    theme(
      legend.position = "bottom",
      panel.grid.major.x = element_line(color = "#E1E1E1", linewidth = 0.25),
      plot.title = element_text(face = "bold", size = 9),
      axis.text.y = element_text(size = 7.8)
    )
}

group_levels <- levels(appraisal_summary$Group)
fig_appraisal_all <- make_appraisal_plot(appraisal_summary, group_levels[1])
fig_appraisal_pre2025 <- make_appraisal_plot(appraisal_summary, group_levels[2])
fig_appraisal_2025 <- make_appraisal_plot(appraisal_summary, group_levels[3])

####
## > plot appraisal ----
####

fig_appraisal_all
fig_appraisal_pre2025
fig_appraisal_2025

####
# > Bibliometrics -----
####

## > world map ----

extract_first_last <- function(author_list) {
  authors <- strsplit(author_list, ";")[[1]]
  c(trimws(authors[1]), trimws(authors[length(authors)]))
}

extract_affiliation <- function(affiliation_list) {
  affiliations <- strsplit(affiliation_list, ";")[[1]]
  c(trimws(affiliations[1]), trimws(affiliations[length(affiliations)]))
}

extract_country <- function(affiliation) {
  tail(strsplit(affiliation, ",")[[1]], 1) |> trimws()
}

country_mapping <- c(
  "USA" = "United States",
  "USA." = "United States",
  "United States of America" = "United States",
  "United Kingdom." = "United Kingdom",
  "Russian Federation" = "Russia",
  "Czech Republic" = "Czechia"
)

bibs <- scopus |>
  as.data.frame() |>
  mutate(
    first_author = vapply(authors, \(x) extract_first_last(x)[1], character(1)),
    first_author_affiliation = vapply(authors_with_affiliations, \(x) extract_affiliation(x)[1], character(1)),
    first_author_country_raw = vapply(first_author_affiliation, extract_country, character(1)),
    first_author_country = recode(first_author_country_raw, !!!country_mapping),
    first_author_iso3 = countrycode(first_author_country, origin = "country.name", destination = "iso3c")
  )

country_counts <- bibs |>
  filter(!is.na(first_author_iso3)) |>
  count(first_author_iso3, first_author_country, name = "n_first_authors")

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, name, geometry) |>
  left_join(country_counts, by = c("iso_a3" = "first_author_iso3"))

fig_map <- ggplot(world) +
  geom_sf(aes(fill = n_first_authors), color = "#7b837d", linewidth = 0.08) +
  scale_fill_gradient(
    low = "#d8eade",
    high = "#2a8352",
    na.value = "#ecf5ee",
    breaks = pretty_breaks(n = 5),
    labels = label_number(accuracy = 1)
  ) +
  labs(fill = "First-author\naffiliations") +
  guides(fill = guide_colorbar(
    title.position = "top",
    barwidth = unit(3.5, "cm"),
    barheight = unit(0.28, "cm")
  )) +
  theme_void(base_size = 9) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    plot.margin = margin(3, 3, 3, 3)
  )

fig_map


## > collaboration ----
bib_data <- convert2df(file.path(data_dir, "scopusData.bib"), dbsource = "scopus", format = "bibtex")
bib_data <- metaTagExtraction(bib_data, Field = "AU_CO", sep = ";")

net_matrix <- biblioNetwork(
  bib_data,
  analysis = "collaboration",
  network = "countries",
  sep = ";"
) |>
  as.matrix()

net_matrix[lower.tri(net_matrix)] <- 0
rownames(net_matrix) <- str_to_title(rownames(net_matrix))
colnames(net_matrix) <- str_to_title(colnames(net_matrix))
rownames(net_matrix)[rownames(net_matrix) == "Usa"] <- "USA"
colnames(net_matrix)[colnames(net_matrix) == "Usa"] <- "USA"
rownames(net_matrix)[rownames(net_matrix) == "United Kingdom"] <- "UK"
colnames(net_matrix)[colnames(net_matrix) == "United Kingdom"] <- "UK"

country_strength <- rowSums(net_matrix) + colSums(net_matrix)

### > top 18 -----
keep_countries_18 <- names(sort(country_strength[country_strength > 0], decreasing = TRUE))[1:min(18, sum(country_strength > 0))]
net_matrix_keep_18 <- net_matrix[keep_countries_18, keep_countries_18, drop = FALSE]
net_matrix_keep_18 <- net_matrix_keep_18[rowSums(net_matrix_keep_18) + colSums(net_matrix_keep_18) > 0,
                                   colSums(net_matrix_keep_18) + rowSums(net_matrix_keep_18) > 0,
                                   drop = FALSE]

chord_cols_18 <- setNames(
  c("#3B6EA8", "#4B9A8D", "#D6A24A", "#B76D68", "#6D7783", "#8A6FA8",
    "#7FA35B", "#5E8C9A", "#C58A5A", "#8C7853", "#5B8E7D", "#A05C7B",
    "#7895B2", "#B8A052", "#6F7D55", "#9B7E7E", "#4F7B8A", "#8E9A6B")[seq_along(rownames(net_matrix_keep_18))],
  rownames(net_matrix_keep_18)
)

chord_png_18 <- file.path(tempdir(), "Figure_5_chord_temp.png")
png(chord_png_18, width = 5400, height = 5400, res = 450, bg = "white")
par(mar = c(0.5, 0.5, 0.5, 0.5))
circos.clear()
circos.par(
  track.margin = c(0.01, 0.01),
  gap.degree = 2,
  canvas.xlim = c(-1.25, 1.25),
  canvas.ylim = c(-1.25, 1.25)
)
chordDiagram(
  net_matrix_keep_18,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.04, 0.02),
  preAllocateTracks = 1,
  grid.col = chord_cols_18,
  transparency = 0.35
)
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    sector.name <- get.cell.meta.data("sector.index")
    circos.text(
      mean(xlim),
      ylim[1] + 0.25,
      sector.name,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.7,
      col = "#333333"
    )
  },
  bg.border = NA
)
circos.clear()
dev.off()

chord_img <- png::readPNG(chord_png_18)
fig_chord_18 <- ggdraw() +
  draw_image(chord_img, x = 0, y = 0, width = 1, height = 1) +
  theme(plot.margin = margin(0, 0, 0, 0))


### > all ----

keep_all <- names(country_strength[country_strength > 0])
net_matrix_all <- net_matrix[keep_all, keep_all, drop = FALSE]
net_matrix_all <- net_matrix_all[
  rowSums(net_matrix_all) + colSums(net_matrix_all) > 0,
  colSums(net_matrix_all) + rowSums(net_matrix_all) > 0,
  drop = FALSE
  ]

country_strength_all <- rowSums(net_matrix_all) + colSums(net_matrix_all)
ord_all <- names(sort(country_strength_all, decreasing = TRUE))
net_matrix_all <- net_matrix_all[ord_all, ord_all, drop = FALSE]

base_cols <- setNames(
  c("#3B6EA8", "#4B9A8D", "#D6A24A", "#B76D68", "#6D7783", "#8A6FA8",
    "#7FA35B", "#5E8C9A", "#C58A5A", "#8C7853", "#5B8E7D", "#A05C7B",
    "#7895B2", "#B0C052", "#6F7D55", "#9B7E7E", "#4F7B8A", "#8E9A6B", # 18
    "#9e907e", "#dea5a4", "#898441", "#af9392", "#8d9490", "#708090", # 24
    "#cd7f32", "#32cd4b", "#7f32cd", "#ca4a46", "#63000f", "#de3163", # 30
    "#434d57", "#707390", "#908070", "#e3735e", "#e35e7e", "#364758", # 36
    "#b22222", "#8eb222", "#22b2b2", "#ffd700", "#682860", "#685028", # 42
    "#013220", "#038756"
    )[seq_along(rownames(net_matrix_all))],
  rownames(net_matrix_all)
)

country_names_all <- rownames(net_matrix_all)
country_names_for_code <- country_names_all

country_names_for_code[country_names_for_code == "UK"] <- "United Kingdom"

country_codes_all <- countrycode(
  sourcevar = country_names_for_code,
  origin = "country.name",
  destination = "iso3c",
  warn = TRUE
  )

country_codes_all[country_names_all == "USA"] <- "USA"
country_codes_all[country_names_all == "UK"]  <- "GBR"


country_codes_all[is.na(country_codes_all)] <- country_names_all[is.na(country_codes_all)]

rownames(net_matrix_all) <- country_codes_all
colnames(net_matrix_all) <- country_codes_all


cols_all <- setNames(
  colorRampPalette(base_cols)(nrow(net_matrix_all)),
  rownames(net_matrix_all)
  )

chord_png_all <- file.path(tempdir(), "Figure_chord_allcountries_temp.png")

png(chord_png_all, width = 5000, height = 5000, res = 500, bg = "white")
par(mar = c(0.5, 0.5, 0.5, 0.5))

circos.clear()

circos.par(
  track.margin = c(0.005, 0.005),
  gap.degree = 1,
  canvas.xlim = c(-1.30, 1.30),
  canvas.ylim = c(-1.30, 1.30)
  )

chordDiagram(
  net_matrix_all,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.035, 0.015),
  preAllocateTracks = 1,
  grid.col = cols_all,
  transparency = 0.45
  )

circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    sector.name <- get.cell.meta.data("sector.index")
    
    circos.text(
      mean(xlim),
      ylim[1] + mm_y(3.0),
      sector.name,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5),
      cex = 0.45,
      col = "#333333"
    )
  },
  bg.border = NA
)

circos.clear()
dev.off()

img_all <- png::readPNG(chord_png_all)

fig_chord_all <- ggdraw() +
  draw_image(img_all, x = 0, y = 0, width = 1, height = 1) +
  theme(plot.margin = margin(0, 0, 0, 0))

## > plot bibliometrics figures ----
fig_map
fig_chord_18
fig_chord_all

fig_bibliometrics <- (fig_map / fig_chord_18)
fig_bibliometrics


####
# > save plots ----
####

save_figure(fig_algorithm, "Figure_algorithm_upset", width = 8, height = 6)
save_figure(fig_era_upset, "Figure_era_upset", width = 8, height = 6)
save_figure(fig_task_upset, "Figure_task_upset", width = 8, height = 6)
save_figure(fig_modality_upset, "Figure_modality_upset", width = 8, height = 6)
save_figure(fig_source_upset, "Figure_source_upset", width = 8, height = 6)

save_figure(bar_algorithm, "Figure_algorithm_bar", width = 7, height = 5)
save_figure(bar_algo_era, "Figure_algorithm_era_bar", width = 8, height = 5.5)
save_figure(bar_task, "Figure_task_bar", width = 7, height = 5)
save_figure(bar_modality, "Figure_modality_bar", width = 7, height = 5)
save_figure(bar_source, "Figure_source_bar", width = 7, height = 5)
save_figure(bar_training, "Figure_training_bar", width = 7, height = 5)
save_figure(bar_era, "Figure_era_bar", width = 7, height = 5)

save_figure(fig_sankey_original, "Figure_sankey_original", width = 12, height = 8)
save_figure(fig_sankey_3var, "Figure_sankey_3var", width = 11, height = 7.5)
save_figure(fig_sankey_domain_data_model, "Figure_sankey_domain_data_model", width = 11, height = 7.5)

save_figure(fig_heat_domain_task, "Figure_heat_domain_task", width = 8, height = 5.5)
save_figure(fig_stack_domain_data, "Figure_stack_domain_data", width = 8.5, height = 5.5)
save_figure(fig_bubble_task_data, "Figure_bubble_task_data", width = 8.5, height = 6)
save_figure(fig_heat_task_model, "Figure_heat_task_model", width = 8.5, height = 6)
save_figure(fig_year_domain, "Figure_year_domain", width = 8.5, height = 5.5)
save_figure(fig_year_model, "Figure_year_model", width = 8.5, height = 5.5)
save_figure(fig_treemap_review, "Figure_treemap_review", width = 7, height = 5)

save_figure(fig_appraisal_all, "Figure_appraisal_all", width = 8.5, height = 6)
save_figure(fig_appraisal_pre2025, "Figure_appraisal_pre2025", width = 8.5, height = 6)
save_figure(fig_appraisal_2025, "Figure_appraisal_2025", width = 8.5, height = 6)

save_figure(fig_map, "Figure_map", width = 9, height = 5)
save_figure(fig_chord_18, "Figure_chord_18", width = 7, height = 7)
save_figure(fig_chord_all, "Figure_chord_all", width = 8, height = 8)
save_figure(fig_bibliometrics, "Figure_bibliometrics", width = 10, height = 10)
