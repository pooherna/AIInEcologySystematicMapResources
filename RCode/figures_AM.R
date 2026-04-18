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
out_dir <- "Figures_AM"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message("Writing figures to: ", normalizePath(out_dir))

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
  pdf_file <- file.path(out_dir, paste0(name, ".pdf"))
  png_file <- file.path(out_dir, paste0(name, ".png"))
  ggsave(pdf_file, plot, width = width, height = height, device = cairo_pdf)
  ggsave(png_file, plot, width = width, height = height, dpi = dpi, bg = "white")
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

####
# >> Data ----
####

map_data <- read_excel(file.path(data_dir, "map_20260416.xlsx"))
appraisal_data <- read_excel(file.path(data_dir, "appraisal_20260312.xlsx"))
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
# >> upset, bar, etc. ----
####

####
## >> upset -----
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
## >> bar -----
####

#' *the total number of papers is not consistent among long format data…*
# n_reviews <- nrow(map_data) 

algo_bar <- as.data.table(dat_algorithms)[
  , .(used = 1L), by = .(FileID, Algorithm)][
    , .(n_studies = .N), by = Algorithm][order(n_studies)]

bar_algorithm <- ggplot(
  algo_bar,
  aes(x = reorder(Algorithm, n_studies), y = n_studies)
) +
  geom_col(fill = "#53868B") +
  coord_flip() +
  labs(x = NULL, y = "Number of studies") +
  theme_classic()

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


## >> sankey ----


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
  theme_classic()


####
## >> plot map variables ----
####

### >> upset ----
fig_algorithm 
fig_era_upset
fig_task_upset 
fig_modality_upset
fig_source_upset

### >> bar ------

bar_algorithm
bar_algo_era
bar_task
bar_modality
bar_source
bar_training
bar_era

### >> sankey -----
fig_sankey_original


####
# >> Critical appraisal -----
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
## >> plot apprisal ----
####

fig_appraisal_all
fig_appraisal_pre2025
fig_appraisal_2025

####
# >> Bibliometrics -----
####

## >> world map ----

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


## >> collaboration ----
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

### >> top 18 -----
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

chord_png_18 <- file.path(out_dir, "Figure_5_chord_temp.png")
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


### >> all ----

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

chord_png_all <- file.path(out_dir, "Figure_chord_allcountries_temp.png")

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

## >> plot ----
fig_map
fig_chord_18
fig_chord_all

fig_bibliometrics <- (fig_map / fig_chord_18)
fig_bibliometrics

save_figure(
  plot = fig_bibliometrics,
  name = "Figure_bibliometrics",
  width = 10,
  height = 10
)
