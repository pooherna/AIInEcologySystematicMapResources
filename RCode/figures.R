library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(UpSetR)
# library(ComplexUpset)  # does not work :(


####
# fig. 2: trend 
# fig. 3: bar plot / 2 x upset plots
# fig. 4: appraisal
# fig. 7: bibliometrics
####

dat_algorithms <- fread("data_AM/algorithmsLong.csv")
dat_modality <- fread("data_AM/dataModalityLong.csv")
dat_source <- fread("data_AM/dataModalityLong.csv")
dat_task <- fread("data_AM/taskLong.csv")
dat_training <- fread("data_AM/trainingLong.csv")

# >> fig 1 -------


# >> fig 2 -------
## algorithms ----
algo_upset <- as.data.table(dat_algorithms)[
  , .(present = 1L), by = .(FileID, Algorithm)
  ] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Algorithm,
    values_from = present,
    values_fill = 0
  )

names(algo_upset)

fig2a <- UpSetR::upset(
        as.data.frame(algo_upset),
        sets = setdiff(names(algo_upset), "FileID"),
        main.bar.color = "#53868B",
        sets.bar.color = "#838B8B",
        matrix.color = "#CD950C",
        order.by = "freq")

## era ----
era_upset <- as.data.table(dat_algorithms)[
  , .(present = 1L), by = .(FileID, Era)
] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Era,
    values_from = present,
    values_fill = 0
  )

names(era_upset)

fig2b <- UpSetR::upset(
        as.data.frame(era_upset),
        sets = setdiff(names(era_upset), "FileID"),
        main.bar.color = "#53868B",
        sets.bar.color = "#838B8B",
        matrix.color = "#CD950C",
        order.by = "freq")

## era ----
era_upset <- as.data.table(dat_algorithms)[
  , .(present = 1L), by = .(FileID, Era)
] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Era,
    values_from = present,
    values_fill = 0
  )

names(era_upset)

UpSetR::upset(
  as.data.frame(era_upset),
  sets = setdiff(names(era_upset), "FileID"),
  order.by = "freq")

## tasks -----
task_upset <- as.data.table(dat_task)[
  , .(present = 1L), by = .(FileID, Task)
] |>
  as.data.frame() |>
  tidyr::pivot_wider(
    names_from = Task,
    values_from = present,
    values_fill = 0
  )

names(task_upset)

UpSetR::upset(
  as.data.frame(task_upset),
  sets = setdiff(names(task_upset), "FileID"),
  main.bar.color = "#53868B",
  sets.bar.color = "#838B8B",
  matrix.color = "#CD950C",
  order.by = "freq")
