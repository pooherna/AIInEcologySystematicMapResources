rm(list = ls())
pacman::p_load(
  tidyverse,
  ggplot2,
  here,
  stringr,
  paletteer,
  readr,
  dplyr,
  forcats,
  scales,
  treemapify,
  rphylopic,
  RColorBrewer,
  viridis,
  tidyr,
  forcats,
  bibliometrix,
  scales,
  treemap,
  ComplexUpset, # for upset plot
  patchwork,
  wordcloud2,
  textstem,
  SnowballC,
  sf,
  rnaturalearthdata,
  rnaturalearth,
  ggalluvial,
  circlize,
  countrycode
)

biblio <- read_csv(here("Data", "scopusData.csv"))
#bib_data <- convert2df(here("Data", "bibliometric.bib"), dbsource = "scopus", format = "bibtex")
bib_data <- convert2df(here("Data", "scopusData.bib"), dbsource = "scopus", format = "bibtex")

# Figure 4: Bibliometric analysis

## Panel A: World map with authors' affiliations


# Load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load bibliometric data
scopus <- biblio

colnames(scopus)
# Define functions for extraction
extract_first_last <- function(author_list) {
  authors <- strsplit(author_list, ";")[[1]]
  c(trimws(authors[1]), trimws(authors[length(authors)]))
}

extract_affiliation <- function(affiliation_list) {
  affiliations <- strsplit(affiliation_list, ";")[[1]]
  c(trimws(affiliations[1]), trimws(affiliations[length(affiliations)]))
}

extract_country <- function(affiliation) {
  tail(strsplit(affiliation, ",")[[1]], 1) %>% trimws()
}

# Define country name corrections
country_mapping <- c(
  "USA" = "United States of America",
  "USA." = "United States of America",
  "United States" = "United States of America",
  "Russian Federation" = "Russia",
  "Czech Republic" = "Czechia", 
  "United Kingdom." = "United Kingdom"
)

# Clean and recode meta-analysis data
bibs <- scopus %>%
  mutate(
    first_author = sapply(authors, \(x) extract_first_last(x)[1]),
    last_author = sapply(authors, \(x) extract_first_last(x)[2]),
    first_author_affiliation = sapply(`authors_with_affiliations`, \(x) extract_affiliation(x)[1]),
    last_author_affiliation = sapply(`authors_with_affiliations`, \(x) extract_affiliation(x)[2]),
    first_author_country = sapply(first_author_affiliation, extract_country),
    last_author_country = sapply(last_author_affiliation, extract_country),
    first_author_country = recode(first_author_country, !!!country_mapping)
  )


# Count countries
first_country_counts <- table(bibs$first_author_country) %>%
  as.data.frame() %>%
  rename(Country = Var1, First_counts = Freq)


# Join counts to world map
world_first <- world %>%
  left_join(first_country_counts, by = c("name" = "Country"))


# Plot a. (meta-analysis)
fig4_a <- ggplot(world_first) +
  geom_sf(aes(fill = First_counts), color = NA) +
  scale_fill_paletteer_c("ggthemes::Purple", 
                         na.value = "#D8D8D8FF",
                         labels = number_format(accuracy = 1)) +#[AM: set the bar scale to display integers.]
  labs(fill = "Number of first authors") + #[AM: added]
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    barwidth = 15,
    barheight = 0.5
  ))

fig4_a
#ggsave(here("Figures/raw", "Figure_4_bibliomap.pdf"), width = 11, height = 10, dpi = 300)
ggsave(here("Figures/raw", "Figure_4_bibliomap.png"), width = 11, height = 10, dpi = 300)




## Panel B: Chord diagram of authors collaborations


# Load bibliometric data
dim(bib_data) 

bib_data <- metaTagExtraction(bib_data, Field = "AU1_CO", sep = ";")
bib_data <- metaTagExtraction(bib_data, Field = "AU_CO", sep = ";")
dim(bib_data)

# Generate collaboration network matrix (countries)
NetMatrix2 <- biblioNetwork(bib_data, 
                            analysis = "collaboration",
                            network = "countries", 
                            sep = ";")
net_matrix2 <- as.matrix(NetMatrix2)

# Clean country names
net_matrix2[lower.tri(net_matrix2)] <- 0  # Remove lower triangle
colnames(net_matrix2) <- str_to_title(colnames(net_matrix2))
rownames(net_matrix2) <- str_to_title(rownames(net_matrix2))
colnames(net_matrix2)[colnames(net_matrix2) == "Usa"] <- "USA"
rownames(net_matrix2)[rownames(net_matrix2) == "Usa"] <- "USA"
colnames(net_matrix2)[colnames(net_matrix2) == "United Kingdom"] <- "UK"
rownames(net_matrix2)[rownames(net_matrix2) == "United Kingdom"] <- "UK"

# Set circular layout
country_order <- names(sort(rowSums(net_matrix2), decreasing = TRUE))
net_matrix2_sorted <- net_matrix2[country_order, country_order]

# Remapped Unicorn Pro Contrast (25 countries)
cols <- c(
  "UK"                 = "#6C5FC7", # indigo
  "USA"                = "#91D3C5", # teal
  "Canada"             = "#5D5D5D", # charcoal
  "Australia"          = "#BCCF7F", # olive green
  "Netherlands"        = "#A9C7E8", # steel blue
  "Germany"            = "#C49ACF", # mauve
  "Italy"              = "#B28A85", # clay rose
  "Norway"             = "#7A8BD1", # periwinkle
  "France"             = "#6BB6C9", # teal-gray
  "Brazil"             = "#D6A9A5", # rosewood
  "China"              = "#8573C1", # muted purple
  "Spain"              = "#E8C1A8", # beige apricot
  "New Zealand"        = "#D9E7A8", # sage yellow
  "Denmark"            = "#E7D7A1", # muted gold
  "Iran"               = "#E0E0D1", # warm gray
  "Portugal"           = "#88A6D9", # dusty blue
  "Saudi Arabia"       = "#9D7ED0", # violet
  "Hong Kong"          = "#9FBBB0", # stone teal
  "Japan"              = "#CC9999", # muted coral
  "Malta"              = "#A6A6A6", # neutral gray
  "Slovenia"           = "#8E72A9", # plum
  "South Africa"       = "#B7E3D6", # pale aqua
  "Sweden"             = "#E7D7A1", # gold (slightly brighter)
  "Switzerland"        = "#BFA3D6", # lavender-gray
  "Trinidad And Tobago"= "#D7BFAF"  # pale taupe
)

# Save as PDF
pdf(here("Figures/raw", "Figure_4b_chorddiagram.pdf"), width = 12, height = 9)

# Plot chord diagram
circos.clear()
circos.par(track.margin = c(0.01, 0.01), gap.degree = 1.5)

chordDiagram(net_matrix2_sorted,
             annotationTrack = "grid",
             annotationTrackHeight = c(0.04, 0.02),
             preAllocateTracks = 1,
             grid.col = cols)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim <- get.cell.meta.data("xlim")
  ylim <- get.cell.meta.data("ylim")
  sector.name <- get.cell.meta.data("sector.index")
  circos.text(mean(xlim), 
              ylim[1] + 0.3,
              sector.name,
              facing = "clockwise", 
              niceFacing = TRUE,
              adj = c(0, 0.5), 
              cex = 0.85)
  circos.axis(h = "bottom", labels = FALSE, major.tick = FALSE)
  #labels.cex = 0.5)
}, 
bg.border = NA)

#dev.off()   # close PNG
