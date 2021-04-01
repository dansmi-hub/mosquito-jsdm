## Load Willy Exported Data

library(tidyverse)
library(skimr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# File directory to save the plots
path <- file.path("reports/figures/willy-exported-points/")

# World map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Tue Mar 23 12:47:00 2021 ------------------------------

data <- read_csv("data/raw/ExportedReportData-2019-09-23.csv")

skimr::skim(data)

# All species over 500 numbers
count(data, VectorSpecies) %>% filter(n > 500)

# All Aedes species and over 100 records
count(data, VectorSpecies) %>% filter(str_detect(VectorSpecies, "Aedes") &
                                        n > 100)

# All Anopholes species and over 100 records
count(data, VectorSpecies) %>% filter(str_detect(VectorSpecies, "Ano") &
                                        n > 100)

# All Culex/Culiseta species and over 100 records
count(data, VectorSpecies) %>% filter(str_detect(VectorSpecies, "Cu") &
                                        n > 100)

# Tue Mar 23 12:49:21 2021 ------------------------------

# Data must have geo coords
data_sf <- data %>% filter(!is.na(longitude) & !is.na(latitude))

# Backup coords as well
data_sf <- data_sf %>% mutate(lat = latitude, lon = longitude)


# Turn this into an sf object>
data_sf <- st_as_sf(
  x = data_sf,
  coords = c("longitude", "latitude"),
  crs = st_crs(world)
)
# Same CRS?
# st_crs(data_sf) = st_crs(world)
st_transform(data_sf, st_crs(world))


# Quick density plot
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = data_sf, aes(colour = as.factor(vectorid))) +
  coord_sf(xlim = c(-16, 61), ylim = c(24, 67))


# Tue Mar 23 14:46:59 2021 ------------------------------

# Try a different CRS? EPSG:3035 linear and in SI units (m)

data_3035 <- st_transform(data_sf, 3035)
world_3035 <- st_transform(world, 3035)


# Quick density plot
ggplot() +
  geom_sf(data = world_3035) +
  geom_sf(data = data_3035, aes(colour = as.factor(VectorSpecies))) +
  coord_sf(
    xlim = c(1651007 + 100000, 7567579 + 100000),
    ylim = c(525513.3 + 100000, 4878584 + 100000),
    crs = 3035
  )


# Common species n > 100
common_species <-
  data_3035 %>% count(VectorSpecies) %>% filter(n > 100) %>% pull(VectorSpecies)
data_3035_filtered <-
  data_3035 %>% filter(VectorSpecies %in% common_species)

# Quick density plot common species
ggplot() +
  geom_sf(data = world_3035) +
  geom_sf(data = data_3035_filtered, aes(colour = as.factor(VectorSpecies))) +
  coord_sf(
    xlim = c(1651007 + 100000, 7567579 + 100000),
    ylim = c(525513.3 + 100000, 4878584 + 100000),
    crs = 3035
  )

# Tue Mar 23 15:09:33 2021 ------------------------------

# Now can we distinguish between present and absent plots?

# The status of a species recorded here
data_3035_filtered$statusterm %>% unique()

# Refactoring
data_sf_refactor_3035 <- data_3035_filtered %>%
  mutate(
    newstatus = case_when(
      statusterm == "Present" ~ "Present",
      # Self explanatory
      statusterm == "Established" ~ "Present",
      # Is present
      statusterm == "Unknown" ~ "Other",
      # Be safe and put NA for this
      statusterm == "Introduced" ~ "Present",
      # Same thing as present
      statusterm == "Yes" ~ "Present",
      # Self explanatory
      statusterm == "Absent" ~ "Absent",
      # Self explanatory
      statusterm == "Confirmed Absent" ~ "Absent",
      # The same thing
      statusterm == "Anticipated Absent" ~ "Absent",
      # Assume these are True
      TRUE ~ "Other"
      # For any left over terms they'll remain unchanged)
    )
  )

# How many P vs A for each species?
data_sf_refactor_3035 %>% pull(newstatus) %>% table


# Tue Mar 23 16:45:55 2021 ------------------------------

# A bar chart for PvsA for each species?
bar_plot <- data_sf_refactor_3035 %>%
  filter(newstatus != "Other") %>%
  group_by(VectorSpecies) %>%
  count(newstatus) %>%
  ggplot(aes(
    x = reorder(VectorSpecies,-n),
    y = n,
    fill = newstatus
  )) +
  geom_col() +
  ylab(NULL) +
  xlab(NULL) +
  see::theme_modern() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 8
    ),
    legend.title = element_blank()
  )

ggsave(
  bar_plot,
  file = paste0(path, "count_PvsA.png"),
  width = 14,
  height = 10,
  units = "cm",
  dpi = 300
)


# Tue Mar 23 16:50:28 2021 ------------------------------

# Now for each species PvsA with a sapply call
PA <- sapply(common_species, function(x) {
  data_sf_refactor_3035 %>%
    filter(VectorSpecies == x) %>%
    pull(newstatus) %>%
    table()
})

# Gives a list of all the factor frequencies for the dataset (P vs A vs Other)

# From these results we should select species that have a decent number of
# presence and absence points. Presence points are default and more common than
# absence points so set a limit of absence n > 100?

# If Absent is present and Freq > 100 -> Return the name of the species (index)
over_100_absence <- lapply(PA, function(x) {
  df <- as.data.frame(x)
  n <- df %>% filter(. == "Absent") %>% pull(Freq)
  ifelse(n >= 50, TRUE, FALSE)
}) %>% flatten()

# Discard any species without 100 absences or more
spp_with_50_absences <-
  discard(over_50_absence, isFALSE) %>% names()

# If Present is TRUE and Freq > 100 -> Return the name of the species (index)
over_50_Present <- lapply(PA, function(x) {
  df <- as.data.frame(x)
  n <- df %>% filter(. == "Present") %>% pull(Freq)
  ifelse(n >= 50, TRUE, FALSE)
}) %>% flatten()

# Discard any species without 100 absences or more
spp_with_100_presence <-
  discard(over_50_Present, isFALSE) %>% names()

# What names appear in both lists?
robust_species <-
  intersect(spp_with_100_absences, spp_with_100_presence)

# Tue Mar 23 15:46:15 2021 ------------------------------

# Lets try and plot stuff with these species and determine their PA on maps
PA_points <-
  data_sf_refactor_3035 %>% filter(VectorSpecies %in% robust_species &
                                     newstatus != "Other")


# Now to do that for individual species in a loop:
# list of values to loop over
uniq_species = unique(PA_points$VectorSpecies)

# Loop

for (i in uniq_species) {
  temp_plot = ggplot() +
    geom_sf(data = world_3035, fill = "antiquewhite") +
    geom_sf(
      data = subset(PA_points, VectorSpecies == i),
      aes(colour = newstatus),
      size = 1,
      alpha = 0.4,
      shape = 21
    ) +
    coord_sf(
      xlim = c(1651007 + 100000, 7567579 + 100000),
      ylim = c(525513.3 + 100000, 4878584 + 100000),
      crs = 3035
    ) +
    ggtitle(paste0(i)) +
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "aliceblue"))
  
  
  ggsave(
    temp_plot,
    file = paste0(path, tolower(str_squish(i)), ".png"),
    width = 14,
    height = 10,
    units = "cm",
    dpi = 300
  )
}


# Tue Mar 23 17:07:24 2021 ------------------------------

# PDF only version
pdf(file = paste0(path, "_allplots.pdf"),
    width = 11.7,
    height = 8.3)

for (i in uniq_species) {
  pdfplot <- ggplot() +
    geom_sf(data = world_3035, fill = "antiquewhite") +
    geom_sf(
      data = subset(PA_points, VectorSpecies == i),
      aes(colour = newstatus),
      size = 1,
      # alpha = 0.4,
      shape = 21
    ) +
    coord_sf(
      xlim = c(1651007 + 100000, 7567579 + 100000),
      ylim = c(525513.3 + 100000, 4878584 + 100000),
      crs = 3035
    ) +
    ggtitle(paste0(i)) +
    theme(legend.title = element_blank(),
          panel.background = element_rect(fill = "aliceblue"))
  
  plot(pdfplot)
}

dev.off()

# Tue Mar 23 16:22:25 2021 ------------------------------

# Generate summary tables for these species to put into opendoc

PA_points %>%
  count(VectorSpecies, newstatus) %>%
  as.data.frame() %>%
  select(-geometry) %>%
  pivot_wider(names_from = newstatus,
              values_from = n,
              values_fill = 0) %>%
  arrange(desc(Absent)) %>%
  write.table("/tmp/misctable.txt", sep = ";")
