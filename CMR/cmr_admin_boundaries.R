## ==========================================================================
##  CAMEROON - ADMINISTRATIVE BOUNDARIES MAP
##  Health districts coloured by region, region boundaries + labels,
##  capital city, neighbouring countries, north arrow, scale bar and an
##  Africa locator inset + legend placed inside the map frame.
##  Adapted from the Tanzania script (../Administartive_Boundaries.R).
##
##  Input : Shapefiles/2025/Districts.shp  (WGS84, 205 health districts)
##  Output: Images/cmr_admin_boundaries.png / .pdf
## ==========================================================================

## ---- 0. PACKAGES ---------------------------------------------------------
required_pkgs <- c("sf", "dplyr", "ggplot2", "ggspatial",
                   "rnaturalearth", "rnaturalearthdata")
new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) install.packages(new_pkgs, dependencies = TRUE)

suppressMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(ggspatial)
  library(rnaturalearth)
})
sf_use_s2(FALSE)   # planar unions avoid slivers between district polygons

## ---- 1. WORKING DIRECTORY ------------------------------------------------
wd <- "D:/Updating_CountryProfiles/CMR"
setwd(wd)

## ---- 2. READ DATA --------------------------------------------------------
adm2 <- st_read("Shapefiles/2025/Districts.shp", quiet = TRUE) %>%
  st_make_valid() %>%
  st_transform(4326)

# English region names (as used in the profile text)
region_en <- c("Adamaoua"     = "Adamawa",
               "Centre"       = "Centre",
               "Est"          = "East",
               "Extreme Nord" = "Far North",
               "Littoral"     = "Littoral",
               "Nord"         = "North",
               "Nord-Ouest"   = "North-West",
               "Ouest"        = "West",
               "Sud"          = "South",
               "Sud Ouest"    = "South-West")
stopifnot(all(adm2$nom_region %in% names(region_en)))
adm2$Region <- unname(region_en[adm2$nom_region])

# Regions = dissolved districts. A tiny buffer out/in closes sliver gaps
# between district polygons so no phantom inner boundaries are drawn.
close_gaps <- function(g, d = 0.002) st_buffer(st_buffer(g, d), -d)

adm1 <- adm2 %>%
  group_by(Region) %>%
  summarise(geometry = close_gaps(st_union(geometry)), .groups = "drop")

cmr_outline <- close_gaps(st_union(adm2))

# Neighbouring countries for context
africa <- ne_countries(continent = "Africa", scale = "medium", returnclass = "sf") %>%
  st_make_valid()
neighbours <- africa %>% filter(admin != "Cameroon")

# Ocean = map extent minus Africa (so border mismatches show as land, not sea)
ocean <- st_difference(
  st_as_sfc(st_bbox(c(xmin = 7, ymin = 0.5, xmax = 23, ymax = 14), crs = st_crs(4326))),
  st_union(africa)
)

## ---- 3. DISTRICT COUNTS & COLOURS ----------------------------------------
adm1_lookup <- adm2 %>%
  st_drop_geometry() %>%
  count(Region, name = "n_districts") %>%
  arrange(Region) %>%
  mutate(legend_label = paste0(Region, " (", n_districts, ")"))

# Gold / grey / black theme, assigned so that bordering regions contrast
fill_values <- c("Adamawa"    = "#FBEBB5",
                 "Centre"     = "#D4AF37",
                 "East"       = "#9E9E9E",
                 "Far North"  = "#D4AF37",
                 "Littoral"   = "#D9D9D9",
                 "North"      = "#8C6D1F",
                 "North-West" = "#E6C77A",
                 "South"      = "#8C6D1F",
                 "South-West" = "#5A5A5A",
                 "West"       = "#2B2B2B")
stopifnot(all(adm1_lookup$Region %in% names(fill_values)))

adm2$Region <- factor(adm2$Region, levels = adm1_lookup$Region)

## ---- 4. LABEL POSITIONS --------------------------------------------------
adm1_labels <- adm1 %>%
  st_point_on_surface() %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2])

# small manual nudges where the surface point sits awkwardly
nudge <- c("Littoral" = -0.15, "South-West" = -0.2, "North-West" = 0, "West" = 0)
adm1_labels$lon <- adm1_labels$lon + ifelse(adm1_labels$Region %in% names(nudge),
                                            nudge[adm1_labels$Region], 0)

country_labels <- data.frame(
  label = c("NIGERIA", "CHAD", "CENTRAL AFRICAN\nREPUBLIC", "CONGO", "GABON",
            "EQUATORIAL\nGUINEA", "Gulf of Guinea"),
  lon   = c(10.2,  16.4,  17.9, 15.3, 11.8, 9.9, 8.6),
  lat   = c(8.6,  10.6,   8.0,  1.6,  1.55, 1.55, 3.0)
)

## ---- 5. CITIES -----------------------------------------------------------
capital  <- st_as_sf(data.frame(name = "Yaound\u00e9", lon = 11.5021, lat = 3.8480),
                     coords = c("lon", "lat"), crs = 4326)
big_city <- st_as_sf(data.frame(name = "Douala", lon = 9.7679, lat = 4.0511),
                     coords = c("lon", "lat"), crs = 4326)

## ---- 6. MAIN MAP ---------------------------------------------------------
bg_col     <- "#C9C6BD"
sea_col    <- "#A9CCE3"
land_col   <- "#E4E1D8"
admin1_col <- "#D4AF37"
admin2_col <- "#2B2B2B"

# Extent: tight around Cameroon; inset sits over NE Nigeria, legend over CAR
xlim_main <- c(8.1, 19.9)
ylim_main <- c(1.3, 13.4)

main_map <- ggplot() +
  geom_sf(data = neighbours, fill = land_col, color = "grey55", linewidth = 0.3) +
  geom_sf(data = ocean, fill = sea_col, color = NA) +
  geom_sf(data = adm2, aes(fill = Region), color = admin2_col, linewidth = 0.12) +
  geom_sf(data = adm1, fill = NA, color = admin1_col, linewidth = 1.0) +
  geom_sf(data = cmr_outline, fill = NA, color = "black", linewidth = 0.6) +
  geom_text(data = country_labels, aes(x = lon, y = lat, label = label),
            color = "grey40", size = 3, fontface = "italic", lineheight = 0.9) +
  geom_label(data = adm1_labels, aes(x = lon, y = lat, label = Region),
             color = "white", fill = "grey30", alpha = 0.6,
             fontface = "bold", size = 3.5, linewidth = 0,
             label.padding = unit(0.15, "lines"), label.r = unit(0.05, "lines")) +
  geom_sf(data = big_city, shape = 21, fill = "white", color = "black", size = 2.4) +
  geom_sf_text(data = big_city, aes(label = name), nudge_x = -0.45, nudge_y = 0.12,
               size = 3.2, fontface = "bold") +
  geom_sf(data = capital, shape = 23, fill = "red", color = "black", size = 3.4) +
  geom_sf_text(data = capital, aes(label = name), nudge_y = -0.25,
               size = 3.4, fontface = "bold") +
  scale_fill_manual(values = fill_values, drop = FALSE) +
  scale_x_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  coord_sf(xlim = xlim_main, ylim = ylim_main, expand = FALSE) +
  annotation_scale(location = "br", width_hint = 0.2,
                   bar_cols = c("black", "white"),
                   pad_x = unit(0.04, "npc"), pad_y = unit(0.03, "npc")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1.4, "cm"), width = unit(1.4, "cm"),
                         pad_x = unit(0.03, "npc"), pad_y = unit(0.03, "npc")) +
  labs(x = NULL, y = NULL,
       title = "Administrative Map of Cameroon",
       subtitle = "Regions and health districts (2025)") +
  theme_bw(base_size = 12) +
  theme(
    panel.background = element_rect(fill = land_col, color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 1.3),
    panel.grid.major = element_line(color = "grey55", linewidth = 0.3),
    plot.background  = element_rect(fill = bg_col, color = NA),
    plot.margin      = margin(10, 10, 6, 6),
    plot.title       = element_text(face = "bold", size = 20, hjust = 0.5,
                                    margin = margin(b = 2)),
    plot.subtitle    = element_text(size = 13, hjust = 0.5, colour = "grey25",
                                    margin = margin(b = 8)),
    legend.position  = "none"
  )

## ---- 7. AFRICA LOCATOR INSET (vector, no internet needed) -----------------
inset_map <- ggplot() +
  geom_sf(data = africa, fill = "#EDE7D9", color = "grey40", linewidth = 0.15) +
  geom_sf(data = cmr_outline, fill = "red", color = "black", linewidth = 0.4) +
  coord_sf(xlim = c(-19, 53), ylim = c(-36, 38), expand = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        plot.background  = element_rect(fill = "aliceblue", color = "black", linewidth = 0.8),
        plot.margin      = margin(2, 2, 2, 2))

## ---- 8. LEGEND -----------------------------------------------------------
leg_df <- adm1_lookup %>%
  mutate(i = row_number() - 1, x = 0, y = 7 - i * 0.75)

legend_plot <- ggplot() +
  annotate("text", x = -0.15, y = 11.4, label = "Legend", hjust = 0, size = 5.5,
           fontface = "bold") +
  annotate("point", x = 0, y = 10.5, shape = 23, fill = "red", color = "black", size = 3) +
  annotate("text",  x = 0.35, y = 10.5, label = "Capital city", hjust = 0, size = 3.6) +
  annotate("point", x = 0, y = 9.8, shape = 21, fill = "white", color = "black", size = 2.4) +
  annotate("text",  x = 0.35, y = 9.8, label = "Major city", hjust = 0, size = 3.6) +
  annotate("segment", x = -0.14, xend = 0.14, y = 9.1, yend = 9.1,
           color = admin1_col, linewidth = 1.1) +
  annotate("text",  x = 0.35, y = 9.1, label = "Region boundary", hjust = 0, size = 3.6) +
  annotate("segment", x = -0.14, xend = 0.14, y = 8.4, yend = 8.4,
           color = admin2_col, linewidth = 0.3) +
  annotate("text",  x = 0.35, y = 8.4, label = "Health district boundary", hjust = 0, size = 3.6) +
  annotate("text",  x = -0.15, y = 7.65, label = "Region (no. of districts)", hjust = 0,
           size = 3.8, fontface = "bold") +
  geom_tile(data = leg_df, aes(x = x, y = y, fill = Region),
            width = 0.3, height = 0.5, color = "grey30", linewidth = 0.2,
            show.legend = FALSE) +
  geom_text(data = leg_df, aes(x = x + 0.3, y = y, label = legend_label),
            hjust = 0, size = 3.5) +
  scale_fill_manual(values = fill_values) +
  coord_cartesian(xlim = c(-0.3, 3.2), ylim = c(-0.3, 12), expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
        plot.margin     = margin(6, 6, 6, 6))

## ---- 9. COMBINE ----------------------------------------------------------
final_map <- main_map +
  annotation_custom(ggplotGrob(inset_map),
                    xmin = 8.25, xmax = 11.3, ymin = 10.1, ymax = 13.3) +
  annotation_custom(ggplotGrob(legend_plot),
                    xmin = 16.55, xmax = 19.8, ymin = 2.1, ymax = 7.5)

print(final_map)

## ---- 10. EXPORT ----------------------------------------------------------
out_dir <- file.path(wd, "Images")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(file.path(out_dir, "cmr_admin_boundaries.png"),
       plot = final_map, width = 11, height = 12, dpi = 300, bg = bg_col)
ggsave(file.path(out_dir, "cmr_admin_boundaries.pdf"),
       plot = final_map, width = 11, height = 12, bg = bg_col)

cat("\nMap exported to:", normalizePath(out_dir), "\n")
