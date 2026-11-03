# ---------------------------------------------------------------------------
#  HISTORY OF MALARIA IN CAMEROON (2006 - present)
#  Adapted from the Tanzania timeline script (../Timelinescripta.R).
#  Output: Images/cmr_timeline.png / .pdf
#
#  Sources for every value/event are listed next to it (see also the
#  figure caption in profile_cmr.md).
# ---------------------------------------------------------------------------

library(ggplot2)

## ---- 0. OUTPUT ------------------------------------------------------------
out_dir <- "D:/Updating_CountryProfiles/CMR/Images"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## ---- 1. DATA --------------------------------------------------------------
# Malaria prevalence, children 6-59 months, RDT (DHS Program API, national)
#   CM2011DHS 30.0% (27.4-32.5) | CM2018DHS 24.0% (21.3-26.6; fieldwork
#   Jun 2018-Jan 2019) | CM2022MIS 26.2% (22.9-29.4)
prev <- data.frame(
  x      = c(2011, 2018.5, 2022),
  value  = c(30.0, 24.0,   26.2),
  survey = c("DHS 2011", "DHS 2018", "MIS 2022"),
  stringsAsFactors = FALSE
)

events <- data.frame(
  year = c(2007, 2007, 2011, 2011, 2011, 2012, 2014,
           2015, 2015, 2016, 2017, 2019, 2019, 2022, 2022, 2023, 2024,
           2024, 2025, 2026),
  text = c(
    "NMSP 2007\u20132010",                                # UVU / PNLP
    "IPTp-SP introduced\nin ANC",                         # Severe Malaria Observatory (CMR page)
    "1st mass LLIN campaign\n(~8 million nets)",          # PMC6815446
    "Free treatment of uncomplicated\nmalaria for children <5", # PNLP guidelines
    "NMSP 2011\u20132015",                                # Sante tropicale; PSNLP 2024-28
    "IPTp increased to\n\u22653 doses",                   # Malaria J 2020 (s12936-020-03155-2)
    "NMSP 2014\u20132018",                                # GF concept note 2014
    "DHIS2 adopted for\nroutine reporting",               # CHAI/NMCP preprint (since 2015)
    "2nd mass LLIN campaign\n(2015\u201316, ~12 million nets)", # PMC6815446
    "SMC introduced in\nNorth & Far North",               # PMC10640753; SMC Alliance
    "Became a PMI\nfocus country",                        # USAID press release 21 Sep 2017
    "3rd mass LLIN campaign\n(PBO nets in some areas)",   # PMC13032020
    "NMSP 2019\u20132023",                                # PNLP annual report 2023
    "PMC piloted in 6\nCentre-region districts",          # PSI Plus Project; F1000 14-908
    "4th mass LLIN campaign\n(2022\u201323; PBO & dual-AI)", # AMP minutes 2021; GiveWell
    "UHC phase 1: free malaria\ncare for children <5",    # MINSANTE; PMI profile FY24
    "RTS,S vaccine in routine EPI\n(42 districts; world first)", # PMC11002407; Gavi
    "NMSP 2024\u20132028",                                # PSNLP 2024-2028
    "5th mass campaign:\n17.4 million dual-AI nets",      # MINSANTE / PNLP 2025
    "District stratification\nupdated (2020\u201324 data)"  # profile section 8
  ),
  type = c("Policy", "Intervention", "Intervention", "Policy", "Policy",
           "Policy", "Policy", "Data", "Intervention", "Intervention",
           "Policy", "Intervention",
           "Policy", "Intervention", "Intervention", "Policy", "Intervention",
           "Policy", "Intervention", "Data"),
  stringsAsFactors = FALSE
)

## ---- 2. STYLE -------------------------------------------------------------
NAVY  <- "#1F5C7A"; INK <- "#1F2933"
TYPE_COL <- c(Policy = "#C0392B", Intervention = "#3A8F5C", Data = "#8A5CA8")
X_MIN <- 2005.5; X_MAX <- 2027
STEM0  <- 0.25     # stems start this far above the axis
STEM_K <- 0.10     # stem height per 1% prevalence
CHAR_W <- 0.15     # approx. width of one character, in years (lane packing)
LANE_H <- 0.85     # vertical distance between event lanes

## ---- 3. LAYOUT (no need to edit below) -------------------------------------
prev$ytop <- STEM0 + prev$value * STEM_K
prev$lab  <- paste0(prev$value, "%")

events <- events[order(events$year), ]
longest <- sapply(strsplit(events$text, "\n"), function(s) max(nchar(s)))
events$x_end <- events$year + 0.15 + longest * CHAR_W
lane_end <- numeric(0)
events$lane <- NA
for (i in seq_len(nrow(events))) {
  free <- which(lane_end < events$year[i] - 0.05)
  if (length(free) == 0) { lane_end <- c(lane_end, -Inf); free <- length(lane_end) }
  events$lane[i] <- free[1]
  lane_end[free[1]] <- events$x_end[i]
}
events$y <- -0.55 - (events$lane - 1) * LANE_H

ticks <- data.frame(x = seq(2006, 2026, 1))
ticks$lab <- ifelse(ticks$x %% 2 == 0, ticks$x, "")

## ---- 4. PLOT --------------------------------------------------------------
p <- ggplot() +
  annotate("segment", x = X_MIN, xend = X_MAX, y = 0, yend = 0,
           colour = NAVY, linewidth = 3, lineend = "butt",
           arrow = arrow(length = unit(0.18, "inches"), type = "closed")) +
  geom_segment(data = ticks, aes(x = x, xend = x, y = -0.07, yend = 0.07),
               colour = "white", linewidth = 0.6) +
  geom_text(data = ticks, aes(x = x, y = 0.16, label = lab),
            size = 3.6, fontface = "bold", colour = NAVY, vjust = 0) +
  # prevalence
  geom_segment(data = prev, aes(x = x, xend = x, y = 0.45, yend = ytop),
               colour = "grey55", linewidth = 0.8) +
  geom_point(data = prev, aes(x = x, y = ytop, fill = "U5", size = value),
             shape = 21, colour = "white", stroke = 1.2) +
  geom_text(data = prev, aes(x = x, y = ytop, label = lab),
            colour = "white", fontface = "bold", size = 3.5) +
  geom_text(data = prev, aes(x = x, y = ytop + 0.45, label = survey),
            colour = "grey35", size = 2.9) +
  # events
  geom_segment(data = events, aes(x = year, xend = year, y = -0.08, yend = y + 0.2),
               colour = "grey65", linewidth = 0.4) +
  geom_point(data = events, aes(x = year, y = 0, colour = type), size = 3.2) +
  geom_point(data = events, aes(x = year, y = 0), colour = "white", size = 1.2) +
  geom_point(data = events, aes(x = year, y = y + 0.2, colour = type), size = 2) +
  geom_label(data = events, aes(x = year + 0.08, y = y + 0.24, label = text),
             hjust = 0, vjust = 1, size = 3.2, colour = INK, lineheight = 0.95,
             fill = "white", linewidth = 0, label.padding = unit(0.08, "lines")) +
  scale_fill_manual(values = c(U5 = NAVY),
                    labels = c("Children 6\u201359 months, RDT (DHS/MIS)"),
                    name = "Malaria prevalence") +
  scale_colour_manual(values = TYPE_COL, name = "Event") +
  scale_size_continuous(range = c(15, 18), guide = "none") +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 6)),
         colour = guide_legend(order = 2)) +
  coord_cartesian(xlim = c(X_MIN, X_MAX + 0.3),
                  ylim = c(min(events$y) - 0.8, max(prev$ytop) + 0.8),
                  clip = "off") +
  labs(title = "History of Malaria in Cameroon, 2006\u2013present",
       caption = paste0(
         "Prevalence: Cameroon DHS 2011, DHS 2018, MIS 2022 (RDT, children 6\u201359 months). ",
         "Events: NMCP/PNLP strategic plans and reports, MINSANTE, Gavi/WHO, and published literature.")) +
  theme_void(base_size = 12) +
  theme(plot.title      = element_text(face = "bold", size = 17, hjust = 0.5,
                                       colour = INK, margin = margin(b = 8)),
        plot.caption    = element_text(size = 8.5, colour = "grey35", hjust = 0),
        legend.position = "top",
        legend.box      = "horizontal",
        legend.title    = element_text(face = "bold", size = 10),
        legend.text     = element_text(size = 9.5),
        plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(12, 20, 10, 20))

## ---- 5. SAVE --------------------------------------------------------------
W <- 14; H <- 8
ggsave(file.path(out_dir, "cmr_timeline.png"), p, width = W, height = H, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "cmr_timeline.pdf"), p, width = W, height = H, bg = "white",
       device = cairo_pdf)
cat("wrote cmr_timeline.png / .pdf to", out_dir, "\n")
