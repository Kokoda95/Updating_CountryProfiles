# ---------------------------------------------------------------------------
#  HISTORY OF MALARIA IN MAINLAND TANZANIA (2006 - present)

# ---------------------------------------------------------------------------

library(ggplot2)

## ---- 0. OUTPUT ------------------------------------------------------------
out_dir <- "D:/TZ profile/Updating_CountryProfiles/TZA/Images"

## ---- 1. DATA  ----------------------------------------------------

prev <- data.frame(
  x      = c(2007.5, 2011.5, 2015.5, 2017, 2022, 2025),
  value  = c(18.1,   9.5,    14.8,   7.5,  8.1,  7.5),
  group  = c("U5",   "U5",   "U5",   "U5", "U5", "SAC"),
  survey = c("THMIS 2007-08", "THMIS 2011-12", "TDHS-MIS 2015-16",
             "TMIS 2017", "TDHS-MIS 2022", "SMPS 2025"),
  stringsAsFactors = FALSE
)

# 1b. Events: 
events <- data.frame(
  year = c(2007, 2007, 2008, 2009, 2010, 2013, 2013, 2014, 2015, 2015,
           2018, 2019, 2021, 2023,2012, 2022),
  text = c("ACT (AL) first-line\ntreatment implemented",
           "IRS starts in Kagera",
           "Malaria Strategic Plan\n2008\u20132013",
           "RDTs rolled out; under-5\nLLIN catch-up campaign",
           "Universal LLIN coverage\ncampaign (2010\u201311)",
           "School Net Programme\n(Southern Zone)",
           "DHIS2 adopted",
           "Malaria Strategic Plan\n2014\u20132020",
           "First SMPS; DHIS2\nnationwide",
           "Biolarvicide plant\n(Kibaha) established",
           "Council-level risk\nstratification",
           "\u201cZero Malaria Starts\nwith Me\u201d launched",
           "NMSP 2021\u20132025",
           "End Malaria Council\nformed",
           "Census: Mainland\npop. 43.6 million",
           "Census: Mainland\npop. 59.9 million"),
  type = c("Policy", "Intervention", "Policy", "Intervention", "Intervention",
           "Intervention", "Data", "Policy", "Data", "Intervention",
           "Policy", "Policy", "Policy", "Policy", "Data", "Data"),
  stringsAsFactors = FALSE
)

## ---- 2. STYLE -----------------------------------------------------------------
NAVY  <- "#1F5C7A"; AMBER <- "#D98E04"; INK <- "#1F2933"
TYPE_COL <- c(Policy = "#C0392B", Intervention = "#3A8F5C", Data = "#8A5CA8")
X_MIN <- 2005.5; X_MAX <- 2026.5
STEM0  <- 0.25     # stems start this far above the axis
STEM_K <- 0.13     # stem height per 1% prevalence
CHAR_W <- 0.13     # approx. width of one character, in years (for lane packing)
LANE_H <- 0.85     # vertical distance between event lanes

## ---- 3. LAYOUT (no need to edit below) ----------------------------------------
prev$ytop <- STEM0 + prev$value * STEM_K
prev$lab  <- paste0(prev$value, "%")

# pack events into lanes
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

## ---- 4. PLOT -------------------------------------------------------------------
p <- ggplot() +
  # time axis
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
  geom_point(data = prev, aes(x = x, y = ytop, fill = group, size = value),
             shape = 21, colour = "white", stroke = 1.2) +
  geom_text(data = prev, aes(x = x, y = ytop, label = lab),
            colour = "white", fontface = "bold", size = 3.5) +
  geom_text(data = prev, aes(x = x, y = ytop + 0.42, label = survey),
            colour = "grey35", size = 2.9) +
  # events: leader line, dot on the axis, text card
  geom_segment(data = events, aes(x = year, xend = year, y = -0.08, yend = y + 0.2),
               colour = "grey65", linewidth = 0.4) +
  geom_point(data = events, aes(x = year, y = 0, colour = type), size = 3.2) +
  geom_point(data = events, aes(x = year, y = 0), colour = "white", size = 1.2) +
  geom_point(data = events, aes(x = year, y = y + 0.2, colour = type), size = 2) +
    # white label background hides any leader line passing behind the text
  geom_label(data = events, aes(x = year + 0.08, y = y + 0.24, label = text),
             hjust = 0, vjust = 1, size = 3.3, colour = INK, lineheight = 0.95,
             fill = "white", label.size = 0, label.padding = unit(0.08, "lines")) +
  # scales
  scale_fill_manual(values = c(U5 = NAVY, SAC = AMBER), breaks = c("U5", "SAC"),
                    labels = c("Children 6\u201359 months, RDT (DHS/MIS)",
                               "School children 5\u201316 (SMPS)"),
                    name = "Malaria prevalence") +
  scale_colour_manual(values = TYPE_COL, name = "Event") +
  scale_size_continuous(range = c(15, 19), guide = "none") +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 6)),
         colour = guide_legend(order = 2)) +
  coord_cartesian(xlim = c(X_MIN, X_MAX + 0.3),
                  ylim = c(min(events$y) - 0.8, max(prev$ytop) + 0.8),
                  clip = "off") +
  labs(title = "History of Malaria in Mainland Tanzania, 2007\u2013present",
       caption = paste0("Prevalence (Mainland): ", paste(prev$survey, collapse = "; "),
                        ". Population: NBS Population and Housing Census 2012, 2022. Events: NMCP/NMSP documents and published literature.")) +
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

## ---- 5. SAVE -------------------------------------------------------------------
W <- 14; H <- 7.5
ggsave(file.path(out_dir, "tza_timeline.png"), p, width = W, height = H, dpi = 300, bg = "white")
ggsave(file.path(out_dir, "tza_timeline.pdf"), p, width = W, height = H, bg = "white",
       device = cairo_pdf)
cat("wrote tza_timeline.png / .pdf to", out_dir, "\n")
