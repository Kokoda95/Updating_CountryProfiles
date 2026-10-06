# ---------------------------------------------------------------------------
# "Timeline of Malaria Control Efforts" — a rebuild of the original graphic in R,
# extended from 2023 to 2026.
#
# Layout follows the original exactly: thin gold spine, gold diamonds, the year
# tucked in beside the diamond, event text in two fixed columns either side, and
# "NOW" at the foot. A row may carry text on the left, on the right, or on both.
#
# Only dependency: ggplot2.  The en-dash and the accent are written as \uXXXX
# escapes so the script renders identically whatever locale or file encoding R
# reads it under.
#
# To change the timeline, edit `ev` below — nothing beneath it needs touching.
# ---------------------------------------------------------------------------

library(ggplot2)

GOLD  <- "#F7C948"   # spine and diamonds
INK   <- "#1F2933"   # event text
MUTED <- "#6B7280"   # year labels and "NOW"

# --- the timeline ----------------------------------------------------------
#   left / right : event text for that side of the spine ("" for none)
#   yr_side      : which side of the diamond the year label sits on, "L" or "R"
#   face_left    : "plain" or "italic", for the left-hand label
#   Use \n inside a label to wrap it onto a second line.
ev <- data.frame(
  year = c("2006", "2010", "2012", "2014", "2015",
           "2017", "2019", "2020", "2022", "2023", "2024", "2026"),
  
  left = c(
    "",
    "Adoption of ACT for P. falciparum",            
    "Introduction of RDTs",
    "Introduction of IRS in high burden\nDistricts",
    "",
    "Introduction of Mass ITN Distribution",
    "",
    "Integration of malaria diagnosis in community-level RDTs",
    "Targeted interventions in urban malaria hotspots",
    "Pilot programs for digital health",
    "New ITN Distribution Models",
    "Interceptor G2 dual-AI nets introduced",
    "Introduction of Malaria Vaccine (R21, Zamb\u00e9zia)", 
    ""
  ),
  
  right = c(
    "NMCP reestablished",
    "", "", "",
    "Launch of Strategic Plan (2012\u20132016)",
    "",
    "Nationwide DHIS2 Implementation",
    "Launch of Strategic Plan (2017\u20132022)",
    "Malaria Fund Establishment",
    "",
    "SMC scale-up across Nampula districts",
    "Launch of Strategic Plan (2023\u20132030)",
    "",
    "Global Fund regional malaria grant"
  ),
  
  yr_side = c("R", "L", "L", "L", "R", "L", "R",
              "L", "R", "R", "R", "R", "R", "R"),
  
  face_left = c("plain", "plain", "plain", "plain", "plain", "plain", "plain",
                "plain", "plain", "plain", "italic", "plain", "plain", "plain"),
  
  stringsAsFactors = FALSE
)

# --- layout ----------------------------------------------------------------
n     <- nrow(ev)
ev$y  <- -seq_len(n)

YEAR_X <- 0.30  
TEXT_X <- 1.25  

yrs <- data.frame(
  x     = ifelse(ev$yr_side == "L", -YEAR_X, YEAR_X),
  y     = ev$y,
  label = ev$year,
  hj    = ifelse(ev$yr_side == "L", 1, 0)
)
lft <- subset(data.frame(y = ev$y, label = ev$left,  face = ev$face_left,
                         stringsAsFactors = FALSE), label != "")
rgt <- subset(data.frame(y = ev$y, label = ev$right, stringsAsFactors = FALSE),
              label != "")

p <- ggplot() +
  annotate("segment", x = 0, xend = 0, y = -0.45, yend = -(n + 0.55),
           colour = GOLD, linewidth = 0.9, lineend = "round") +
  geom_point(data = ev, aes(x = 0, y = y), shape = 23, size = 4.2,
             fill = GOLD, colour = GOLD, stroke = 0) +
  geom_text(data = yrs, aes(x = x, y = y, label = label, hjust = hj),
            size = 3.3, colour = MUTED) +
  geom_text(data = lft, aes(x = -TEXT_X, y = y, label = label, fontface = face),
            hjust = 1, size = 3.5, colour = INK, lineheight = 1.2) +
  geom_text(data = rgt, aes(x = TEXT_X, y = y, label = label),
            hjust = 0, size = 3.5, colour = INK, lineheight = 1.2) +
  annotate("text", x = 0, y = -(n + 1.15), label = "NOW",
           size = 4.6, colour = MUTED) +
  scale_x_continuous(limits = c(-7, 7),        expand = c(0, 0)) +
  scale_y_continuous(limits = c(-(n + 1.7), 0.6), expand = c(0, 0)) +
  ggtitle("Timeline of Malaria Control Efforts") +
  theme_void() +
  theme(
    plot.title  = element_text(face = "bold", size = 17, hjust = 0.5,
                               colour = INK, margin = margin(b = 18)),
    plot.margin = margin(16, 16, 10, 16)
  ) 

W <- 12
H <- 0.52 * n + 1.8

ggsave("C:/Users/HP/Desktop/Map's_Project/Country_Profile/Mozambique/Outputs/figure_04_timeline.png", p, width = W, height = H, dpi = 300, bg = "white")

ggsave("C:/Users/HP/Desktop/Map's_Project/Country_Profile/Mozambique/Outputs/figure_04_timeline.pdf", p, width = W, height = H, bg = "white",
       device = cairo_pdf)

cat("wrote figure_04_timeline.png / .pdf\n")
