# --------------------------------------------------- #
# Author: Catalina Torres & Marius D. PASCARIU
# License: GNU General Public License v3.0
# Last update: Fri Mar 01 10:34:19 2019
# --------------------------------------------------- #
remove(list = ls())

library(tidyverse)
library(MortalityLaws)

# Code for downloading the data

# K = "FRATNP"
# I = "1x10"
# U = "..."
# P = "..."
# 
# LTF <- ReadHMD(what = "LT_f",
#                countries = K, interval = I, username = U, password = P, save = T)
# LTM <- ReadHMD(what = "LT_m",
#                countries = K, interval = I, username = U, password = P, save = T)

# Load data
load("EGPA/manuscript/data/HMD_LT_f.Rdata")
load("EGPA/manuscript/data/HMD_LT_m.Rdata")

lt_f <- HMD_LT_f$data
lt_m <- HMD_LT_m$data

years <- c("1900-1909", "1950-1959", "2010-2016")


mx_f <- lt_f %>%
  filter(Year %in% years) %>%
  select(country, Year, Age, mx) %>%
  mutate(Sex = "FEMALES",
         Age = as.numeric(as.character(Age)),
         Year = as.character(Year))

mx_m <- lt_m %>%
  filter(Year %in% years) %>%
  select(country, Year, Age, mx) %>%
  mutate(Sex = "MALES",
         Age = as.numeric(as.character(Age)),
         Year = as.character(Year))




x_breaks <- c(0, 30, 60, 90, 110)
x_labs <- x_breaks
y_breaks <- log(c(.0001, .001, .01, .1, 1))
y_labs <- y_breaks %>% exp() %>% round(4) %>% format(width = 4, scientific = F)

mx_plot <- rbind(mx_f, mx_m) %>%
  mutate(Age = as.numeric(as.character(Age)),
         Year = as.character(Year)) %>%
  ggplot() +
  geom_line(aes(x = Age, y = log(mx), linetype = Year), size = 0.7, color = 1) +
  facet_grid(~ Sex) +
  scale_y_continuous(breaks = y_breaks, labels = y_labs) +
  scale_x_continuous(breaks = x_breaks, labels = x_labs) +
  scale_linetype_manual(name = "Time interval",
                        labels = c("1900 - 09", "1950 - 59", "2010 - 16"),
                        values = c(3, 2, 1)) +
  guides(linetype = guide_legend(override.aes = list(size = 2))) +
  theme_bw() +
  theme(legend.text  = element_text(colour = 1, size = 15),
        legend.title = element_text(colour = 1, size = 18, hjust = 0.5),
        legend.position = "right",
        legend.key.width = unit(1, "cm"),
        strip.text  = element_text(colour = 1, size = 14),
        axis.text.x  = element_text(colour = 1, size = 18),
        axis.text.y  = element_text(colour = 1, size = 14),
        axis.title   = element_text(colour = 1, size = 18, face = 'bold'),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.line.x  = element_line(linetype = 1),
        axis.ticks   = element_line(size = 1),
        panel.spacing.x = unit(.7, "cm"),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(colour = 1, size = 13)) +
  labs(caption = "Note: Data presented on a logarithmic scale",
     x = "Age (Years)", y = "Age-specific death rate\n")
  # annotate("segment", x = 12, xend = 40, y = log(1), yend = log(1),
  #          arrow = arrow(angle = 20), alpha = 0.2) + # horizontal
  # annotate("segment", x = 8, xend = 8, y = log(0.8), yend = log(0.1),
  #          arrow = arrow(angle = 20), alpha = 0.2) + # vertical
  # annotate("segment", x = 12, xend = 40, y = log(0.8), yend = log(0.1),
  #          arrow = arrow(angle = 20), alpha = 0.2) # diagonal

mx_plot

file <- paste0(getwd(), "/EGPA/manuscript/FoM_figure.pdf")
pdf(file, width = 13, height = 6)
print(mx_plot)
dev.off()



