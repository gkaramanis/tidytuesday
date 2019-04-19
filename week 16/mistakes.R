library(tidyverse)
library(cowplot)

trade <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")
trade <- trade %>%
  mutate(in_billions = - trade_deficit / 1e9, employment = manufacture_employment/1e5 )

# "predictions"
model <- lm(in_billions ~ year, data = trade)
model2 <- lm(employment ~ year, data = trade)
newYears <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
newYears$trade_deficit <- predict(model, newdata = newYears)
newYears$employment <- predict(model2, newdata = newYears)

p1 <- ggplot() +
  # plot "predictions" first
  geom_line(data = newYears, aes(year, trade_deficit),
            linetype="dotted", size = 1, alpha = 0.4, color = "red") +
  geom_line(data = trade, aes(year, in_billions),
            size = 1, color = "#1380A1") +
  ylim(260, 400) +
  xlim (2010, 2016) +
  # reduction arrow
  geom_curve(aes(x = 2015, y = 360, xend = 2015.8, yend = 320), 
             colour = "#555555", size = 0.5, curvature = 0.5, 
             arrow = arrow(length = unit(0.03, "npc"), ends = "last")) +
  geom_label(aes(x = 2013, y = 305),
             label = "Deficit decreased by $47.9 billion\n(86 % of the 2016 projected deficit)", 
             hjust = 0, vjust = 0.5, colour = "#555555", 
             fill = "white", label.size = NA, size = 4) +
  # prediction arrow
  geom_curve(aes(x = 2014.8, y = 390, xend = 2015.9, yend = 375), 
             colour = "red", size = 0.5, curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc"), ends = "last")) +
  geom_label(aes(x = 2013.4, y = 390),
             label = "Projected increase", 
             hjust = 0, vjust = 0.5, colour = "red", 
             fill = "white", label.size = NA, size = 4) +
  labs(title = "The 2016 decrease in US trade deficit with China",
       x = "", y = "", size = 2) +
  theme(panel.grid.minor = element_blank()) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 13)
  )
#  ggsave("./week 16/mistakes.png", dpi = 600, height = 3, width = 4)


# original code by @theotheredgar, with minor changes for cowplot
# https://mobile.twitter.com/theotheredgar/status/1118668150780907520  
p2 <- trade %>%
   mutate(in_billions = -(trade_deficit / 1e9) ) %>%
   ggplot() +
   geom_col(aes(year, in_billions), alpha = 0.8, fill = "#1380A1") +
   geom_label(x = 2001, y = 300, label = " Great Recession", 
               hjust = 0, vjust = 0.5, colour = "#555555", 
               fill = "white", label.size = NA, size = 4) +
   geom_curve(x = 2007, y = 300, xend = 2009, yend = 230, 
               colour = "#555555", size=0.5, curvature = -0.5, 
               arrow = arrow(length = unit(0.03, "npc"))) +
    geom_curve(x = 2002, y = 110, xend = 2004.5, yend = 200, 
               colour = "#555555", size=0.5, curvature = -0.5, 
               arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
    geom_label(x = 1997.5, y = 190, label = "Deficit doubled", 
               hjust = 0, vjust = 0.5, colour = "#555555", 
               fill = "white", label.size = NA, size = 4) +
    labs(title = "US trade deficit with China in good, $bn", x = "", y = "") +
    scale_y_continuous(breaks = c(0, 100, 200, 300), labels = c("0", "-100", "-200", "-300")) +
    theme(panel.grid.minor = element_blank()) +
    theme_bw() 

pp <- plot_grid(p2, p1, ncol = 2)

title <- ggdraw() +
  draw_label("Original", x = 0.25, y = 0.5, size = 13, fontface = 'bold') +
  draw_label("Worse", x = 0.77, y = 0.5, size = 13, fontface = 'bold')

tpp <- plot_grid(title, pp, nrow = 2,  rel_heights = c(0.1, 1, 1))
save_plot("./week 16/worse.png", tpp, base_aspect_ratio = 3)

