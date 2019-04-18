library(tidyverse)

trade <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/trade.csv")
trade <- trade %>%
  mutate(in_billions = - trade_deficit / 1e9, employment = manufacture_employment/1e5 )

# "predictions"
model <- lm(in_billions ~ year, data = trade)
model2 <- lm(employment ~ year, data = trade)
newYears <- data.frame(year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016))
newYears$trade_deficit <- predict(model, newdata = newYears)
newYears$employment <- predict(model2, newdata = newYears)

ggplot() +
  # plot "predictions" first
  geom_line(data = newYears, aes(year, trade_deficit),
            linetype="dotted", size = 2, alpha = 0.4, color = "red") +
  #geom_line(data = newYears, aes(year, employment),
  #          linetype="dotted", size = 2, alpha = 0.4, color = "red") +
  # actual numbers
  geom_line(data = trade, aes(year, in_billions),
            size = 2, color = "#1380A1") +
  ylim(260, 400) +
  xlim (2010, 2016) +
  # reduction arrow
  geom_curve(aes(x = 2015, y = 360, xend = 2015.8, yend = 320), 
  colour = "#555555", size = 0.5, curvature = 0.5, 
  arrow = arrow(length = unit(0.03, "npc"), ends = "last")) +
  geom_label(aes(x = 2013.6, y = 305), label = "Deficit reduced by $47.9 billion\nor 86 % of the predicted deficit", 
  hjust = 0, vjust = 0.5, colour = "#555555", 
  fill = "white", label.size = NA, size = 4) +
  # prediction arrow
  geom_curve(aes(x = 2015, y = 390, xend = 2015.6, yend = 375), 
             colour = "#555555", size = 0.5, curvature = -0.2, 
             arrow = arrow(length = unit(0.03, "npc"), ends = "last")) +
  geom_label(aes(x = 2013, y = 390), label = "Predicted deficit increase", 
             hjust = 0, vjust = 0.5, colour = "red", 
             fill = "white", label.size = NA, size = 4) +
  labs(title = "US trade deficit with China in goods, $bn", x = "", y = "") +
  theme(panel.grid.minor = element_blank()) +
  theme_bw() 
  
  
# trade %>%
#   mutate(in_billions = -(trade_deficit / 1e9) ) %>%
#   ggplot() +
#   geom_col(aes(year, in_billions), alpha = 0.8, fill = "#1380A1") +
#   geom_label(x = 2002, y = 300, label = " Great Recession", 
#              hjust = 0, vjust = 0.5, colour = "#555555", 
#              fill = "white", label.size = NA, size = 4) +
#   geom_curve(x = 2007, y = 300, xend = 2009, yend = 230, 
#              colour = "#555555", size=0.5, curvature = -0.5, 
#              arrow = arrow(length = unit(0.03, "npc"))) +
#   geom_curve(x = 2002, y = 110, xend = 2004.5, yend = 200, 
#              colour = "#555555", size=0.5, curvature = -0.5, 
#              arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
#   geom_label(x = 1998.5, y = 190, label = "Deficit doubled", 
#              hjust = 0, vjust = 0.5, colour = "#555555", 
#              fill = "white", label.size = NA, size = 4) +
#   labs(title = "US trade deficit with China in good, $bn", x = "", y = "") +
#   scale_y_continuous(breaks = c(0, 100, 200, 300), labels = c("0", "-100", "-200", "-300")) +
#   theme(panel.grid.minor = element_blank()) +
#   theme_bw() 
