library(tidyverse)
library(udpipe)
library(igraph)
library(ggraph)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 8, height = 8, units = "in", dpi = 320)

horror_articles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-31/horror_articles.csv')

# Uncomment to download model
# udpipe_download_model(language = "english")

udpipe_model <- udpipe_load_model(here::here("english-ewt-ud-2.5-191206.udpipe"))

horror_annot <- udpipe_annotate(udpipe_model, x = horror_articles$claim) %>% 
  as.data.frame()

cooc <- cooccurrence(x = subset(horror_annot, upos %in% c("NOUN", "ADJ", "VERB")), term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))

wordnetwork <- cooc %>%
  subset(., cooc > 2) %>% 
  graph_from_data_frame()

f1 <- "Outfit"

set.seed(100)

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange", lineend = "round") +
  geom_node_point(color = "grey5") +
  geom_node_text(aes(label = name), family = f1, color = "grey87", size = 5, repel = TRUE) +
  scale_edge_width(range = c(1, 4)) +
  coord_fixed(clip = "off") +
  labs(
    title = "Snopes horror legends", 
    subtitle = "Most common cooccurrences of adjective, noun and verb within a sentence for 253 Snopes articles",
    caption = "Source: Snopes · Graphic: Georgios Karamanis"
    ) +
  theme_void(base_family = f1) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey15", color = NA),
    plot.title = element_text(color = "grey99", face = "bold", size = 22),
    plot.subtitle = element_text(color = "grey99", margin = margin(5, 0, 10, 0)),
    plot.caption = element_text(color = "grey99"),
    plot.margin = margin(10, 10, 10, 10)
  )

