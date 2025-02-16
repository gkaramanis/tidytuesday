library(tidyverse)
library(treemapify)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 12, height = 8, units = "in", dpi = 320)

cdc_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/cdc_datasets.csv')

fpi_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/fpi_codes.csv')

cdc_tags <- cdc_datasets %>% 
  left_join(fpi_codes, by = c("program_code" = "program_code_pod_format")) %>% 
  filter(tags != "This dataset does not have any tags") %>% 
  select(dataset_url, contact_name, program_name, tags) %>% 
  mutate(tags2 = str_split(tags, ",")) %>% 
  unnest(tags2) %>% 
  mutate(tags2 = str_trim(tags2)) %>% 
  count(program_name, tags2) %>% 
  filter(!is.na(program_name)) 

f1 <- "Bricolage Grotesque 96pt Condensed"
f2 <- "Sofia Sans Extra Condensed"

pal <- MetBrewer::met.brewer(name = "Signac", n = 15, direction = -1)

cdc_programs <- cdc_datasets %>% 
  left_join(fpi_codes, by = c("program_code" = "program_code_pod_format")) %>% 
  count(program_name, category) %>% 
  filter(!is.na(program_name)) %>% 
  mutate(category = if_else(category == "This dataset has not been categorized", "No category", category))

ggplot(cdc_programs, aes(area = n, fill = program_name, label = category, subgroup = program_name)) +
  geom_treemap(color = "white", size = 0.25) +
  geom_treemap_text(color = "white", family = f2, place = "center") +
  geom_treemap_subgroup_border(color = "grey99", size = 2) +
  geom_treemap_subgroup_text(aes(label = program_name, color = after_scale(colorspace::darken(fill, 0.6))), place = "bottomleft", family = f1) +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Preserving public health knowledge",
    subtitle = str_wrap("This treemap shows CDC datasets backed up to the Internet Archive, with rectangles sized by dataset count and grouped by program area. The preservation effort became crucial during a period of federal health website content removal, ensuring vital public health information from Environmental Health to Infectious Diseases remains accessible.", 155),
    caption = "Source: Internet Archive · Graphic: Georgios Karamanis"
  ) +
  theme_void(base_family = f2) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "grey99", color = NA),
    plot.title = element_text(size = 26, family = f1, color = "grey10"),
    plot.subtitle = element_text(size = 20, family = f2, color = "grey10", margin = margin(6, 0, 8, 0)),
    plot.caption = element_text(size = 16, family = f2, color = "grey10"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Alt-text: 
# A treemap visualization showing CDC dataset categories grouped by program. Each program is represented by a distinct color section containing smaller rectangles representing different dataset categories. The size of each rectangle corresponds to the number of datasets in that category.