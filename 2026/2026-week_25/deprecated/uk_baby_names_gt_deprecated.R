library(gtExtras)

names_ratios <- 
  bind_rows(england_wales_names |> mutate(geo = "England & Whales"), 
            ni_names |> mutate(geo = "Northern Ireland"),
            scotland_names |> mutate(geo = "Scotland")) |>
  filter(between(Year, 1997, 2024)) |> 
  filter(!is.na(Name), !is.na(Number)) |> 
  pivot_wider(id_cols = c(Year, Name, geo), values_from = Number, names_from = Sex) |> 
  mutate(ratio = Girl/Boy) |>  
  # keep only names for both girls and boys:
  filter(!is.na(ratio)) 


names_gt <- names_ratios |> 
  filter(between(ratio, 0.9, 1.1)) |> 
  mutate(
    n = Boy + Girl,
    ratio = round(ratio, 2)
  ) |> 
  group_by(geo, Name) |>
  mutate(total_n = sum(n)) |> 
  slice_max(order_by = total_n, n = 10) |> 
  arrange(desc(total_n)) |> 
  ungroup() |> 
  group_by(geo, Name) |> 
  mutate(ratio_list = list(ratio)) |> 
  ungroup()
  
gt(names_gt) |> 
  gt_plt_sparkline(ratio_list)
