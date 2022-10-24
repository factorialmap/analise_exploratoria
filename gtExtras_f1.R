# objetivo ----------------------------------------------------------------------------------------------
"quais equipes tem melhor taxa vitoria vs corrida?"

# packages ----------------------------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(gt)
library(gtExtras)

# data --------------------------------------------------------------------------------------------------

#baixar todos os dados
f1_full <- tidytuesdayR::tt_load(2021, "37")

#escolher os dados que eu quero
data_results <- f1_full$results %>% clean_names()
data_constructors <- f1_full$constructors %>% clean_names()

#que dados sao esses?
data_results %>% glimpse()
data_constructors %>% glimpse()

#amostra
data_results %>% slice_sample(n=5)
data_constructors %>% slice_sample(n=5)

# join tables ------------------------------------------------------------------------------------------
data_f1_trans <- 
  data_results %>% 
  select(result_id:position_order) %>% 
  left_join(data_constructors %>% select(-url), by = "constructor_id") %>% 
  mutate(type = case_when(position_order == 1 ~ "win", TRUE ~ "race")) %>% 
  group_by(name, type, race_id) %>% 
  summarise(result = n_distinct(name)) %>% 
  pivot_wider(names_from = "type", values_from = "result", values_fill = 0) %>% 
  summarise(race = sum(race), win = sum(win)) %>% 
  mutate(ratio = win/race, .after = "win") %>% 
  filter(win>30) %>% 
  arrange(desc(win))


# table -------------------------------------------------------------------------------------------------
data_f1_trans %>% 
  gt() %>% 
  gt_plt_bullet(column = race,
                target = win, 
                colors = c("red", "navy"),
                keep_column = TRUE) %>% 
                gt_theme_nytimes() %>% 
  fmt_symbol_first(column = ratio,
                   suffix = "%",
                   decimals = 1, 
                   scale_by = 100) %>% 
  cols_label(race = html("<span style = 'color:red;'>Race</span> vs <span style='color:navy;'>Win</span>")) %>% 
  tab_header(title = "F1 races and wins",
             subtitle = md("**Mercedes** leads wins to races ratio")) %>% 
  gt_highlight_rows(rows = name == "Mercedes",
                    fill = "grey",
                    alpha = 0.4) %>% 
  gt_add_divider(ratio,
                 color = "grey80",
                 weight = px(1)) %>% 
  tab_source_note(md("Data: TidyTuesday **Package**: gtExtras by thomas_mock"))
