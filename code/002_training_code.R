# 001_load ----------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(tidytext)
library(govtrackR)
library(ggtext)
library(gganimate)
library(hrbrthemes)
library(rtemis)
library(viridis)
library(ggrepel)
library(highcharter)
library(tidylo)
library(widyr)
library(gt)
library(tidygraph)
library(ggraph)
library(igraph)
library(d3r)
library(treemap)
library(scales)
library(sunburstR)
library(reactable)
library(skimr)
library(trelliscopejs)
library(glue)
options(highcharter.theme = hc_theme_hcrt(tooltip = list(valueDecimals = 2)))

# 002_psc -----------------------------------------------------------------

### Explore PSC

tbl_psc <- dictionary_psc_active(only_active = T, snake_names = T)

glimpse(tbl_psc)

psc_tbl <- tbl_psc %>%
  select(
    is_active_psc,
    type_psc,
    name_solicitation_group,
    code_product_service,
    name_product_service,
    date_start,
    date_end,
    details_product_service_includes
  ) %>%
  reactable(
    filterable = T,
    resizable = T,
    searchable = T,
    showPageSizeOptions = T,
    defaultPageSize = 4,
    pageSizeOptions = c(5, 10, 20),
    sortable = T,
    compact = T
  )

tbl_psc



### Breakdown of Products vs Services
gg_psc_bkd <- tbl_psc %>%
  count(type_psc, sort = T, name = "count") %>%
  mutate(type_psc = fct_reorder(type_psc, count)) %>%
  ggplot(aes(x = type_psc, y = count, fill = type_psc)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE, name = "") +
  theme_ipsum() +
  ylab("Number of Product Service Codes") +
  xlab("") +
  ggtitle("Products Versus Services") +
  coord_flip()

gg_psc_bkd

### How Many New Codes by Type

tbl_new_codes <-
  tbl_psc %>%
  group_by(type_psc, date_start) %>%
  summarise(count_added = n(), .groups = "drop")


hc_new_psc <-
  hchart(tbl_new_codes,
         "line",
         hcaes(x = date_start, y = count_added, group = type_psc)) %>%
  hc_title(text = "New Product Service Codes by Date Added") %>%
  hc_yAxis(title = list(text = "Count of New Codes")) %>%
  hc_xAxis(title = list(text = "Date Added"))

hc_new_psc

### Lets Change the theme
hc_new_psc <-
  hc_new_psc %>%
  hc_add_theme(hc_theme_hcrt())


## What are the 5 newest prodct and service codes ?

tbl_10_new_psc <-
  tbl_psc %>%
  filter(!is_parent_psc) %>%
  arrange(desc(date_start)) %>%
  group_by(type_psc) %>%
  slice(1:10) %>%
  ungroup()

tbl_10_new_psc

### Explore Heirarchy

hc_new_psc_node_graph <-
  tbl_10_new_psc %>%
  select(name_solicitation_group, name_product_service) %>%
  as_tbl_graph() %>%
  hchart() %>%
  hc_title(text = "New Product Service Codes Node Graph") %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_xAxis(visible = FALSE) %>%
  hc_yAxis(visible = FALSE)

hc_new_psc_node_graph

# 003_fpds_csv ------------------------------------------------------------

### Lets explore

args(fpds_csv)

### Lets Explore Pyrotechnics
tbl_pyro <-
  fpds_csv(product_or_service_code = "1370", snake_names = T)

### Data
glimpse(tbl_pyro)
skim(tbl_pyro)

### How Much Have We Spent ??

tbl_by_day <-
  tbl_pyro %>%
  group_by(date_obligation) %>%
  summarise(amount = sum(amount_obligation), .groups = "drop")

tbl_by_day %>%
  slice_tail(n = 5) %>%
  munge_data() %>%
  gt()

### How many days are equal to zero

tbl_by_day %>% filter(amount == 0) %>% nrow()

tbl_by_day <- tbl_by_day %>% filter(amount != 0)

### How Much Have Spent by Day

hc_by_day <-
  tbl_by_day %>%
  filter(amount != 0) %>%
  hchart("spline",
         hcaes(x = date_obligation, y = amount)) %>%
  hc_title(text = "Pyrotechnic Spend By Day Spend by Date") %>%
  hc_yAxis(title = list(text = "Amount Obligated")) %>%
  hc_xAxis(title = list(text = "Date Added")) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_navigator(enabled = T)


tbl_by_day <-
  tbl_by_day %>%
  mutate(amount_cumulative = cumsum(amount))

### Cumulative
gg_area_pyro <-
  tbl_by_day %>%
  ggplot(aes(date_obligation, amount_cumulative)) +
  geom_area(fill = "#22908C", alpha = .5) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date() +
  theme(legend.position = "none") +
  theme_ipsum() +
  labs(title = "Cumulative Federal Procurement on Explosives", x = "Date", y = "Cumulative Procurement Spend")

gg_area_pyro

### Add Model Fits

gg_area_pyro +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess", color = "black")

### Lets look for seasonality

tbl_monthly_pyro <-
  tbl_pyro %>%
  mutate(month_obligation = lubridate::month(date_obligation, label = T)) %>%
  count(year_fiscal_obligation,
        month_obligation,
        wt = amount_obligation,
        name = "amount")

### Seasonality

govt_months <-
  c("Oct",
    "Nov",
    "Dec",
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep")

tbl_monthly_pyro <- tbl_monthly_pyro %>%
  mutate(month_obligation = factor(month_obligation,
                                   levels = govt_months,
                                   ordered = T))


fntltp <- JS(
  "function(){
  return this.point.x + ' ' +  this.series.yAxis.categories[this.point.y] + ': ' +
  Highcharts.numberFormat(this.point.value, 2);
}"
)


hc_pyro_hm <-
  hchart(
    tbl_monthly_pyro,
    "heatmap",
    hcaes(x = year_fiscal_obligation,
          y = month_obligation,
          value = amount)
  ) %>%
  hc_colorAxis(
    stops = color_stops(20, colors = scales::viridis_pal(option = "B")(20)),
    # fuerza a utilzar mejor el espectro de colores para que HJS no amplie el
    # eje para tener numero "redondos
    startOnTick = FALSE,

    endOnTick =  FALSE,
    reversed = T
  ) %>%
  hc_yAxis(
    title = list(text = ""),
    reversed = TRUE,
    offset = -20,
    tickLength = 0,
    gridLineWidth = 0,
    minorGridLineWidth = 0,
    labels = list(style = list(fontSize = "9px"))
  ) %>%
  hc_tooltip(formatter = fntltp) %>%
  hc_title(text = "Spend by Month and Fiscal Year") %>%
  hc_legend(
    layout = "horizontal",
    verticalAlign = "top",
    align = "left",
    valueDecimals = 0
  ) %>%
  hc_add_theme(hc_theme_darkunica())

hc_pyro_hm

## Model It

mod <-
  rtemis::s.LM(
    x = tbl_monthly_pyro$month_obligation,
    y = tbl_monthly_pyro$amount,
    intercept = F
  )
var_imp <- mod$varimp

tbl_coef <-
  tibble(month = names(var_imp), amount_coef = var_imp) %>%
  arrange(desc(amount_coef)) %>%
  munge_data()

gt(tbl_coef)

### How do we spend the money


tbl_depts <-
  tbl_pyro %>%
  count(
    name_department_award,
    wt = amount_obligation,
    name = "amount",
    sort = T
  ) %>%
  mutate(name_department_award = fct_reorder(name_department_award, amount))

tbl_depts %>% munge_data() %>% gt()

gg_spend <-
  tbl_depts %>%
  mutate(amount_millions = amount /1000000) %>%
  ggplot(aes(x = name_department_award, y = amount_millions)) +
  geom_segment(
    aes(
      x = name_department_award ,
      xend = name_department_award,
      y = 0,
      yend = amount_millions
    ),
    color = "grey"
  ) +
  geom_point(size = 3, color = "#69b3a2") +
  coord_flip() +
  theme_ipsum() +
  theme(
    legend.position = "none",
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    text = element_text(size = 7)
  ) +
  xlab("") +
  scale_y_log10(labels = scales::dollar, n.breaks = 10) +
  labs(title = "Which Departments Purchased the Explosives?",
       x = "",
       y = "Procurement $ in milions (log10 transformed)")

gg_spend

### How Big are the Contracts by Agency

tbl_pyro %>%
  filter(amount_obligation > 0) %>% count(name_agency_cgac_award,
                                          wt = amount_obligation,
                                          sort = T,
                                          name = "amount") %>%
  munge_data() %>%
  gt()

tbl_agency_sum <-
  tbl_pyro %>%
  filter(amount_obligation > 0) %>%
  mutate(
    name_agency_cgac_award = name_agency_cgac_award %>% fct_lump(8, w = amount_obligation, other_level = "OTHER 6 AGENCIES")
  ) %>%
  group_by(name_agency_cgac_award, id_contract_analysis) %>%
  summarise(amount = sum(amount_obligation, na.rm = T)) %>%
  ungroup() %>%
  filter(amount > 1000) %>%
  mutate(name_agency_cgac_award = fct_reorder(name_agency_cgac_award, amount))

gg_contract <-
  tbl_agency_sum %>%
  ggplot(aes(
    x = factor(name_agency_cgac_award),
    y = amount,
    fill = name_agency_cgac_award
  )) +
  geom_boxplot() +
  geom_jitter(color = "black",
              size = 0.3,
              alpha = .5)  +
  scale_fill_viridis(discrete = TRUE, alpha = .5) +
  theme_ipsum() +
  scale_y_log10(labels = scales::dollar) +
  theme(legend.position = "none",
        plot.title = element_text(size = 11)) +
  labs(
    title = "Distrubition of Award Size by Agency Group",
    x = "",
    y = "",
    credits = "Awards over $1000"
  ) +
  coord_flip()

### How do we buy pyro

lvl_opts <-  list(
  list(
    level = 1,
    borderWidth = 0,
    borderColor = "transparent",
    dataLabels = list(
      enabled = TRUE,
      align = "left",
      verticalAlign = "top",
      style = list(
        fontSize = "12px",
        textOutline = FALSE,
        color = "white"
      )
    )
  ),
  list(
    level = 2,
    borderWidth = 0,
    borderColor = "transparent",
    colorVariation = list(key = "brightness", to = 0.250),
    dataLabels = list(enabled = T),
    style = list(
      fontSize = "8px",
      textOutline = FALSE,
      color = "white"
    )
  )
)

hc_treemap_spend <-
  tbl_pyro %>%
  count(name_department_award, type_award, sort = T) %>%
  highcharter::data_to_hierarchical(
    group_vars = c("name_department_award", "type_award"),
    size_var = "n"
  ) %>%
  hchart(
    type = "treemap",
    levels = lvl_opts,
    tooltip = list(valueDecimals = FALSE)
  ) %>%
  hc_add_theme(hc_theme_superheroes())

###### VENDORS

tbl_vendors <- tbl_pyro %>%
  group_by(id_duns) %>%
  summarise(
    name_vendor = name_vendor[which.max(amount_obligation)],
    date_first_award = min(date_obligation, na.rm = T),
    date_recent_award = max(date_obligation, na.rm = T),
    count_actions = n(),
    count_contracts = n_distinct(id_contract_analysis, na.rm = T),
    amount_contracts = sum(amount_obligation),
    count_departments = n_distinct(name_department_award, na.rm = T),
    count_agencies = n_distinct(name_agency_cgac_award, na.rm = T)
  ) %>%
  arrange(desc(amount_contracts))


### Top 25 Vendors

tbl_vendors %>%
  munge_data() %>%
  slice(1:25) %>%
  gt::gt()

tbl_vendors %>% skim()

tbl_vendors <- tbl_vendors %>% filter(amount_contracts > 500000)

gg_top_vendors_pyro <-
  tbl_vendors %>%
  mutate(
    name_vendor_lumped = name_vendor %>% fct_lump(10, w = amount_contracts, other_level = "ALL OTHER VENDORS")
  ) %>%
  count(name_vendor_lumped, wt = amount_contracts, name = "amount") %>%
  mutate(name_vendor_lumped = fct_reorder(name_vendor_lumped, amount)) %>%
  ggplot(aes(x = name_vendor_lumped, y = amount)) +
  geom_bar(stat = "identity", fill = "#B71212") +
  coord_flip() +
  theme_ipsum() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none"
  ) +
  xlab("") +
  labs(title = "Top Explosive Vendors") +
  scale_y_continuous(labels = scales::dollar, n.breaks = 10)

mplot3.xy(
  log(tbl_vendors$count_actions),
  log(tbl_vendors$amount_contracts / 1000000),
  cluster = "PAM",
  cluster.params = list(k = 4),
  main = "Contracts Actions vs Contract $ by PAM Cluster Group",
  fit = "lm",
  xlab = "Contract Actions",
  ylab = "Contract $ (millions)",
  theme = "white"
)

tbl_vendors %>%
  select(-c(id_duns)) %>%
  select_if(is.numeric) %>%
  preprocess(scale = T) %>%
  mplot3.x(group.title = "Normalized Numeric Feature Distributions [Explosive Vendors]",
           theme = 'white',
           density.line = T)


###  OFFICE Dimension Reduction

tbl_pyro_office <-
  tbl_pyro %>%
  group_by(name_office_award) %>%
  summarise(
    department = name_department_award[which.max(amount_obligation)],
    agency = name_agency_cgac_award[which.max(amount_obligation)],
    year_first = year(date_obligation) %>% min(),
    date_recent_award = year(date_obligation) %>% max(),
    count_actions = n(),
    count_contracts = n_distinct(id_contract_analysis, na.rm = T),
    amount_contracts = sum(amount_obligation),
    count_distinct_vendors = n_distinct(id_duns, na.rm = T),
    count_distinct_parents = n_distinct(id_duns_parent, na.rm = T),
    .groups = "drop"
  ) %>%
  arrange(desc(amount_contracts))


tbl_pyro_office <-
  tbl_pyro_office %>%
  mutate(agency_lumped = fct_lump(agency, n = 6, other_level = "ALL OTHER AGENCIES"))

tbl_umap <-
  tbl_pyro_office %>%
  select_if(is.numeric) %>%
  uwot::umap(n_neighbors = 14, metric = "manhattan") %>%
  as_tibble() %>%
  setNames(c("umap_001", "umap_002"))


tbl_pyro_office <-
  tbl_pyro_office %>%
  bind_cols(
    tbl_umap
  )

x <-
  c(
    "Department",
    "Agency",
    "Office",
    "Contracts",
    "Vendors",
    "Amount $",
    "Actions",
    "umap 1",
    "umap 2"
  )

y <-
  sprintf(
    "{point.%s:.2f}",
    c(
      "department",
      "agency",
      "name_office_award",
      "count_actions",
      "count_distinct_vendors",
      "amount_contracts",
      "count_actions",
      "umap_001",
      "umap_002"
    )
  )

tltip <- tooltip_table(x, y)


hc_umap <-
  tbl_pyro_office %>%
  hchart(
    "scatter",
    hcaes(
      x = umap_001,
      y = umap_002,
      group = agency_lumped,
      name = name_office_award
    ),
    marker = list(radius = 1, symbol = 'circle')
  ) %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_xAxis(visible = F) %>%
  hc_yAxis(visible = F) %>%
  hc_title(text = "Pyrotechnic Office Dimension Reduction") %>%
  hc_tooltip(
    useHTML = TRUE,
    headerFormat = "{point.name}",
    pointFormat = tltip,
    table = T
  )

hc_umap


# 004_fpds_atom -----------------------------------------------------------

args(fpds_atom)

tbl_anduril <-
  fpds_atom(
    vendor_name = "ANDURIL",
    parse_contracts = T,
    snake_names = T
  )

### Data Structure

glimpse(tbl_anduril)
skim(tbl_anduril)

tbl_anduril <-
  tbl_anduril %>% preprocess(removeConstants = T) %>% as_tibble()


### Checck the matches
tbl_count <-
  tbl_anduril %>%
  count(name_vendor, id_duns, name = "count_actions")

gt(tbl_count)

### Filter bad match

tbl_anduril <-
  tbl_anduril %>%
  filter(id_duns != 103307059)

## Awards by Agency and Yea

tbl_anduril %>%
  count(name_agency_cgac_award,
        year_budget,
        wt = amount_obligation,
        name = "amount") %>%
  arrange(desc(amount)) %>%
  munge_data() %>%
  gt()


## Remove Excess Features

#### Word Analysis

stop_words <- get_stopwords(source = "smart")

tidy_ngram <- tbl_anduril %>%
  select(id_contract_analysis,
         description_obligation,
         amount_obligation) %>%
  unnest_tokens(bigram,
                description_obligation,
                token = "ngrams",
                n = 2)


bigram_counts <-
  tidy_ngram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

gg_bigraph <-
  bigram_counts %>%
  filter(!is.na(word1)) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "nicely") +
  geom_edge_link(
    aes(edge_alpha = n),
    show.legend = FALSE,
    arrow = arrow(length = unit(1.5, 'mm')),
    start_cap = circle(3, 'mm'),
    end_cap = circle(3, 'mm')
  ) +
  geom_node_text(aes(label = name)) +
  theme_graph() +
  labs(title = "Anduril FPDS Description Bigram")

gg_bigraph

tbl_ngrams <-
  tbl_anduril %>%
  select(id_contract_analysis,
         description_obligation,
         amount_obligation) %>%
  unnest_tokens(ngram, description_obligation, token = "ngrams", n = 3) %>%
  group_by(ngram) %>%
  summarise(
    amount = sum(amount_obligation),
    count_contracts = n_distinct(id_contract_analysis)
  ) %>%
  arrange(desc(amount))

tbl_ngrams %>%
  slice(1:15) %>%
  gt()

# 005_phase_3 -------------------------------------------------------------


df_all <-
  fpds_research_csv(
    research_codes = c("SR3", "ST3"),
    use_future = F,
    snake_names = T
  )

glimpse(df_all)

skimr::skim(df_all)

### Cumulative Area
tbl_cumulative_sbir <-
  df_all %>%
  count(date_obligation, wt = amount_obligation, name = "amount") %>%
  mutate(amount_cumulative = cumsum(amount))

gg_cumulative_spend <-
  tbl_cumulative_sbir %>%
  ggplot(aes(x = date_obligation, y = amount_cumulative)) +
  theme_minimal() +
  geom_line(color = "#00B0F0", size = .5) +
  geom_area(fill = "#00B0F0",
            alpha = 0.25,
            color = NA) +
  theme(
    panel.background = element_rect(fill = "#fffff8", color = NA),
    plot.background = element_rect(fill = "#fffff8", color = NA)
  ) +
  labs(
    x = NULL,
    y = "$ Obligated",
    title = "Cumulative Phase III STTR/SBIR Obligations",
    subtitle = "",
    caption = ""
  ) +
  scale_x_date(expand = c(0, 0),
               breaks = scales::pretty_breaks(n = 10)) +
  hrbrthemes::theme_ipsum_rc(
    grid = "XY",
    plot_title_size = 10,
    subtitle_size = 8.5,
    caption_size = 8.5,
    axis_text_size = 10,
    axis_title_size = 10,
    strip_text_size = 10
  ) +
  geom_smooth(
    colour = "#000000",
    method = 'loess',
    span = .3,
    size = .5,
    alpha = 0.45
  ) +
  geom_smooth(
    colour = "red",
    method = 'lm',
    size = .5,
    alpha = 0.45
  ) +
  scale_y_continuous(labels = scales::dollar,
                     breaks = scales::pretty_breaks(n = 10))

gg_cumulative_spend

plot_gg_cumulative_area <-
  function(data,
           plot_title = "",
           use_lm = T,
           use_loess = T)  {
    gg <- data %>%
      ggplot(aes(x = date_obligation, y = amount_cumulative)) +
      theme_minimal() +
      geom_line(color = "#00B0F0", size = .5) +
      geom_area(fill = "#00B0F0",
                alpha = 0.25,
                color = NA) +
      theme(
        panel.background = element_rect(fill = "#fffff8", color = NA),
        plot.background = element_rect(fill = "#fffff8", color = NA)
      ) +
      labs(
        x = NULL,
        y = "$ Obligated",
        title = plot_title,
        subtitle = "",
        caption = ""
      ) +
      scale_x_date(expand = c(0, 0),
                   breaks = scales::pretty_breaks(n = 10)) +
      hrbrthemes::theme_ipsum_rc(
        grid = "XY",
        plot_title_size = 7,
        subtitle_size = 6.5,
        caption_size = 6.5,
        axis_text_size = 7,
        axis_title_size = 7,
        strip_text_size = 7
      ) +
      scale_y_continuous(labels = scales::dollar,
                         breaks = scales::pretty_breaks(n = 10))

    if (use_loess) {
      gg <- gg  +
        geom_smooth(
          colour = "#000000",
          method = 'loess',
          span = .3,
          size = .5,
          alpha = 0.45
        )
    }

    if (use_lm) {
      gg <- gg +
        geom_smooth(
          colour = "red",
          method = 'lm',
          size = .5,
          alpha = 0.45
        )
    }

    gg

  }

plot_gg_cumulative_area(
  data = tbl_cumulative_sbir,
  plot_title = "Does this work?",
  use_lm = F,
  use_loess = T
)



### How much have we spent

### Year
tbl_year <- df_all %>%
  group_by(code_research, year_fiscal_obligation) %>%
  summarise(amount = sum(amount_obligation),
            .groups = "drop")

tbl_year %>%
  munge_data() %>%
  arrange(year_fiscal_obligation) %>%
  gt()


hc_bar_year <- hchart(tbl_year,
                      "column",
                      hcaes(x = year_fiscal_obligation,
                            y = amount,
                            group = code_research)) %>%
  hc_add_theme(hc_theme_elementary())  %>%
  hc_title(text = "SBIR/STTR Phase III Awards by Year") %>%
  hc_plotOptions(series = list(stacking = "normal"))


hc_bar_year

### Spend by Department

tbl_by_dept <-
  df_all %>%
  count(year_budget,
        name_department_award,
        wt = amount_obligation,
        name = "amount") %>%
  filter(amount > 0, year_budget >= 2000) %>%
  mutate(
    department_group = fct_lump(
      name_department_award,
      n = 5,
      w = amount,
      other_level = "ALL OTHER DEPARTMENTS"
    )
  ) %>%
  count(year_budget, department_group, wt = amount, name = "amount") %>%
  mutate(department_group = fct_reorder(department_group, -amount))

hc_dept_stream_graph <-
  tbl_by_dept %>%
  hchart("streamgraph",
         hcaes(year_budget, amount, group = department_group)) %>%
  hc_yAxis(visible = T,
           startOnTick = FALSE,
           endOnTick = FALSE) %>%
  hc_title(text = "SBIR Phase III by Department")

hc_dept_stream_graph

### Agency
tbl_agency_amt <-
  df_all %>%
  filter(!is.na(name_agency_cgac_award)) %>%
  count(name_agency_cgac_award,
        wt = amount_obligation,
        name = "amount",
        sort = T)

tbl_agency_amt %>%
  munge_data() %>%
  gt()


gg_agency <-
  tbl_agency_amt %>%
  mutate(
    agency_group = fct_lump(
      name_agency_cgac_award,
      n = 15,
      w = amount,
      other_level = "ALL OTHER AGENCIES"
    )
  ) %>%
  count(agency_group,
        wt = amount,
        sort = T,
        name = "amount") %>%
  mutate(agency_group = fct_reorder(agency_group, amount)) %>%
  ggplot(aes(x = agency_group, y = amount)) +
  geom_bar(
    stat = "identity",
    fill = "#f68060",
    alpha = .6,
    width = .4
  ) +
  coord_flip() +
  xlab("") +
  scale_y_continuous(labels = scales::dollar, n.breaks = 5) +
  geom_text(
    aes(label = round(amount / 1000000, digits = 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.25,
    check_overlap = T
  ) +
  ggtitle("Top SBIR/STTR Phase III Awarding Agencies") +
  labs(subtitle = "By Lumped Government-twide Accounting Agency [CGAC] - Top 15",
       x = "") +
  theme_ipsum()

gg_agency

### Lets Make it Look better

gg_agency <-
  gg_agency +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    text = element_text(size = 8,  family = "serif")
  ) +
  scale_y_sqrt(labels = scales::dollar, n.breaks = 4)

gg_agency

### Office

tbl_office <-
  df_all %>%
  filter(!is.na(name_office_award)) %>%
  group_by(name_department_award,
           name_agency_cgac_award,
           name_office_award) %>%
  summarise(
    date_first = min(date_obligation, na.rm = T),
    date_recent = max(date_obligation, na.rm = T),
    distinct_vendors = n_distinct(id_duns, na.rm = T),
    amount = sum(amount_obligation, na.rm = T),
    amount_mean = mean(amount_obligation),
    actions = n(),
    contracts = n_distinct(id_contract_analysis, na.rm = T),
    .groups = "drop"
  ) %>%
  filter(amount > 0) %>%
  arrange(desc(amount))


table_office <- tbl_office %>%
  reactable(
    filterable = F,
    resizable = T,
    searchable = T,
    showPageSizeOptions = T,
    pageSizeOptions = c(5, 10, 20),
    sortable = T,
    compact = T,
    columns = list(
      date_first = colDef(name = "First Award"),
      date_recent = colDef(name = "Recent Award"),
      name_department_award = colDef(
        name = "Department",
        sortable = T,
        filterable = T
      ),
      name_agency_cgac_award = colDef(
        name = "Agency",
        sortable = T,
        filterable = T
      ),
      name_office_award = colDef(
        name = "Office",
        sortable = T,
        filterable = T
      ),
      distinct_vendors = colDef(name = "Unique Vendors"),
      amount = colDef(
        name = "$ Total",
        format = colFormat(
          prefix = "$",
          separators = TRUE,
          digits = 0
        )
      ),
      amount_mean = colDef(
        name = "$ Mean",
        format = colFormat(
          prefix = "$",
          separators = TRUE,
          digits = 0
        )
      ),
      contracts = colDef(name = "Unique Contracts"),
      actions = colDef(name = "Contract Actions")
    )
  )

table_office

### New Vendors

tbl_first <-
  df_all %>%
  filter(id_duns != 123456787) %>%
  group_by(id_duns) %>%
  filter(date_obligation == min(date_obligation)) %>%
  filter(amount_obligation > 0) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    id_duns,
    year_budget_first = year_budget,
    date_first_phase_3 = date_obligation,
    id_contract_analysis_first = id_contract_analysis,
    department_first = name_department_award,
    agency_first = name_agency_cgac_award,
    office_first = name_office_award
  ) %>%
  arrange(desc(year_budget_first))

gg_first_time <- tbl_first %>%
  count(year_budget_first) %>%
  filter(year_budget_first >= 1998, year_budget_first <= 2020) %>%
  ggplot(aes(year_budget_first, n)) +
  geom_line() +
  theme_ipsum() +
  labs(title = "New Phase III SBIR/STTR Recipients",
       x = "Budget Year",
       y = "Count")

gg_first_time

colors <-
  c("#00008BFF",
    "#1EFAA0FF",
    "#00FA00FF",
    "#ADFF2FFF",
    "#FA7D00FF",
    "#78005AFF")

### Share of New Vendors
hc_attribution <- tbl_first %>%
  count(year_budget_first, agency_first) %>%
  filter(year_budget_first >= 1998, year_budget_first <= 2020) %>%
  mutate(agency_lumped = fct_lump(agency_first, n = 5, other_level = "ALL OTHER AGENCIES")) %>%
  count(year_budget_first, agency_lumped, wt = n) %>%
  mutate(agency_lumped = fct_reorder(agency_lumped, n)) %>%
  filter(!is.na(agency_lumped)) %>%
  hchart(
    "column",
    hcaes(year_budget_first, n, group = agency_lumped),
    stacking = "percent",
    borderWidth = 0,
    groupPadding = 0,
    pointPadding  = 0,
  ) %>%
  hc_colors(colors) %>%
  hc_add_theme(hc_theme_538()) %>%
  hc_xAxis(gridLineWidth = 0,
           title = list(text = NULL)) %>%
  hc_tooltip(
    table = TRUE,
    outside = TRUE,
    shared = TRUE,
    useHTML = TRUE,
    headerFormat = "<small>{point.key}</small><table>",
    pointFormat = str_c(
      "<tr><td style=\"color: {series.color}\">{series.name}: </td>",
      "<td style=\"text-align: right\"><b>{point.y:0.f}</b></td>"
    ),
    footerFormat = "<tr><td><b>Total</b>: </td><td style=\"text-align: right\"><b>{point.total:0.f}</b></td></tr></table>",
    style = list(fontSize = "0.7em")
  ) %>%
  hc_legend(
    verticalAlign = "top",
    align = "left",
    itemStyle =  list(fontWeight = 500)
  ) %>%
  hc_title(text = "Share of New Phase III Awardees by Agency Group") %>%
  hc_plotOptions(series = list(
    marker = list(
      radius = 0,
      enabled = FALSE,
      symbol = "circle"
    ),
    states = list(hover = list(halo = list(size = 0)))
  )) %>%
  hc_yAxis(title = list(text = "Share of New Vendors"),
           type = "percent")


hc_attribution

###  Vendor Summary

tbl_vendors <-
  df_all %>%
  filter(!is.na(id_duns)) %>%
  group_by(id_duns) %>%
  summarise(
    vendor = name_vendor[which.max(amount_obligation)],
    date_first = min(date_obligation, na.rm = T),
    date_recent = max(date_obligation, na.rm = T),
    count_types = n_distinct(code_research, na.rm = T),
    count_agencies = n_distinct(id_cgac_award, na.rm = T),
    count_departments = n_distinct(id_department_award),
    count_offices = n_distinct(name_office_award, na.rm = T),
    amount = sum(amount_obligation, na.rm = T),
    amount_mean = mean(amount_obligation),
    actions = n(),
    contracts = n_distinct(id_contract_analysis, na.rm = T),
    .groups = "drop"
  ) %>%
  filter(amount > 0) %>%
  arrange(desc(amount)) %>%
  mutate(
    year_recent = year(date_recent),
    year_first = year(date_first),
    .before = "date_first"
  )

tbl_vendors %>%
  slice(1:10) %>%
  munge_data() %>%
  gt()

tbl_vendors <-
  tbl_vendors %>%
  filter(id_duns != 123456787)


### Explore the data interactively!!

## Remove Miscelations

esquisse::esquisser(data = tbl_vendors)

tbl_vendors %>% skim()


tbl_vendors <-
  tbl_vendors %>%
  filter(contracts >= 2, amount >= 500000)

mplot3.xy(
  log(tbl_vendors$actions),
  log(tbl_vendors$amount / 1000000),
  cluster = "PAM",
  cluster.params = list(k = 10),
  main = "Phase III $ by Actions",
  fit = "lm",
  xlab = "Contract Actions",
  ylab = "Contract $ (millions)",
  theme = "white"
)

clusters <- tbl_vendors %>% select(-id_duns) %>%
  select_if(is.numeric) %>%
  rtemis::u.PAM(k = 10)

tbl_vendors <-
  tbl_vendors %>%
  mutate(pam_cluster = as.character(clusters$clusters.train),
         .before = "id_duns")

tbl_cumulative <-
  df_all %>%
  filter(id_duns %in% c(tbl_vendors$id_duns)) %>%
  group_by(id_duns, date_obligation) %>%
  summarise(amount = sum(amount_obligation)) %>%
  mutate(amount_cumulative = cumsum(amount)) %>%
  ungroup() %>%
  group_by(id_duns) %>%
  nest() %>%
  ungroup()

tbl_cumulative$data[[7]] %>% plot_gg_cumulative_area(plot_title = "Test")

tbl_trelliscope_data <-
  tbl_vendors %>%
  left_join(tbl_cumulative, by = "id_duns") %>%
  left_join(tbl_first %>% select(id_duns, department_first, agency_first, office_first),
            by = "id_duns") %>%
  mutate_at(c("year_recent", "year_first"), as.character)

tbl_trelliscope_data %>%
  sample_n(10) %>%
  mutate(amount = round(amount / 1000000, digits = 2),
         panel = map_plot(data,
                          function(data) {
                            plot_gg_cumulative_area(
                              data = data,
                              plot_title = "Cumulative Phase IIIs",
                              use_lm = T,
                              use_loess = T
                            )
                          })) %>%
  arrange(desc(amount)) %>%
  trelliscope(
    name = "Phase III Cumulative Awardee Trelliscope",
    desc = "Minimum of 2 Phase III contracts",
    nrow = 1,
    ncol = 2,
    width = 500,
    height = 500,
    state = list(
      labels = c(
        "vendor",
        "amount",
        "contracts",
        'date_first',
        "date_recent",
        "pam_cluster",
        "department_first",
        "agency_first",
        "office_first"
      ),
      sort_spec(name = "amount", dir = "desc")
    )
  )


phase_3_trelliscope <-
  tbl_trelliscope_data %>%
  mutate(amount = round(amount / 1000000, digits = 2),
         panel = map_plot(data,
                          function(data) {
                            plot_gg_cumulative_area(
                              data = data,
                              plot_title = "Cumulative Phase IIIs",
                              use_lm = T,
                              use_loess = T
                            )
                          })) %>%
  arrange(desc(amount)) %>%
  trelliscope(
    name = "Phase III Cumulative Awardee Trelliscope",
    desc = "Minimum of 2 Phase III contracts",
    nrow = 1,
    ncol = 2,
    width = 500,
    height = 500,
    state = list(
      labels = c(
        "vendor",
        "amount",
        "contracts",
        'date_first',
        "date_recent",
        "pam_cluster",
        "department_first",
        "agency_first",
        "office_first"
      ),
      sort_spec(name = "amount", dir = "desc")
    )
  )

phase_3_trelliscope

# 006_sbir_afwerx ----------------------------------------------------------------

df_afwerx <- sbir_afwerx_portfolio(use_cached = T)

df_afwerx %>% glimpse()

df_afwerx %>% skim()

df_afwerx %>%
  count(id_program, id_phase, sort = T) %>%
  gt()

df_afwerx %>%
  count(id_phase, type_solicitation, sort = T) %>%
  gt()

df_afwerx %>%
  count(
    id_phase,
    id_program,
    type_solicitation,
    group_solicitation,
    name_solicitation,
    sort = T
  ) %>%
  gt()

### keywords

tbl_company_keywords <- df_afwerx %>%
  select(
    id_phase,
    id_program,
    type_solicitation,
    group_solicitation,
    name_solicitation,
    name_company_clean,
    keyword_company_clean_afwerx
  ) %>%
  separate_rows(keyword_company_clean_afwerx, sep = "\\|") %>%
  mutate_all(str_squish)

### Top 50 Keywords

tbl_af_keywords <-
  tbl_company_keywords %>%
  mutate_all(str_squish) %>%
  count(keyword_company_clean_afwerx, sort = T)

af_keywords <- reactable(tbl_af_keywords, filterable = T, searchable = T)

af_keywords


gg_top_50 <-
  tbl_af_keywords %>%
  slice(1:50) %>%
  mutate(keyword_company_clean_afwerx = fct_reorder(keyword_company_clean_afwerx, n)) %>%
  ggplot(aes(x = keyword_company_clean_afwerx, y = n)) +
  geom_bar(
    stat = "identity",
    fill = "#f68060",
    alpha = .6,
    width = .4
  ) +
  coord_flip() +
  xlab("") +
  theme_bw() +
  scale_y_log10() +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.25) +
  ggtitle("Top 50 Keywords for AFWERX Companies")

gg_top_50

### Tidy Lo -- Keywords by Type

tbl_keyword_counts <- tbl_company_keywords %>%
  count(type_solicitation, keyword_company_clean_afwerx, sort = T)

mplot3.x(x = tbl_keyword_counts$n)

tbl_keyword_counts %>%
  filter(n >= 5) %>%
  pull(n) %>%
  mplot3.x(x = .)

tbl_lo <-
  tbl_keyword_counts %>%
  filter(n >= 5) %>%
  tidylo::bind_log_odds(set = type_solicitation, feature = keyword_company_clean_afwerx, n = n) %>%
  arrange(desc(log_odds_weighted))

reactable(tbl_lo, filterable = T, sortable = T, searchable = T)


### Which Words Are counted together
tbl_company_keywords %>%
  pairwise_count(feature = name_company_clean, item = keyword_company_clean_afwerx) %>%
  arrange(desc(n)) %>%
  reactable(filterable = T, searchable = T)

## Highly Correlated Companies

hc_cor_key <- tbl_company_keywords %>%
  count(name_company_clean, keyword_company_clean_afwerx) %>%
  pairwise_cor(item = name_company_clean, feature = keyword_company_clean_afwerx) %>%
  filter(correlation > .70) %>%
  graph_from_data_frame() %>%
  hchart() %>%
  hc_add_theme(hc_theme_darkunica()) %>%
  hc_title(text = "Highly Correlated Afwerx Portfolio Company by Keywords") %>%
  hc_xAxis(visible = F) %>%
  hc_yAxis(visible = F)

hc_cor_key

# 2021 budget -------------------------------------------------------------

df_budget <- dod_years_budgets(budget_years = 2021, snake_names = T)

glimpse(df_budget)

### Breakdown of Items

df_budget %>%
  count(name_agency_cgac,
        wt = amount_item,
        name = "amount",
        sort = T) %>%
  munge_data() %>%
  gt()

df_budget %>%
  count(
    name_dod_budget_group,
    name_agency_cgac,

    wt = amount_item,
    name = "amount",
    sort = T
  ) %>%
  munge_data() %>%
  gt()


### What Items are we Buying?


tbl_items <-
  df_budget %>%
  filter(amount_unit_cost > 0, count_item > 0) %>%
  select(
    count_item,
    amount_unit_cost,
    name_program_element_actual,
    name_agency_cgac,
    slug_organization_account
  ) %>%
  distinct()


gg_items <-
  tbl_items %>%
  ggplot(aes(x = count_item, y = amount_unit_cost, color = name_agency_cgac)) +
  geom_jitter() +
  theme_ipsum() +
  labs(title = "2021 Requested Budget Items",
       subtitle = "Untransformed and Static") +
  scale_y_continuous(labels = scales::dollar)

gg_items

hc_items <-
  hchart(
  tbl_items,
  "scatter",
  hcaes(
    x = count_item,
    y = amount_unit_cost,
    group = name_agency_cgac,
    name = name_program_element_actual
  ),
  marker = list(radius = 3, symbol = 'circle'),
  regression = TRUE
) %>%
  hc_title(text = "2021 Defense Budget Requested Items") %>%
  hc_xAxis(title = list(text = "Item Count (log10 transformed)"),
           type = "logarithmic") %>%
  hc_yAxis(title = list(text = "$ Amount Per Unit (log 10 transformed)"),
           type = "logarithmic") %>%
  hc_tooltip(
    table = TRUE,
    outside = TRUE,
    shared = TRUE,
    useHTML = TRUE,
    headerFormat = "<small>{point.key}</small><table>",
    pointFormat = str_c(
      "<tr><td style=\"color: {series.color}\">{series.name}: </td>",
      "<tr><td style=\"text-align: right\"><b>Items: {point.x:,.0f}</b></td></tr>",
      "<tr><td style=\"text-align: right\"><b>$ Per item: {point.y:,0.f}</b></td></tr>",
      "<td style=\"text-align: right\"></td>"
    ),
    style = list(fontSize = "0.7em")
  ) %>%
  hc_colors(c("#d35400", "#2980b9", "#2ecc71", "black")) %>%
  hc_add_dependency("plugins/highcharts-regression.js") %>%
  hc_add_theme(hc_theme_538())

hc_items


### Budget Sunburst

treemap_columns <-
  c(
    "type_budget",
    "name_dod_budget_group",
    "name_agency_cgac",
    "name_account_omb_clean",
    "name_budget_parent",
    "name_budget_activity",
    "name_program_element",
    "slug_organization_account"
  )

tbl_treemap <- df_budget %>%
  count(
    !!!syms(treemap_columns),
    wt = amount_item,
    name = "amount",
    sort = T
  ) %>%
  filter(amount > 0)

### TREEMAP OF the Budget

tm <-
  tbl_treemap %>%
  treemap(index = treemap_columns,
          vSize = "amount",
          #vColor="continent",
          type = "index")

tm_nest <- d3_nest(tm$tm[, c(treemap_columns,
                             "vSize",
                             "color")],
                   value_cols = c("vSize", "color"))

sun <- sund2b(
  tm_nest,
  colors = htmlwidgets::JS(# yes this is a little different, so please pay attention
    #  "function(d) {return d.color}" will not work
    "function(name, d){return d.color || '#ccc';}"),
  valueField = "vSize",
  elementId = "my-sunburst"
)

sun



### Rpivot

rpiv <- rpivotTable::rpivotTable(df_budget,
                                 rows = "name_agency_cgac",
                                 vals = "amount_item",
                                 aggregatorName = "Sum")


rpiv
