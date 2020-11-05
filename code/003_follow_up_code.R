library(tidyverse)
library(govtrackR)

## Phase I and API limits

df_phase_01 <- fpds_research_csv(research_codes = "SR1")

tbl_anduril <-
  fpds_atom(
    vendor_name = "ANDURIL",
    parse_contracts = T,
    snake_names = T
  )

tbl_anduril <- tbl_anduril %>%
  filter(id_duns != 103307059)

tbl_phase_2_date <-
  tbl_anduril %>%
  filter(!is.na(code_research)) %>%
  filter(code_research == "SR2") %>%
  group_by(code_research) %>%
  filter(date_obligation == min(date_obligation)) %>%
  select(
    id_duns,
    id_contract_phase_2 = id_contract_analysis,
    date_phase_2 = date_obligation,
    ageny_phase_2 = name_agency_cgac_award,
    office_award_phase_2 = name_office_award,
    office_fundinging_phase_2 = name_office_funding,
  ) %>%
  ungroup()

tbl_anduril <-
  tbl_anduril %>%
  left_join(tbl_phase_2_date, by = "id_duns")


tbl_anduril_post_phase_2 <-
  tbl_anduril %>%
  mutate(id_contract_analysis = id_contract_analysis %>% substr(1, 13)) %>%
  filter(id_contract_analysis != "FA865020C9300") %>%
  group_by(
    id_duns,
    name_vendor,
    id_contract_analysis,
    type_action,
    date_phase_2,
    type_research,
    type_inherently_government_function
  ) %>%
  summarise(
    date_award = min(date_obligation, na.rm = T),
    contract_actions = n(),
    department_award =  name_department_award[which.max(amount_obligation)],
    agency_award =  name_agency_award[which.max(amount_obligation)],
    office_award = name_office_award[which.max(amount_obligation)],
    amount_obligated = sum(amount_obligation, na.rm = T),
    .groups = "drop"
  ) %>%
  mutate(is_after_phase_2 = date_award > date_phase_2) %>%
  filter(is_after_phase_2) %>%
  ungroup() %>%
  select(-date_phase_2) %>%
  arrange(date_award) %>%
  mutate(contract_number_post_phase_2 = 1:n())

tbl_anduril_post_phase_2 %>%
  munge_data() %>%
  gt::gt()
