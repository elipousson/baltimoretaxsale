# Load 2006 to 2020 auction results
load("/data/auction_results_2006_2020.rda")

# Create summary functions for auction results data ----
summarise_results <- function(grouped_results) {
  grouped_results %>%
    summarise(
      num_advertised = n(),
      num_removed = sum(removed == "Y", na.rm = TRUE),
      num_scheduled = sum(removed == "N", na.rm = TRUE),
      num_sold = sum(!is.na(winning_bidder_name)),
      sum_lien_amount = sum(lien_amount),
      median_lien_amount = median(lien_amount),
      median_assessed_value = median(assessed_value),
      num_winning_bidders = n_distinct(winning_bidder_name),
#      sum_winning_bids = sum(winning_bid, na.rm = TRUE),
#      mean_winning_bid = round(mean(winning_bid, na.rm = TRUE), digits = 2),
#      median_winning_bid = median(winning_bid, na.rm = TRUE),
      mean_num_bids = round(mean(num_bids, na.rm = TRUE), digits = 2)
    ) %>%
    mutate(
      perc_removed = round((num_removed / num_advertised) * 100, digits = 2)
    )
}

summarise_homestead <- function(grouped_results) {
  grouped_results %>%
    filter(homestead == "Y") %>%
    summarise(
      num_advertised_homestead = n(),
      num_scheduled_homestead = sum(removed == "N", na.rm = TRUE)
      ) %>%
    mutate(
      perc_scheduled_homestead = round((num_scheduled_homestead / num_advertised_homestead) * 100, digits = 2)
    )
}

# TODO: Add owner occupied to 2020 results summary
summarise_results_2020 <- function(grouped_results){
  grouped_results %>%
    summarise(
      num_advertised = n(),
      num_owner_occupied = sum(owner_occupied == "1", na.rm = TRUE),
      num_owner_occupied_removed = sum((owner_occupied == "1") & (removed == "Y"), na.rm = TRUE),
      num_vacant = sum(vacind == "Y", na.rm = TRUE)
    ) %>%
    mutate(
      perc_owner_occupied = round((num_owner_occupied / num_advertised) * 100, digits = 2),
      perc_owner_occupied_removed = round((num_owner_occupied_removed / num_owner_occupied) * 100, digits = 2),
      perc_vacant = round((num_vacant / num_advertised) * 100, digits = 2)
    )
}

# Summarise by area ----

summarise_by_area <- function(results, area) {

  results_summary_by_area <- results %>%
    rename(area = {{area}}) %>%
    group_by(area, auction_year) %>%
    summarise_results()

  if (2019 %in% unique(results$auction_year)) {
    results_summary_by_area_pre_2020 <- results %>%
      filter(auction_year != 2020) %>%
      rename(area = {{area}}) %>%
      group_by(area, auction_year) %>%
      summarise_homestead()

    results_summary_by_area <- results_summary_by_area %>%
      left_join(results_summary_by_area_pre_2020)
  }

  if (2020 %in% unique(results$auction_year)) {
    results_summary_by_area_2020 <- results %>%
      filter(auction_year == 2020) %>%
      rename(area = {{area}}) %>%
      group_by(area, auction_year) %>%
      summarise_results_2020()

    results_summary_by_area <- results_summary_by_area %>%
      left_join(results_summary_by_area_2020)
    }

  area_label <- deparse(substitute(area))

  results_summary_by_area <- results_summary_by_area %>%
    right_join(filter(auction_year_area_index, type == area_label), by = c("area", "auction_year"))

  results_summary_by_area <- results_summary_by_area %>%
    relocate(type, .after = area) %>%
    arrange(area, auction_year) %>%
    left_join(area_property_count) %>%
    relocate(num_properties, .after = type) %>%
    mutate(
      num_advertised = if_else(is.na(num_advertised), 0L, num_advertised),
      perc_properties_area = round((num_advertised / num_properties) * 100, digits = 2),
      perc_properties_sold_area = round((num_sold / num_properties) * 100, digits = 2),
      .after = num_properties
    )

  return(results_summary_by_area)
}

summarise_by_area_2021 <- function(results, area) {

  results_summary_by_area <- results %>%
    rename(area = {{area}}) %>%
    group_by(area) %>%
    summarise(
      num_advertised = n(),
      sum_lien_amount = sum(lien_amount, na.rm = TRUE),
      median_lien_amount = median(lien_amount),
      median_assessed_value = median(assessment),
      num_vacant = sum(vacind == "Y"),
      num_vacant_lot = sum(no_imprv == "Y"),
      num_owner_occupied = sum(occupied %in% c("D", "H")),
      num_owner_occupied_not_vacant = sum((occupied %in% c("D", "H")) & vacind != "Y")
    ) %>%
    mutate(
      perc_vacant = round((num_vacant / num_advertised) * 100, digits = 2),
      perc_owner_occupied_not_vacant = round((num_owner_occupied_not_vacant / num_advertised) * 100, digits = 2),
      perc_vacant_lot = round((num_vacant_lot / num_advertised) * 100, digits = 2)
    )

  area_label <- deparse(substitute(area))

  results_summary_by_area <- results_summary_by_area %>%
    right_join(filter(auction_year_area_index_2021, type == area_label), by = "area")

  results_summary_by_area <- results_summary_by_area %>%
    relocate(type, .after = area) %>%
    arrange(area, auction_year) %>%
    left_join(area_property_count) %>%
    relocate(num_properties, .after = type) %>%
    mutate(
      num_advertised = if_else(is.na(num_advertised), 0L, num_advertised),
      perc_properties_area = round((num_advertised / num_properties) * 100, digits = 2),
      .after = num_properties
    )

  return(results_summary_by_area)
}

# Summarise by neighborhood
auction_results_summary_by_neighborhood_year <- auction_results_2006_2020 %>%
  summarise_by_area(area = neighborhood)

# Summarise by Council District
auction_results_summary_by_district_year <- auction_results_2006_2020 %>%
  summarise_by_area(area = council_district)

# Summarise by CSA
auction_results_summary_by_csa_year <- auction_results_2006_2020 %>%
  summarise_by_area(area = csa)

# Summarise 2021 advertised properties by Council District
property_summary_by_district_2021 <- advertised_properties_2021_sf %>%
  sf::st_drop_geometry() %>%
  summarise_by_area_2021(area = council_district)

readr::write_csv(property_summary_by_district_2021, "property_summary_by_district_2021.csv")

# Summarise 2021 advertised properties by neighborhood
property_summary_by_neighborhood_2021 <- advertised_properties_2021_sf %>%
  sf::st_drop_geometry() %>%
  summarise_by_area_2021(area = neighborhood)

readr::write_csv(property_summary_by_neighborhood_2021, "property_summary_by_neighborhood_2021.csv")

# Summarise 2021 advertised properties by CSA
property_summary_by_csa_2021 <- advertised_properties_2021_sf %>%
  sf::st_drop_geometry() %>%
  summarise_by_area_2021(area = csa)

readr::write_csv(property_summary_by_csa_2021, "property_summary_by_csa_2021.csv")

# Tables (migrate to another document) ----

auction_result_2020_by_district_table %>%
  mutate(
    id = forcats::fct_relevel(id, c("1",  "2",  "3",  "4",  "5",  "6", "7",  "8",  "9", "10", "11", "12", "13", "14"))
  ) %>%
  arrange(id) %>%
  gt() %>%
  fmt_percent(columns = starts_with("perc_")) %>%
  cols_label(
    id = "Council district",
    num_properties = "Properties listed for sale",
    removed_properties = "Properties removed before auction",
    scheduled_properties = "Properties scheduled for auction",
    perc_removed = "Properties removed before auction (% of total)",
    properties_removed_after_original_deadline = "Properties removed after original April 30 deadline",
    num_owner_occupied = "Owner-occupied properties listed",
    num_owner_occupied_removed = "Owner-occupied properties removed before auction",
    perc_owner_occupied = "Owner-occupied properties (% of total listed)",
    perc_owner_occupied_removed = "Owner-occupied properties removed (% of total owner-occupied)",
  ) %>%
  tab_header(
    title = "2020 Tax Sale Auction results by City Council District",
  ) %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()
    )
  ) %>%
  tab_options(
    # Remove border between column headers and title
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    # Remove border around table
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    # Reduce the height of rows
    data_row.padding = px(3),
    # Adjust font sizes and alignment
    source_notes.font.size = 11,
    heading.align = "left"
  )

# Bidder summary for 2006 to 2019 data
bidder_summary_2006_2019 <- auction_results_2006_2020 %>%
  filter(
    auction_year != 2020,
    removed == "N",
    !is.na(winning_bidder_name)
  ) %>%
  group_by(auction_year, winning_bidder_name) %>%
  summarise(
    num_bids_won = n(),
    num_homestead = sum(homestead == "Y", na.rm = TRUE),
    mean_winning_bid = mean(winning_bid, na.rm = TRUE),
    median_winning_bid = median(winning_bid, na.rm = TRUE),
    sum_winning_bid_amount = sum(winning_bid, na.rm = TRUE)
  ) %>%
  mutate(
    perc_homestead_properties = round((num_homestead / num_bids_won) * 100, digits = 2)
  ) %>%
  arrange(auction_year, desc(num_bids_won))

taxsale_2021_neighborhood <- taxsale_2021_match %>%
  group_by(neighborhood) %>%
  summarise(
    num_properties = n(),
    mean_assessment = mean(as.numeric(assessment)),
    median_assessment = median(as.numeric(assessment)),
    sum_assessment = sum(as.numeric(assessment)),
    mean_lien_amount = mean(as.numeric(lien_amount)),
    median_lien_amount = median(as.numeric(lien_amount)),
    sum_lien_amount = sum(as.numeric(lien_amount)),
    num_vacant = sum(vacind == "Y", na.rm = TRUE),
    perc_vacant = round((num_vacant / num_matched) * 100, digits = 2),
    num_vacant_lot = sum(no_imprv == "Y", na.rm = TRUE),
    perc_vacant_lot = round((num_vacant_lot / num_matched) * 100, digits = 2),
    num_owner_occupied = sum(occupied %in% c("H", "D"), na.rm = TRUE),
    perc_owner_occupied = round((num_owner_occupied / num_properties) * 100, digits = 2)
  ) %>%
  rename(area_name = neighborhood) %>%
  add_column(type = "neighborhood")


taxsale_2021_council_districts <- taxsale_2021_match %>%
  group_by(council_district) %>%
  summarise(
    num_properties = n(),
    num_matched = sum(!is.na(objectid)),
    num_unmatched = sum(is.na(objectid)),
    perc_matched = round((num_unmatched / num_properties) * 100, digits = 2),
    mean_assessment = mean(as.numeric(assessment)),
    median_assessment = median(as.numeric(assessment)),
    sum_assessment = sum(as.numeric(assessment)),
    mean_lien_amount = mean(as.numeric(lien_amount)),
    median_lien_amount = median(as.numeric(lien_amount)),
    sum_lien_amount = sum(as.numeric(lien_amount)),
    num_vacant = sum(vacind == "Y", na.rm = TRUE),
    perc_vacant = round((num_vacant / num_matched) * 100, digits = 2),
    num_vacant_lot = sum(no_imprv == "Y", na.rm = TRUE),
    perc_vacant_lot = round((num_vacant_lot / num_matched) * 100, digits = 2),
    num_owner_occupied = sum(occupied %in% c("H", "D"), na.rm = TRUE),
    perc_owner_occupied = round((num_owner_occupied / num_properties) * 100, digits = 2)
  ) %>%
  rename(area_name = council_district) %>%
  add_column(type = "council district")


taxsale_2021_csas <- taxsale_2021_match %>%
  group_by(csa) %>%
  summarise(
    num_properties = n(),
    num_matched = sum(!is.na(objectid)),
    num_unmatched = sum(is.na(objectid)),
    perc_matched = round((num_unmatched / num_properties) * 100, digits = 2),
    mean_assessment = mean(as.numeric(assessment)),
    median_assessment = median(as.numeric(assessment)),
    sum_assessment = sum(as.numeric(assessment)),
    mean_lien_amount = mean(as.numeric(lien_amount)),
    median_lien_amount = median(as.numeric(lien_amount)),
    sum_lien_amount = sum(as.numeric(lien_amount)),
    num_vacant = sum(vacind == "Y", na.rm = TRUE),
    perc_vacant = round((num_vacant / num_matched) * 100, digits = 2),
    num_vacant_lot = sum(no_imprv == "Y", na.rm = TRUE),
    perc_vacant_lot = round((num_vacant_lot / num_matched) * 100, digits = 2),
    num_owner_occupied = sum(occupied %in% c("H", "D"), na.rm = TRUE),
    perc_owner_occupied = round((num_owner_occupied / num_properties) * 100, digits = 2)
  ) %>%
  rename(area_name = csa) %>%
  add_column(type = "csa")

taxsale_2021_summary <-
  bind_rows(
    taxsale_2021_council_districts,
    taxsale_2021_csas,
    taxsale_2021_neighborhood
  )

library(gt)

map(
  c("council district", "csa", "neighborhood"),
  ~ taxsale_2021_summary %>%
    filter(type == .x) %>%
    select(-c(num_matched:perc_matched), type) %>%
    gt() %>%
    fmt_currency(
      columns = c(starts_with("mean"), starts_with("median"), starts_with("sum"))
    ) %>%
    fmt_percent(
      columns = starts_with("perc"),
      scale_values = 1
    ) %>%
    tab_header(
      title = .x
    )
)

# Match to real property data

# Match to Council district
# Count by Council district
# Plot by Council district
# Table by Council district

# Match to neighborhood
# Count by neighborhood
# Plot by neighborhood
# Table by neighborhood
