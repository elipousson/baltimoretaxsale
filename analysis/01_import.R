# Load packages ----
library(dplyr)
library(stringr)

# Import real property data ----
# Cache real property data with mapbaltimore
# mapbaltimore::cache_real_property()
real_property_path <- paste0(rappdirs::user_cache_dir("mapbaltimore"), "/real_property.gpkg")

real_property <-
  sf::read_sf(real_property_path) %>%
  sf::st_transform(2804) %>%
  select(objectid, fulladdr, owner_1, pin, block, lot, ward, section, no_imprv, vacind, permhome, lot_size, structarea, neighborhood, council_district, csa, tract)

# Import date index of assignment and certificate sales (2006 to 2020) ----
auction_index_2006_2020 <-
  readr::read_csv("data/auction_index.csv",
                  col_types = c(auction_year = "i", tax_year = "i"))
# TODO: Add date posted to the auction index

area_property_count <-
  purrr::map_dfr(
    c("neighborhood", "csa", "council_district"),
    ~ real_property %>%
      sf::st_drop_geometry() %>%
      count(.data[[.x]], name = "num_properties") %>%
      bind_cols(type = .x) %>%
      rename(area = {{.x}})
    ) %>%
  tidyr::replace_na(list(area = "Unknown"))

usethis::use_data(area_property_count, overwrite = TRUE)

auction_year_area_index <-
  purrr::map_dfr(
    c("neighborhood", "csa", "council_district"),
    ~ purrr::cross_df(
      list(
        area = unique(real_property[[.x]]),
        auction_year = unique(auction_index_2006_2020$auction_year),
        type = .x
        )
      )
  ) %>%
  tidyr::replace_na(list(area = "Unknown"))

usethis::use_data(auction_year_area_index, overwrite = TRUE)

auction_year_area_index_2021 <-
  purrr::map_dfr(
    c("neighborhood", "csa", "council_district"),
    ~ purrr::cross_df(
      list(
        area = unique(real_property[[.x]]),
        auction_year = 2021L,
        type = .x
      )
    )
  ) %>%
  tidyr::replace_na(list(area = "Unknown"))

usethis::use_data(auction_year_area_index_2021, overwrite = TRUE)

# Import certificate sale results from Bid Baltimore (2006 to 2019) ----
auction_results_2006_2019 <-
  list.files("data", full.names = TRUE) %>%
  tibble(path = .) %>%
  filter(
    str_detect(path, "certificate_sale_results"),
    !str_detect(path, "2020")
    ) %>%
  mutate(
    results = purrr::map(
      path,
      ~ readr::read_csv(.x, col_types = readr::cols(.default = "c"), trim_ws = TRUE)
      )
    ) %>%
  tidyr::unnest(results) %>%
  janitor::clean_names("snake") %>%
  rename(
    removed_date = removed_time_stamp,
    lien_amount = face_amount,
    winning_bid_premium = winning_premium,
    winning_bidder_number = winning_bidder,
    winning_bidder_amt_due = winning_amount_due
  ) %>%
  mutate(
    adv_number = as.integer(adv_number),
    # Extract block and lot from account number and build pin
    block = str_sub(account_number, end = 5),
    lot = str_sub(account_number, start = 6),
    block = if_else(str_detect(block, "[:alpha:]"), block, str_sub(block, start = 2)),
    lot = if_else(str_detect(lot, "[:alpha:]"), lot, str_sub(lot, start = 2)),
    pin = str_c(block, lot),
    # Format removed_because values and add removed indicator
    removed_because = str_to_title(removed_because),
    removed = if_else(!is.na(removed_because), "Y", "N"),
    removed_date = lubridate::as_date(removed_date),
    homestead = if_else(homestead == 1, "Y", "N"),
    # Convert variables to numeric
    tax_year = as.integer(tax_year),
    num_ties = as.integer(num_ties),
    num_bids = as.integer(num_bids),
    winning_bid = as.double(winning_bid),
    winning_bid_premium = as.double(winning_bid_premium),
    winning_bidder_amt_due = as.double(winning_bidder_amt_due),
    lien_amount = as.double(lien_amount),
    assessed_value = as.double(assessed_value),
    # Create auction year and type to match to index
    auction_year = as.integer(tax_year + 1),
    auction_type = case_when(
      str_detect(path, "assignment") ~ "assignment_sale",
      str_detect(path, "certificate") ~ "certificate_sale")
  ) %>%
  select(-c(path)) %>%
  left_join(auction_index_2006_2020, by = c("tax_year", "auction_year", "auction_type"))

auction_results_2020 <-
  readr::read_csv("data/bidbaltimore_certificate_sale_report_2020-07-20.csv", col_types = readr::cols(.default = "c"), trim_ws = TRUE) %>%
  janitor::clean_names("snake") %>%
  rename(
    batch_number = batch,
    adv_number = adv_no,
    certificate_number = cert_no,
  ) %>%
  mutate(
    adv_number = as.integer(adv_number),
    lien_amount = as.double(lien_amount),
    assessed_value = as.double(assessed_value),
    winning_bid = as.double(winning_bid),
    winning_bid_premium = as.double(winning_bid_premium),
    winning_bidder_amt_due = as.double(winning_bidder_amt_due),
    tax_year = as.integer(tax_year),
    auction_year = as.integer(tax_year + 1),
    removed_because = str_to_title(removed_because),
    removed = if_else(!is.na(removed_because), "Y", "N"),
    removed_date = lubridate::as_date(removed_date),
    auction_type = "certificate_sale"
  ) %>%
  left_join(auction_index_2006_2020, by = c("tax_year", "auction_year", "auction_type"))

# Join auction results to real property data ----
# The auction results contains records for 396260 properties
# 392462 (99%) are matched by pin
# 349476 in original - 350924 after match
# NOTE: The 2020 data in the real property data should not be summarized or interpreted for auction results prior to 2020
# It is used in this case for the location but not the other values no_imprv, vacind, permhome
auction_results_2006_2019_sf <- auction_results_2006_2019 %>%
  select(-c(block, lot)) %>%
  left_join(real_property, by = "pin") %>%
  distinct(pin, auction_year, .keep_all = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_centroid()

usethis::use_data(auction_results_2006_2019_sf, overwrite = TRUE)

auction_results_2020_sf <- auction_results_2020 %>%
  left_join(select(real_property, -section), by = c("block", "lot", "ward")) %>%
  arrange(csa) %>%
  distinct(adv_number, .keep_all = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_centroid()

usethis::use_data(auction_results_2020_sf, overwrite = TRUE)

# Combine 2006 to 2019 certificate sales and 2020 auction results data ----
auction_results_2006_2020 <- bind_rows(
  sf::st_drop_geometry(filter(auction_results_2006_2019_sf, auction_type == "certificate_sale")),
  sf::st_drop_geometry(auction_results_2020_sf))

usethis::use_data(auction_results_2006_2020, overwrite = TRUE)

# Import 2021 address data ----
advertised_properties_2021 <-
  readxl::read_excel(
    'data/tax_sale_advertised_2021-03-15.xlsx',
    col_types = "text") %>%
  janitor::clean_names("snake") %>%
  rename(
    street_name = address,
    adv_number = seq
  ) %>%
  mutate(
    adv_number = as.integer(adv_number),
    assessment = as.double(assessment),
    lien_amount = as.double(lien_amount),
  #  base_ex_amount = as.double(base_ex_amount),
  #  base_tax_amount = as.double(base_tax_amount)
    tax_year = 2020L,
    auction_year = 2021L
  )

advertised_properties_2021_sf <-
  advertised_properties_2021 %>%
  left_join(real_property, by = c("block", "lot", "ward", "section")) %>%
  distinct(block, lot, ward, section, .keep_all = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_centroid()

usethis::use_data(advertised_properties_2021_sf, overwrite = TRUE)


# Import 2021 (updated) address data ----
advertised_properties_20210406 <-
  readxl::read_excel(
    'data/tax_sale_advertised_2021-04-06.xlsx',
    col_types = "text") %>%
  janitor::clean_names("snake") %>%
  rename(
    street_name = address,
    assessment = assessed_value,
    lien_amount = lien,
    block = blk,
    ward = wd,
    section = sec
  ) %>%
  mutate(
    assessment = as.double(assessment),
    lien_amount = as.double(lien_amount),
    #  base_ex_amount = as.double(base_ex_amount),
    #  base_tax_amount = as.double(base_tax_amount)
    tax_year = 2020L,
    auction_year = 2021L
  )

advertised_properties_20210406_sf <-
  advertised_properties_20210406 %>%
  left_join(real_property, by = c("block", "lot", "ward", "section")) %>%
  distinct(block, lot, ward, section, .keep_all = TRUE) %>%
  sf::st_as_sf() %>%
  sf::st_centroid()
