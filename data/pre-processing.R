library(tidyverse) ; library(sf) ; library(rnaturalearth)

sf_use_s2(FALSE)

sf <- ne_countries(continent = "africa", returnclass = "sf") |>
  select(name = geounit) |>
  mutate(name = case_when(name %in% c("Somalia", "Somaliland") ~ "Somalia", TRUE ~ name)) |>
  group_by(name) |>
  summarise(geometry = st_union(geometry)) |>
  ungroup() |>
  mutate(url_name = str_to_lower(str_replace_all(name, " ", "-")),
    url_name = case_when(
      name == "Ivory Coast" ~ "cote-d-ivoire",
      name == "Republic of the Congo" ~ "congo",
      TRUE ~ url_name),
    url = glue::glue(
      "https://www.gov.uk/foreign-travel-advice/{url_name}"
      )
    ) |>
  select(name, url)

st_write(sf, "africa.geojson")
