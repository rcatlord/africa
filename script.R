library(tidyverse) ; library(sf) ; library(rvest) ; library(leaflet) ; library(htmlwidgets)

sf <- st_read("data/africa.geojson")

# one line travel advice
read_status <- ~{
  message(.)
  read_html(.) |>
    html_nodes("strong") |>
    html_text2() 
}

# date updated
read_updated <- ~{
  message(.)
  read_html(.) |>
    html_nodes(".gem-c-metadata__definition") |>
    html_text2()
}

status <- sf %>%
  st_drop_geometry() |>
  mutate(status = map(pull(sf, url), read_status)) |>
  unnest(status) %>% 
  select(name, status)

updated <- sf %>%
  st_drop_geometry() |>
  mutate(updated = map(pull(sf, url), read_updated)) |>
  unnest(updated) |>
  filter(row_number() %% 3 == 2) |> 
  select(name, updated)

africa <- left_join(sf, status, by = "name") |>
  left_join(updated, by = "name") |>
  mutate(status = case_when(
    str_detect(status, "essential") ~ "Advise against all but essential travel",
    is.na(status) ~ "Check travel advice before travelling",
    TRUE ~ "Advise against all travel"),
    status = as_factor(status)
    )

factpal <- colorFactor(palette = c("#E94F35","#FCB700","#C5D54E"), 
                       levels = c("Advise against all travel",
                                  "Advise against all but essential travel",
                                  "Check travel advice before travelling"))

map <- leaflet(africa) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(weight = 1, color = "#FFFFFF", opacity = 1, 
              fillColor = ~factpal(africa$status), fillOpacity = 0.8,
              popup = paste0(
                "<strong>", africa$name, "</strong>",
                "<br>",
                "<a style='text-decoration: none;', href='", africa$url,"' target='_blank'>FCDO travel advice</a>",
                "<br>",
                "<em>Updated: ", africa$updated, "</em>"
              ),
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
              highlightOptions = highlightOptions(color = "#000000", weight = 2, bringToFront = TRUE)) |>
              addLegend(position = "bottomleft", colors = c("#E94F35","#FCB700","#C5D54E"),
                        labels = c("Advise against all travel",
                                   "Advise against all but essential travel",
                                   "Check travel advice before travelling"), opacity = 0.8) |>
  addControl(paste0("<strong>Latest FCDO travel advice</strong>"), position = "topright")

saveWidget(map, "index.html", title = "Latest FCDO travel advice")

