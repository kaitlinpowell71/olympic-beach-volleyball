suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(plotly)
  library(crosstalk)
})

if (!requireNamespace("olympicAthletes", quietly = TRUE)) {
  stop('Install the source package with install.packages("olympicAthletes").')
}

data("olympic_athletes", package = "olympicAthletes", envir = .GlobalEnv)

source_records <- nrow(olympic_athletes)
summer_athletes <- olympic_athletes |>
  filter(season == "Summer")
summer_records_used <- nrow(summer_athletes)

beach_event <- regex("Beach Volleyball", ignore_case = TRUE)

beach_athletes <- summer_athletes |>
  filter(
    sport == "Volleyball",
    str_detect(event, beach_event),
    between(year, 1996, 2024)
  ) |>
  mutate(
    gender = recode(sex, M = "Men", F = "Women"),
    country = str_remove(team, "-[0-9]+$")
  )

if (anyNA(beach_athletes$gender)) {
  stop("Beach volleyball records contain an unrecognized sex value.")
}

participation_by_year <- summer_athletes |>
  group_by(year) |>
  summarise(
    all_summer_sports = n(),
    volleyball_sport = sum(sport == "Volleyball"),
    beach_volleyball_event = sum(
      sport == "Volleyball" & str_detect(event, beach_event),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "records"
  ) |>
  mutate(
    series = recode(
      series,
      all_summer_sports = "All Summer sport entries",
      volleyball_sport = "Volleyball sport entries",
      beach_volleyball_event = "Beach volleyball event entries"
    )
  )

beach_participation_by_year <- beach_athletes |>
  count(year, gender, name = "athlete_event_records")

source_beach_medals <- beach_athletes |>
  filter(!is.na(medal), year <= 2016) |>
  distinct(year, event, team, medal, .keep_all = TRUE)

if (anyNA(source_beach_medals$team)) {
  stop("A source medal record is missing its team identifier.")
}

source_beach_medals <- source_beach_medals |>
  transmute(
    year,
    event,
    team,
    gender,
    country = str_remove(team, "-[0-9]+$"),
    medal,
    source = "olympicAthletes"
  )

recent_medals <- tribble(
  ~year, ~gender, ~country, ~medal,
  2020, "Men", "Norway", "Gold",
  2020, "Men", "ROC", "Silver",
  2020, "Men", "Qatar", "Bronze",
  2020, "Women", "United States", "Gold",
  2020, "Women", "Australia", "Silver",
  2020, "Women", "Switzerland", "Bronze",
  2024, "Men", "Sweden", "Gold",
  2024, "Men", "Germany", "Silver",
  2024, "Men", "Qatar", "Bronze",
  2024, "Women", "Brazil", "Gold",
  2024, "Women", "Canada", "Silver",
  2024, "Women", "Latvia", "Bronze"
) |>
  mutate(
    event = if_else(
      gender == "Men",
      "Beach Volleyball Men's Beach Volleyball",
      "Beach Volleyball Women's Beach Volleyball"
    ),
    team = paste(country, "podium team"),
    source = "Official Olympic results"
  )

beach_medals <- bind_rows(source_beach_medals, recent_medals) |>
  distinct(year, event, team, medal, .keep_all = TRUE)

if (any(beach_medals |> count(year, gender) |> pull(n) != 3)) {
  stop("Expected exactly three medal-winning teams per year and gender.")
}

country_medal_type_totals <- beach_medals |>
  count(country, medal, name = "medal_count")

country_medal_totals <- beach_medals |>
  count(country, name = "total_medals") |>
  arrange(desc(total_medals), country)

medals_by_year_country_gender <- beach_medals |>
  group_by(year, country, gender) |>
  summarise(
    gold = sum(medal == "Gold"),
    silver = sum(medal == "Silver"),
    bronze = sum(medal == "Bronze"),
    team_medals = n(),
    .groups = "drop"
  )

medals_by_year_country <- medals_by_year_country_gender |>
  transmute(year, country, gender, medal_count = team_medals)

medals_by_year_country_total <- medals_by_year_country_gender |>
  group_by(year, country) |>
  summarise(
    across(c(gold, silver, bronze, team_medals), sum),
    .groups = "drop"
  )

medals_by_country_gender <- medals_by_year_country_gender |>
  group_by(country, gender) |>
  summarise(
    across(c(gold, silver, bronze, team_medals), sum),
    .groups = "drop"
  )

medal_timeline <- expand_grid(
  year = sort(unique(beach_medals$year)),
  country = sort(unique(beach_medals$country)),
  gender = c("Men", "Women")
) |>
  left_join(medals_by_year_country, by = c("year", "country", "gender")) |>
  mutate(medal_count = replace_na(medal_count, 0L)) |>
  arrange(year) |>
  group_by(country, gender) |>
  mutate(cumulative_medals = cumsum(medal_count)) |>
  ungroup()

brazil_medals <- country_medal_totals |>
  filter(country == "Brazil") |>
  pull(total_medals)
brazil_gold_medals <- country_medal_type_totals |>
  filter(country == "Brazil", medal == "Gold") |>
  pull(medal_count)
usa_medals <- country_medal_totals |>
  filter(country == "United States") |>
  pull(total_medals)
usa_gold_medals <- country_medal_type_totals |>
  filter(country == "United States", medal == "Gold") |>
  pull(medal_count)
