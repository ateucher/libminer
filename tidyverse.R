library(dplyr)

var_summary <- function(data, var) {
  data |>
    summarise(
      min_var = min(var),
      max_var = max(var)
    )
}

# This errors because it can't find 'var' in data
mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

var_summary <- function(data, var1, var2) {
  data |>
    summarise(
      min_var = min({{ var1 }}),
      max_var = max({{ var2 }})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg, disp)

# Use .data[[]] for string inputs
mtcars |>
  group_by(cyl) |>
  summarise(
    min_var = min(.data[["mpg"]]),
    max_var = max(.data[["disp"]])
  )

var_summary <- function(data, var1, var2) {
  data |>
    summarise(
      min_var = min(.data[[var1]]),
      max_var = max(.data[[var2]])
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary("mpg", "disp")

big_cars_summary <- function(var) {
  mtcars |>
    filter(.data$cyl >= 6) |>
    group_by(.data$cyl) |>
    summarise(
      n = n(),
      mean = mean({{ var }})
    )
}

big_cars_summary(mpg)

starwars_mass_summary <- function(group_var) {
  starwars |>
    group_by({{group_var}}) |>
    summarize(
      n = n(),
      mean_mass = mean(.data$mass, na.rm = TRUE),
      sd_mass = sd(.data$mass, na.rm = TRUE)
    )
}

starwars_mass_summary(species)

star_summary <- function(data, var1, var2, var3){
  data |>
    summarise(
      mean_var_1 = mean(.data[[var1]]),
      max_var_1 = max(.data[[var1]]),
      min_var_1 = min(.data[[var1]]),
      mean_var_2 = mean(.data[[var2]]),
      max_var_2 = max(.data[[var2]]),
      min_var_2 = min(.data[[var2]]),
      mean_var_3 = mean(.data[[var3]]),
      max_var_3 = max(.data[[var3]]),
      min_var_3 = min(.data[[var3]])
    )
}

starwars |>
  group_by(homeworld) |>
  star_summary("height", "mass", "birth_year")

# group by any variable:
height_sum <- function(data, group_var, ...) {
  data |>
    dplyr::group_by({{ group_var }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height, ...)
    )
}

height_sum(starwars, hair_color, na.rm = TRUE, trim = 7)

# group by > 1 variable:
height_sum <- function(data, ...) {
  data |>
    dplyr::group_by(...) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_height = mean(.data$height)
    )
}

height_sum(starwars, hair_color, homeworld)

# Dynamic column naming with :=
var_summary <- function(data, var, var_name) {
  data |>
    summarise(
      "{var_name}" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg, "min_cyl")

# Use {{}} syntax to dynamically create new column name from variable name
var_summary <- function(data, var) {
  data |>
    summarise(
      "{{var}}_min" := min({{var}})
    )
}

mtcars |>
  group_by(cyl) |>
  var_summary(mpg)

# homework
homework1 <- function(var){
  starwars |>
    summarise("mean_{{var}}" := mean({{var}}, na.rm = T),
              "max_{{var}}" := max({{var}}, na.rm = T),
              count = n())
}

homework1(height)

homework1 <- function(var){
  starwars |>
    summarise("mean_{var}" := mean(.data[[var]], na.rm = T),
              "max_{var}" := max(.data[[var]], na.rm = T),
              count = n())
}

homework1("height")

var_summary <- function(data, group_var, var) {
  data |>
    group_by({{group_var}}) |>
  summarise(
    "n_distinct_{{var}}" := n_distinct({{var}})
  )
}

var_summary(starwars, species, hair_color)

# across() inside data-masking verbs

summy <- function(df, group_var, cols) {
  df |>
    group_by({{ group_var }}) |>
    summarise(
      across({{ cols }}, .fns = list(min = min, max = max))
    )
}

mtcars |>
  summy(cyl, c(mpg, disp))

mtcars |>
  summy(cyl, starts_with("mp"))

mtcars |>
  summy(cyl, where(is.numeric))


