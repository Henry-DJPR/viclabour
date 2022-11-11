# Please add the viz prefix to all charts

viz_ind_emppop_state_line <- function(...){
  
  df <- get_data(
    series_id = c(
      "A84423272J",
      "A84423356T",
      "A84423286W",
      "A84423370L",
      "A84423328J",
      "A84423300F",
      "A84423314V",
      "A84423342C"
    )
  )
  
  # Fix state names and dates
  df[, series := recode_states(series)]
  
  # Generate title
  title_df <- df[date == max(date) & !(series %in% c("NT", "ACT"))] 
  vic_rank <- title_df[order(-value), which(series == "Vic")]
  vic_level <- title_df[series == "Vic", round(value, 1)]
  titlefun <- function(pos) sprintf(
    "%s%% of Victorian adults are employed, the %s ratio of any Australian state",
    vic_level,
    pos
  )
  
  title <- fcase(
    vic_rank == 1, titlefun("highest"),
    vic_rank == 2, titlefun("second highest"),
    vic_rank == 3, titlefun("third highest"),
    default = "Victoria's employment to population ratio compared to other states and territories"
  )
  
  # Configure data
  state_order <- state_order[state_order %in% unique(df$series)]
  df[, series := factor(series, levels = state_order)]
  setkey(df, series, date)
  df[, value := round(value, 2)]
  data <- to_series_list(
    df, 
    x = "date", 
    y = "value", 
    group = "series", 
    series_type = "line", 
    inactive = c(
      "QLD",
      "SA",
      "WA",
      "Tas",
      "NT",
      "ACT"
    )
  )
  
  # Output
  list(
    title = list(text = title),
    subtitle = list(
      text = "Employment to population ratio in Australian states and territories"
    ),
    xAxis = list(type = 'datetime'),
    yAxis = list(
      title = list(
        text = "Employment to population ratio"
      ),
      labels = list(format = "{text}%")
    ),
    scrollbar = list(enabled = FALSE),
    tooltip = list(valueSuffix = "%"),
    navigator = list(enabled = FALSE),
    rangeSelector = preset_rangeSelector,
    legend = list(enabled = TRUE),
    series = data
  )
}



viz_ind_empgro_line <- function(...){
  df <- get_data(series_id = c("A84423349V", "A84423043C"))
  df[, series := recode_states(series)]
  state_order <- state_order[state_order %in% unique(df$series)]
  df[, series := factor(series, levels = state_order)]
  setkey(df, series, date)
  
  # Difference data
  df[, value := (value - shift(value, 12)) / shift(value, 12) * 100, series]
  df <- df[!is.na(value)]
  
  # Generate title
  latest_vic <- df[date == max(date) & series == "Vic", value]
  latest_aus <- df[date == max(date) & series == "Aus", value]
  latest_date <- df[, max(date)]
  title <- fcase(
    latest_vic > latest_aus,
      "Employment growth in Victoria outpaced Australia as a whole in the 12 months to ",
    latest_vic < latest_aus,
      "Employment growth in Victoria lagged behind Australia as a whole in the 12 months to ",
    latest_vic == latest_aus,
      "Employment in Victoria grew at the same pace as the Australian total in the 12 months to"
  )
  title <- paste0(title, format(latest_date, "%B %Y"))
  
  # Configure data
  df[, value := round(value, 2)]
  data <- to_series_list(
    df, 
    x = "date", 
    y = "value", 
    group = "series", 
    series_type = "line"
  )
  
  # Output
  list(
    title = list(text = title),
    subtitle = list(
      text = "Year to date annual employment growth"
    ),
    xAxis = list(type = 'datetime'),
    yAxis = list(
      title = list(
        text = "Employment growth"
      ),
      labels = list(format = "{text}%"),
      plotLines = list(
        list(
          value = 0,
          dashStyle = "Dash"
        )
      )
    ),
    scrollbar = list(enabled = FALSE),
    tooltip = list(valueSuffix = "%"),
    navigator = list(enabled = FALSE),
    rangeSelector = preset_rangeSelector,
    series = data
  )
  
}


viz_ind_gen_full_part_line <- function(...){
  
  # Get data and recode series labels
  df <- get_data(
    series_id = c("pt_emp_vic","A84423357V"), 
    where = "date >= '2020-03-01'"
  )
  df[series_id == "pt_emp_vic", series := "Part-time"]
  df[series_id == "A84423357V", series := "Full-time"]
  df[, series := factor(series, levels = c("Full-time", "Part-time"))]
  setkey(df, series, date)
  
  # Difference data
  df[, ref_value := value[date == as.Date("2020-03-01")], series]
  df[, value := (value - ref_value) / ref_value * 100, series]
  
  # Make title
  latest <- df[series == "Full-time" & date == max(date), round(value, 2)]
  title <- paste0(
    "The number of Victorians employed full-time is ",
    fcase(
      latest  > 0, paste0(latest, "% higher than "),
      latest == 0, "the same as ",
      latest  < 0, paste0(latest, "% lower than ")
    ),
    "it was in before COVID-19"
  )
  
  # Configure data
  df[, value := round(value, 2)]
  data <- to_series_list(
    df, 
    x = "date", 
    y = "value", 
    group = "series", 
    series_type = "line"
  )
  
  # Output
  list(
    title = list(text = title),
    subtitle = list(
      text = "Cumulative change in full-time and part-time employment since March 2020"
    ),
    xAxis = list(type = 'datetime'),
    yAxis = list(
      title = list(
        text = "Employment growth"
      ),
      labels = list(format = "{text}%"),
      plotLines = list(
        list(
          value = 0,
          dashStyle = "Dash"
        )
      )
    ),
    scrollbar = list(enabled = FALSE),
    tooltip = list(valueSuffix = "%"),
    navigator = list(enabled = FALSE),
    rangeSelector = list(enabled = FALSE),
    series = data
  )
  
}

