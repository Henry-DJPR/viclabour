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
  df[, series := factor(series, levels = state_order)]
  setkey(df, series, date)
  data <- to_series_list(df, x = "date", y = "value", group = "series", "line")
  
  # Output
  list(
    title = list(
      text = title
    ),
    subtitle = list(
      text = "Employment to population ratio in Australian states and territories"
    ),
    xAxis = list(
      type = 'datetime'
    ),
    yAxis = list(
      title = list(
        text = "Employment to population ratio"
      )
    ),
    legend = list(
      enabled = TRUE
    ),
    series = data
  )
}

write_json(viz_ind_emppop_state_line(), "../public/highcharts_data/test.json")
