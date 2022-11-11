preset_rangeSelector <- list(
  # floating = TRUE,
  inputEnabled = F,
  buttonPosition = list(align = "right"),
  selected = 1,
  buttons = list(
    list(
      type  = 'all',
      text  =  'All',
      title =  'View all'
    ),
    list(
      type  = 'year',
      count = 5,
      text  = '5y',
      title = 'View five years'
    ),
    list(
      type  = 'year',
      count = 3,
      text  = '3y',
      title = 'View three years'
    ),
    list(
      type  = 'year',
      text  = '1y',
      title = 'View one year'
    )
  )
)