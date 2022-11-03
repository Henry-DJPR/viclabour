# Minimal p_load function from the pacman package
p_load <- function(...){
  pkgs <- as.character(match.call(expand.dots = FALSE)[[2]])
  pkgs_avail <- pkgs %in% .packages(all.available = TRUE)
  install.packages(pkgs[!pkgs_avail])
  lapply(pkgs, library, character.only = TRUE)
  return()
}


# Gets corresponding table number from series ID
series_table_no <- function(series_ids, conn = NULL){
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  dbGetQuery(
    conn = conn,
    statement = 
      paste0(
        'SELECT DISTINCT "series_id", "table_no"',
        'FROM "abs_labour_force"',
        "WHERE series_id IN ('",
        paste0(series_ids, collapse = "', '"),
        "')"
      )
  )
}


# Determine whether a particular collection of series requires an update
series_req_update <- function(series_ids, update_tbls = NULL, conn = NULL){
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  if(is.null(update_tbls) & !exists("tables_to_update")){
    stop("Please provide update_tbls")
  } else if(is.null(update_tbls)) {
    update_tbls <- tables_to_update
  } 
  
  lookup <- series_table_no(series_ids, conn)
  any(lookup$table_no %in% update_tbls)
}


# Path helpers
path_asset <- function(x) normalizePath(file.path("../src/assets", x))
path_src <- function(x) normalizePath(file.path("../src/src", x))


# Number formatters
comma <- function(val, suffix = NULL, add_sign = FALSE, decimal = 2){
  
  if(add_sign){
    signs <- ifelse(sign(val) == -1L, "-", "+")
    val <- abs(val)
  } else {
    signs <- NULL
  }
  
  paste0(
    signs, 
    format(round(val, decimal), justify = "none", big.mark = ","), 
    suffix
  )
  
}

format_percent <- function(val, add_sign = FALSE){
  comma(val, "%", add_sign)
}

format_number <- function(val, add_sign = FALSE){
  val_abs <- abs(val)
  fcase(
    val_abs >= 2e09, comma(val / 1e09, "b", add_sign, 0),
    val_abs >= 2e06, comma(val / 1e06, "m", add_sign, 0),
    val_abs >= 2e03, comma(val / 1e03, "k", add_sign, 0),
    val_abs <  2e03, comma(val,       NULL, add_sign, 0)
  )
}

format_any <- function(val, form = "number", add_sign = FALSE){
  fcase(
    form == "PERCENT", format_percent(val, add_sign),
    form == "STOCK"  , format_number(val, add_sign),
    form == "FLOW"   , format_number(val, add_sign),
    form == "PPT"    , comma(val, " ppt", add_sign, 1),
    TRUE             , comma(val, NULL, add_sign)
  )
}

# HTML funtions
tagfun <- function(..., tag){
  i <- match.call(expand.dots = FALSE)[[2]]
  n <- names(i)
  
  
  if(!is.null(n)){
    attrs <- i[n != ""]
    attrs <- unlist(attrs)
    i     <- i[n == ""]
    n     <- n[n != ""]
    attrs <- paste0(" ", paste0(n , ' = "', attrs, '"'), collapse = " ")
  } else {
    attrs <- ""
  }
  
  open_tag <- paste0("<", tag, attrs, ">")
  close_tag <- paste0("</", tag, ">")
  return(c(open_tag, unlist(i), close_tag))
}

div <- function(...){
  tagfun(..., tag = "div")
}

span <- function(...){
  tagfun(..., tag = "span")
}

small <- function(...){
  tagfun(..., tag = "small")
}

th <- function(...){
  tagfun(..., tag = "th")
}

td <- function(...){
  tagfun(..., tag = "td")
}

tr <- function(...){
  tagfun(..., tag = "tr")
}

table <- function(...){
  tagfun(..., tag = "table")
}

thead <- function(...){
  tagfun(..., tag = "thead")
}

tbody <- function(...){
  tagfun(..., tag = "tbody")
}





# Column for tables; defined as a list of title and value functions
# table_breakpoints <- list(
#   # Current value column
#   current = list(
#     title = function(date){
#       sprintf(
#         '<strong>Current figure</strong><br/><small class="text-muted text-uppercase">%s</small>',
#         format(max(date), "%b %Y")
#       )
#     },
#     value = function(date, value, form){
#       paste0(
#         "<td>",
#         num_format(value[date == max(date)], form),
#         "</td>"
#         )
#     }
#   ),
#   # Last month column
#   last_month = list(
#     title = function(date){
#       sprintf(
#         '<strong>one month change</strong><br/><small class="text-muted text-uppercase">%s</small>',
#         format(max(date) - months(1), "%b %Y")
#       )
#     },
#     value = function(date, value, form){
#       
#       this_month <- value[date == max(date)]
#       last_month <- value[date == max(date) - months(1)]
#       month_diff <- this_month - last_month
#       month_diff_form <- ifelse(
#         form == "percent", 
#         '<abbr title = "Percentage points" class="text-decoration-none"> ppts</abbr>', 
#         paste0(
#           '<br/><small class="text-muted">(',
#           num_format(month_diff, "percent"),
#           ")</small>"
#         )
#         )
#       
#       form <- ifelse(form == "percent", "number", form)
#       
#       paste0(
#         "<td>",
#         num_format(value[date == max(date) - months(1)], form),
#         month_diff_form,
#         "</td>"
#       )
#     }
#   )
# )


# Generate html table 
make_table <- function(
    series_ids, 
    highlight_rows, 
    smoothing = NULL, 
    caption = NULL,
    conn = NULL
){
  
  # Ensure connection exists
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  # Get data
  df <- dbGetQuery(
    conn = conn,
    statement = 
      paste0(
        'SELECT *',
        'FROM "abs_labour_force"',
        "WHERE series_id IN ('",
        paste0(series_ids, collapse = "', '"),
        "')"
      )
  )
  
  
  # Ensure correct data ordering
  setDT(df)
  df[, series_id := factor(series_id, levels = series_ids)]
  setkey(df, series_id, date)
  
  # God I HATE it when databased information is in thousands
  df[unit == "000",       value := value * 1000]
  df[unit == "000 Hours", value := value * 1000]
  df[, unit := NULL]
  
  # Smooth data
  smoothing[is.na(smoothing)] <- 1
  df[, smoothing := smoothing[match(series_id, series_ids)]]
  df[, value2 := frollmean(value, smoothing[1]), series_id]
  
  # Check time interval
  interval <- df[, unique(frequency)]
  if(length(interval) > 1) stop(
    "Tables can only have one frequency (e.g. not monthly AND quarterly)"
  )
  
  # Get interval dates
  date_latest <- df[, max(date)]
  date_last   <- df[, max(date[date != max(date)])]
  date_year   <- df[, max(date) - months(12)]
  date_covid  <- as.Date("2019-03-01")
  
  
  # Generate time summary
  df_current <- df[
    date == date_latest, 
    .(current = format_any(value, data_type)), 
    keyby = series_id
  ]
  df_last <- df[
    date %in% c(date_latest, date_last),
    .(
      last = format_any(
        value[date == date_latest] - value[date == date_last], 
        form = ifelse(data_type[1] == "PERCENT", "PPT", data_type),
        add_sign =  TRUE
      )
    ), 
    keyby = series_id
  ]
  df_year <- df[
    date %in% c(date_latest, date_year),
    .(
      year = format_any(
        value[date == date_latest] - value[date == date_year], 
        form = ifelse(data_type[1] == "PERCENT", "PPT", data_type),
        add_sign =  TRUE
      )
    ),  
    keyby = series_id
  ]
  df_covid <- df[
    date %in% c(date_latest, date_covid),
    .(
      covid = format_any(
        value[date == date_latest] - value[date == date_covid], 
        form = ifelse(data_type[1] == "PERCENT", "PPT", data_type),
        add_sign =  TRUE
      )
    ),  
    keyby = series_id
  ]
  
  # Combine summary data
  Reduce(
    merge.data.table,
    list(
      df_current, 
      df_last,
      df_year,
      df_covid
    )
  )
  
}
