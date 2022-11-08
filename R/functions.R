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
path_sparkline <- function(x) normalizePath(
  file.path("../public/sparklines", paste0(x, ".svg"))
  )
path_src <- function(x) normalizePath(file.path("../src/lib", x))


# Number formatters
comma <- function(val, suffix = NULL, add_sign = FALSE, decimal = 2){
  
  if(add_sign){
    signs <- ifelse(sign(val) == -1L, "-", "+")
    val <- abs(val)
  } else {
    signs <- NULL
  }
  
  
  out <- format(
    round(val, decimal), 
    justify = "none", 
    big.mark = ",", 
    width = 0
  )
  
  paste0(
    signs, 
    gsub(" ", "", out), 
    suffix
  )

}

format_percent <- function(val, add_sign = FALSE){
  comma(val, "%", add_sign, decimal = 1)
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
    form == "PPT"    , comma(val, ppt_abbr, add_sign, 1),
    TRUE             , comma(val, NULL, add_sign)
  )
}

# HTML funtions
tagfun <- function(..., tag, selfclosing = FALSE){
  i <- list(...)
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
  
  if(selfclosing){
    open_tag <- paste0("<", tag, " ", attrs, "/>")
    return(open_tag)
  } else {
    open_tag <- paste0("<", tag, " ", attrs, ">")
    close_tag <- paste0("</", tag, ">")
    return(c(open_tag, unlist(i), close_tag))
  }
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

table_tag <- function(...){
  tagfun(..., tag = "table")
}

thead <- function(...){
  tagfun(..., tag = "thead")
}

tbody <- function(...){
  tagfun(..., tag = "tbody")
}

caption <- function(...){
  tagfun(..., tag = "caption")
}

svg_tag <- function(...){
  tagfun(..., tag = "svg")
}

br <- function(){"<br/>"}

img <- function(...){
  tagfun(..., tag = "img", selfclosing = TRUE)
}

abbr <- function(...){
  tagfun(..., tag = "abbr")
}

ppt_abbr <- paste(
  abbr(
  class = "text-decoration-none", 
  title="Percentage points",
  "&nbsp;ppt"
  ),
  collapse = " "
)

# Sparkline fun
pal <- c(
  "#62BB46",
  "#1D9EC3",
  "#14826D",
  "#BCD3EF",
  "#2A6FA2",
  "#004676",
  "#745ECF",
  "#C0E4B5",
  "#53565A",
  "#1F1547"
)

rescale <- function(x, newmax, newmin = 0, decimal = 2){
  x <- (x-min(x))/(max(x)-min(x))
  round(x * (newmax - newmin) + newmin, decimal)
}

sparkline <- function(
    y, 
    width = 150, 
    height = 50, 
    pad = 6, 
    colour = "#201547"
  ){
  n <- length(y)
  x <- rescale(seq_len(n), newmax = width - pad,  newmin = pad)
  y <- rescale(y, newmax = pad, newmin = height - pad)
  
  points <- paste(paste0(x, ",", y), collapse = " ")
  
  svg_tag(
    height = height,
    width = width,
    version = 1.1,
    xmlns = "http://www.w3.org/2000/svg",
    paste0(
      '<polyline points="',
      points,
      '" style="fill:none;stroke:',
      colour,
      ';stroke-width:3" />'
    ),
    sprintf(
      '<circle cx="%s" cy="%s" r="4" fill="white" stroke = "%s" stroke-width="3"/>',
      last(x), 
      last(y),
      colour
      )
  )
}


# Fill function
fill <- function(v){
  fixer <- function(a, b) if(is.na(b)){c(a, a[length(a)])} else {c(a, b)}
  Reduce(fixer, v)
}


# Generate html table 
make_table <- function(
    series_ids, 
    row_headers,
    highlight_rows, 
    up_is_good,
    smoothing = NULL, 
    notes = NULL,
    conn = NULL,
    table_class = "table table-hover table-borderless"
){
  
  # Ensure connection exists
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  
  # Collapse caption
  notes <- paste(unique(notes), collapse = " ")
  
  
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
  df[, value := frollmean(value, smoothing[1]), series_id]
  
  # Check time interval
  interval <- df[, unique(frequency)]
  if(length(interval) > 1) stop(
    "Tables can only have one frequency (e.g. not monthly AND quarterly)"
  )
  
  # Generate sparkline colours
  colourgroup <- data.table(
    series_id = factor(series_ids),
    colour = fill(
      fifelse(highlight_rows, series_ids, as.character(NA))
    )
  )
  
  sparkcolours <- rep(
    pal, 
    ceiling(
      length(unique(colourgroup$colour)) / length(pal)
      )
    )
  
  colourgroup[, colour := sparkcolours[as.integer(factor(colour))]]
  df <- colourgroup[df, on = "series_id"]
  
  # Generate sparklines and save
  sparkdata <- df[
    date >= max(date) - months(12 * 3), 
    .(sparkline = sparkline(value, colour = colour[1])),
    series_id
  ]
  sparkdata[, writeLines(sparkline, path_sparkline(series_id)), series_id]

  # Generate sparkline alt text
  sparktext <- df[
    date > max(date) - months(12 * 3), 
    .(
      current = last(value),
      average = mean(value, na.rm = TRUE),
      name = tolower(row_headers[match(series_id[1], series_ids)])
    ),
    series_id
  ]
  
  sparktext[
    , `:=`(
    name = fcase(
      grepl("rate|ratio", name), paste("The", name, "is"),
      grepl("persons", name), paste("The number of", name, "is"),
      grepl("employed ", name), paste("The number of people", name, "is"),
      rep(TRUE, .N), paste0(toupper(substring(name, 1,1)), substring(name, 2)," is")
    ),
    movement = fifelse(
      current >= average, 
      "higher than average",
      "lower than average"
      )
  )
  ]
  
  sparktext[, alt := paste(name, movement)]
  
  
  # Get interval dates
  breaks <- c(
    current =  df[, max(date)],
    last    = df[, max(date[date != max(date)])],
    year    = df[, max(date) - months(12)],
    covid   = as.Date("2019-03-01")
  )
  
  # Generate deltas
  deltas <- df[date %in% breaks]
  deltas[, date := names(breaks)[match(date, breaks)]]
  deltas[, date := factor(date, levels = names(breaks))]
  deltas <- dcast(deltas, series_id + data_type ~ date, value.var = "value")
  
  diff_cols <- names(breaks)[names(breaks) != "current"]
  deltas[, (diff_cols) := lapply(.SD, \(x) current - x), .SDcols = diff_cols]
  
  
  # Generate current backgrounds
  avg_3_year <- df[
    date >= max(date) - months(12 * 3), 
    .(avg  = mean(value)),
    keyby = series_id
  ]
  
  backgrounds <- deltas[avg_3_year][,
    `:=`(
      up_is_good = up_is_good[match(series_id, series_ids)],
      current = current - avg,
      avg = NULL,
      data_type = NULL
    )
  ][,
    (names(breaks)) := lapply(.SD, \(x){
      fifelse(
        (x >= 0 & up_is_good) | (x <= 0 & !up_is_good),
        "table-success",
        "table-danger"
        )
    }),
    .SDcols = names(breaks)
  ][,
    up_is_good := NULL
  ]
  
  backgrounds <- melt.data.table(
    data = backgrounds, 
    id.vars = "series_id",
    measure.vars = names(breaks),
    variable.name = "date",
    value.name = "bg"
    )
  
  
  # format deltas
  deltas[, current := format_any(current, data_type)]
  deltas[data_type == "PERCENT", data_type := "PPT"]
  deltas[, 
    (diff_cols) := lapply(.SD, format_any, form = data_type, add_sign = TRUE),
    .SDcols = diff_cols
  ]
  deltas[, data_type := NULL]
  
  
  # Convert to HTML table
  tdeltas <- as.data.table(t(deltas))
  
  rows <- sapply(tdeltas, function(x){
    row_title     <- row_headers[match(x[1], series_ids)]
    row_content   <- x[2:length(x)]
    row_highlight <- highlight_rows[match(row_title, row_headers)]
    alt_text      <- sparktext$alt[match(x[1], sparktext$series_id)]
    row_classes   <- backgrounds[series_id == x[1], bg] 
    
    if(row_highlight){
      tr(
        class = "border-top",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", x[1], ".svg"), 
            alt = alt_text,
            class = "img-fluid sparkline float-start"
            )
          ),
        mapply(td, row_content, class = row_classes)
      )
    } else {
      tr(
        class = "subrow",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", x[1], ".svg"), 
            alt = alt_text,
            class = "img-fluid sparkline float-start"
          )
        ),
        mapply(td, row_content, class = row_classes)
      )
    }
    
  })
  
  rows <- tbody(rows)
  
  header <- thead(
    td(""),
    th(
      class = "text-start",
      scope = "col",
      "Recent trend",
      br(),
      small(
        class = "text-muted p-0", 
        "Last 3 years"
      )
    ),
    th(
      scope = "col",
      "Latest figures",
      br(),
      small(
        class = "text-muted p-0", 
        format(breaks["current"], "%b&nbsp;%Y")
        )
    ),
    th(
      scope = "col",
      paste("One", tolower(interval), "change"),
      br(),
      small(
        class = "text-muted p-0", 
        format(breaks["last"], "%b&nbsp;%Y")
      )
    ),
    th(
      scope = "col",
      "One year change",
      br(),
      small(
        class = "text-muted p-0", 
        format(breaks["year"], "%b&nbsp;%Y")
      )
    ),
    th(
      scope = "col",
      "Change since COVID&#8209;19",
      br(),
      small(
        class = "text-muted p-0", 
        format(breaks["covid"], "%b&nbsp;%Y")
      )
    )
  )
  
  # Return table
  return(
    table_tag(
      class = table_class, 
      caption(notes),
      header,
      rows
    )
  )
  
}
