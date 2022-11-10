# Minimal p_load function from the pacman package
p_load <- function(...){
  pkgs <- as.character(match.call(expand.dots = FALSE)[[2]])
  pkgs_avail <- pkgs %in% .packages(all.available = TRUE)
  install.packages(pkgs[!pkgs_avail])
  lapply(pkgs, library, character.only = TRUE)
  return()
}


# Path helpers
path_asset <- function(x) normalizePath(file.path("../src/assets", x))
path_sparkline <- function(x) normalizePath(
  file.path("../public/sparklines", paste0(x, ".svg"))
)
path_src <- function(x) normalizePath(file.path("../src/lib", x))



# Fill function 
fill <- function(v){
  fixer <- function(a, b) if(is.na(b)){c(a, a[length(a)])} else {c(a, b)}
  Reduce(fixer, v)
}


# convert datetime to milliseconds since 1970 (highcharts format)
datetime_to_timestamp <- function(dt) {
  tmstmp <- as.numeric(as.POSIXct(dt))
  tmstmp <- 1000 * tmstmp
  tmstmp
}

# Extract sates from chararacter vector
state_order <- c("NSW", "Vic", "QLD", "SA", "WA", "Tas", "NT", "ACT")

recode_states <- function(v){
  
  states <- c(
    NSW = "New South Wales",
    Vic = "Victoria",
    QLD = "Queensland",
    SA  = "South Australia",
    WA  = "Western Australia",
    Tas = "Tasmania",
    NT  = "Northern Territory",
    ACT = "Australian Capital Territory"
  )
  
  v <- tstrsplit(v, split = ";")
  v <- v[sapply(v, \(x) any(grepl(paste(states, collapse = "|"), x)))]
  
  
  if(length(v) == 1){ 
    v <- v[[1]]
  } else if(length(v) > 1){
    stop("Multiple state name matches")
  } else {
    stop("cannot find state names")
  }
  
  
  v <- gsub("[[:punct:]]", "", v)
  v <- gsub("^[[:blank:]]*|[[:blank:]]*$", "", v)
  
  for(i in names(states)){
    v[v == states[i]] <- i
  }
  
  v
}



# Prep data as series list
to_series_list <- function(
    df, 
    x, 
    y, 
    group, 
    series_type = "line", 
    visible = NULL
  ){
  df <- copy(df)
  setDT(df)
  setnames(df, c(group, x, y), c("group", "x", "y"))
  dropnames <- names(df)[!(names(df) %in% c("group", "x", "y"))]
  df[, (dropnames) := NULL]
  date_cols <- which(
    sapply(df, inherits, what = c("Date", "POSIXt"))
  )
  if(length(date_cols) > 0){
    df[, (date_cols) := lapply(.SD, datetime_to_timestamp), .SDcols = date_cols]
  }
  df <- split(df, by = "group", keep.by = FALSE)
  
  # mapply(
  #   function(n, type){#, vis){
  #     list(
  #       name = n,
  #       type = type,
  #       # visible = vis,
  #       data = df[[n]]
  #     )
  #   },
  #   n = names(df),
  #   type =  series_type,
  #   # vis = visible,
  #   SIMPLIFY = FALSE
  # )
  
  lapply(
    names(df),
    \(x) list(
      name = x,
      type = series_type,
      visible = TRUE,
      data = df[[x]]
    )
  )

}

