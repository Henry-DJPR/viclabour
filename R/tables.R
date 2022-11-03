

# Ensure this code is being run in the correct directory (not root)
if(basename(getwd()) != "R") setwd(normalizePath("./R"))




# Set cran mirror 
options(repos=structure(c(CRAN="http://cran.rstudio.com/")))




# Load in custom functions
source("functions.R")




# Load packages. Only packages with minimal dependencies. p_load in functions.R
p_load(
  data.table,
  RPostgres,
  rjson
)




# Check for Database connection environment variables
if(
  Sys.getenv("PG_READ_OPEN_USER") == "" | 
  Sys.getenv("PG_READ_OPEN_PW") == "" 
){
  stop(
    "Please ensure database user and password are set with the following",
    "names:\n",
    "username PG_READ_OPEN_USER\n",
    "password PG_READ_OPEN_PW"
    )
}




# pseudo config package set-up
db_location <- switch(
  Sys.getenv("R_CONFIG_ACTIVE"),
  "github" = list(
    host = "sppdatarepo1.caz6tvdhkmqm.ap-southeast-2.rds.amazonaws.com",
    port = 5432
  ),
  list(
    host = "10.210.1.26",
    port = 443
  )
)




# Connect to database
con <- dbConnect(
  drv = Postgres(),
  dbname = "opendata",
  user = Sys.getenv("PG_READ_OPEN_USER"),
  password  = Sys.getenv("PG_READ_OPEN_PW"),
  host = db_location$host,
  port = db_location$port
  )



# Check latest data update
latest <- dbGetQuery(
  conn = con,
  statement = 
    '
    SELECT "table_no", MAX("timestamp") AS "db_date"
    FROM "abs_labour_force"
    GROUP BY "table_no"
   '
)




# Check if any data needs updating
setDT(latest, key = "table_no")

if(!file.exists("timestamps.rds")){
  timestamp_cache <- copy(latest)
  setnames(timestamp_cache, "db_date", "website_date")
  saveRDS(timestamp_cache, "timestamps.rds")
  
  latest <- readRDS("timestamps.rds")[latest]
  tables_to_update <- latest$table_no
  
} else {
  latest <- readRDS("timestamps.rds")[latest]
  tables_to_update <- latest[website_date < db_date, table_no]
}

update_required <- length(tables_to_update) > 0




# Update content
if(update_required){
  
  # Read in table generation csv
  table_meta <- fread("table_meta.csv", data.table = TRUE)
  
  
  
  
  # Determine tables to update
  to_update <- table_meta[, .(update = series_req_update(series_id)), table_name]
  to_update <- to_update[update == TRUE, table_name]
  
  
  
  
  # Generate tables
  if(length(to_update) > 0){
    message(
      "Updating the following website tables:\n",
      paste(to_update, collapse = "\n")
    )
    
    
    
    
  }
  
  
}



# Youth data = 12m rolling average
data <- data %>%
  dplyr::group_by(.data$series_id) %>%
  dplyr::mutate(value = dplyr::if_else(.data$series_id %in% c(
    "A84433601W",
    "A84424691V",
    "A84424687C",
    "A84424692W"
  ),
  slider::slide_mean(.data$value, before = 11, complete = TRUE),
  .data$value
  ))

# Regional data = 3m rolling average
data <- data %>%
  dplyr::mutate(
    value = dplyr::if_else(
      .data$series_id == "A84600079X",
      slider::slide_mean(.data$value, before = 2, complete = TRUE),
      .data$value
  )) %>%
  dplyr::ungroup()


djprlabourdash:::create_summary_df(data)


