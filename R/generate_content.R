
# Force update?
force_update <- TRUE



# Ensure this code is being run in the correct directory (not root)
if(basename(getwd()) != "R") setwd(normalizePath("./R"))




# Set cran mirror 
options(repos=structure(c(CRAN="http://cran.rstudio.com/")))




# Load in custom functions
lapply(list.files("R", full.names = TRUE), source)



# Load packages. Only packages with minimal dependencies. p_load in functions.R
p_load(
  data.table,
  RPostgres,
  jsonlite,
  pbapply
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

if(!file.exists("timestamps.rds") | force_update){
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
  
  # Read in table generation csv & find all viz functions
  table_meta <- fread("table_meta.csv", data.table = TRUE)
  vizualisations <- ls(pattern = "viz_")
  
  
  
  # Determine tables to update
  update_tables <- table_meta[, .(update = series_req_update(series_id)), table_name]
  update_tables <- update_tables[update == TRUE, table_name]
  
  
  
  
  # Generate tables
  if(length(update_tables) > 0){
    message(
      "Updating the following website tables:\n",
      paste(update_tables, collapse = "\n")
    )
    
    table_list <- split(
      table_meta[table_name %in% update_tables],
      by = "table_name"
    )
    
    table_list <- pblapply(table_list, \(x){
      make_table(
        series_ids = x$series_id,
        row_headers = x$name,
        highlight_rows = x$highlight,
        smoothing = x$smoothing_months,
        up_is_good = x$up_is_good,
        notes = x$caption
      )
    }) 
    
    mapply(
      function(table, table_name){
        path <- path_src(paste0("Table", table_name, ".svelte"))
        writeLines(table, path)
        },
      table = table_list,
      table_name = names(table_list)
    )
    
    
  }
  
  
  # Update chart data
  message("Updating chart data")
  pblapply(vizualisations, function(v){
    out <- get(v)()
    write_json(
      out, 
      paste0("../public/highcharts_data/", v, ".json"), 
      auto_unbox = TRUE
      )
  })
  
  
}


