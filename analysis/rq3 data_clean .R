# --- Direct TS006 (Census 2021) LSOA population density ---

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(readr)
})

cat("\n=== Downloading Census 2021 TS006 (Population density) ===\n")
zip_url <- "https://www.nomisweb.co.uk/output/census/2021/census2021-ts006.zip"

tmp_zip <- tempfile(fileext = ".zip")
download.file(zip_url, tmp_zip, mode = "wb", quiet = TRUE)

# List files in the zip archive and find the CSV that contains LSOA (case may vary)
zf <- unzip(tmp_zip, list = TRUE)
lsoa_csv_name <- zf$Name[grep("lsoa.*\\.csv$", tolower(zf$Name))][1]
if (is.na(lsoa_csv_name)) stop("LSOA CSV not found in the TS006 zip archive")

tmp_csv <- tempfile(fileext = ".csv")
unzip(tmp_zip, files = lsoa_csv_name, exdir = tempdir(), overwrite = TRUE)
file.copy(file.path(tempdir(), lsoa_csv_name), tmp_csv, overwrite = TRUE)

# Read the LSOA table; TS006 column names can vary across versions, do compatibility handling here
ts006 <- read_csv(tmp_csv, show_col_types = FALSE)
nm <- tolower(names(ts006))

# Guess the code column (prefer geography_code / lsoa21cd / lsoa11cd)
code_col <- c("geography_code","lsoa21cd","lsoa11cd","geography code")
code_col <- code_col[code_col %in% nm][1]
if (is.na(code_col)) stop("LSOA code column not found")

# Guess the "population density" column (TS006 often uses obs_value or v1)
value_col <- c("obs_value","v1","population_density","density","value")
value_col <- value_col[value_col %in% nm][1]
if (is.na(value_col)) stop("Population density value column not found")

ts006_lsoa <- ts006 %>%
  rename(lsoa_code = all_of(names(ts006)[match(code_col, nm)]),
         population_density = all_of(names(ts006)[match(value_col, nm)])) %>%
  mutate(lsoa_code = as.character(lsoa_code)) %>%
  select(lsoa_code, population_density)

# Read your Inner London LSOA list
inner_london <- read_csv(
  here("output","full_london_gvi","inner_london_all_lsoas_complete.csv"),
  show_col_types = FALSE
) %>% mutate(lsoa_code = as.character(lsoa_code))

# Keep only Inner London
pop_inner <- ts006_lsoa %>% semi_join(inner_london, by = "lsoa_code")

cat(paste("Number of Inner London LSOAs matched:", nrow(pop_inner), "\n"))

# Write output to processed directory (new filename)
out_dir <- here("data","processed","rq3")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

out_file <- file.path(out_dir, "population_density_lsoa_from_TS006.csv")
write_csv(pop_inner, out_file)
cat(paste("Saved to:", out_file, "\n"))
