# ==== packages ====
pkgs <- c("readODS", "dplyr", "readr", "stringr", "janitor", "tidyr", "purrr")
inst <- rownames(installed.packages())
if (length(setdiff(pkgs, inst)) > 0) {
  install.packages(setdiff(pkgs, inst), repos = "https://cloud.r-project.org")
}
library(readODS); library(dplyr); library(readr); library(stringr); library(janitor); library(tidyr); library(purrr)

# ==== paths ====
raw_dir  <- "C:/Users/z1782/OneDrive - University College London/Attachments/004/methodology/Dissertation_v2/data/raw/rq3"
proc_dir <- "C:/Users/z1782/OneDrive - University College London/Attachments/004/methodology/Dissertation_v2/data/processed/rq3"
ods_path <- file.path(raw_dir,  "Live_Tables_-_Land_Use_Stock_2022_-_LSOA.ods")
inner_csv <- file.path(proc_dir, "population_density_lsoa.csv")  # first column is LSOA code

# ==== read the 1,771 Inner London LSOA list ====
inner_codes <- read_csv(inner_csv, show_col_types = FALSE, col_select = 1) |>
  pull(1) |>
  as.character() |>
  str_trim() |>
  unique() |>
  na.omit()

# ---- quick check ----
if (length(inner_codes) != 1771) {
  message("Note: the list contains ", length(inner_codes), " LSOAs (expected 1771).")
}

# ==== Read ODS sheet P404a (percentage table, unit = %) ====
raw <- read_ods(ods_path, sheet = "P404a", col_names = FALSE)
hdr_row <- which(raw[[1]] == "LSOA code")[1]
stopifnot(!is.na(hdr_row))

tbl <- raw[(hdr_row + 1):nrow(raw), ]
names(tbl) <- raw[hdr_row, ] |> unlist() |> as.character()
tbl <- clean_names(tbl)  # e.g. "LSOA code" -> lsoa_code

# Keep only real LSOA rows, drop MSOA columns
tbl <- tbl |>
  filter(!is.na(lsoa_code), str_detect(lsoa_code, "^E010")) |>
  select(-any_of(c("msoa_code", "msoa_name")))

# Replace ".", "-" and blanks with NA and convert numeric columns to numeric
num_cols <- setdiff(names(tbl), c("lsoa_code", "lsoa_name"))
tbl <- tbl |>
  mutate(across(all_of(num_cols), ~{
    x <- as.character(.)
    x[x %in% c(".", "-", "")] <- NA
    readr::parse_number(x)
  }))

# Keep only the Inner London 1771 LSOAs
tbl <- tbl |> filter(lsoa_code %in% inner_codes)

# ==== Compute: land_use_type (dominant land use) ====
# Select "base category" columns: exclude all total summary columns and grand_total
base_cols <- setdiff(num_cols, num_cols[str_detect(num_cols, "total$|grand_total")])

# For each row, pick the column with the maximum percentage as land_use_type
mat <- as.matrix(select(tbl, all_of(base_cols)))
max_idx <- max.col(mat, ties.method = "first")
tbl$land_use_type <- base_cols[max_idx]

# ==== Compute: building_density (building coverage, %) ====
# Here "building" is interpreted as: residential buildings + institutional/public buildings + industrial/commercial buildings + agricultural buildings + unidentified buildings
# You can adjust the matching rules below according to names(tbl)
building_patterns <- c(
  "^residential$",                          # residential (avoid residential_gardens)
  "community_buildings$",                   # community buildings
  "leisure_and_recreational_buildings$",    # leisure/recreational buildings
  "^industry$", "^offices$", "^retail$", "storage_and_warehousing$",  # industry/commercial
  "institutional.*communal.*accommodations",# institutional / communal accommodations
  "agricultural_buildings$",                # agricultural buildings
  "unidentified_building$"                  # unidentified building
)
building_cols <- base_cols[str_detect(base_cols, paste(building_patterns, collapse = "|"))]

# If none matched, prompt for adjustment
if (length(building_cols) == 0) {
  stop("No building-related columns matched. Please run names(tbl) to inspect column names and adjust building_patterns.")
}

message("Columns used to compute building_density:\n  ",
        paste(building_cols, collapse = ", "))

tbl <- tbl |>
  mutate(building_density = rowSums(across(all_of(building_cols)), na.rm = TRUE))
# building_density is in percentage (0–100); divide by 100 if you need proportion 0–1

# ==== Output final dataset ====
out <- tbl |>
  select(lsoa_code, building_density, land_use_type)

out_path <- file.path(proc_dir, "land_use_building_density_inner_london.csv")
write_csv(out, out_path, na = "")

# Friendly notices
missing_codes <- setdiff(inner_codes, out$lsoa_code)
if (length(missing_codes) > 0) {
  message("There are ", length(missing_codes), " LSOAs in the list not found in P404a (or filtered out):\n",
          paste(missing_codes, collapse = ", "))
}
message("Done: ", out_path, "  ; rows = ", nrow(out))
