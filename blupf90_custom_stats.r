# BLUPF90 Data and Pedigree Statistics (Custom Format)
# ----------------------------------------------------------
# This script reads a pedigree and data file, filters out invalid records,
# and calculates key statistics, considering "0" in the trait column as missing.
# ----------------------------------------------------------
# Replace 'renadd04.ped' and 'renf90.dat' with your actual filenames.
# Adjust column numbers as needed based on your actual data format.

# Assumptions:
#   - **Pedigree file**: 10 columns
#       * Column 1 = Animal ID (numeric, internal)
#       * Column 2 = Sire ID (numeric)
#       * Column 3 = Dam ID (numeric)
#       * Column 10 = Animal ID (alphanumeric, external)
#   - **Data file**: 8 columns
#       * Column 1 = Trait value (0 means missing)
#       * Column 6 = Animal ID (alphanumeric, external)

# Load required package
library(dplyr)

# Step 1: Read input files (no headers, all values as character)
ped  <- read.table("renadd04.ped", header = FALSE, colClasses = "character")
data <- read.table("renf90.dat", header = FALSE, colClasses = "character")

# Step 2: Filter out records with missing trait values (Column 1 == "0")
# -----------------------------------------------------------------------
# We assume trait value is in column 1, and animal ID is in column 6
data_filtered <- data[data[, 1] != "0", ]

# Step 3: Extract animal IDs from filtered data and full pedigree
# ----------------------------------------------------------------
# All animals with valid trait records (regardless of being sire, dam, or offspring)
data_animals <- unique(data_filtered[, 6])  # Alphanumeric animal IDs

# All animals listed in the pedigree file (Column 10 = alphanumeric ID)
ped_animals <- unique(ped[, 10])

# Step 4: Extract numeric sire and dam IDs from pedigree (columns 2 and 3)
# -------------------------------------------------------------------------
sires_in_ped <- unique(ped[, 2])
dams_in_ped  <- unique(ped[, 3])

# Remove "0" entries which indicate unknown parents
sires_in_ped <- sires_in_ped[sires_in_ped != "0"]
dams_in_ped  <- dams_in_ped[dams_in_ped != "0"]

# Step 5: Map numeric IDs (sire/dam) to alphanumeric IDs using ID map
# --------------------------------------------------------------------
# Create a mapping table: numeric ID (column 1) → alphanumeric ID (column 10)
id_map <- ped[, c(1, 10)] %>%
  rename(numeric_id = V1, alphanumeric_id = V10) %>%
  distinct()

# Use the map to convert sire and dam numeric IDs to alphanumeric equivalents
sires_alphanumeric <- id_map$alphanumeric_id[match(sires_in_ped, id_map$numeric_id)]
dams_alphanumeric  <- id_map$alphanumeric_id[match(dams_in_ped, id_map$numeric_id)]

# Step 6: Calculate statistics
# -----------------------------
# 1. Number of animals with valid (non-zero) records
#    Includes any animal who had a non-zero trait value (offspring, sire, or dam)
animals_with_records <- length(data_animals)

# 2. Number of sires who themselves had valid records
#    That is: they were sires in the pedigree AND also appear in data with valid records
sires_with_records <- sum(sires_alphanumeric %in% data_animals)

# 3. Number of dams who themselves had valid records
dams_with_records <- sum(dams_alphanumeric %in% data_animals)

# 4. Total number of unique animals in the pedigree (based on alphanumeric ID)
total_animals <- length(ped_animals)

# 5. Total number of valid records (i.e., records where trait ≠ "0")
total_records <- nrow(data_filtered)

# Step 7: Output results
# -----------------------
cat("Number of animals with valid records (trait ≠ 0):", animals_with_records, "\n")
cat("Number of sires who have valid records themselves:", sires_with_records, "\n")
cat("Number of dams who have valid records themselves:", dams_with_records, "\n")
cat("Total animals in pedigree:", total_animals, "\n")
cat("Total number of valid records (trait ≠ 0):", total_records, "\n")

# Step 7: Output results as a neat table
# ---------------------------------------
# Use data.frame() or tibble() to organize the output in a structured way, then print it nicely:

# Load library for nice table display
library(tibble)

# Create a summary table
summary_table <- tibble::tibble(
  Statistic = c(
    "Number of animals with valid records (trait ≠ 0)",
    "Number of sires with valid records",
    "Number of dams with valid records",
    "Total animals in pedigree",
    "Total number of valid records"
  ),
  Count = c(
    animals_with_records,
    sires_with_records,
    dams_with_records,
    total_animals,
    total_records
  )
)

# Print the table
print(summary_table)

# If you want nicer formatting, try knitr::kable():
# Optional: prettier output
knitr::kable(summary_table, format = "simple", caption = "BLUPF90 Data Summary Statistics")


###############################################################################################

# Optional Additional Statistics
# -------------------------------
#Here are some extra stats that could be useful:

# Number of animals in both pedigree and data ---> Helps identify overlap
# Number of sires/dams with no records ---> Understand data gaps
# Number of records per animal (min, max, avg) ---> To detect data imbalance
# Proportion of pedigree animals with records ---> Data coverage metric
# Proportion of sires/dams with records ---> Coverage of parental info

# Animals in both pedigree and data
animals_in_both <- sum(data_animals %in% ped_animals)

# Sires/dams with no records
sires_without_records <- sum(!(sires_alphanumeric %in% data_animals))
dams_without_records  <- sum(!(dams_alphanumeric %in% data_animals))

# Number of records per animal (descriptive stats)
records_per_animal <- table(data_filtered[, 6])
min_records <- min(records_per_animal)
max_records <- max(records_per_animal)
avg_records <- mean(records_per_animal)

# Proportions
prop_animals_with_records <- round(animals_with_records / total_animals, 3)
prop_sires_with_records <- round(sires_with_records / length(sires_alphanumeric), 3)
prop_dams_with_records  <- round(dams_with_records / length(dams_alphanumeric), 3)


# Add extra stats to the summary table
extended_summary <- tibble::tibble(
  Statistic = c(
    "Number of animals with valid records (trait ≠ 0)",
    "Number of sires with valid records",
    "Number of dams with valid records",
    "Total animals in pedigree",
    "Total number of valid records",
    "Animals in both pedigree and data",
    "Sires without records",
    "Dams without records",
    "Min records per animal",
    "Max records per animal",
    "Avg records per animal",
    "Proportion of pedigree animals with records",
    "Proportion of sires with records",
    "Proportion of dams with records"
  ),
  Value = c(
    animals_with_records,
    sires_with_records,
    dams_with_records,
    total_animals,
    total_records,
    animals_in_both,
    sires_without_records,
    dams_without_records,
    min_records,
    max_records,
    round(avg_records, 2),
    prop_animals_with_records,
    prop_sires_with_records,
    prop_dams_with_records
  )
)

# Print the extended summary
knitr::kable(extended_summary, format = "simple", caption = "Extended BLUPF90 Data Summary")



















