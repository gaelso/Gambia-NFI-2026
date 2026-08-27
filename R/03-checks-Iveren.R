

# ================================================================
# THE GAMBIA NATIONAL FOREST INVENTORY 2025
# FINAL TREE DATA CLEANING AND QA/QC WORKFLOW
# ATTRIBUTES 1–16
# ================================================================
#
# PURPOSE
#
# This script consolidates the finalized tree-data cleaning and
# QA/QC procedures for the 2025 Gambia National Forest Inventory.
#
# The workflow follows the order of the tree attributes on the
# NFI field form, from Attribute 1 to Attribute 16.
#
# Earlier exploratory checks, validation exercises and consultations
# with the Department of Forestry (DoF) are not repeated in full.
# Instead, this script applies the final agreed cleaning decisions.
#
#
# IMPORTANT PRINCIPLES
#
# 1. Original Arena variables are retained wherever possible.
#
# 2. Cleaned variables are created separately where corrections or
#    analytical treatment are required.
#
# 3. Verified corrections from DoF are incorporated into the
#    cleaned variables.
#
# 4. Where a separate cleaning exercise has already been completed
#    (e.g. species-name standardization), the finalized mapping table
#    is applied rather than repeating the exploratory cleaning.
#
# 5. Unresolved measurements are not replaced with guessed values.
#    Where necessary, they are set to NA in the cleaned variable.
#
# 6.The final tree dataset must retain one row per tree/stump.
#

# ================================================================
# 0. LOAD REQUIRED PACKAGES
# ================================================================

# library(dplyr)
# library(tidyr)
# library(stringr)
# library(readr)
# library(readxl)   # Import finalized Excel species-name map

# ----------------------------------------------------------------
# Import the main Arena tree dataset.
#
# This is the primary dataset to which the finalized cleaning and
# QA/QC decisions for Form 5 Attributes 1–16 will be applied.
# ----------------------------------------------------------------

#tree <- read_csv("data/input/Trees_data.csv")
tree_bkp <- tree
tree <- data_clean$tree

dim(tree)
names(tree)


# ============================================================
# PART 1: BASIC RECORD AND SAMPLING-STRUCTURE QA
# ============================================================
#
# This section examines the first three attributes of the NFI
# tree field form:
#
#   Attribute 1 - Tree/Stump No.
#   Attribute 2 - Plot/Subplot
#   Attribute 3 - Land Cover No.
#
# These attributes establish the identity and sampling location
# of each tree/stump record and are therefore checked before the
# biological and measurement attributes.
#
# Previous exploratory QA found no errors requiring correction
# for Attributes 1-3. The checks are retained here so that the
# final cleaning workflow remains reproducible and documents the
# basis for retaining the original Arena variables unchanged.



# ============================================================
# ATTRIBUTE 1: TREE / STUMP NO.
# ============================================================

# Tree/Stump No. identifies individual tree or stump records within
# each plot.
#
# The QA checks assess whether:
#   - Tree/Stump No. is complete;
#   - values are valid (> 0); and
#   - Tree/Stump numbers are unique within each cluster and plot.
#
# Previous QA found no problems with this attribute.


# ------------------------------------------------------------
# 1.1 Basic validity check
# ------------------------------------------------------------

tree %>%
  summarise(
    total_records = n(),
    missing_tree_no = sum(is.na(tree_no)),
    zero_tree_no = sum(tree_no == 0, na.rm = TRUE),
    negative_tree_no = sum(tree_no < 0, na.rm = TRUE),
    min_tree_no = min(tree_no, na.rm = TRUE),
    max_tree_no = max(tree_no, na.rm = TRUE)
  )


# ------------------------------------------------------------
# 1.2 Duplicate Tree/Stump No. check
# ------------------------------------------------------------

# Tree/Stump numbers do not need to be globally unique across
# the entire inventory.
#
# They are expected to be unique within each cluster and plot.

tree_no_duplicates <- tree %>%
  count(
    cluster_no,
    plot_no,
    tree_no,
    name = "n"
  ) %>%
  filter(
    !is.na(tree_no),
    n > 1
  )

tree_no_duplicates


# ------------------------------------------------------------
# 1.3 Inspect duplicates if any are detected
# ------------------------------------------------------------

# This inspection will return no records when the duplicate check
# above is empty.

tree %>%
  semi_join(
    tree_no_duplicates,
    by = c("cluster_no", "plot_no", "tree_no")
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_plot_type_label,
    tree_lulc_no,
    tree_status_label,
    tree_species_scientific_name,
    tree_dbh,
    tree_height
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


# ------------------------------------------------------------
# ATTRIBUTE 1 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

#  QA result:
#   - 3,672 tree/stump records were checked.
#   - No Tree/Stump No. was missing.
#   - No zero or negative Tree/Stump No. was recorded.
#   - No duplicate Tree/Stump No. occurred within the same
#     cluster and plot.
#   - Observed Tree/Stump numbers ranged from 1 to 52.
#
# Final decision:
# No cleaning is required.
#
# The original Arena variable:
# tree_no is retained unchanged.


# ============================================================
# ATTRIBUTE 2: PLOT / SUBPLOT
# ============================================================

# The NFI uses two tree-measurement plot sizes:
#
#   Main plot (Rectangular):
#     DBH >= 20 cm
#
#   Subplot (Square):
#     DBH >= 10 cm and < 20 cm
#
# The Plot/Subplot attribute determines the sampling area from
# which each tree/stump was measured.
#
# The QA checks assess whether:
#   - Plot/Subplot information is complete;
#   - only the expected Arena categories are present; and
#   - the recorded plot type agrees with the tree DBH.
#
## ------------------------------------------------------------
# 2.1 Check Plot/Subplot categories
# ------------------------------------------------------------

# Display the Arena plot-type codes, labels and record counts.

tree %>%
  count(
    tree_plot_type,
    tree_plot_type_label,
    .drop = FALSE
  )


# ------------------------------------------------------------
# 2.2 Check for missing Plot/Subplot information
# ------------------------------------------------------------

tree %>%
  summarise(
    missing_plot_type =
      sum(is.na(tree_plot_type)),
    
    missing_plot_type_label =
      sum(is.na(tree_plot_type_label))
  )


# ------------------------------------------------------------
# 2.3 Check consistency between Plot/Subplot and DBH
# ------------------------------------------------------------

# Expected DBH allocation:
#
#   Main plot:
#     DBH >= 20 cm
#
#   Subplot:
#     DBH >= 10 cm and < 20 cm
#
# The check below identifies any records that violate these rules.
#
# A result of zero rows means that all records comply with the
# expected Plot/Subplot DBH allocation.

plot_dbh_violations <- tree %>%
  filter(
    
    # Main-plot tree below the 20 cm threshold
    (tree_plot_type_label == "Main plot (Rectangular)" &
       tree_dbh < 20) |
      
      # Subplot tree outside the 10-19.9 cm DBH range
      (tree_plot_type_label == "Subplot (Square)" &
         (tree_dbh < 10 | tree_dbh >= 20))
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_plot_type,
    tree_plot_type_label,
    tree_status_label,
    tree_dbh
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )

plot_dbh_violations


# Number of Plot/Subplot-DBH violations

nrow(plot_dbh_violations)


# ------------------------------------------------------------
# ATTRIBUTE 2 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Plot/Subplot information was complete for all 3,672 records
#
# Only the two expected categories were present:
#
#   1 = Main plot (Rectangular) - 2,066 records
#   2 = Subplot (Square)        - 1,606 records
#
# No records violated the applicable DBH allocation rule
#
# Final decision:
# No cleaning is required.
#
# The original Arena variables `tree_plot_type` and
# `tree_plot_type_label` are retained unchanged


# ============================================================
# ATTRIBUTE 3: LAND COVER NO.
# ============================================================

# Land Cover No. links each tree/stump record to the corresponding
# LU/LC section recorded within the sample plot.
#
# A sample plot may contain more than one LU/LC section where the
# plot crosses a land-cover boundary. Individual tree records should
# therefore be linked to the LU/LC section in which they occur.
#
# For the present Arena tree dataset:
#
#   LU/LC id 1 = Forest land
#   LU/LC id 2 = Cropland
#
# Previous QA identified two plots containing both LU/LC sections.
# These were treated as legitimate mixed-LU/LC plots rather than
# as data-entry errors.


# ------------------------------------------------------------
# 3.1 Check Land Cover No. categories and frequencies
# ------------------------------------------------------------

tree %>%
  count(
    tree_lulc_no,
    tree_lulc_no_label,
    .drop = FALSE
  )


# ------------------------------------------------------------
# 3.2 Check for missing Land Cover No.
# ------------------------------------------------------------

tree %>%
  summarise(
    missing_lulc_no =
      sum(is.na(tree_lulc_no)),
    
    missing_lulc_label =
      sum(is.na(tree_lulc_no_label))
  )


# ------------------------------------------------------------
# 3.3 Identify plots containing more than one LU/LC section
# ------------------------------------------------------------

# This check identifies plots where individual tree/stump records
# are associated with more than one Land Cover No.
#
# More than one LU/LC section within a plot is not automatically
# an error because a sample plot may cross a land-cover boundary.

plots_multiple_lulc <- tree %>%
  group_by(
    cluster_no,
    plot_no
  ) %>%
  summarise(
    n_lulc = n_distinct(
      tree_lulc_no,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  filter(n_lulc > 1)

plots_multiple_lulc


# ------------------------------------------------------------
# 3.4 Inspect records linked to LU/LC id 2
# ------------------------------------------------------------

# Only 15 tree/stump records were linked to LU/LC id 2.
#
# These records are retained for inspection to document where
# the second LU/LC section occurs.

lulc_2_records <- tree %>%
  filter(tree_lulc_no == 2) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_plot_type_label,
    tree_lulc_no,
    tree_lulc_no_label,
    tree_status_label,
    tree_species_scientific_name,
    tree_local_name,
    tree_dbh,
    tree_height
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )

lulc_2_records


# ------------------------------------------------------------
# 3.5 Examine LU/LC distribution within the affected plots
# ------------------------------------------------------------

#  LU/LC id 2 occurred only within:
#
#   Cluster 15, Plot 4
#   Cluster 37, Plot 3
#
# The table below shows the number of tree records assigned to
# each LU/LC section within those plots.

tree %>%
  filter(
    (cluster_no == 15 & plot_no == 4) |
      (cluster_no == 37 & plot_no == 3)
  ) %>%
  count(
    cluster_no,
    plot_no,
    tree_lulc_no,
    tree_lulc_no_label
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_lulc_no
  )


# ------------------------------------------------------------
# ATTRIBUTE 3 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# # Land Cover No. was complete for all 3,672 tree/stump records
#
# Of these:
#
#   3,657 records were linked to LU/LC id 1 (Forest land)
#   15 records were linked to LU/LC id 2 (Cropland)
#
# LU/LC id 2 occurred only in:
#
#   Cluster 15, Plot 4
#   Cluster 37, Plot 3
#
# Both plots contained tree records assigned to LU/LC id 1 and
# LU/LC id 2. This is consistent with plots crossing an LU/LC
# boundary and individual trees being assigned to the section
# in which they occur.
#
# Final decision:
# No cleaning is required
#
# The original Arena variables `tree_lulc_no` and
# `tree_lulc_no_label` are retained unchanged


# ============================================================
# ATTRIBUTE 4: TREE STATUS
# ============================================================

# Tree Status identifies whether each record represents:
#
#   - a live standing tree;
#   - a dead standing tree; or
#   - a stump.
#
# Tree status is important because it determines how several later
# attributes are interpreted, including decomposition status,
# tree-health variables and Mother Tree recommendation
#
# During the earlier dedicated tree-measurement QA/QC exercise (see Tree data Prep.R),
# 11 records originally classified in Arena as stumps were identified
# as unusually tall (>5 m) and were submitted to the Department of
# Forestry (DoF) for verification.
#
# DoF confirmed that all 11 records should be treated as
# dead standing trees rather than stumps
#
# The original Arena tree-status variables are NOT overwritten.
# The verified corrections are stored in new cleaned variables:
#
#   tree_status_clean
#   tree_status_label_clean
#
# All subsequent cleaning steps that depend on tree status should
# use these cleaned status variables.


# ------------------------------------------------------------
# 4.1 Check original Arena tree-status distribution
# ------------------------------------------------------------

tree %>%
  count(
    tree_status,
    tree_status_label,
    .drop = FALSE
  ) %>%
  arrange(tree_status)


# Expected original distribution:
#
#   Live standing tree = 3,152
#   Stump              =   454
#   Dead standing tree =    66


# ------------------------------------------------------------
# 4.2 Define tree-record identifier columns
# ------------------------------------------------------------

# These three variables uniquely identify individual tree/stump
# records and will also be reused for later verification joins.

key_cols <- c(
  "cluster_no",
  "plot_no",
  "tree_no"
)


# ------------------------------------------------------------
# 4.3 Enter DoF-verified tree-status corrections
# ------------------------------------------------------------

# These 11 records were originally entered as stumps in Arena.
#
# They were identified during the earlier stump-height QA because
# their recorded heights exceeded 5 m.
#
# Following review, DoF confirmed that they represent dead standing
# trees. The verified decision is applied below.

dof_stumps_to_dead <- tibble::tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  16,          2,         1,
  28,          2,         7,
  29,          2,        19,
  29,          2,        20,
  34,          4,         8,
  43,          4,         1,
  45,          1,         4,
  58,          1,         1,
  65,          5,         1,
  97,          1,         5,
  98,          4,        10
) %>%
  mutate(
    dof_status_decision =
      "DoF verified: reclassify as dead standing tree"
  )


# Confirm that 11 verified corrections are included.

nrow(dof_stumps_to_dead)

# Expected = 11


# ------------------------------------------------------------
# 4.4 Identify the Arena code for Dead standing tree
# ------------------------------------------------------------

# Rather than hard-coding the numerical status code, retrieve it
# directly from the Arena data. This ensures that the cleaned code
# remains consistent with the existing Arena coding system.

tree %>%
  distinct(
    tree_status,
    tree_status_label
  ) %>%
  arrange(tree_status)


dead_standing_code <- tree %>%
  filter(
    tree_status_label == "Dead standing tree"
  ) %>%
  distinct(tree_status) %>%
  pull(tree_status)


# There should be exactly one Arena code corresponding to
# "Dead standing tree".

stopifnot(
  length(dead_standing_code) == 1
)


# ------------------------------------------------------------
# 4.5 Create cleaned tree-status variables
# ------------------------------------------------------------

# Join the DoF verification decisions to the main tree dataset using
# the unique tree identifiers.
#
# Only the 11 verified records are changed.
# All other records retain their original Arena status.

tree <- tree %>%
  left_join(
    dof_stumps_to_dead,
    by = key_cols
  ) %>%
  mutate(
    
    tree_status_clean = case_when(
      
      # DoF-verified stump -> dead standing tree
      !is.na(dof_status_decision) ~
        dead_standing_code,
      
      # All other records retain the original Arena code
      TRUE ~
        tree_status
    ),
    
    
    tree_status_label_clean = case_when(
      
      # DoF-verified stump -> dead standing tree
      !is.na(dof_status_decision) ~
        "Dead standing tree",
      
      # All other records retain the original Arena label
      TRUE ~
        tree_status_label
    )
  )


# ------------------------------------------------------------
# 4.6 Inspect the 11 corrected records
# ------------------------------------------------------------

tree_status_changes <- tree %>%
  filter(
    tree_status_label != tree_status_label_clean
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name,
    tree_dbh,
    tree_diameter_height,
    tree_height,
    tree_status,
    tree_status_label,
    tree_status_clean,
    tree_status_label_clean,
    tree_stump_decomposition_label,
    dof_status_decision
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )

tree_status_changes %>%
  print(n = Inf, width = Inf)

#View(tree_status_changes)


# Confirm number of corrected records.

nrow(tree_status_changes)

# Expected = 11


# ------------------------------------------------------------
# 4.7 Compare original and cleaned tree-status distributions
# ------------------------------------------------------------

cat("\nOriginal Arena tree status:\n")

tree %>%
  count(
    tree_status_label,
    sort = TRUE
  )


cat("\nCleaned tree status:\n")

tree %>%
  count(
    tree_status_label_clean,
    sort = TRUE
  )


# Expected cleaned distribution:
#
#   Live standing tree = 3,152
#   Stump              =   443
#   Dead standing tree =    77


# ------------------------------------------------------------
# 4.8 Final consistency checks
# ------------------------------------------------------------

# Confirm that:
#
#   - exactly 11 status records were corrected;
#   - the cleaned status totals still equal the full dataset;
#   - the expected final status distribution has been obtained.

stopifnot(
  nrow(tree_status_changes) == 11,
  
  sum(
    tree$tree_status_label_clean == "Live standing tree",
    na.rm = TRUE
  ) == 3152,
  
  sum(
    tree$tree_status_label_clean == "Stump",
    na.rm = TRUE
  ) == 443,
  
  sum(
    tree$tree_status_label_clean == "Dead standing tree",
    na.rm = TRUE
  ) == 77,
  
  nrow(tree) == 3672
)
cat(
  "\nAttribute 4 QA checks completed successfully.\n"
)

# ------------------------------------------------------------
# ATTRIBUTE 4 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# QA result:
#
# Original Arena tree status:
#
#   Live standing tree = 3,152
#   Stump              =   454
#   Dead standing tree =    66
#
# During earlier stump-height QA/QC, 11 records classified as
# stumps were identified as unusually tall (>5 m) and submitted
# to DoF for verification.
#
# DoF confirmed that all 11 records should be classified as
# dead standing trees.
#
# After applying the verified corrections:
#
#   Live standing tree = 3,152
#   Stump              =   443
#   Dead standing tree =    77
#
# Final decision:
#
# The original Arena variables:
#
#   tree_status
#   tree_status_label
#
# are retained unchanged for traceability.
#
# Verified corrections are stored in:
#
#   tree_status_clean
#   tree_status_label_clean
#
# From this point onward, all cleaning decisions that depend on
# tree status will use tree_status_clean /
# tree_status_label_clean rather than the original Arena status.


# ============================================================
# ATTRIBUTES 5–6: TREE SPECIES IDENTIFICATION
# ============================================================
#
# Species-name review and validation were undertaken previously in
# a separate dedicated species-cleaning workflow (Tree species data prep.R)
#
# That earlier exercise:
#   - reviewed Arena-listed species
#   - investigated UNL and UNK records
#   - compared scientific and local names;
#   - prepared species records for DoF validation and
#   - incorporated the returned DoF decisions into a finalized
#     species-name mapping table
#
# The exploratory species-validation procedure is therefore NOT
# repeated in this master cleaning script
#
# Instead, the finalized DoF species-name map is imported and
# applied directly to the full tree dataset
#
# IMPORTANT:
#   - The full 3,672-record tree dataset is retained
#   - Original Arena species variables are not overwritten
#   - Cleaned scientific and local names are created separately
#   - The species-validation exercise primarily targeted live trees
#     because species composition/diversity analysis is based on
#     live standing trees
#   - Non-live records are nevertheless retained in the master
#     dataset and receive mapped species information wherever the
#     finalized map provides a match



# ============================================================
# ATTRIBUTE 5: SPECIES SCIENTIFIC NAME
# ============================================================

# Attribute 5 records the scientific identity of the tree species.
#
# The original Arena variables are:
#
#   tree_species
#   tree_species_scientific_name
#
# The finalized DoF map provides:
#
#   species_code_clean
#   scientific_name_clean
#   scientific_name_report
#
# The cleaned scientific name retains continuity with the NFI field
# database, while scientific_name_report provides the accepted name
# to be used later in reporting where a taxonomic update is required.
#
# The map also contains:
#
#   growth_form
#   include_in_tree_species_analysis
#
# These fields distinguish trees from records subsequently identified
# by DoF as shrubs.


# ------------------------------------------------------------
# 5.1 Import finalized DoF species-name map
# ------------------------------------------------------------

species_map <- read_excel(
  "data/reference/Tree_Species_DoF_Name_Map.xlsx",
  sheet = "Species_Name_Map"
)

# Check imported variables.

names(species_map)


# Retain only the mapping variables required in the master workflow.

species_map <- species_map %>%
  select(
    map_key,
    species_code_clean,
    scientific_name_clean,
    scientific_name_report,
    local_name_clean,
    growth_form,
    include_in_tree_species_analysis
  )


# ------------------------------------------------------------
# 5.2 Confirm that each mapping key is unique
# ------------------------------------------------------------

# The species map must contain only one row for each map_key.
#
# Duplicate keys could cause a many-to-many join and increase the
# number of tree records in the master dataset.
#
# This check therefore protects the one-row-per-tree structure.

species_map_duplicates <- species_map %>%
  count(
    map_key,
    name = "n"
  ) %>%
  filter(n > 1)

species_map_duplicates


# The final validated map should contain no duplicate mapping keys.

stopifnot(
  nrow(species_map_duplicates) == 0
)


# Add a temporary indicator that allows us to determine whether
# each tree record successfully matched the finalized map.

species_map <- species_map %>%
  mutate(
    species_map_match = TRUE
  )


# ------------------------------------------------------------
# 5.3 Create species mapping key in the FULL tree dataset
# ------------------------------------------------------------

# The mapping key follows the same logic used when the finalized
# species map was prepared.
#
# Existing Arena-listed species:
#   -> map using the Arena species code.
#
# UNL and UNK records:
#   -> map using the Arena species code together with the local
#      name recorded in the field.
#
# Records without a local name use:
#
#      __NO_NAME__
#
# IMPORTANT:
# local_name_key is created only for matching.
# The original tree_local_name variable is not modified.

tree <- tree %>%
  mutate(
    
    local_name_key = case_when(
      
      is.na(tree_local_name) |
        str_squish(tree_local_name) == "" ~
        "__NO_NAME__",
      
      TRUE ~
        str_to_lower(
          str_squish(tree_local_name)
        )
    ),
    
    
    map_key = case_when(
      
      # Missing Arena species code cannot be used to construct
      # a species-map key.
      is.na(tree_species) |
        str_squish(tree_species) == "" ~
        NA_character_,
      
      # UNL and UNK require the recorded local name to distinguish
      # the different field identifications.
      str_to_upper(str_squish(tree_species)) %in%
        c("UNL", "UNK") ~
        paste0(
          str_to_upper(str_squish(tree_species)),
          "|",
          local_name_key
        ),
      
      # Existing Arena-listed species are mapped using species code.
      TRUE ~
        str_squish(tree_species)
    )
  )

# ------------------------------------------------------------
# 5.4 Join finalized DoF species map to the full tree dataset
# ------------------------------------------------------------
# Record the number of rows before the join.
#
# The species join must NOT create or remove tree records.

n_before_species_join <- nrow(tree)


tree <- tree %>%
  left_join(
    species_map,
    by = "map_key"
  )


# Confirm that one row per original tree/stump record is retained.

stopifnot(
  nrow(tree) == n_before_species_join
)


# ------------------------------------------------------------
# 5.5 Create cleaned species code and scientific-name variables
# ------------------------------------------------------------

# Where a validated map entry exists, use the DoF-validated value.
#
# Where a non-live record was outside the scope of the live-tree
# validation map, retain the original Arena information rather than
# inventing a species identification.
#
# IMPORTANT:
# All live-tree records are expected to match the finalized map.
# This is checked explicitly below.

tree <- tree %>%
  mutate(
    
    # --------------------------------------------------------
    # Cleaned species code
    # --------------------------------------------------------
    
    tree_species_clean =
      coalesce(
        species_code_clean,
        tree_species
      ),
    
    
    # --------------------------------------------------------
    # Cleaned scientific name
    # --------------------------------------------------------
    
    tree_species_scientific_name_clean =
      coalesce(
        scientific_name_clean,
        tree_species_scientific_name
      ),
    
    
    # --------------------------------------------------------
    # Scientific name for reporting
    # --------------------------------------------------------
    
    # Where an accepted reporting name is supplied by the map,
    # use it
    #
    # Otherwise use the cleaned NFI scientific name
    
    tree_species_scientific_name_report =
      coalesce(
        scientific_name_report,
        scientific_name_clean,
        tree_species_scientific_name
      )
  )

# ------------------------------------------------------------
# 5.6 Check species mapping among live standing trees
# ------------------------------------------------------------

# Species validation was undertaken for live standing trees
#
# Because Attribute 4 has already been cleaned, the cleaned tree
# status is used here rather than the original Arena status

live_species_mapping_check <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree"
  ) %>%
  summarise(
    
    total_live_trees = n(),
    
    mapped_live_records =
      sum(
        species_map_match %in% TRUE
      ),
    
    unmapped_live_records =
      sum(
        is.na(species_map_match)
      ),
    
    records_for_tree_species_analysis =
      sum(
        include_in_tree_species_analysis == "Yes",
        na.rm = TRUE
      ),
    
    records_identified_as_shrubs =
      sum(
        include_in_tree_species_analysis == "No",
        na.rm = TRUE
      ),
    
    unknown_tree_species =
      sum(
        tree_species_scientific_name_clean == "Unknown" &
          include_in_tree_species_analysis == "Yes",
        na.rm = TRUE
      )
  )


print(
  live_species_mapping_check,
  width = Inf
)


# Expected from the finalized species-validation exercise:
#
#   Total live trees                    = 3,152
#   Mapped live records                 = 3,152
#   Unmapped live records               =     0
#   Records for tree-species analysis   = 3,130
#   Records identified as shrubs        =    22
#   Unknown tree species                =    11

# ------------------------------------------------------------
# 5.7 Confirm finalized live-tree species results
# ------------------------------------------------------------

# These checks ensure that the finalized DoF species map reproduces
# the results established during the dedicated species-validation
# workflow

stopifnot(
  
  nrow(
    tree %>%
      filter(
        tree_status_label_clean == "Live standing tree"
      )
  ) == 3152,
  
  
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      tree$species_map_match %in% TRUE,
    na.rm = TRUE
  ) == 3152,
  
  
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      tree$include_in_tree_species_analysis == "No",
    na.rm = TRUE
  ) == 22,
  
  
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      tree$include_in_tree_species_analysis == "Yes" &
      tree$tree_species_scientific_name_clean == "Unknown",
    na.rm = TRUE
  ) == 11
)


cat(
  "\nAttribute 5 species-map QA checks completed successfully.\n"
)


# ------------------------------------------------------------
# 5.8 Review live records identified as shrubs
# ------------------------------------------------------------

# DoF identified 22 live records as shrubs rather than trees.
#
# These records remain in the full NFI tree dataset for
# traceability but are flagged for exclusion from subsequent
# tree-species composition and diversity analysis.

tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    include_in_tree_species_analysis == "No"
  ) %>%
  count(
    tree_local_name,
    tree_species_scientific_name_clean,
    growth_form,
    sort = TRUE
  ) %>%
  print(n = Inf, width = Inf)


# ------------------------------------------------------------
# 5.9 Review live tree records remaining unidentified
# ------------------------------------------------------------

# Live trees that remained unidentified after DoF validation were
# standardized as:
#
#   tree_species_clean                  = "UNK"
#   tree_species_scientific_name_clean  = "Unknown"
#
# They remain valid tree records and are retained in the
# tree-species analysis under the Unknown category.

unknown_live_tree_species <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    include_in_tree_species_analysis == "Yes",
    tree_species_scientific_name_clean == "Unknown"
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species,
    tree_species_clean,
    tree_species_scientific_name,
    tree_species_scientific_name_clean,
    tree_local_name,
    growth_form,
    include_in_tree_species_analysis
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


unknown_live_tree_species %>%
  print(n = Inf, width = Inf)

#View(unknown_live_tree_species)


# ------------------------------------------------------------
# 5.10 Check species-map matching outside the live-tree subset
# ------------------------------------------------------------

# The finalized DoF species map was developed primarily for live
# standing trees, because species composition and diversity
# analyses are based on live-tree records.
#
# This check documents how the map behaved when applied to the
# full 3,672-record tree dataset.
#
# Important:
# An unmatched stump or dead standing tree is not automatically
# a data-quality error. Original Arena species information is
# retained for such records, and no species identity is guessed.

species_match_status_summary <- tree %>%
  mutate(
    species_map_result = case_when(
      species_map_match %in% TRUE ~ "Matched to finalized species map",
      TRUE ~ "Not matched to finalized species map"
    )
  ) %>%
  count(
    tree_status_label_clean,
    species_map_result
  ) %>%
  arrange(
    tree_status_label_clean,
    species_map_result
  )

print(species_match_status_summary, n = Inf)

#Inspect non-live records that matched the finalized species map

matched_non_live_species <- tree %>%
  filter(
    tree_status_label_clean %in% c("Stump", "Dead standing tree"),
    species_map_match %in% TRUE
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_species,
    tree_species_clean,
    tree_species_scientific_name,
    tree_species_scientific_name_clean,
    tree_species_scientific_name_report,
    growth_form,
    include_in_tree_species_analysis
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    cluster_no,
    plot_no,
    tree_no
  )

cat(
  "Non-live records matched to the finalized species map:",
  nrow(matched_non_live_species),
  "\n"
)

matched_non_live_species %>%
  print(n = Inf, width = Inf)

#View(matched_non_live_species)


# ------------------------------------------------------------
# ATTRIBUTE 5 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Species scientific-name review and validation were undertaken
# previously through a dedicated species-cleaning workflow and
# consultation with the Department of Forestry (DoF)
#
# The finalized DoF species-name map was therefore applied here
# rather than repeating the earlier exploratory validation process
#
# LIVE STANDING TREES
#
# 3,152 live standing-tree species records were successfully finalized
#
# For species already correctly listed in Arena, the existing
# Arena species code served as the mapping key and the confirmed
# species name was retained unchanged
#
# # Following DoF validation:
#
#   - 3,130 live records were retained for tree-species
#     composition and diversity analysis;
#
#   - of these, 3,119 were identified trees;
#
#   - 11 trees remained unidentified and were standardized as:
#
#         tree_species_clean = "UNK"
#         tree_species_scientific_name_clean = "Unknown"
#
#   - 22 records were identified by DoF as shrubs rather than
#     trees. These records remain in the full NFI dataset for
#     traceability but are flagged for exclusion from subsequent
#     tree-species composition and diversity analysis.
#
#
# NON-LIVE RECORDS
#
# Applying the same finalized map to the complete tree dataset
# also resulted in successful matches for 28 non-live records
#
#   - 10 dead standing trees;
#   - 18 stumps.
# 
# Of these, 26 matched through their existing Arena species codes, 
# while 2 UNL records matched through the recorded local-name 
# information used in the species-map key
#
# The remaining 492 non-live records did not match the finalized
# species map:
#
#   - 67 dead standing trees;
#   - 425 stumps.
#
#   Of these 492 records:
#
#   - 491 had no recorded species code, scientific name or local
#     name. Therefore no species identity is assigned
#
#   - one stump (Cluster 94, Plot 3, Tree 8) was recorded in Arena
#     as ANNON/SENEG / Annona senegalensis. The original Arena species 
#     code and scientific name are retained in the cleaned variables 
#     through the fallback cleaning rule.
#
# 
# FINAL DECISION
#
# Original Arena variables remain unchanged:
#
#   tree_species
#   tree_species_scientific_name
#
# The following cleaned variables were created:
#
#   tree_species_clean
#   tree_species_scientific_name_clean
#   tree_species_scientific_name_report
#
# Supporting variables from the validated species map are retained:
#
#   growth_form
#   include_in_tree_species_analysis
#
# The reporting-name variable allows currently accepted scientific
# names to be used in final outputs where these differ from the
# scientific names retained in the NFI working dataset.
#
# No automatic or inferred species identification was applied
# where DoF validation did not provide sufficient information



# ============================================================
# ATTRIBUTE 6: SPECIES LOCAL NAME
# ============================================================

# Attribute 6 records the local/vernacular name used for the species
#
# Local-name review and standardization were undertaken as part of
# the earlier DoF species-validation exercise used for Attribute 5
#
# The finalized species map contains a standardized local name
# where DoF supplied or confirmed one
#
# Cleaning rule:
#
#   - where the finalized map provides a standardized local name,
#     use that value
#
#   - where no standardized replacement was supplied, retain the
#     original field-recorded local name
#
# The original Arena variable tree_local_name is not overwritten


# ------------------------------------------------------------
# 6.1 Create cleaned local-name variable
# ------------------------------------------------------------

tree <- tree %>%
  mutate(
    tree_local_name_clean =
      coalesce(
        local_name_clean,
        tree_local_name
      )
  )


# ------------------------------------------------------------
# 6.2 Identify records receiving a standardized/updated local name
# ------------------------------------------------------------

# This table shows records for which the finalized DoF local name
# differs from the original field-recorded local name
#
# It also includes records where no local name was originally
# recorded but the finalized map provides one.

local_name_changes <- tree %>%
  filter(
    !is.na(local_name_clean),
    
    is.na(tree_local_name) |
      tree_local_name_clean != tree_local_name
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name,
    tree_local_name_clean
  ) %>%
  arrange(
    tree_species_scientific_name_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Records receiving a standardized/updated local name:",
  nrow(local_name_changes),
  "\n"
)

local_name_changes %>%
  print(n = Inf, width = Inf)

#View(local_name_changes)


# ------------------------------------------------------------
# 6.3 Check local-name completeness among live-tree species
#     analysis records
# ------------------------------------------------------------

# Missing local names are not automatically considered errors.
#
# Scientific species identification remains the primary variable
# for species composition and diversity analysis.
#
# This check is therefore descriptive only.

tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    include_in_tree_species_analysis == "Yes"
  ) %>%
  summarise(
    
    n_species_analysis_records = n(),
    
    with_clean_local_name =
      sum(
        !is.na(tree_local_name_clean) &
          str_squish(tree_local_name_clean) != ""
      ),
    
    missing_clean_local_name =
      sum(
        is.na(tree_local_name_clean) |
          str_squish(tree_local_name_clean) == ""
      )
  )

# ------------------------------------------------------------
# ATTRIBUTE 6 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# # A total of 2,706 records received a standardized or completed
# local name from the finalized species map
#
# # The changes include:
#
#   - standardization of spelling variants
#   - capitalization and naming consistency
#   - replacement of variant local names with the DoF-confirmed
#     standardized name. and
#   - completion of previously missing local names where the validated
#     species identification provided a standardized local name.
#
# Among the 3,130 live records retained for tree-species
# composition and diversity analysis:
#
#   - 3,106 (99.2%) records have a cleaned local name 
#   -    24 records remain without a cleaned local name.
#
#
# Missing local names are not automatically treated as data-quality
# errors because scientific species identification remains the
# primary variable for tree-species composition and diversity
# analysis.
#
# FINAL DECISION:
#
# The original Arena variable tree_local_name is retained unchanged.
#
# The cleaned variable tree_local_name_clean is used for subsequent
# analysis and reporting.
#
# No local name was inferred where neither the finalized map nor
# the original field record provided one.


# ============================================================
# ATTRIBUTE 7: PHOTO
# ============================================================
# Attribute 7 provides a field photograph of the tree/stump where
# one was taken during data collection.
#
# No quantitative QA/QC or cleaning was required for Attribute 7.

# ============================================================
# ATTRIBUTE 8: DIAMETER (DBH)
# ============================================================

# Attribute 8 records tree/stump stem diameter in centimetres.
#
# Under the NFI sampling design:
#
#   Main plot:
#       DBH >= 20 cm
#
#   Subplot:
#       DBH 10.0–19.9 cm
#
# These inclusion thresholds apply to live standing trees,
# dead standing trees and stumps
#
# IMPORTANT:
# Compliance with the DBH-based Main plot/Subplot allocation rule
# was already checked under Attribute 2. No violations were found.
# That structural check is therefore not repeated here
#
# Attribute 8 focuses on the completeness and plausibility of the
# recorded diameter values themselves.
#
# Very large DBH values are not automatically considered errors
# Large trees are valid and important observations in a national
# forest inventory. Therefore, the largest measurements are reviewed
# individually rather than applying an arbitrary maximum-DBH cutoff
#
# The original Arena variable tree_dbh is not overwritten.


# ------------------------------------------------------------
# 8.1 Check completeness and basic validity of DBH
# ------------------------------------------------------------

# DBH should be present and positive for every tree/stump record.
#
# The minimum DBH is also reviewed in relation to the NFI minimum
# inclusion diameter of 10 cm. The plot/subplot allocation itself
# has already been verified under Attribute 2.

dbh_basic_check <- tree %>%
  summarise(
    total_records = n(),
    
    missing_dbh =
      sum(is.na(tree_dbh)),
    
    zero_or_negative_dbh =
      sum(
        !is.na(tree_dbh) &
          tree_dbh <= 0
      ),
    
    dbh_below_10_cm =
      sum(
        !is.na(tree_dbh) &
          tree_dbh < 10
      ),
    
    min_dbh =
      min(tree_dbh, na.rm = TRUE),
    
    median_dbh =
      median(tree_dbh, na.rm = TRUE),
    
    mean_dbh =
      mean(tree_dbh, na.rm = TRUE),
    
    max_dbh =
      max(tree_dbh, na.rm = TRUE)
  )

print(
  dbh_basic_check,
  width = Inf
)


# ------------------------------------------------------------
# 8.2 Review DBH distribution by cleaned tree status
# ------------------------------------------------------------

# Attribute 4 corrected the tree status of 11 records following
# DoF verification. Therefore, all subsequent status-based checks
# use tree_status_label_clean rather than the original Arena status.
#
# These descriptive statistics help show the range of recorded
# diameters among live trees, dead standing trees and stumps.
# They are not themselves error thresholds.

dbh_summary_status <- tree %>%
  group_by(
    tree_status_label_clean
  ) %>%
  summarise(
    n = n(),
    
    min_dbh =
      min(tree_dbh, na.rm = TRUE),
    
    q1_dbh =
      quantile(
        tree_dbh,
        0.25,
        na.rm = TRUE
      ),
    
    median_dbh =
      median(
        tree_dbh,
        na.rm = TRUE
      ),
    
    mean_dbh =
      mean(
        tree_dbh,
        na.rm = TRUE
      ),
    
    q3_dbh =
      quantile(
        tree_dbh,
        0.75,
        na.rm = TRUE
      ),
    
    max_dbh =
      max(tree_dbh, na.rm = TRUE),
    
    .groups = "drop"
  )

print(
  dbh_summary_status,
  width = Inf
)


# ------------------------------------------------------------
# 8.3 Inspect the largest recorded DBH values
# ------------------------------------------------------------

# Exceptionally large DBH values are reviewed individually because
# they may represent genuine large trees and should not be changed
# simply because they are uncommon.
#
# At this stage no arbitrary upper DBH limit is imposed.
#
# The later DBH-height QA under Attribute 11 will provide an
# additional check by examining diameter and height together.

largest_dbh_check <- tree %>%
  arrange(
    desc(tree_dbh)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label_clean,
    tree_plot_type_label,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_diameter_height,
    tree_height
  ) %>%
  slice_head(
    n = 30
  )

print(
  largest_dbh_check,
  n = 30,
  width = Inf
)

#View(largest_dbh_check)

# ------------------------------------------------------------
# ATTRIBUTE 8 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# All 3,672 tree/stump records contain a recorded DBH value.
#
# QA checks found:
#
#   - 0 missing DBH values;
#   - 0 zero or negative DBH values;
#   - 0 DBH values below the NFI minimum inclusion diameter of 10 cm.
#
# The recorded DBH values range from 10 cm to 260 cm, with:
#
#   - median DBH = 21.0 cm;
#   - mean DBH   = 23.8 cm.
#
# The largest recorded DBH values were reviewed individually
# Several of the largest values belong to Adansonia digitata,
# a species capable of attaining very large stem diameters
#
# No arbitrary maximum-DBH threshold was imposed because unusually
# large trees are not automatically data errors in NFI
#
# The DBH-based Main plot/Subplot inclusion rule had already been
# checked under Attribute 2, where no allocation violations were found.
#
# FINAL DECISION:
#
# No DBH correction is required.
#
# The original Arena variable tree_dbh is retained unchanged for
# subsequent analysis.
#
# DBH and tree height combinations are assessed under Attribute 11



# ============================================================
# ATTRIBUTE 9: DIAMETER MEASUREMENT HEIGHT
# ============================================================

# Attribute 9 records the height above ground, in metres, at which
# the stem diameter recorded under Attribute 8 was measured.
#
# Standard measurement height:
#
#       1.30 m above ground
#
# However, 1.30 m is not appropriate in every situation.
#
# For standing trees, diameter may legitimately be measured above
# or below 1.30 m because of buttresses, deformity, branching,
# damage or other stem irregularities.
#
# For a stump shorter than 1.30 m, the field protocol requires
# diameter to be measured at the top of the stump. Therefore:
#
#       if stump height < 1.30 m,
#       diameter measurement height should approximately correspond
#       to stump height.
#
# Arena automatically populated 1.30 m as the default measurement
# height. Field teams were expected to replace this value when the
# diameter was measured elsewhere.
#
# IMPORTANT:
# The recorded tree_height is used here only to check the physical
# consistency of stump measurements. Tree height itself is cleaned
# later under Attribute 11.
#
# Because Attribute 4 has already been finalized, stump-specific
# checks use tree_status_label_clean rather than the original Arena
# tree status.
#
# The original Arena variable tree_diameter_height is not overwritten.


# ------------------------------------------------------------
# 9.1 Check completeness and distribution
# ------------------------------------------------------------

diameter_height_basic_check <- tree %>%
  summarise(
    total_records = n(),
    
    missing_measurement_height =
      sum(is.na(tree_diameter_height)),
    
    measured_at_1_30_m =
      sum(
        tree_diameter_height == 1.30,
        na.rm = TRUE
      ),
    
    measured_away_from_1_30_m =
      sum(
        !is.na(tree_diameter_height) &
          tree_diameter_height != 1.30
      ),
    
    min_measurement_height =
      min(tree_diameter_height, na.rm = TRUE),
    
    max_measurement_height =
      max(tree_diameter_height, na.rm = TRUE)
  )

print(
  diameter_height_basic_check,
  width = Inf
)


# Review recorded measurement heights by cleaned tree status.

tree %>%
  count(
    tree_status_label_clean,
    tree_diameter_height,
    sort = TRUE
  ) %>%
  print(n = Inf)


# ------------------------------------------------------------
# 9.2 Review standing trees measured away from 1.30 m
# ------------------------------------------------------------

# A standing tree measured away from 1.30 m is NOT automatically
# considered an error.
#
# The field team may have moved the measurement point because of
# stem form or obstruction.
#
# These records are therefore retained and reviewed rather than
# automatically corrected.

standing_nonstandard_diameter_height <- tree %>%
  filter(
    tree_status_label_clean %in%
      c(
        "Live standing tree",
        "Dead standing tree"
      ),
    
    !is.na(tree_diameter_height),
    
    tree_diameter_height != 1.30
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_diameter_height,
    tree_height
  ) %>%
  arrange(
    tree_status_label_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Standing trees measured away from 1.30 m:",
  nrow(standing_nonstandard_diameter_height),
  "\n"
)

print(
  standing_nonstandard_diameter_height,
  n = Inf,
  width = Inf
)

#View(standing_nonstandard_diameter_height)

# QA finding:
#
# Five standing-tree records were measured at heights other than
# the standard 1.30 m:
#
#   - 4 dead standing trees;
#   - 1 live standing tree.
#
# Two of the four dead standing trees were originally recorded as
# stumps and were reclassified as dead standing trees following
# DoF verification under Attribute 4. Their recorded diameter
# measurement heights (0.60 m and 0.70 m) are retained but will be
# flagged for consideration during later biomass analysis.
#
# The remaining two dead standing trees and one live standing tree
# retain their recorded non-standard measurement heights because
# measurement away from 1.30 m can be legitimate under field observed 
# conditions and no evidence supports an automatic correction


# ------------------------------------------------------------
# 9.3 Check stump height against diameter measurement height
# ------------------------------------------------------------
#
# # Three situations require special attention:
#
#   1. Questionable very low stump heights (<0.10 m)
#
#      These were previously identified as questionable because
#      repeated values such as 0.03, 0.06, 0.07 and 0.09 m may
#      represent decimal-position errors.
#
#      Such stump heights are not considered sufficiently reliable
#      to derive a cleaned diameter measurement height
#
#   2. Short stumps retaining Arena's default 1.30-m measurement height
#
#      For stumps shorter than 1.30 m, the field protocol requires
#      diameter to be measured at the top of the stump
#
#      Therefore, where stump height is <1.30 m but
#      tree_diameter_height remains at Arena's default value of
#      1.30 m, the recorded measurement height is physically
#      inconsistent.
#
#      Where the recorded stump height is considered plausible,
#      the stump height is used as the cleaned diameter measurement
#      height, because diameter should have been measured at the
#      stump top.
#
#   3. Other cases where diameter measurement height exceeds stump height
#
#      A diameter measurement height cannot physically exceed the
#      total height of the stump.
#
#      Where this occurs and the measurement height is not Arena's
#      1.30-m default, the dataset does not provide sufficient
#      information to determine whether the stump height or the
#      diameter measurement height is incorrect.
#
#      These cases therefore remain unresolved and are not corrected
#      automatically.
# 
#   



tree <- tree %>%
  mutate(
    
    # Stump-height QA flag
    
    stump_height_flag = case_when(
      
      tree_status_label_clean == "Stump" &
        !is.na(tree_height) &
        tree_height < 0.10 ~
        "VERIFY: unusually low stump height",
      
      tree_status_label_clean == "Stump" ~
        "OK",
      
      TRUE ~
        NA_character_
    ),
    
    
    # Stump diameter measurement-height QA flag
    
    stump_diameter_height_flag = case_when(
      
      tree_status_label_clean == "Stump" &
        !is.na(tree_diameter_height) &
        !is.na(tree_height) &
        tree_diameter_height > tree_height &
        tree_diameter_height != 1.30 ~
        "VERIFY",
      
      tree_status_label_clean == "Stump" ~
        "OK",
      
      TRUE ~
        NA_character_
    )
  )


# Summarize the two stump QA flags.

tree %>%
  filter(
    tree_status_label_clean == "Stump"
  ) %>%
  count(
    stump_height_flag
  )

tree %>%
  filter(
    tree_status_label_clean == "Stump"
  ) %>%
  count(
    stump_diameter_height_flag
  )


# ------------------------------------------------------------
# 9.4 Identify short stumps retaining Arena's 1.30-m default
# ------------------------------------------------------------

# For a stump shorter than 1.30 m, retaining the Arena default
# measurement height of 1.30 m is physically impossible.
#
# Where the recorded stump height is at least 0.10 m and otherwise
# plausible, the recorded stump height provides a defensible
# measurement height because the field protocol required diameter
# to be taken at the stump top.
#
# These cases can therefore be corrected without inventing a new
# measurement.

short_stump_default_1_30 <- tree %>%
  filter(
    tree_status_label_clean == "Stump",
    
    !is.na(tree_height),
    
    tree_height >= 0.10,
    tree_height < 1.30,
    
    tree_diameter_height == 1.30
  )


cat(
  "Short stumps eligible for protocol-based measurement-height correction:",
  nrow(short_stump_default_1_30),
  "\n"
)


# ------------------------------------------------------------
# 9.5 Create cleaned diameter measurement height
# ------------------------------------------------------------

# Cleaning rules:
#
#   1. Questionable very-low stump height (<0.10 m)
#        -> cleaned measurement height = NA
#
#   2. Non-default measurement height exceeds stump height
#        -> cleaned measurement height = NA
#
#   3. Plausible short stump retaining Arena's 1.30-m default
#        -> replace 1.30 m with recorded stump height
#
#   4. All other records
#        -> retain recorded measurement height
#
# No guessed numerical values are introduced.

tree <- tree %>%
  mutate(
    
    tree_diameter_height_clean = case_when(
      
      # Questionable very-low stump height.
      tree_status_label_clean == "Stump" &
        stump_height_flag != "OK" ~
        NA_real_,
      
      # Unresolved non-default physical conflict.
      tree_status_label_clean == "Stump" &
        stump_diameter_height_flag == "VERIFY" ~
        NA_real_,
      
      # Short stump retaining Arena's default 1.30 m.
      tree_status_label_clean == "Stump" &
        tree_height >= 0.10 &
        tree_height < 1.30 &
        tree_diameter_height == 1.30 ~
        tree_height,
      
      # Otherwise retain the recorded value.
      TRUE ~
        tree_diameter_height
    ),
    
    
    # Audit variable explaining how each cleaned value was treated.
    
    tree_diameter_height_clean_status = case_when(
      
      tree_status_label_clean == "Stump" &
        stump_height_flag != "OK" ~
        "Unresolved: questionable stump height",
      
      tree_status_label_clean == "Stump" &
        stump_diameter_height_flag == "VERIFY" ~
        "Unresolved: stump measurement-height conflict",
      
      tree_status_label_clean == "Stump" &
        tree_height >= 0.10 &
        tree_height < 1.30 &
        tree_diameter_height == 1.30 ~
        "Corrected: Arena 1.30-m default replaced using recorded stump height",
      
      TRUE ~
        "Recorded value retained"
    )
  )


# ------------------------------------------------------------
# 9.6 Summarize final treatment
# ------------------------------------------------------------

tree %>%
  count(
    tree_diameter_height_clean_status,
    sort = TRUE
  ) %>%
  print(n = Inf)


diameter_height_clean_summary <- tree %>%
  summarise(
    
    corrected_measurement_heights =
      sum(
        !is.na(tree_diameter_height_clean) &
          !is.na(tree_diameter_height) &
          tree_diameter_height_clean != tree_diameter_height
      ),
    
    unresolved_measurement_heights =
      sum(
        is.na(tree_diameter_height_clean)
      )
  )


print(
  diameter_height_clean_summary,
  width = Inf
)


# ------------------------------------------------------------
# 9.7 Inspect unresolved stump measurement-height records
# ------------------------------------------------------------

unresolved_stump_measurements <- tree %>%
  filter(
    tree_status_label_clean == "Stump",
    is.na(tree_diameter_height_clean)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_dbh,
    tree_height,
    tree_diameter_height,
    tree_diameter_height_clean,
    stump_height_flag,
    stump_diameter_height_flag,
    tree_diameter_height_clean_status
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Unresolved stump diameter measurement heights:",
  nrow(unresolved_stump_measurements),
  "\n"
)

print(
  unresolved_stump_measurements,
  n = Inf,
  width = Inf
)

#View(unresolved_stump_measurements)


# ------------------------------------------------------------
# 9.8 Flag reclassified dead trees measured away from 1.30 m
# ------------------------------------------------------------

# The 11 records reclassified from stump to dead standing tree under
# Attribute 4 retain their original DBH and measurement height
#
# Some may have been measured as stumps at heights other than 1.30 m
# These measurements are not changed because no new field measurement
# is available.
#
# They are instead flagged for consideration during later volume/
# biomass analysis, where measurement height may affect interpretation.

tree <- tree %>%
  mutate(
    
    reclassified_dead_dbh_flag = case_when(
      
      !is.na(dof_status_decision) &
        tree_diameter_height != 1.30 ~
        "REVIEW DURING ANALYSIS: diameter measured away from 1.30 m",
      
      TRUE ~
        NA_character_
    )
  )


tree %>%
  filter(
    !is.na(reclassified_dead_dbh_flag)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_dbh,
    tree_diameter_height,
    tree_height,
    reclassified_dead_dbh_flag
  ) %>%
  print(n = Inf, width = Inf)


# Confirm finalized Attribute 9 results.

stopifnot(
  nrow(short_stump_default_1_30) == 171,
  sum(tree$stump_height_flag == "VERIFY: unusually low stump height",
      na.rm = TRUE) == 14,
  sum(tree$stump_diameter_height_flag == "VERIFY",
      na.rm = TRUE) == 8,
  sum(is.na(tree$tree_diameter_height_clean)) == 21
)

cat(
  "\nAttribute 9 diameter measurement-height QA checks completed successfully.\n"
)


# ------------------------------------------------------------
# ATTRIBUTE 9 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# All 3,672 records contained a diameter measurement height.
#
# Of these:
#
#   - 3,434 were recorded at the standard 1.30-m measurement height;
#   -   238 were recorded at other measurement heights.
#
# Most non-standard measurements occurred among stumps, as expected
# under the field protocol for stumps shorter than 1.30 m.
#
# QA/QC identified:
#
#   - 171 short stumps with plausible recorded heights for which
#     Arena's unchanged 1.30-m default was replaced by the recorded
#     stump height, consistent with the stump measurement protocol;
#
#   - 14 stumps with questionable very-low recorded heights
#     (<0.10 m);
#
#   - 7 additional stumps with unresolved physical conflicts between
#     stump height and diameter measurement height.
#
# The 14 questionable stump heights and 7 additional conflicts give
# 21 unique unresolved diameter measurement heights. These are set
# to NA in tree_diameter_height_clean rather than assigned an
# unsupported numerical value
#
# Five standing trees were recorded with diameter measured away from
# 1.30 m. These values are retained because non-standard diameter 
# measurement heights can be valid under field conditions
#
# Two of these standing-tree records were originally recorded as
# stumps and subsequently reclassified by DoF as dead standing trees
# Their recorded diameter measurement heights of 0.60 m and 0.70 m are retained
# and flagged for consideration during later biomass analysis
#
# FINAL DECISION:
#
# The original Arena variable tree_diameter_height remains unchanged.
#
# The cleaned variable tree_diameter_height_clean contains:
#
#   - the original recorded value where accepted;
#   - protocol-based corrections for the 171 defensible short-stump
#     default-value cases; and
#   - NA for the 21 unresolved stump measurement-height records
#
# No unsupported or guessed numerical corrections were introduced


# ============================================================
# ATTRIBUTE 10: DECOMPOSITION STATUS
# ============================================================

# Attribute 10 records the decomposition condition of stumps and
# dead standing trees
#
# Arena decomposition categories in the current dataset are:
#
#   Sound
#   Intermediate
#   Rotten
#
# Decomposition status is not applicable to live standing trees.
#
# IMPORTANT:
# Attribute 4 has already finalized tree status. Therefore,
# decomposition is assessed against tree_status_label_clean rather
# than the original Arena tree status
#
# Previous QA/QC identified four records entered as live standing
# trees with "Sound" decomposition recorded. DoF subsequently
# confirmed that all four records are genuinely live trees
# Their decomposition entries are therefore treated as not applicable
#
# The 66 trees originally recorded as dead standing trees had no
# decomposition status recorded. DoF verification did not provide
# sufficient information to assign a decomposition class, so these
# values remain NA rather than being inferred
#
# The 11 records reclassified from stump to dead standing tree under
# Attribute 4 retain their originally recorded decomposition class.
#
# Original Arena decomposition variables are not overwritten


# ------------------------------------------------------------
# 10.1 Review recorded decomposition against cleaned tree status
# ------------------------------------------------------------

decomposition_status_check <- tree %>%
  count(
    tree_status_label_clean,
    tree_stump_decomposition_label,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_stump_decomposition_label
  )

print(
  decomposition_status_check,
  n = Inf
)


# ------------------------------------------------------------
# 10.2 Record DoF verification of live trees with decomposition
# ------------------------------------------------------------

# These four records had "Sound" entered in the decomposition field.
# DoF confirmed that their tree status is correctly recorded as live.
#
# Because decomposition is not applicable to a live standing tree,
# the decomposition entry will be removed only from the cleaned
# decomposition variable.

dof_live_decomposition <- tibble::tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  13,          2,        8,
  18,          4,        4,
  71,          1,        8,
  117,         3,       16
) %>%
  mutate(
    dof_live_decomposition_confirmed = TRUE
  )


# Join the verification lookup to the working tree dataset.
#
# The join uses the unique tree identifier already defined earlier:
#
#   cluster_no + plot_no + tree_no

n_before_decomposition_join <- nrow(tree)

tree <- tree %>%
  left_join(
    dof_live_decomposition,
    by = key_cols
  )

# Confirm that the join has not created or removed records.

stopifnot(
  nrow(tree) == n_before_decomposition_join
)


# ------------------------------------------------------------
# 10.3 Create cleaned decomposition variables
# ------------------------------------------------------------

tree <- tree %>%
  mutate(
    
    tree_stump_decomposition_clean = case_when(
      
      # Decomposition is not applicable to live standing trees.
      tree_status_label_clean == "Live standing tree" ~
        NA_real_,
      
      # Non-live records retain the recorded Arena value.
      TRUE ~
        tree_stump_decomposition
    ),
    
    
    tree_stump_decomposition_label_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        NA_character_,
      
      TRUE ~
        tree_stump_decomposition_label
    )
  )

# ------------------------------------------------------------
# 10.4 Confirm treatment of the four verified live trees
# ------------------------------------------------------------

tree %>%
  filter(
    dof_live_decomposition_confirmed %in% TRUE
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_stump_decomposition_label,
    tree_stump_decomposition_label_clean
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 10.5 Review final decomposition by cleaned tree status
# ------------------------------------------------------------

decomposition_clean_summary <- tree %>%
  count(
    tree_status_label_clean,
    tree_stump_decomposition_label_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_stump_decomposition_label_clean
  )

print(
  decomposition_clean_summary,
  n = Inf
)


# ------------------------------------------------------------
# 10.6 Check decomposition of reclassified dead standing trees
# ------------------------------------------------------------

# The 11 records corrected from stump to dead standing tree retain
# the decomposition class actually recorded in the field.

reclassified_dead_decomposition <- tree %>%
  filter(
    !is.na(dof_status_decision)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_stump_decomposition_label,
    tree_stump_decomposition_label_clean
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )

print(
  reclassified_dead_decomposition,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 10.7 Final reproducibility checks
# ------------------------------------------------------------

stopifnot(
  
  # Four confirmed live trees have decomposition removed.
  sum(
    tree$dof_live_decomposition_confirmed %in% TRUE
  ) == 4,
  
  # All live standing trees should have NA decomposition
  # after cleaning.
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      !is.na(tree$tree_stump_decomposition_label_clean)
  ) == 0
)

cat(
  "\nAttribute 10 decomposition-status QA checks completed successfully.\n"
)

# ------------------------------------------------------------
# ATTRIBUTE 10 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# QA/QC confirmed that four live standing trees had "Sound"
# decomposition recorded in Arena.
#
# DoF verification confirmed that these four records are correctly
# classified as live standing trees. Their cleaned decomposition
# values are therefore set to NA because decomposition status is
# not applicable to live trees.
#
# The 11 records reclassified from stump to dead standing tree under
# Attribute 4 retain their originally recorded decomposition status:
#
#   - Sound        = 9
#   - Intermediate = 2
#
# The 66 trees originally recorded as dead standing trees had no
# decomposition status recorded. Because no verified decomposition
# class was available, these values remain NA.
#
# Final cleaned decomposition distribution:
#
#   Live standing trees:
#       NA            = 3,152
#
#   Dead standing trees:
#       Sound         =     9
#       Intermediate  =     2
#       NA            =    66
#
#   Stumps:
#       Sound         =   407
#       Intermediate  =    27
#       Rotten        =     9
#
# FINAL DECISION:
#
# The original Arena decomposition variables remain unchanged.
#
# Cleaned decomposition variables are:
#
#   tree_stump_decomposition_clean
#   tree_stump_decomposition_label_clean
#
# No decomposition class was inferred where field or DoF
# verification did not provide sufficient information.


# ============================================================
# ATTRIBUTE 11: TREE HEIGHT
# ============================================================

# Attribute 11 records the total height of standing trees and the
# height of stumps, in metres
#
# Tree-height QA/QC is carried out in several steps because different
# types of inconsistencies require different treatment:
#
#   1. Review completeness and the overall range of recorded heights
#
#   2. Review stump heights, including the unusually low stump
#      heights already identified under Attribute 9
#
#   3. Check whether the total height of a standing tree is physically
#      consistent with its diameter measurement height
#
#   4. For live standing trees, evaluate DBH-height consistency using
#      a stratified IQR-based statistical screening procedure
#
# IMPORTANT:
#
# An unusual height is NOT automatically an error.
#
# Tree height varies naturally among species, tree forms and forest
# conditions. Therefore, no arbitrary maximum tree-height threshold
# is used.
#
# Statistical screening is used only to identify observations that
# require review relative to comparable trees.
#
# The original Arena variable tree_height is never overwritten
#
# Where a recorded height remains unresolved after QA/QC and DoF
# verification, the cleaned observed height is set to NA
#

# ------------------------------------------------------------
# 11.1 Check completeness and overall tree-height distribution
# ------------------------------------------------------------

height_basic_check <- tree %>%
  summarise(
    total_records = n(),
    
    missing_height =
      sum(is.na(tree_height)),
    
    zero_or_negative_height =
      sum(
        !is.na(tree_height) &
          tree_height <= 0
      ),
    
    min_height =
      min(tree_height, na.rm = TRUE),
    
    median_height =
      median(tree_height, na.rm = TRUE),
    
    mean_height =
      mean(tree_height, na.rm = TRUE),
    
    max_height =
      max(tree_height, na.rm = TRUE)
  )

print(
  height_basic_check,
  width = Inf
)


# ------------------------------------------------------------
# 11.2 Review height distribution by cleaned tree status
# ------------------------------------------------------------

# Cleaned tree status is used because Attribute 4 reclassified
# 11 records originally entered as stumps to dead standing trees.
#
# Quartiles are included to describe the central distribution
# without allowing a small number of extreme observations to
# dominate the summary.

height_summary_status <- tree %>%
  group_by(
    tree_status_label_clean
  ) %>%
  summarise(
    n = n(),
    
    min_height =
      min(tree_height, na.rm = TRUE),
    
    q1_height =
      quantile(
        tree_height,
        0.25,
        na.rm = TRUE
      ),
    
    median_height =
      median(
        tree_height,
        na.rm = TRUE
      ),
    
    mean_height =
      mean(
        tree_height,
        na.rm = TRUE
      ),
    
    q3_height =
      quantile(
        tree_height,
        0.75,
        na.rm = TRUE
      ),
    
    max_height =
      max(tree_height, na.rm = TRUE),
    
    .groups = "drop"
  )

print(
  height_summary_status,
  width = Inf
)


# ------------------------------------------------------------
# 11.3 Review heights of records still classified as stumps
# ------------------------------------------------------------

# The 11 records previously identified as unusually tall stumps
# (>5 m) were verified by DoF and reclassified as dead standing
# trees under Attribute 4.
#
# They are therefore no longer treated as stumps here.
#
# The following table summarizes the height distribution of the
# 443 records that remain classified as stumps after verification.

stump_height_distribution <- tree %>%
  filter(
    tree_status_label_clean == "Stump"
  ) %>%
  mutate(
    stump_height_class = case_when(
      
      tree_height < 0.10 ~
        "<0.10 m",
      
      tree_height >= 0.10 &
        tree_height <= 1.30 ~
        "0.10–1.30 m",
      
      tree_height > 1.30 &
        tree_height <= 2 ~
        ">1.30–2 m",
      
      tree_height > 2 &
        tree_height <= 5 ~
        ">2–5 m",
      
      tree_height > 5 ~
        ">5 m"
    )
  ) %>%
  count(
    stump_height_class
  )

print(
  stump_height_distribution,
  n = Inf
)


# Confirm the number of unusually low stump heights already
# identified under Attribute 9.

tree %>%
  filter(
    tree_status_label_clean == "Stump"
  ) %>%
  count(
    stump_height_flag
  )


# Check whether any records still classified as stumps exceed 5 m.

remaining_tall_stumps <- tree %>%
  filter(
    tree_status_label_clean == "Stump",
    tree_height > 5
  )

cat(
  "Remaining cleaned-status stumps with height >5 m:",
  nrow(remaining_tall_stumps),
  "\n"
)

print(
  remaining_tall_stumps,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11.4 Check physical consistency of standing-tree height
# ------------------------------------------------------------

# For a live or dead standing tree, total height cannot be lower
# than the height at which diameter was measured.
#
# Therefore:
#
#       tree_height >= tree_diameter_height_clean
#
# must logically hold.
#
# A violation is a physical inconsistency, not merely a statistical
# outlier.
#
# No automatic numerical correction is made because the dataset
# cannot determine whether the incorrect measurement is the total
# height, the diameter measurement height, or another field entry.

standing_height_conflict <- tree %>%
  filter(
    tree_status_label_clean %in%
      c(
        "Live standing tree",
        "Dead standing tree"
      ),
    
    !is.na(tree_height),
    !is.na(tree_diameter_height_clean),
    
    tree_height < tree_diameter_height_clean
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_diameter_height_clean,
    tree_height
  ) %>%
  arrange(
    tree_height
  )


cat(
  "Standing trees with total height below diameter measurement height:",
  nrow(standing_height_conflict),
  "\n"
)

print(
  standing_height_conflict,
  n = Inf,
  width = Inf
)

#View(standing_height_conflict)


# Add a QA flag to the working dataset.

tree <- tree %>%
  mutate(
    standing_height_flag = case_when(
      
      tree_status_label_clean %in%
        c(
          "Live standing tree",
          "Dead standing tree"
        ) &
        
        !is.na(tree_height) &
        !is.na(tree_diameter_height_clean) &
        
        tree_height < tree_diameter_height_clean ~
        "VERIFY: total height below diameter measurement height",
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 11.5 Prepare live trees for DBH-height statistical screening
# ------------------------------------------------------------

# PURPOSE OF THE SCREENING
#
# Tree height generally increases with tree diameter, but the
# relationship varies among species and forest conditions.
#
# Rather than applying one national height-diameter rule, each live
# tree is compared with trees having:
#
#   - the same forest stratum; and
#   - a similar DBH represented by the same 5-cm DBH class.
#
# Example:
#
# A tree with DBH = 17.3 cm is assigned to the 15–19.9 cm DBH class.
# Its height is compared with other live trees in the same forest
# stratum and the same DBH class.
#
# This produces more meaningful comparison groups than comparing
# every tree in the national dataset with every other tree.
#
# Physically inconsistent standing-tree heights identified above
# are excluded from the comparison distributions.


dbh_height_check <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    !is.na(tree_dbh),
    !is.na(tree_height),
    tree_dbh > 0,
    tree_height > 0,
    is.na(standing_height_flag)
  ) %>%
  mutate(
    
    # Create 5-cm DBH classes.
    #
    # Examples:
    #
    #   10 = 10.0–14.9 cm
    #   15 = 15.0–19.9 cm
    #   20 = 20.0–24.9 cm
    
    dbh_class_5cm =
      floor(tree_dbh / 5) * 5
  )


cat(
  "Live trees available for DBH-height screening:",
  nrow(dbh_height_check),
  "\n"
)


# ------------------------------------------------------------
# 11.6 Review the size of DBH-height comparison groups
# ------------------------------------------------------------

# The statistical comparison group is:
#
#       forest stratum × 5-cm DBH class
#
# A minimum of 10 trees is required before the IQR procedure is used.
#
# The value 10 is a practical minimum adopted for this QA/QC
# procedure. It is not a biological threshold or universal rule.
#
# Groups with fewer than 10 observations are considered too sparse
# for reliable within-group statistical screening.
#
# Their heights are retained and labelled:
#
#       INSUFFICIENT COMPARISON DATA

min_comparison_n <- 10


dbh_height_class_counts <- dbh_height_check %>%
  count(
    cluster_info_stratum,
    dbh_class_5cm,
    name = "n_trees"
  ) %>%
  arrange(
    cluster_info_stratum,
    dbh_class_5cm
  )


print(
  dbh_height_class_counts,
  n = Inf
)


dbh_height_class_counts %>%
  mutate(
    comparison_status = case_when(
      
      n_trees >= min_comparison_n ~
        "Adequate comparison group",
      
      TRUE ~
        "Insufficient comparison data"
    )
  ) %>%
  count(
    comparison_status
  )


# ------------------------------------------------------------
# 11.7 Calculate height distributions within comparison groups
# ------------------------------------------------------------

# The screening uses Tukey's interquartile-range (IQR) method.
#
# For each forest-stratum × DBH-class group:
#
#   Q1     = 25th percentile of height
#   Median = 50th percentile
#   Q3     = 75th percentile
#   IQR    = Q3 - Q1
#
# The following limits are then calculated:
#
#   REVIEW limits:
#
#       Q1 - 1.5 × IQR
#       Q3 + 1.5 × IQR
#
#   PRIORITY VERIFY limits:
#
#       Q1 - 3 × IQR
#       Q3 + 3 × IQR
#
# These are statistical screening thresholds only.
#
# A tree outside these limits may be a genuine unusual tree.
# It is NOT automatically considered a measurement error.


dbh_height_limits <- dbh_height_check %>%
  group_by(
    cluster_info_stratum,
    dbh_class_5cm
  ) %>%
  summarise(
    
    n_trees = n(),
    
    height_q1 =
      quantile(
        tree_height,
        0.25,
        na.rm = TRUE
      ),
    
    height_median =
      median(
        tree_height,
        na.rm = TRUE
      ),
    
    height_q3 =
      quantile(
        tree_height,
        0.75,
        na.rm = TRUE
      ),
    
    height_iqr =
      IQR(
        tree_height,
        na.rm = TRUE
      ),
    
    lower_limit_1_5 =
      height_q1 - 1.5 * height_iqr,
    
    upper_limit_1_5 =
      height_q3 + 1.5 * height_iqr,
    
    lower_limit_3 =
      height_q1 - 3 * height_iqr,
    
    upper_limit_3 =
      height_q3 + 3 * height_iqr,
    
    .groups = "drop"
  )


print(
  dbh_height_limits,
  n = Inf
)


# ------------------------------------------------------------
# 11.8 Classify DBH-height combinations
# ------------------------------------------------------------

# Classification is applied in the following order:
#
#   Fewer than 10 comparable trees
#       -> INSUFFICIENT COMPARISON DATA
#
#   Outside 3 × IQR limits
#       -> PRIORITY VERIFY
#
#   Outside 1.5 × IQR limits
#       -> REVIEW
#
#   Within 1.5 × IQR limits
#       -> OK


dbh_height_outliers <- dbh_height_check %>%
  left_join(
    dbh_height_limits,
    by = c(
      "cluster_info_stratum",
      "dbh_class_5cm"
    )
  ) %>%
  mutate(
    dbh_height_review_flag = case_when(
      
      n_trees < min_comparison_n ~
        "INSUFFICIENT COMPARISON DATA",
      
      tree_height > upper_limit_3 ~
        "PRIORITY VERIFY: extremely tall for DBH",
      
      tree_height < lower_limit_3 ~
        "PRIORITY VERIFY: extremely short for DBH",
      
      tree_height > upper_limit_1_5 ~
        "REVIEW: unusually tall for DBH",
      
      tree_height < lower_limit_1_5 ~
        "REVIEW: unusually short for DBH",
      
      TRUE ~
        "OK"
    )
  )


# Summarize the screening outcome.

dbh_height_outliers %>%
  count(
    dbh_height_review_flag,
    sort = TRUE
  )


# ------------------------------------------------------------
# 11.9 Create DBH-height verification table
# ------------------------------------------------------------

# Only REVIEW and PRIORITY VERIFY records are included here.
#
# Records with insufficient comparison data are not considered
# statistical outliers and are therefore not included in the
# verification table.
#
# Crown and overall-condition fields are shown only as contextual
# information at this stage. Those attributes are cleaned later
# under Attributes 13 and 14.

dbh_height_verification <- dbh_height_outliers %>%
  filter(
    grepl(
      "REVIEW|PRIORITY VERIFY",
      dbh_height_review_flag
    )
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    cluster_info_stratum,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    dbh_class_5cm,
    tree_height,
    n_trees,
    height_q1,
    height_median,
    height_q3,
    lower_limit_1_5,
    upper_limit_1_5,
    lower_limit_3,
    upper_limit_3,
    tree_condition_crown_label,
    tree_condition_overall_label,
    dbh_height_review_flag
  )


cat(
  "Live trees requiring DBH-height statistical review:",
  nrow(dbh_height_verification),
  "\n"
)


dbh_height_verification %>%
  count(
    dbh_height_review_flag,
    sort = TRUE
  )


#View(dbh_height_verification)


# ------------------------------------------------------------
# 11.10 Record DoF verification of statistical outliers
# ------------------------------------------------------------

# The DBH-height verification table was previously submitted to DoF.
#
# DoF explicitly confirmed the recorded heights of 13 statistically
# unusual live trees.
#
# These are genuine recorded measurements and are therefore retained
# unchanged in the cleaned height variable.

dof_height_confirmed <- tibble::tribble(
  ~cluster_no, ~plot_no, ~tree_no,
  46,          4,        4,
  46,          4,        5,
  90,          5,        2,
  18,          2,        1,
  15,          2,        9,
  55,          3,       16,
  102,         5,        8,
  80,          3,       10,
  55,          1,       11,
  55,          2,       19,
  72,          1,       33,
  35,          5,        6,
  33,          1,        2
) %>%
  mutate(
    dof_height_status =
      "DoF confirmed recorded height"
  )


cat(
  "Statistical DBH-height outliers explicitly confirmed by DoF:",
  nrow(dof_height_confirmed),
  "\n"
)


# ------------------------------------------------------------
# 11.11 Identify statistical outliers remaining unresolved
# ------------------------------------------------------------

# Statistical outliers that were not explicitly confirmed by DoF
# are not assigned suggested replacement measurements.
#
# Because no independently verified field measurement is available,
# their observed height remains unresolved.

dbh_height_unresolved_dof <- dbh_height_verification %>%
  anti_join(
    dof_height_confirmed,
    by = key_cols
  ) %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    
    height_issue =
      "Unresolved: DBH-height outlier not confirmed by field measurement"
  ) %>%
  distinct()


cat(
  "Statistical DBH-height outliers remaining unresolved:",
  nrow(dbh_height_unresolved_dof),
  "\n"
)


# ------------------------------------------------------------
# 11.12 Identify physically inconsistent standing-tree heights
# ------------------------------------------------------------

# These records have total height below their diameter measurement
# height and are physically inconsistent as recorded.
#
# Suggested numerical replacements are not used because they were
# not based on a new field measurement.

physical_height_unresolved <- standing_height_conflict %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    
    height_issue =
      "Unresolved: total height below diameter measurement height"
  ) %>%
  distinct()


cat(
  "Physically inconsistent standing-tree heights:",
  nrow(physical_height_unresolved),
  "\n"
)


# ------------------------------------------------------------
# 11.13 Identify unresolved stump heights
# ------------------------------------------------------------

# Attribute 9 identified stump records for which either:
#
#   - the stump height itself is unusually low/questionable; or
#
#   - stump height and diameter measurement height remain physically
#     inconsistent.
#
# These same records also have unresolved observed stump height.
#
# Only records that remain classified as stumps after Attribute 4
# are included.

stump_height_unresolved <- tree %>%
  filter(
    tree_status_label_clean == "Stump",
    
    stump_height_flag != "OK" |
      stump_diameter_height_flag == "VERIFY"
  ) %>%
  transmute(
    cluster_no,
    plot_no,
    tree_no,
    
    height_issue = case_when(
      
      stump_height_flag != "OK" ~
        "Unresolved: questionable stump height",
      
      stump_diameter_height_flag == "VERIFY" ~
        "Unresolved: stump height/measurement-height conflict"
    )
  ) %>%
  distinct()


cat(
  "Stump heights remaining unresolved:",
  nrow(stump_height_unresolved),
  "\n"
)


stump_height_unresolved %>%
  count(
    height_issue,
    sort = TRUE
  )


# ------------------------------------------------------------
# 11.14 Combine all unresolved observed-height records
# ------------------------------------------------------------

height_unresolved_all <- bind_rows(
  
  dbh_height_unresolved_dof,
  
  physical_height_unresolved,
  
  stump_height_unresolved
)


# Check whether the same record has entered the unresolved table
# through more than one QA pathway.

height_unresolved_all %>%
  count(
    cluster_no,
    plot_no,
    tree_no
  ) %>%
  filter(
    n > 1
  )


# Create one lookup row per unresolved tree/stump record.

height_unresolved_lookup <- height_unresolved_all %>%
  distinct(
    cluster_no,
    plot_no,
    tree_no,
    .keep_all = TRUE
  )


cat(
  "Total records with unresolved observed height:",
  nrow(height_unresolved_lookup),
  "\n"
)


height_unresolved_lookup %>%
  count(
    height_issue,
    sort = TRUE
  )


# ------------------------------------------------------------
# 11.15 Join height QA decisions back to the master dataset
# ------------------------------------------------------------

# First create a compact lookup containing the statistical screening
# result for each live tree included in the DBH-height assessment.

dbh_height_flag_lookup <- dbh_height_outliers %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    dbh_height_review_flag
  ) %>%
  distinct()


# Record row count before joining.
#
# The joins must preserve the one-row-per-tree structure.

n_before_height_join <- nrow(tree)


tree <- tree %>%
  left_join(
    dbh_height_flag_lookup,
    by = key_cols
  ) %>%
  left_join(
    dof_height_confirmed,
    by = key_cols
  ) %>%
  left_join(
    height_unresolved_lookup,
    by = key_cols
  )


stopifnot(
  nrow(tree) == n_before_height_join
)


# ------------------------------------------------------------
# 11.16 Create cleaned observed tree height
# ------------------------------------------------------------

# tree_height_clean contains only observed field heights that are
# considered sufficiently reliable following QA/QC and verification.
#
# Treatment:
#
#   Unresolved measurement
#       -> NA
#
#   Statistical outlier explicitly confirmed by DoF
#       -> retain recorded height
#
#   Sparse DBH comparison group
#       -> retain recorded height because statistical evidence is
#          insufficient to classify it as unusual
#
#   All other accepted observations
#       -> retain recorded height
#
# No estimated or modelled heights are introduced here.

tree <- tree %>%
  mutate(
    
    tree_height_clean = case_when(
      
      !is.na(height_issue) ~
        NA_real_,
      
      TRUE ~
        tree_height
    ),
    
    
    # Audit variable explaining how the height was treated.
    
    tree_height_clean_status = case_when(
      
      !is.na(height_issue) ~
        height_issue,
      
      !is.na(dof_height_status) ~
        "Recorded height retained: explicitly confirmed by DoF",
      
      dbh_height_review_flag ==
        "INSUFFICIENT COMPARISON DATA" ~
        "Recorded height retained: insufficient comparison data for statistical screening",
      
      TRUE ~
        "Recorded height retained"
    )
  )


# ------------------------------------------------------------
# 11.17 Summarize final cleaned-height treatment
# ------------------------------------------------------------

height_clean_summary <- tree %>%
  summarise(
    
    total_records = n(),
    
    original_height_missing =
      sum(
        is.na(tree_height)
      ),
    
    clean_height_available =
      sum(
        !is.na(tree_height_clean)
      ),
    
    clean_height_unresolved =
      sum(
        is.na(tree_height_clean)
      )
  )


print(
  height_clean_summary,
  width = Inf
)


tree %>%
  count(
    tree_height_clean_status,
    sort = TRUE
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 11.18 Confirm DoF-verified statistical outliers were retained
# ------------------------------------------------------------

confirmed_height_check <- tree %>%
  filter(
    !is.na(dof_height_status)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_dbh,
    tree_height,
    tree_height_clean,
    dbh_height_review_flag,
    dof_height_status
  )


print(
  confirmed_height_check,
  n = Inf,
  width = Inf
)


confirmed_height_check %>%
  summarise(
    all_confirmed_heights_retained =
      all(
        tree_height == tree_height_clean
      )
  )


# ------------------------------------------------------------
# 11.19 Review final unresolved height records
# ------------------------------------------------------------

height_unresolved_final <- tree %>%
  filter(
    !is.na(height_issue)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    cluster_info_stratum,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_diameter_height,
    tree_diameter_height_clean,
    tree_height,
    tree_height_clean,
    height_issue,
    dbh_height_review_flag
  ) %>%
  arrange(
    tree_status_label_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Final unresolved observed-height records:",
  nrow(height_unresolved_final),
  "\n"
)


#View(height_unresolved_final)


# ------------------------------------------------------------
# 11.20 Final Attribute 11 reproducibility checks
# ------------------------------------------------------------

# These checks confirm that the master workflow reproduces the
# finalized tree-height QA/QC results.

stopifnot(
  
  # Four physically inconsistent standing-tree heights.
  nrow(standing_height_conflict) == 4,
  
  # Number of live trees entering statistical screening.
  nrow(dbh_height_check) == 3148,
  
  # Total statistical DBH-height review records.
  nrow(dbh_height_verification) == 42,
  
  # DoF-confirmed statistical outliers.
  nrow(dof_height_confirmed) == 13,
  
  # Statistical outliers remaining unresolved.
  nrow(dbh_height_unresolved_dof) == 29,
  
  # Unresolved stump heights.
  nrow(stump_height_unresolved) == 21,
  
  # Total unresolved observed heights.
  nrow(height_unresolved_lookup) == 54,
  
  # Final cleaned-height result.
  sum(is.na(tree$tree_height_clean)) == 54,
  
  # Confirm all 13 DoF-verified heights were retained unchanged.
  all(
    confirmed_height_check$tree_height ==
      confirmed_height_check$tree_height_clean
  )
)

cat(
  "\nAttribute 11 tree-height QA checks completed successfully.\n"
)


# ------------------------------------------------------------
# ATTRIBUTE 11 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# All 3,672 records contained an original recorded height.
#
# Following the tree-status corrections completed under Attribute 4,
# no records remaining classified as stumps exceeded 5 m in height.
# Fourteen stumps retained unusually low recorded heights (<0.10 m)
# and had already been flagged for verification under Attribute 9.
#
# Four live standing trees had physically inconsistent heights because
# their recorded total height was lower than the height at which stem
# diameter was measured. These heights remain unresolved.
#
# DBH-height plausibility was assessed for 3,148 live standing trees
# using a stratified Tukey IQR screening procedure. Trees were compared
# within forest stratum and 5-cm DBH class, with a minimum comparison
# group size of 10 trees.
#
# Screening results were:
#
#   - 3,047 records within the expected range;
#   -    59 records with insufficient comparison data;
#   -    33 records unusually tall for DBH (REVIEW);
#   -     7 records extremely tall for DBH (PRIORITY VERIFY);
#   -     2 records unusually short for DBH (REVIEW).
#
# A total of 42 live-tree records were therefore identified for
# statistical DBH-height review. These statistical flags indicate
# unusual observations and do not, by themselves, demonstrate that
# a field measurement is incorrect.
#
# DoF explicitly confirmed the recorded heights of 13 flagged trees.
# These values are retained unchanged.
#
# The remaining unresolved observed heights comprise:
#
#   - 29 statistical DBH-height outliers not confirmed by field
#     remeasurement;
#   -  4 physically inconsistent standing-tree heights;
#   - 14 questionable very-low stump heights;
#   -  7 stump height/diameter-measurement-height conflicts.
#
# Total unresolved observed heights = 54.
#
# FINAL DECISION:
#
# The original Arena variable tree_height remains unchanged.
#
# The cleaned variable tree_height_clean retains 3,618 recorded
# heights considered suitable for subsequent analysis and contains
# NA for the 54 unresolved observations
#
# Trees in comparison groups containing fewer than 10 observations
# retain their recorded heights because there was insufficient
# statistical evidence to classify them as unusual
#
# No replacement heights were guessed or inferred during cleaning.
# Where required for subsequent analysis, model-based estimation of
# unresolved live-tree heights may be undertaken separately during
# the data-analysis stage.


# ============================================================
# ATTRIBUTE 12: STEM QUALITY
# ============================================================

# Attribute 12 describes the quality/form of the tree stem.
#
# Arena categories in the current dataset are:
#
#   Low
#   Medium
#   High
#
# Stem quality is treated as an attribute of LIVE STANDING TREES.
#
# It is not retained as a valid analytical attribute for stumps or
# dead standing trees because those records no longer represent a
# live stem whose quality is being assessed
#
# IMPORTANT:
#
# Attribute 4 has already finalized tree status. Therefore,
# all checks and cleaning below use tree_status_label_clean.
#
# Original Arena stem-quality variables are not overwritten:
#
#   tree_stem_quality
#   tree_stem_quality_label
#
# New cleaned variables will be created for subsequent analysis.


# ------------------------------------------------------------
# 12.1 Review recorded stem-quality categories
# ------------------------------------------------------------

# This shows the Arena codes, labels and number of records in each
# category, including missing values.

stem_quality_categories <- tree %>%
  count(
    tree_stem_quality,
    tree_stem_quality_label,
    .drop = FALSE
  )

print(
  stem_quality_categories,
  n = Inf
)


# ------------------------------------------------------------
# 12.2 Review stem quality by cleaned tree status
# ------------------------------------------------------------

# This cross-tabulation distinguishes:
#
#   - valid stem-quality values recorded for live trees; and
#   - values recorded for records now classified as non-live.
#
# Missing stem quality among non-live records is expected because
# the attribute is not applicable to them.

stem_quality_by_status <- tree %>%
  count(
    tree_status_label_clean,
    tree_stem_quality_label,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_stem_quality_label
  )

print(
  stem_quality_by_status,
  n = Inf
)


# ------------------------------------------------------------
# 12.3 Check completeness among live standing trees
# ------------------------------------------------------------

# Every live standing tree should have a stem-quality assessment.
#
# This check identifies any live records where the attribute was
# not recorded.

live_missing_stem_quality <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    is.na(tree_stem_quality_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_stem_quality,
    tree_stem_quality_label
  )


cat(
  "Live standing trees with missing stem quality:",
  nrow(live_missing_stem_quality),
  "\n"
)

print(
  live_missing_stem_quality,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 12.4 Inspect stem-quality values recorded for non-live records
# ------------------------------------------------------------

# A stem-quality entry on a stump or dead standing tree is retained
# in the original Arena variable for traceability, but it is not
# treated as a valid value in the cleaned analytical variable.

non_live_with_stem_quality <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_stem_quality_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_stem_quality,
    tree_stem_quality_label
  ) %>%
  arrange(
    tree_status_label_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Non-live records with stem quality recorded:",
  nrow(non_live_with_stem_quality),
  "\n"
)

print(
  non_live_with_stem_quality,
  n = Inf,
  width = Inf
)

#View(non_live_with_stem_quality)


# ------------------------------------------------------------
# 12.5 Create cleaned stem-quality variables
# ------------------------------------------------------------

# Cleaning rule:
#
#   Live standing tree
#       -> retain recorded stem-quality value.
#
#   Dead standing tree or stump
#       -> cleaned stem-quality value = NA.
#
# A remark is added where a non-live record originally contained
# a stem-quality value so that the change remains fully traceable.

tree <- tree %>%
  mutate(
    
    tree_stem_quality_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_stem_quality,
      
      TRUE ~
        NA_real_
    ),
    
    
    tree_stem_quality_label_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_stem_quality_label,
      
      TRUE ~
        NA_character_
    ),
    
    
    tree_stem_quality_remark = case_when(
      
      tree_status_label_clean == "Dead standing tree" &
        !is.na(tree_stem_quality_label) ~
        "Stem quality removed: not applicable to dead standing tree",
      
      tree_status_label_clean == "Stump" &
        !is.na(tree_stem_quality_label) ~
        "Stem quality removed: not applicable to stump",
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 12.6 Review records changed during stem-quality cleaning
# ------------------------------------------------------------

stem_quality_cleaned_records <- tree %>%
  filter(
    !is.na(tree_stem_quality_remark)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_stem_quality_label,
    tree_stem_quality_label_clean,
    tree_stem_quality_remark
  )


print(
  stem_quality_cleaned_records,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 12.7 Confirm final stem-quality structure
# ------------------------------------------------------------

# After cleaning:
#
#   - every retained stem-quality value should belong to a
#     live standing tree;
#
#   - no stump or dead standing tree should retain a cleaned
#     stem-quality value.

tree %>%
  count(
    tree_status_label_clean,
    tree_stem_quality_label_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_stem_quality_label_clean
  ) %>%
  print(n = Inf)


non_live_clean_stem_quality <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_stem_quality_label_clean)
  )


cat(
  "Non-live records retaining cleaned stem quality:",
  nrow(non_live_clean_stem_quality),
  "\n"
)

# ------------------------------------------------------------
# 12.8 Final Attribute 12 reproducibility checks
# ------------------------------------------------------------

stopifnot(
  
  # Stem quality is complete for all live standing trees.
  nrow(live_missing_stem_quality) == 0,
  
  # Four non-live records originally contained stem quality.
  nrow(non_live_with_stem_quality) == 4,
  
  # No non-live record retains stem quality after cleaning.
  nrow(non_live_clean_stem_quality) == 0,
  
  # All 3,152 live trees retain a cleaned stem-quality value.
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      !is.na(tree$tree_stem_quality_label_clean)
  ) == 3152
)

cat(
  "\nAttribute 12 stem-quality QA checks completed successfully.\n"
)


# ------------------------------------------------------------
# ATTRIBUTE 12 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Stem quality was recorded for all 3,152 live standing trees.
#
# Live-tree stem-quality distribution:
#
#   - High   = 1,599
#   - Medium = 1,205
#   - Low    =   348
#
# Four non-live records also contained stem-quality values:
#
#   - 3 dead standing trees;
#   - 1 stump.
#
# These four values were all recorded as "Low". Because stem quality
# is treated as a live-tree attribute, the values are not retained
# in the cleaned analytical variable.
#
# After cleaning:
#
#   - all 3,152 live standing trees retain their recorded
#     stem-quality assessment;
#
#   - all 77 dead standing trees have NA stem quality;
#
#   - all 443 stumps have NA stem quality.
#
# FINAL DECISION:
#
# The original Arena variables remain unchanged:
#
#   tree_stem_quality
#   tree_stem_quality_label
#
# Cleaned variables are:
#
#   tree_stem_quality_clean
#   tree_stem_quality_label_clean
#
# A remark is retained for the four non-live records where a
# stem-quality value was removed from the cleaned variable.


# ============================================================
# ATTRIBUTE 13: CROWN CONDITION
# ============================================================

# Attribute 13 describes the condition of the tree crown.
#
# Arena categories in the current dataset are:
#
#   Healthy
#   Declining health
#   Unhealthy
#   Dying
#   Dead
#
# Crown condition is treated as an attribute of LIVE STANDING TREES.
#
# Stumps and dead standing trees therefore receive NA in the cleaned
# crown-condition variables.
#
# IMPORTANT:
#
# A live standing tree recorded with a "Dead" crown is NOT
# automatically considered an error. A tree may still be classified as
# live while having a dead or severely damaged crown, for example where
# the tree is in the process of dying.
# Such records are therefore retained but reviewed
#
# Attribute 4 has already finalized tree status, so all checks below
# use tree_status_label_clean.
#
# Original Arena crown-condition variables are not overwritten:
#
#   tree_condition_crown
#   tree_condition_crown_label


# ------------------------------------------------------------
# 13.1 Review recorded crown-condition categories
# ------------------------------------------------------------

crown_condition_categories <- tree %>%
  count(
    tree_condition_crown,
    tree_condition_crown_label,
    .drop = FALSE
  )

print(
  crown_condition_categories,
  n = Inf
)


# ------------------------------------------------------------
# 13.2 Review crown condition by cleaned tree status
# ------------------------------------------------------------

# This cross-tabulation distinguishes valid crown assessments on
# live trees from values recorded on records now classified as
# non-live.

crown_condition_by_status <- tree %>%
  count(
    tree_status_label_clean,
    tree_condition_crown_label,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_condition_crown_label
  )

print(
  crown_condition_by_status,
  n = Inf
)


# ------------------------------------------------------------
# 13.3 Check completeness among live standing trees
# ------------------------------------------------------------

# Every live standing tree should normally have a crown-condition
# assessment.
#
# Any missing values are therefore isolated for review.

live_missing_crown_condition <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    is.na(tree_condition_crown_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown,
    tree_condition_crown_label
  )


cat(
  "Live standing trees with missing crown condition:",
  nrow(live_missing_crown_condition),
  "\n"
)

print(
  live_missing_crown_condition,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 13.4 Review live trees recorded with a "Dead" crown
# ------------------------------------------------------------

# These records are retained as valid live-tree observations unless
# other evidence demonstrates that tree status itself is incorrect.
#
# They are reviewed here because the combination is unusual, but
# not physically impossible.

live_dead_crown <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_crown_label == "Dead"
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown_label,
    tree_condition_overall_label
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Live standing trees recorded with a Dead crown:",
  nrow(live_dead_crown),
  "\n"
)

print(
  live_dead_crown,
  n = Inf,
  width = Inf
)

#View(live_dead_crown)


# ------------------------------------------------------------
# 13.5 Inspect crown-condition values recorded for non-live records
# ------------------------------------------------------------

# Crown condition is not retained as a valid analytical attribute
# for stumps or dead standing trees.
#
# Any such field entries remain preserved in the original Arena
# variables for traceability.

non_live_with_crown_condition <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_crown_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown,
    tree_condition_crown_label
  ) %>%
  arrange(
    tree_status_label_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Non-live records with crown condition recorded:",
  nrow(non_live_with_crown_condition),
  "\n"
)

print(
  non_live_with_crown_condition,
  n = Inf,
  width = Inf
)

#View(non_live_with_crown_condition)


# ------------------------------------------------------------
# 13.6 Create cleaned crown-condition variables
# ------------------------------------------------------------

# Cleaning rule:
#
#   Live standing tree
#       -> retain recorded crown condition, including "Dead".
#
#   Dead standing tree or stump
#       -> cleaned crown condition = NA.
#
# A remark is retained where a non-live record originally contained
# a crown-condition value.

tree <- tree %>%
  mutate(
    
    tree_condition_crown_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_condition_crown,
      
      TRUE ~
        NA_real_
    ),
    
    
    tree_condition_crown_label_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_condition_crown_label,
      
      TRUE ~
        NA_character_
    ),
    
    
    tree_condition_crown_remark = case_when(
      
      tree_status_label_clean == "Dead standing tree" &
        !is.na(tree_condition_crown_label) ~
        "Crown condition removed: not applicable to dead standing tree",
      
      tree_status_label_clean == "Stump" &
        !is.na(tree_condition_crown_label) ~
        "Crown condition removed: not applicable to stump",
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 13.7 Review records changed during crown-condition cleaning
# ------------------------------------------------------------

crown_condition_cleaned_records <- tree %>%
  filter(
    !is.na(tree_condition_crown_remark)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_condition_crown_label,
    tree_condition_crown_label_clean,
    tree_condition_crown_remark
  )


print(
  crown_condition_cleaned_records,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 13.8 Confirm final crown-condition structure
# ------------------------------------------------------------

tree %>%
  count(
    tree_status_label_clean,
    tree_condition_crown_label_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_condition_crown_label_clean
  ) %>%
  print(n = Inf)


non_live_clean_crown_condition <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_crown_label_clean)
  )


cat(
  "Non-live records retaining cleaned crown condition:",
  nrow(non_live_clean_crown_condition),
  "\n"
)



# ------------------------------------------------------------
# 13.9 Final Attribute 13 reproducibility checks
# ------------------------------------------------------------

stopifnot(
  
  # Crown condition is complete for all live standing trees.
  nrow(live_missing_crown_condition) == 0,
  
  # Five live trees legitimately retain a "Dead" crown condition.
  nrow(live_dead_crown) == 5,
  
  # One non-live record originally contained crown condition.
  nrow(non_live_with_crown_condition) == 1,
  
  # No non-live record retains crown condition after cleaning.
  nrow(non_live_clean_crown_condition) == 0,
  
  # All 3,152 live trees retain a cleaned crown-condition value.
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      !is.na(tree$tree_condition_crown_label_clean)
  ) == 3152
)

cat(
  "\nAttribute 13 crown-condition QA checks completed successfully.\n"
)

# ------------------------------------------------------------
# ATTRIBUTE 13 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Crown condition was recorded for all 3,152 live standing trees.
#
# Live-tree crown-condition distribution:
#
#   - Healthy          = 2,117
#   - Declining health =   878
#   - Unhealthy        =   135
#   - Dying            =    17
#   - Dead             =     5
#
# Five live standing trees were recorded with a "Dead" crown.
# These records were retained because a dead crown can occur while
# a tree is still classified as live or dying. All five also had an
# overall condition of either "Dying standing tree" or
# "Dying fallen tree", supporting this interpretation
#
# One dead standing tree had crown condition recorded as "Dying"
# Because crown condition is treated as a live-tree attribute, this
# value is set to NA in the cleaned crown-condition variable.
#
# After cleaning:
#
#   - all 3,152 live standing trees retain their recorded
#     crown-condition assessment
#
#   - all 77 dead standing trees have NA crown condition;
#
#   - all 443 stumps have NA crown condition.
#
# FINAL DECISION:
#
# The original Arena variables remain unchanged:
#
#   tree_condition_crown
#   tree_condition_crown_label
#
# Cleaned variables are:
#
#   tree_condition_crown_clean
#   tree_condition_crown_label_clean
#
# A remark is retained for the one non-live record where a
# crown-condition value was removed from the cleaned variable


# ============================================================
# ATTRIBUTE 14: OVERALL TREE CONDITION
# ============================================================

# Attribute 14 records the overall health/condition of the tree.
#
# Arena categories in the current dataset are:
#
#   Healthy
#   Slightly affected
#   Severely affected
#   Dying standing tree
#   Dying fallen tree
#
# Overall condition is treated as an attribute of LIVE TREES.
#
# Dead standing trees and stumps therefore receive NA in the cleaned
# overall-condition variables.
#
# IMPORTANT:
#
# A live tree recorded as "Dying fallen tree" is not automatically
# treated as an error. A fallen tree may still be alive and therefore
# remain classified as live while being in a dying condition
# 
## Similarly, a tree can have a healthy-looking crown while its
# overall condition is affected because damage may occur on the
# stem, roots or other parts of the tree.
#
# Therefore, crown condition and overall condition are related but
# are not expected to match exactly
#
# Attribute 4 has already finalized tree status, so all cleaning
# decisions below use tree_status_label_clean
#
# Original Arena variables are preserved:
#   tree_condition_overall
#   tree_condition_overall_label


# ------------------------------------------------------------
# 14.1 Review recorded overall-condition categories
# ------------------------------------------------------------

overall_condition_categories <- tree %>%
  count(
    tree_condition_overall,
    tree_condition_overall_label,
    .drop = FALSE
  )

print(
  overall_condition_categories,
  n = Inf
)


# ------------------------------------------------------------
# 14.2 Review overall condition by cleaned tree status
# ------------------------------------------------------------

overall_condition_by_status <- tree %>%
  count(
    tree_status_label_clean,
    tree_condition_overall_label,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_condition_overall_label
  )

print(
  overall_condition_by_status,
  n = Inf
)


# ------------------------------------------------------------
# 14.3 Check completeness among live standing trees
# ------------------------------------------------------------

# Every live standing tree should normally have an overall-condition
# assessment.

live_missing_overall_condition <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    is.na(tree_condition_overall_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown_label_clean,
    tree_condition_overall,
    tree_condition_overall_label
  )


cat(
  "Live standing trees with missing overall condition:",
  nrow(live_missing_overall_condition),
  "\n"
)

print(
  live_missing_overall_condition,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 14.4 Review live trees recorded as "Dying fallen tree"
# ------------------------------------------------------------

# These observations are unusual but can be valid.
#
# They are retained because a fallen tree can still be alive while
# undergoing severe decline.

live_dying_fallen <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label == "Dying fallen tree"
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown_label_clean,
    tree_condition_overall_label
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Live standing trees recorded as Dying fallen tree:",
  nrow(live_dying_fallen),
  "\n"
)

print(
  live_dying_fallen,
  n = Inf,
  width = Inf
)

#View(live_dying_fallen)


# ------------------------------------------------------------
# 14.5 Inspect overall-condition values recorded for non-live records
# ------------------------------------------------------------

# Overall tree condition is not retained as a valid live-tree health
# attribute for dead standing trees or stumps.
#
# Any such recorded values remain preserved in the original Arena
# variables for traceability.

non_live_with_overall_condition <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_overall_label)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown_label,
    tree_condition_overall,
    tree_condition_overall_label
  ) %>%
  arrange(
    tree_status_label_clean,
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Non-live records with overall condition recorded:",
  nrow(non_live_with_overall_condition),
  "\n"
)

print(
  non_live_with_overall_condition,
  n = Inf,
  width = Inf
)

#View(non_live_with_overall_condition)


# ------------------------------------------------------------
# 14.6 Create cleaned overall-condition variables
# ------------------------------------------------------------

# Cleaning rule:
#
#   Live standing tree
#       -> retain the recorded overall condition, including
#          "Dying fallen tree".
#
#   Dead standing tree or stump
#       -> cleaned overall condition = NA.
#
# A remark is retained where a non-live record originally contained
# an overall-condition value.

tree <- tree %>%
  mutate(
    
    tree_condition_overall_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_condition_overall,
      
      TRUE ~
        NA_real_
    ),
    
    
    tree_condition_overall_label_clean = case_when(
      
      tree_status_label_clean == "Live standing tree" ~
        tree_condition_overall_label,
      
      TRUE ~
        NA_character_
    ),
    
    
    tree_condition_overall_remark = case_when(
      
      tree_status_label_clean == "Dead standing tree" &
        !is.na(tree_condition_overall_label) ~
        "Overall condition removed: not applicable to dead standing tree",
      
      tree_status_label_clean == "Stump" &
        !is.na(tree_condition_overall_label) ~
        "Overall condition removed: not applicable to stump",
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 14.7 Review records changed during overall-condition cleaning
# ------------------------------------------------------------

overall_condition_cleaned_records <- tree %>%
  filter(
    !is.na(tree_condition_overall_remark)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_status_label,
    tree_status_label_clean,
    tree_condition_overall_label,
    tree_condition_overall_label_clean,
    tree_condition_overall_remark
  )


print(
  overall_condition_cleaned_records,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 14.8 Confirm final overall-condition structure
# ------------------------------------------------------------

tree %>%
  count(
    tree_status_label_clean,
    tree_condition_overall_label_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_condition_overall_label_clean
  ) %>%
  print(
    n = Inf
  )


non_live_clean_overall_condition <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_overall_label_clean)
  )


cat(
  "Non-live records retaining cleaned overall condition:",
  nrow(non_live_clean_overall_condition),
  "\n"
)

# ------------------------------------------------------------
# 14.9 Final Attribute 14 reproducibility checks
# ------------------------------------------------------------

stopifnot(
  
  # Overall condition is complete for all live standing trees.
  nrow(live_missing_overall_condition) == 0,
  
  # Two live trees retain the valid "Dying fallen tree" condition.
  nrow(live_dying_fallen) == 2,
  
  # One non-live record originally contained overall condition.
  nrow(non_live_with_overall_condition) == 1,
  
  # No non-live record retains overall condition after cleaning.
  nrow(non_live_clean_overall_condition) == 0,
  
  # All 3,152 live trees retain a cleaned overall-condition value.
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      !is.na(tree$tree_condition_overall_label_clean)
  ) == 3152
)

cat(
  "\nAttribute 14 overall-condition QA checks completed successfully.\n"
)

# ------------------------------------------------------------
# ATTRIBUTE 14 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Overall tree condition was recorded for all 3,152 live standing trees.
#
# Live-tree overall-condition distribution:
#
#   - Healthy             = 1,804
#   - Slightly affected   = 1,230
#   - Severely affected   =    99
#   - Dying standing tree =    17
#   - Dying fallen tree   =     2
#
# Two live standing trees were recorded as "Dying fallen tree"
# These records are retained because a fallen tree can still contain
# living tissue and remain classified as live while in a dying
# condition. Both records also had a "Dead" crown condition, which
# supports this interpretation
#
# One dead standing tree had overall condition recorded as "Healthy"
# Because overall condition is treated as a live-tree health attribute,
# this value is set to NA in the cleaned overall-condition variable.
#
# After cleaning:
#
#   - all 3,152 live standing trees retain their recorded
#     overall-condition assessment;
#
#   - all 77 dead standing trees have NA overall condition;
#
#   - all 443 stumps have NA overall condition.
#
# FINAL DECISION:
#
# The original Arena variables remain unchanged:
#
#   tree_condition_overall
#   tree_condition_overall_label
#
# Cleaned variables are:
#
#   tree_condition_overall_clean
#   tree_condition_overall_label_clean
#
# A remark is retained for the one non-live record where an
# overall-condition value was removed from the cleaned variable.


# ============================================================
# ATTRIBUTE 15: CAUSATIVE AGENT(S)
# ============================================================

# Attribute 15 identifies the agent(s) responsible for an affected
# or declining tree condition.
#
# Unlike Attributes 1–14, causative-agent information is stored
# in a separate Arena export:
#
#   14_tree_condition_causes.csv
#
# A tree can have more than one causative agent. Therefore, the
# causes table may contain several rows for the same tree.
#
# Before joining it to the main tree dataset, multiple causes must
# first be combined so that the final master dataset remains:
#
#       one row per tree/stump
#
#
# FINAL CLEANING RULES
#
#   Healthy live tree
#       -> "Not applicable"
#
#   Affected/dying live tree with recorded cause(s)
#       -> retain the recorded cause(s)
#
#   Affected/dying live tree recorded only as "Not applicable"
#       -> "Not recorded"
#
#   Affected/dying live tree with no recorded cause(s)
#       -> "Not recorded"
#
#   "Other"
#       -> retain "Other", but add a remark because the actual
#          causative agent was not specified
#
#   Dead standing tree or stump
#       -> NA
#
# IMPORTANT:
#
# Attribute 4 already finalized tree status and Attribute 14
# finalized overall tree condition. Therefore, the final cleaning
# decisions are based on:
#
#   tree_status_label_clean
#   tree_condition_overall_label_clean
#
# rather than the corresponding raw Arena variables


# ------------------------------------------------------------
# 15.1 Import the separate causative-agent table
# ------------------------------------------------------------

# tree_causes_raw <- read_csv(
#   "data/input/14_tree_condition_causes.csv"
# )
tree_causes_raw <- tree_condition_causes


# For Attribute 15 we only need the tree identifiers and the
# causative-agent fields.

tree_causes <- tree_causes_raw %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_condition_causes,
    tree_condition_causes_label
  )


# Basic structure

dim(tree_causes)

tree_causes %>%
  head(10) %>%
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 15.2 Review causative-agent categories
# ------------------------------------------------------------

# Display all Arena cause codes, labels and frequencies.

tree_causes_categories <- tree_causes %>%
  count(
    tree_condition_causes,
    tree_condition_causes_label,
    .drop = FALSE
  ) %>%
  arrange(
    tree_condition_causes
  )

print(
  tree_causes_categories,
  n = Inf
)


# ------------------------------------------------------------
# 15.3 Confirm number of records and unique trees
# ------------------------------------------------------------

tree_causes_summary <- tree_causes %>%
  summarise(
    
    total_cause_records = n(),
    
    unique_trees = n_distinct(
      paste(
        cluster_no,
        plot_no,
        tree_no,
        sep = "_"
      )
    )
  )

print(
  tree_causes_summary,
  width = Inf
)


# ------------------------------------------------------------
# 15.4 Check number of causes recorded per tree
# ------------------------------------------------------------

# More than one row for a tree is not necessarily a duplicate.
# The field form allows more than one causative agent.

tree_causes_per_tree <- tree_causes %>%
  count(
    cluster_no,
    plot_no,
    tree_no,
    name = "n_causes"
  ) %>%
  count(
    n_causes,
    name = "n_trees"
  )

print(
  tree_causes_per_tree,
  n = Inf
)


# ------------------------------------------------------------
# 15.5 Check for accidental duplication of the same cause
# ------------------------------------------------------------

# Multiple DIFFERENT causes are valid.
#
# The same cause repeated for the same tree would indicate a
# possible duplicate record.

tree_causes_same_cause_duplicate <- tree_causes %>%
  count(
    cluster_no,
    plot_no,
    tree_no,
    tree_condition_causes,
    tree_condition_causes_label,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


cat(
  "Duplicate recording of the same cause:",
  nrow(tree_causes_same_cause_duplicate),
  "\n"
)

print(
  tree_causes_same_cause_duplicate,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15.6 Confirm that all cause records match the main tree dataset
# ------------------------------------------------------------

# Every tree represented in the separate causes table should also
# exist in the main tree dataset.

tree_causes_not_in_tree <- tree_causes %>%
  distinct(
    cluster_no,
    plot_no,
    tree_no
  ) %>%
  anti_join(
    tree %>%
      distinct(
        cluster_no,
        plot_no,
        tree_no
      ),
    by = key_cols
  )


cat(
  "Trees in causes table not found in main tree dataset:",
  nrow(tree_causes_not_in_tree),
  "\n"
)

print(
  tree_causes_not_in_tree,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15.7 Summarize the type of cause information available
# ------------------------------------------------------------

# These indicators help distinguish:
#
#   - actual recorded causes;
#   - "Not applicable";
#   - "Other".
#
# They will later be used to document why a cleaned value was
# assigned.

tree_cause_flags <- tree_causes %>%
  group_by(
    cluster_no,
    plot_no,
    tree_no
  ) %>%
  summarise(
    
    has_actual_cause = any(
      !is.na(tree_condition_causes_label) &
        tree_condition_causes_label != "Not applicable"
    ),
    
    has_not_applicable = any(
      tree_condition_causes_label == "Not applicable",
      na.rm = TRUE
    ),
    
    has_other = any(
      tree_condition_causes_label == "Other",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15.8 Collapse multiple genuine causes to one tree record
# ------------------------------------------------------------

# "Not applicable" is not an actual causative agent and is therefore
# excluded from the collapsed cause list.
#
# Multiple genuine causes are retained and separated by a semicolon.
#
# Example:
#
#   Disease/Fungi; Insects
#
# This produces only one row per tree, which allows the information
# to be joined safely to the master tree dataset.

tree_causes_collapsed <- tree_causes %>%
  filter(
    !is.na(tree_condition_causes_label),
    tree_condition_causes_label != "Not applicable"
  ) %>%
  group_by(
    cluster_no,
    plot_no,
    tree_no
  ) %>%
  summarise(
    
    tree_condition_causes_recorded = paste(
      sort(
        unique(tree_condition_causes_label)
      ),
      collapse = "; "
    ),
    
    tree_condition_causes_n_recorded =
      n_distinct(
        tree_condition_causes_label
      ),
    
    .groups = "drop"
  )


# Confirm one row per tree after collapsing.

tree_causes_collapsed %>%
  count(
    cluster_no,
    plot_no,
    tree_no
  ) %>%
  filter(
    n > 1
  )


# ------------------------------------------------------------
# 15.9 Identify affected live trees missing from the causes table
# ------------------------------------------------------------

# A live tree whose cleaned overall condition is anything other
# than Healthy should normally have information on a causative
# agent.
#
# This check identifies affected/dying live trees with no record
# at all in the separate causes table.

affected_live_missing_cause <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy"
  ) %>%
  anti_join(
    tree_causes %>%
      distinct(
        cluster_no,
        plot_no,
        tree_no
      ),
    by = key_cols
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_condition_crown_label_clean,
    tree_condition_overall_label_clean
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Affected/dying live trees absent from causes table:",
  nrow(affected_live_missing_cause),
  "\n"
)


# ------------------------------------------------------------
# 15.10 Join collapsed cause information to the master dataset
# ------------------------------------------------------------

# IMPORTANT:
#
# We join the COLLAPSED cause table, not the original 1,534-row
# table. Joining the original table directly would create duplicate
# tree rows wherever more than one cause had been recorded.

n_before_cause_join <- nrow(tree)


tree <- tree %>%
  
  left_join(
    tree_causes_collapsed,
    by = key_cols
  ) %>%
  
  left_join(
    tree_cause_flags,
    by = key_cols
  )


# The master dataset must still contain exactly one row per
# tree/stump.

stopifnot(
  nrow(tree) == n_before_cause_join
)


# ------------------------------------------------------------
# 15.11 Create cleaned causative-agent variables
# ------------------------------------------------------------

tree <- tree %>%
  mutate(
    
    # --------------------------------------------------------
    # Final cleaned cause
    # --------------------------------------------------------
    
    tree_condition_causes_clean = case_when(
      
      # Healthy live trees do not require a causative agent.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean == "Healthy" ~
        "Not applicable",
      
      
      # Affected/dying live trees with one or more genuine
      # recorded causes retain those causes.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        !is.na(tree_condition_causes_recorded) ~
        tree_condition_causes_recorded,
      
      
      # Affected/dying live trees without an actual recorded cause
      # are explicitly distinguished from Healthy trees.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        is.na(tree_condition_causes_recorded) ~
        "Not recorded",
      
      
      # Causative-agent information is not applicable to non-live
      # records in the cleaned tree-health dataset.
      TRUE ~
        NA_character_
    ),
    
    
    # --------------------------------------------------------
    # Number of actual causative agents
    # --------------------------------------------------------
    
    tree_condition_causes_n_clean = case_when(
      
      # Healthy tree = zero applicable causative agents.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean == "Healthy" ~
        0L,
      
      
      # Where genuine cause(s) were recorded, retain their number.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        !is.na(tree_condition_causes_n_recorded) ~
        tree_condition_causes_n_recorded,
      
      
      # For "Not recorded", the number of actual causes is unknown.
      TRUE ~
        NA_integer_
    ),
    
    
    # --------------------------------------------------------
    # Audit remark
    # --------------------------------------------------------
    
    tree_condition_causes_remark = case_when(
      
      # A cause had been entered even though the tree was Healthy.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean == "Healthy" &
        has_actual_cause %in% TRUE ~
        "Changed to Not applicable: tree recorded as Healthy",
      
      
      # Other is retained because it is the field-recorded category,
      # but the actual agent is unknown.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        has_other %in% TRUE ~
        "Other causative agent not specified",
      
      
      # An affected tree was entered as Not applicable.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        is.na(tree_condition_causes_recorded) &
        has_not_applicable %in% TRUE ~
        "Changed to Not recorded: affected tree had causative agent recorded as Not applicable",
      
      
      # The affected tree was completely absent from the causes table.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        is.na(tree_condition_causes_recorded) &
        is.na(has_actual_cause) ~
        "Changed to Not recorded: affected tree had no causative-agent record",
      
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 15.12 Review final cleaned causes by overall condition
# ------------------------------------------------------------

tree %>%
  count(
    tree_condition_overall_label_clean,
    tree_condition_causes_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_condition_overall_label_clean,
    tree_condition_causes_clean
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 15.13 Summarize the main cleaning outcomes
# ------------------------------------------------------------

cause_cleaning_summary <- tree %>%
  summarise(
    
    healthy_live =
      sum(
        tree_status_label_clean == "Live standing tree" &
          tree_condition_overall_label_clean == "Healthy"
      ),
    
    affected_live =
      sum(
        tree_status_label_clean == "Live standing tree" &
          tree_condition_overall_label_clean != "Healthy"
      ),
    
    affected_with_recorded_cause =
      sum(
        tree_status_label_clean == "Live standing tree" &
          tree_condition_overall_label_clean != "Healthy" &
          !is.na(tree_condition_causes_recorded)
      ),
    
    affected_not_recorded =
      sum(
        tree_status_label_clean == "Live standing tree" &
          tree_condition_causes_clean == "Not recorded"
      ),
    
    affected_with_other =
      sum(
        tree_status_label_clean == "Live standing tree" &
          tree_condition_overall_label_clean != "Healthy" &
          has_other %in% TRUE
      ),
    
    non_live_with_clean_cause =
      sum(
        tree_status_label_clean != "Live standing tree" &
          !is.na(tree_condition_causes_clean)
      )
  )


print(
  cause_cleaning_summary,
  width = Inf
)


# ------------------------------------------------------------
# 15.14 Review affected trees classified as "Not recorded"
# ------------------------------------------------------------

affected_cause_not_recorded <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_causes_clean == "Not recorded"
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_condition_crown_label_clean,
    tree_condition_overall_label_clean,
    tree_condition_causes_recorded,
    has_not_applicable,
    tree_condition_causes_clean,
    tree_condition_causes_remark
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Affected/dying live trees with causative agent Not recorded:",
  nrow(affected_cause_not_recorded),
  "\n"
)

#View(affected_cause_not_recorded)


# ------------------------------------------------------------
# 15.15 Review records containing "Other"
# ------------------------------------------------------------

cause_other_review <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    has_other %in% TRUE
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_condition_overall_label_clean,
    tree_condition_causes_recorded,
    tree_condition_causes_clean,
    tree_condition_causes_remark
  )


cat(
  "Affected/dying live trees containing Other as a cause:",
  nrow(cause_other_review),
  "\n"
)

#View(cause_other_review)


# ------------------------------------------------------------
# 15.16 Confirm non-live records have no cleaned cause
# ------------------------------------------------------------

non_live_clean_cause <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_causes_clean)
  )


cat(
  "Non-live records retaining cleaned causative-agent information:",
  nrow(non_live_clean_cause),
  "\n"
)

# ------------------------------------------------------------
# 15.17 Final Attribute 15 reproducibility checks
# ------------------------------------------------------------

stopifnot(
  
  # Separate causes table structure.
  nrow(tree_causes) == 1534,
  
  # Number of unique trees represented in the causes table.
  nrow(
    tree_causes %>%
      distinct(cluster_no, plot_no, tree_no)
  ) == 1321,
  
  # No duplicate recording of the same cause for the same tree.
  nrow(tree_causes_same_cause_duplicate) == 0,
  
  # Every tree in the causes table occurs in the main tree dataset.
  nrow(tree_causes_not_in_tree) == 0,
  
  # Number of affected/dying live trees without a usable cause.
  nrow(affected_cause_not_recorded) == 145,
  
  # Of these, 38 were completely absent from the causes table.
  nrow(affected_live_missing_cause) == 38,
  
  # "Other" was recorded for 28 affected/dying live trees.
  nrow(cause_other_review) == 28,
  
  # No non-live record retains a cleaned cause.
  nrow(non_live_clean_cause) == 0,
  
  # All live trees receive a cleaned cause classification.
  sum(
    tree$tree_status_label_clean == "Live standing tree" &
      is.na(tree$tree_condition_causes_clean)
  ) == 0
)

cat(
  "\nAttribute 15 causative-agent QA checks completed successfully.\n"
)


# ------------------------------------------------------------
# ATTRIBUTE 15 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Causative-agent information was stored in a separate Arena table
# containing 1,534 records representing 1,321 unique trees.
#
# Multiple causative agents could legitimately be recorded for the
# same tree:
#
#   - 1 cause = 1,139 trees
#   - 2 causes =   151 trees
#   - 3 causes =    31 trees
#
# No duplicate occurrence of the same cause was found for any tree,
# and all tree identifiers in the causes table matched records in
# the main tree dataset.
#
# Multiple recorded causes were therefore collapsed into a single
# semicolon-separated variable before joining to the master dataset.
# This preserved the one-row-per-tree structure.
#
# Among the 3,152 live standing trees:
#
#   - 1,804 were Healthy;
#   - 1,348 were Slightly affected, Severely affected or Dying.
#
# Healthy trees were assigned "Not applicable" because no causative
# agent is required for a tree recorded as Healthy.
#
# Among the 1,348 affected/dying live trees:
#
#   - 1,203 had one or more usable recorded causative agents;
#   -   145 had no usable recorded cause and were classified
#         as "Not recorded".
#
# Of the 145 "Not recorded" cases:
#
#   - 38 had no corresponding record in the causative-agent table;
#   - 107 were represented in the table but had only
#     "Not applicable" recorded.
#
# The category "Other" occurred among 28 affected/dying trees.
# "Other" is retained because it represents the field-recorded
# information, while a remark indicates that the specific
# causative agent was not provided.
#
# Dead standing trees and stumps have NA in the cleaned
# causative-agent variables.
#
# FINAL DECISION:
#
# Cleaned causative-agent information is stored in:
#
#   tree_condition_causes_clean
#   tree_condition_causes_n_clean
#
# Multiple genuine causes are retained as a semicolon-separated list
#
# "Not applicable" means that the live tree was recorded as Healthy
#
# "Not recorded" means that the live tree was affected/dying but no
# usable causative agent was recorded. This includes trees with no
# causative-agent record and trees for which only "Not applicable"
# was entered.
#
# No causative agent was inferred where the field data did not
# provide sufficient information



# ============================================================
# ATTRIBUTE 16: RECOMMENDED MOTHER TREE
# ============================================================

# Attribute 16 indicates whether a tree was recommended as a
# mother tree for seed collection purposes.
#
# The field guidance states that the enumerator should indicate
# whether the tree should be selected as a mother tree for seed
# collection.
#
# A recommended mother tree should therefore be a LIVE and HEALTHY
# tree. A tree recorded as affected, severely affected or dying is
# not considered suitable for recommendation as a seed source.
#
# For this cleaning step, the cleaned overall tree condition from
# Attribute 14 is used as the primary health criterion.
#
# IMPORTANT:
#
# Healthy live trees retain their original field recommendation.
# Affected or dying live trees are assigned FALSE.
# Non-live records are assigned NA 
#
# The original Arena variable tree_mother_tree is preserved
# unchanged


# ------------------------------------------------------------
# 16.1 Review original mother-tree recommendations
# ------------------------------------------------------------

mother_tree_original_summary <- tree %>%
  count(
    tree_mother_tree,
    .drop = FALSE
  )

print(
  mother_tree_original_summary,
  n = Inf
)


# ------------------------------------------------------------
# 16.2 Review recommendation by cleaned tree status
# ------------------------------------------------------------

# Mother-tree recommendation is relevant only to live standing trees.
#
# This check also confirms whether any recommendation was recorded
# for stumps or dead standing trees.

mother_tree_by_status <- tree %>%
  count(
    tree_status_label_clean,
    tree_mother_tree,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_mother_tree
  )

print(
  mother_tree_by_status,
  n = Inf
)


# ------------------------------------------------------------
# 16.3 Review recommendation against cleaned overall condition
# ------------------------------------------------------------

# This is the main health-consistency check.
#
# A recommendation of TRUE on a Slightly affected, Severely affected
# or Dying tree is inconsistent with the agreed mother-tree health
# criterion.

mother_tree_by_condition <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree"
  ) %>%
  count(
    tree_condition_overall_label_clean,
    tree_mother_tree,
    .drop = FALSE
  ) %>%
  arrange(
    tree_condition_overall_label_clean,
    tree_mother_tree
  )

print(
  mother_tree_by_condition,
  n = Inf
)


# ------------------------------------------------------------
# 16.4 Identify unhealthy trees originally recommended
# ------------------------------------------------------------

# These records were recommended as mother trees in Arena even
# though their cleaned overall condition is not Healthy.
#
# They will be changed to FALSE in the cleaned recommendation.

unhealthy_recommended_mother_trees <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    tree_mother_tree %in% TRUE
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_condition_crown_label_clean,
    tree_condition_overall_label_clean,
    tree_mother_tree
  ) %>%
  arrange(
    cluster_no,
    plot_no,
    tree_no
  )


cat(
  "Non-healthy live trees originally recommended as mother trees:",
  nrow(unhealthy_recommended_mother_trees),
  "\n"
)

#View(unhealthy_recommended_mother_trees)


# ------------------------------------------------------------
# 16.5 Identify non-healthy live trees with missing recommendation
# ------------------------------------------------------------

# Where an affected or dying live tree has no recorded mother-tree
# recommendation, FALSE can be assigned because its health condition
# already makes it unsuitable under the agreed criterion.

unhealthy_missing_mother_tree <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    is.na(tree_mother_tree)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_condition_overall_label_clean,
    tree_mother_tree
  )


cat(
  "Non-healthy live trees with missing mother-tree recommendation:",
  nrow(unhealthy_missing_mother_tree),
  "\n"
)


# ------------------------------------------------------------
# 16.6 Identify Healthy live trees with missing recommendation
# ------------------------------------------------------------

# A Healthy tree is potentially eligible for recommendation, but
# health alone does not tell us whether the field team intended to
# recommend it.
#
# Therefore, a missing recommendation on a Healthy tree remains NA.
# We do NOT automatically convert it to TRUE.

healthy_missing_mother_tree <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean == "Healthy",
    is.na(tree_mother_tree)
  ) %>%
  select(
    cluster_no,
    plot_no,
    tree_no,
    tree_species_scientific_name_clean,
    tree_local_name_clean,
    tree_dbh,
    tree_height_clean,
    tree_mother_tree
  )


cat(
  "Healthy live trees with missing mother-tree recommendation:",
  nrow(healthy_missing_mother_tree),
  "\n"
)

print(
  healthy_missing_mother_tree,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 16.7 Create cleaned mother-tree recommendation
# ------------------------------------------------------------

# Cleaning rules:
#
#   Non-live record
#       -> NA
#
#   Healthy live tree
#       -> retain original TRUE/FALSE recommendation
#          Missing recommendation remains NA.
#
#   Non-healthy live tree
#       -> FALSE
#
# This means health determines whether a tree can remain eligible,
# but health alone does not create a new TRUE recommendation.

tree <- tree %>%
  mutate(
    
    tree_mother_tree_clean = case_when(
      
      # Mother-tree recommendation is not applicable to non-live
      # records.
      tree_status_label_clean != "Live standing tree" ~
        NA,
      
      
      # Healthy live trees retain the field recommendation.
      tree_condition_overall_label_clean == "Healthy" ~
        tree_mother_tree,
      
      
      # Affected or dying live trees are not retained as recommended
      # mother trees.
      tree_condition_overall_label_clean != "Healthy" ~
        FALSE,
      
      
      TRUE ~
        NA
    ),
    
    
    # --------------------------------------------------------
    # Audit remark
    # --------------------------------------------------------
    
    tree_mother_tree_remark = case_when(
      
      # Recommendation recorded for a non-live record.
      tree_status_label_clean != "Live standing tree" &
        !is.na(tree_mother_tree) ~
        "Mother-tree recommendation removed: not applicable to non-live record",
      
      
      # Non-healthy tree had originally been recommended.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        tree_mother_tree %in% TRUE ~
        "Changed from TRUE to FALSE: tree not Healthy",
      
      
      # Non-healthy tree had no recommendation recorded.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean != "Healthy" &
        is.na(tree_mother_tree) ~
        "Set to FALSE: tree not Healthy",
      
      
      # Healthy tree with missing recommendation remains unresolved.
      tree_status_label_clean == "Live standing tree" &
        tree_condition_overall_label_clean == "Healthy" &
        is.na(tree_mother_tree) ~
        "Recommendation not recorded: retained as NA",
      
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 16.8 Review final cleaned recommendation
# ------------------------------------------------------------

mother_tree_clean_summary <- tree %>%
  count(
    tree_status_label_clean,
    tree_condition_overall_label_clean,
    tree_mother_tree_clean,
    .drop = FALSE
  ) %>%
  arrange(
    tree_status_label_clean,
    tree_condition_overall_label_clean,
    tree_mother_tree_clean
  )

print(
  mother_tree_clean_summary,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 16.9 Summarize changes made during cleaning
# ------------------------------------------------------------

tree %>%
  count(
    tree_mother_tree_remark,
    .drop = FALSE
  ) %>%
  arrange(
    tree_mother_tree_remark
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 16.10 Confirm health consistency after cleaning
# ------------------------------------------------------------

# After cleaning there should be no affected or dying live tree
# retaining TRUE as a mother-tree recommendation.

nonhealthy_recommended_after_cleaning <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    tree_mother_tree_clean %in% TRUE
  )


cat(
  "Non-healthy live trees still recommended after cleaning:",
  nrow(nonhealthy_recommended_after_cleaning),
  "\n"
)


# Non-live records should also have no cleaned recommendation.

non_live_mother_tree_after_cleaning <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_mother_tree_clean)
  )


cat(
  "Non-live records retaining cleaned mother-tree recommendation:",
  nrow(non_live_mother_tree_after_cleaning),
  "\n"
)

# ------------------------------------------------------------
# ATTRIBUTE 16 - QA RESULT AND FINAL DECISION
# ------------------------------------------------------------

# Mother-tree recommendation was recorded for all Healthy live trees.
#
# Among the 3,152 live standing trees:
#
#   Healthy trees = 1,804
#
#       TRUE  = 1,693
#       FALSE =   111
#
#   Non-healthy trees = 1,348
#
# Before cleaning:
#
#   - 126 non-healthy trees were recommended as mother trees
#   - 18 non-healthy trees had no recommendation recorded
#
# Under the agreed cleaning rule, a recommended mother tree must
# be both live and Healthy.
#
# Therefore:
#
#   - the 126 non-healthy TRUE recommendations were changed to FALSE;
#   - the 18 missing recommendations on non-healthy trees were set
#     to FALSE.
#
# No Healthy-tree recommendation was changed, and no new TRUE
# recommendation was created during cleaning.
#
# Final cleaned recommendation:
#
#   TRUE  = 1,693
#   FALSE = 1,459
#   NA    =   520
#
# All 1,693 trees retained as recommended mother trees are Healthy
# live standing trees.
#
# FINAL DECISION:
#
# The original Arena variable tree_mother_tree remains unchanged.
#
# The cleaned recommendation is stored in:
#
#   tree_mother_tree_clean
#
# Health status is used as the eligibility criterion for retaining
# the field recommendation.



# ============================================================
# FINAL CROSS-ATTRIBUTE QA/QC
# ============================================================

# All 16 tree attributes have now been reviewed and cleaned where
# necessary.
#
# Before creating the final analysis-ready tree dataset, we carry out
# a small number of cross-attribute checks.
#
# These checks do NOT repeat the detailed QA/QC already completed
# under each attribute.
#
# Instead, they test whether the FINAL CLEANED VARIABLES are logically
# consistent with one another.
#
# Examples:
#
#   - a live tree should not have decomposition status;
#   - a stump should not retain crown or overall condition;
#   - a Healthy tree should have causative agent = "Not applicable";
#   - an affected/dying tree should not have cause = "Not applicable";
#   - a non-healthy tree should not remain recommended as a mother tree;
#   - cleaned total height should not be below cleaned diameter
#     measurement height.
#
# The original Arena variables remain unchanged throughout.


# ------------------------------------------------------------
# FQA 1. Confirm one row per tree/stump
# ------------------------------------------------------------

# cluster_no + plot_no + tree_no is the tree-level key used
# throughout the cleaning workflow.

final_duplicate_keys <- tree %>%
  count(
    cluster_no,
    plot_no,
    tree_no,
    name = "n"
  ) %>%
  filter(
    n > 1
  )


cat(
  "Duplicate tree keys in final working dataset:",
  nrow(final_duplicate_keys),
  "\n"
)


# ------------------------------------------------------------
# FQA 2. Check basic cleaned measurement consistency
# ------------------------------------------------------------

# DBH itself was retained unchanged because no DBH corrections were
# required.
#
# Check that all DBH measurements remain positive and meet the
# inventory minimum DBH of 10 cm.

invalid_final_dbh <- tree %>%
  filter(
    is.na(tree_dbh) |
      tree_dbh < 10
  )


# Where both cleaned total height and cleaned diameter measurement
# height are available, total height must not be lower than the
# diameter measurement height.

final_height_measurement_conflict <- tree %>%
  filter(
    !is.na(tree_height_clean),
    !is.na(tree_diameter_height_clean),
    tree_height_clean < tree_diameter_height_clean
  )


cat(
  "Records with invalid final DBH:",
  nrow(invalid_final_dbh),
  "\n"
)

cat(
  "Cleaned height/measurement-height conflicts:",
  nrow(final_height_measurement_conflict),
  "\n"
)


# ------------------------------------------------------------
# FQA 3. Check decomposition against cleaned tree status
# ------------------------------------------------------------

# Decomposition should not be retained for live standing trees.

live_with_clean_decomposition <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    !is.na(tree_stump_decomposition_label_clean)
  )


cat(
  "Live trees retaining cleaned decomposition:",
  nrow(live_with_clean_decomposition),
  "\n"
)


# ------------------------------------------------------------
# FQA 4. Check live-tree health attributes
# ------------------------------------------------------------

# Stem quality, crown condition and overall condition should be
# available for live standing trees.

live_missing_clean_health <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    is.na(tree_stem_quality_label_clean) |
      is.na(tree_condition_crown_label_clean) |
      is.na(tree_condition_overall_label_clean)
  )


# Conversely, these live-tree health attributes should not be
# retained for stumps or dead standing trees.

nonlive_with_clean_health <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_stem_quality_label_clean) |
      !is.na(tree_condition_crown_label_clean) |
      !is.na(tree_condition_overall_label_clean)
  )


cat(
  "Live trees missing a cleaned health attribute:",
  nrow(live_missing_clean_health),
  "\n"
)

cat(
  "Non-live records retaining cleaned health attributes:",
  nrow(nonlive_with_clean_health),
  "\n"
)


# ------------------------------------------------------------
# FQA 5. Check causative agents against overall condition
# ------------------------------------------------------------

# Healthy live trees should have:
#
#       Not applicable
#
# because there is no affected condition requiring a cause.

healthy_cause_conflict <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean == "Healthy",
    tree_condition_causes_clean != "Not applicable"
  )


# Affected/dying live trees should have either:
#
#   - one or more recorded causes; or
#   - "Not recorded".
#
# They should NOT retain "Not applicable".

affected_cause_conflict <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    is.na(tree_condition_causes_clean) |
      tree_condition_causes_clean == "Not applicable"
  )


# Non-live records should not retain cleaned causative-agent
# information.

nonlive_cause_conflict <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_condition_causes_clean)
  )


cat(
  "Healthy live trees with incorrect cleaned cause:",
  nrow(healthy_cause_conflict),
  "\n"
)

cat(
  "Affected/dying live trees with incorrect cleaned cause:",
  nrow(affected_cause_conflict),
  "\n"
)

cat(
  "Non-live records retaining cleaned causes:",
  nrow(nonlive_cause_conflict),
  "\n"
)


# ------------------------------------------------------------
# FQA 6. Check mother-tree recommendation logic
# ------------------------------------------------------------

# No affected or dying tree should remain recommended as a
# mother tree.

nonhealthy_mother_tree_conflict <- tree %>%
  filter(
    tree_status_label_clean == "Live standing tree",
    tree_condition_overall_label_clean != "Healthy",
    tree_mother_tree_clean %in% TRUE
  )


# Non-live records should have NA mother-tree recommendation.

nonlive_mother_tree_conflict <- tree %>%
  filter(
    tree_status_label_clean != "Live standing tree",
    !is.na(tree_mother_tree_clean)
  )


# Every TRUE cleaned recommendation must therefore correspond to
# a Healthy live standing tree.

mother_tree_true_conflict <- tree %>%
  filter(
    tree_mother_tree_clean %in% TRUE,
    tree_status_label_clean != "Live standing tree" |
      tree_condition_overall_label_clean != "Healthy"
  )


cat(
  "Non-healthy live trees still recommended as mother trees:",
  nrow(nonhealthy_mother_tree_conflict),
  "\n"
)

cat(
  "Non-live records retaining mother-tree recommendation:",
  nrow(nonlive_mother_tree_conflict),
  "\n"
)

cat(
  "TRUE mother-tree recommendations failing eligibility rule:",
  nrow(mother_tree_true_conflict),
  "\n"
)


# ------------------------------------------------------------
# FQA 7. Summarize unresolved cleaned measurements
# ------------------------------------------------------------

# NA in a cleaned measurement does not necessarily mean that the
# original field value was missing.
#
# Some values were deliberately set to NA because QA/QC identified
# an unresolved measurement for which no defensible replacement was
# available.

final_unresolved_measurements <- tree %>%
  summarise(
    
    unresolved_diameter_measurement_height =
      sum(
        is.na(tree_diameter_height_clean)
      ),
    
    unresolved_tree_height =
      sum(
        is.na(tree_height_clean)
      )
  )


print(
  final_unresolved_measurements,
  width = Inf
)


# ------------------------------------------------------------
# FQA 8. Compact final logical-QA summary
# ------------------------------------------------------------

# This table makes the final checks easy to review during the
# workshop.
#
# For all checks below, the expected result is ZERO.

final_cross_attribute_qa <- tibble::tibble(
  
  qa_check = c(
    "Duplicate tree keys",
    "Invalid DBH",
    "Cleaned height below cleaned diameter measurement height",
    "Live tree with decomposition",
    "Live tree missing cleaned health attribute",
    "Non-live record with cleaned health attribute",
    "Healthy live tree with incorrect causative-agent classification",
    "Affected/dying live tree with incorrect causative-agent classification",
    "Non-live record with cleaned causative agent",
    "Non-healthy live tree recommended as mother tree",
    "Non-live record with mother-tree recommendation",
    "TRUE mother-tree recommendation failing eligibility rule"
  ),
  
  n_records = c(
    nrow(final_duplicate_keys),
    nrow(invalid_final_dbh),
    nrow(final_height_measurement_conflict),
    nrow(live_with_clean_decomposition),
    nrow(live_missing_clean_health),
    nrow(nonlive_with_clean_health),
    nrow(healthy_cause_conflict),
    nrow(affected_cause_conflict),
    nrow(nonlive_cause_conflict),
    nrow(nonhealthy_mother_tree_conflict),
    nrow(nonlive_mother_tree_conflict),
    nrow(mother_tree_true_conflict)
  )
)


print(
  final_cross_attribute_qa,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# FQA 9. Final structural and logical safeguard
# ------------------------------------------------------------

# Unlike descriptive checks such as the number of Healthy trees,
# these conditions represent rules that should always hold in the
# cleaned dataset.
#
# The script therefore stops if any of them are violated.

stopifnot(
  
  nrow(tree) == 3672,
  
  nrow(final_duplicate_keys) == 0,
  
  nrow(invalid_final_dbh) == 0,
  
  nrow(final_height_measurement_conflict) == 0,
  
  nrow(live_with_clean_decomposition) == 0,
  
  nrow(live_missing_clean_health) == 0,
  
  nrow(nonlive_with_clean_health) == 0,
  
  nrow(healthy_cause_conflict) == 0,
  
  nrow(affected_cause_conflict) == 0,
  
  nrow(nonlive_cause_conflict) == 0,
  
  nrow(nonhealthy_mother_tree_conflict) == 0,
  
  nrow(nonlive_mother_tree_conflict) == 0,
  
  nrow(mother_tree_true_conflict) == 0
)


cat(
  "\nFinal cross-attribute QA/QC checks completed successfully.\n"
)


# ------------------------------------------------------------
# FINAL CROSS-ATTRIBUTE QA/QC - RESULT
# ------------------------------------------------------------

# All final cross-attribute consistency checks returned zero
# violations.
#
# This confirms that:
#
#   - tree identifiers remain unique;
#   - DBH values remain valid;
#   - cleaned height and diameter measurement height are physically
#     consistent where both are available;
#   - decomposition is not retained for live trees;
#   - live-tree health attributes are complete and are not retained
#     for non-live records;
#   - causative-agent classifications agree with cleaned overall
#     condition;
#   - mother-tree recommendations agree with the finalized health
#     eligibility rule.
#
# Remaining unresolved measurements are:
#
#   - Diameter measurement height = 21 records
#   - Tree height                 = 54 records
#
# These are deliberate NA values where no defensible numerical
# correction was available.
#
# FINAL DECISION:
#
# The cleaned tree dataset is internally consistent and ready for
# preparation as the final Tree analysis dataset


# ============================================================
# CREATE FINAL ANALYSIS-READY TREE DATASET
# ============================================================

# The final dataset retains the original Arena variables first,
# in their original order.
#
# The causative-agent information brought in from the separate
# Arena causes table is placed immediately after the original
# tree variables.
#
# All cleaned and standardized variables are then placed together
# at the far end of the dataset.
#
# This clearly separates:
#
#   1. ORIGINAL / FIELD-RECORDED INFORMATION
#   2. CLEANED / STANDARDIZED VARIABLES
#
# Original Arena variables are never overwritten.


tree_clean <- tree %>%
  select(
    
    # --------------------------------------------------------
    # ORIGINAL ARENA VARIABLES
    # --------------------------------------------------------
    
    cluster_no,
    cluster_info_stratum,
    cluster_info_region_label,
    plot_no,
    plot_access_label,
    tree_no,
    tree_plot_type,
    tree_plot_type_label,
    tree_lulc_no,
    tree_lulc_no_label,
    tree_status,
    tree_status_label,
    tree_species,
    tree_species_scientific_name,
    tree_species_vernacular_name,
    tree_local_name,
    tree_dbh,
    tree_diameter_height,
    tree_stump_decomposition,
    tree_stump_decomposition_label,
    tree_height,
    tree_stem_quality,
    tree_stem_quality_label,
    tree_condition_crown,
    tree_condition_crown_label,
    tree_condition_overall,
    tree_condition_overall_label,
    tree_mother_tree,
    
    
    # --------------------------------------------------------
    # CAUSATIVE-AGENT INFORMATION FROM SEPARATE ARENA TABLE
    # --------------------------------------------------------
    
    # Multiple field-recorded causes were collapsed to one
    # semicolon-separated value so that the final dataset remains
    # one row per tree.
    
    tree_condition_causes_recorded,
    tree_condition_causes_n_recorded,
    
    
    # --------------------------------------------------------
    # CLEANED / STANDARDIZED VARIABLES
    # --------------------------------------------------------
    
    # Attribute 4 - Tree status
    tree_status_clean,
    tree_status_label_clean,
    
    # Attributes 5-6 - Species identification
    tree_species_clean,
    tree_species_scientific_name_clean,
    tree_species_scientific_name_report,
    tree_local_name_clean,
    growth_form,
    include_in_tree_species_analysis,
    
    # Attribute 9 - Diameter measurement height
    tree_diameter_height_clean,
    
    # Attribute 10 - Decomposition status
    tree_stump_decomposition_clean,
    tree_stump_decomposition_label_clean,
    
    # Attribute 11 - Tree height
    tree_height_clean,
    
    # Attribute 12 - Stem quality
    tree_stem_quality_clean,
    tree_stem_quality_label_clean,
    
    # Attribute 13 - Crown condition
    tree_condition_crown_clean,
    tree_condition_crown_label_clean,
    
    # Attribute 14 - Overall tree condition
    tree_condition_overall_clean,
    tree_condition_overall_label_clean,
    
    # Attribute 15 - Causative agents
    tree_condition_causes_clean,
    tree_condition_causes_n_clean,
    
    # Attribute 16 - Recommended mother tree
    tree_mother_tree_clean
  )


# ------------------------------------------------------------
# Review the final dataset
# ------------------------------------------------------------

dim(tree_clean)

names(tree_clean)

glimpse(tree_clean)


# Confirm that the final dataset still contains one row per
# original tree/stump.

stopifnot(
  nrow(tree_clean) == 3672
)

cat(
  "\nFinal cleaned tree dataset created successfully.\n"
)


# ============================================================
# EXPORT FINAL CLEANED TREE DATASET
# ============================================================

# The final analysis-ready dataset contains:
#
#   - all 3,672 original tree/stump records;
#   - the original Arena variables unchanged;
#   - collapsed field-recorded causative-agent information; and
#   - the finalized cleaned/standardized variables.
#
# Temporary QA flags and intermediate working variables are not
# included in this analysis dataset.


# ------------------------------------------------------------
# Final checks before export
# ------------------------------------------------------------

stopifnot(
  nrow(tree_clean) == 3672,
  
  nrow(
    tree_clean %>%
      count(
        cluster_no,
        plot_no,
        tree_no
      ) %>%
      filter(
        n > 1
      )
  ) == 0
)


cat(
  "Final dataset dimensions:",
  nrow(tree_clean),
  "rows x",
  ncol(tree_clean),
  "columns\n"
)


# ------------------------------------------------------------
# Export final analysis-ready tree dataset
# ------------------------------------------------------------

write_csv(
  tree_clean,
  "data/Trees_data_clean.csv",
  na = ""
)

cat(
  "\nFinal cleaned tree dataset exported successfully as:",
  "\nTrees_data_clean.csv\n"
)
