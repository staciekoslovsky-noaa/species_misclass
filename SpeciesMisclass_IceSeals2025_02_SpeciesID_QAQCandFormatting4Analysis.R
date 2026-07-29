# Ice Seals 2025: QA/QC data after species misclass is complete
# S. Koslovsky, June 2026

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")
rm(install_pkg)

# Load data ------------------------------------------------------
# From SMK compiled assignments
dir <- "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\ForSMK2MatchReviewerData"
files <- list.files(dir)

assigned <- read.csv(paste(dir, files[1], sep = "\\")) %>%
  mutate(reviewer = substring(files[1], 30, 32))

for (i in 2:length(files)) {
  assigned_temp <- read.csv(paste(dir, files[i], sep = "\\")) %>%
    mutate(reviewer = substring(files[i], 30, 32))

  assigned <- assigned %>%
    rbind(assigned_temp)
}

assigned <- assigned %>%
  select(reviewer, detection, processed_detection_id)


# Load processed data from reviewers
dir <- "J:\\SpeciesMisclassification\\IceSeals2025"
files <- list.files(dir)
files <- files[grepl(
  '_processed.csv',
  files
)]

reviewed <- read.csv(
  paste(dir, files[1], sep = "\\"),
  skip = 2,
  header = FALSE,
  stringsAsFactors = FALSE,
  col.names = c(
    "detection",
    "image_name",
    "frame_number",
    "bound_left",
    "bound_top",
    "bound_right",
    "bound_bottom",
    "score",
    "length",
    "detection_type",
    "type_score",
    "att1",
    "att2",
    "att3"
  )
) %>%
  mutate(reviewer = substring(files[1], 30, 32))

for (i in 2:length(files)) {
  reviewed_temp <- read.csv(
    paste(dir, files[i], sep = "\\"),
    skip = 2,
    header = FALSE,
    stringsAsFactors = FALSE,
    col.names = c(
      "detection",
      "image_name",
      "frame_number",
      "bound_left",
      "bound_top",
      "bound_right",
      "bound_bottom",
      "score",
      "length",
      "detection_type",
      "type_score",
      "att1",
      "att2",
      "att3"
    )
  ) %>%
    mutate(reviewer = substring(files[i], 30, 32))

  reviewed <- reviewed %>%
    rbind(reviewed_temp)
}
rm(i, assigned_temp, reviewed_temp, files, dir)

reviewed <- data.frame(lapply(reviewed, function(x) {
  gsub("\\(trk-atr\\) *", "", x)
})) %>%
  mutate(detection = as.integer(detection)) %>%
  mutate(
    species_confidence = ifelse(
      grepl("^species_confidence", att1),
      gsub("species_confidence *", "", att1),
      ifelse(
        grepl("^species_confidence", att2),
        gsub("species_confidence *", "", att2),
        ifelse(
          grepl("^species_confidence", att3),
          gsub("species_confidence *", "", att3),
          "NA"
        )
      )
    )
  ) %>%
  mutate(
    age_class = ifelse(
      grepl("^age_class[[:space:]]", att1),
      gsub("age_class *", "", att1),
      ifelse(
        grepl("^age_class[[:space:]]", att2),
        gsub("age_class *", "", att2),
        ifelse(
          grepl("^age_class", att3),
          gsub("age_class *", "", att3),
          "NA"
        )
      )
    )
  ) %>%
  mutate(
    age_class_confidence = ifelse(
      grepl("^age_class_confidence", att1),
      gsub("age_class_confidence *", "", att1),
      ifelse(
        grepl("^age_class_confidence", att2),
        gsub("age_class_confidence *", "", att2),
        ifelse(
          grepl("^age_class_confidence", att3),
          gsub("age_class_confidence *", "", att3),
          "NA"
        )
      )
    )
  ) %>%
  select(
    "reviewer",
    "image_name",
    "detection",
    "detection_type",
    "species_confidence",
    "age_class",
    "age_class_confidence",
    "bound_left",
    "bound_top",
    "bound_right",
    "bound_bottom"
  )

# QA/QC data -------------------------------------------------------
data <- assigned %>%
  inner_join(reviewed, by = c("reviewer", "detection"))

data_qaqc_missing_data <- data %>% # should be 0 records
  filter(
    is.na(detection_type) |
      detection_type == "NA" |
      is.na(species_confidence) |
      species_confidence == "NA" |
      is.na(age_class) |
      age_class == "NA" |
      is.na(age_class_confidence) |
      age_class_confidence == "NA"
  )

data_qaqc_missing_reviews <- data %>% # should be 0 records
  group_by(processed_detection_id) %>%
  summarise(num_reviewers = n()) %>%
  filter(num_reviewers < 3)

data_qaqc_pos_species <- data %>% # should be 0 records
  filter(species_confidence == "positive") %>%
  group_by(processed_detection_id) %>%
  summarise(num_pos_species = n_distinct(detection_type)) %>%
  filter(num_pos_species > 1)

data_qaqc_pos_age_class <- data %>% # should be 0 records -- no different positive age class assignments, even of the species are different
  filter(age_class_confidence == "positive") %>%
  group_by(processed_detection_id) %>%
  summarise(num_pos_age_class = n_distinct(age_class)) %>%
  filter(num_pos_age_class > 1)

records2review <- (data %>%
  inner_join(
    data_qaqc_pos_species %>% select(processed_detection_id),
    by = "processed_detection_id"
  )) %>%
  rbind(
    (data %>%
      inner_join(
        data_qaqc_pos_age_class %>% select(processed_detection_id),
        by = "processed_detection_id"
      ))
  ) %>%
  rbind(
    (data %>%
      inner_join(
        data_qaqc_missing_data %>% select(processed_detection_id),
        by = "processed_detection_id"
      ))
  ) %>%
  arrange(reviewer, detection) %>%
  unique()

# Connect to DB -----------------------------------------------------
con <- RPostgreSQL::dbConnect(
  PostgreSQL(),
  dbname = Sys.getenv("pep_db"),
  host = Sys.getenv("pep_ip"),
  user = Sys.getenv("pep_admin"),
  password = Sys.getenv("admin_pw")
)

# Write data to DB --------------------------------------------------
RPostgreSQL::dbSendQuery(
  con,
  "DELETE FROM species_misclass.tbl_detections_reviewed WHERE project_schema = \'surv_ice_seals_2025\'"
)

next_id <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT max(id) FROM species_misclass.tbl_detections_reviewed"
)
next_id$max <- ifelse(is.na(next_id$max), 0, next_id$max)


import <- data %>%
  mutate(
    id = 1:n() + next_id$max,
    image_name = basename(image_name),
    project_schema = 'surv_ice_seals_2025',
    project_detection_id = processed_detection_id,
    species = detection_type,
    review_file = paste0(
      'iceSeals2025_speciesMisclass_',
      reviewer,
      '_processed.csv'
    ),
    review_type = 'random_selection',
    review_description = 'ice_seals_2025_all'
  ) %>%
  select(
    id,
    project_schema,
    image_name,
    project_detection_id,
    species,
    review_file,
    reviewer,
    review_type,
    review_description,
    bound_left,
    bound_top,
    bound_right,
    bound_bottom
  )

RPostgreSQL::dbWriteTable(
  con,
  c("species_misclass", "tbl_detections_reviewed"),
  import,
  append = TRUE,
  row.names = FALSE
)
# Add original assignments to compiled species misclass data -----------------
original <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT processed_detection_id, detection_type as original_detection_type, species_confidence as original_species_confidence, 
  age_class as original_age_class, age_class_confidence as original_age_class_confidence 
  FROM surv_ice_seals_2025.tbl_detections_processed_rgb"
)
data <- data %>%
  inner_join(
    original,
    by = "processed_detection_id"
  )

sanity_check <- data %>%
  select(
    reviewer,
    processed_detection_id,
    detection_type,
    original_detection_type,
    species_confidence,
    original_species_confidence,
    age_class,
    original_age_class,
    age_class_confidence,
    original_age_class_confidence
  )

# Format data for species_misclass analysis -------------------------

altitude <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT image_name, ins_altitude FROM surv_ice_seals_2025.geo_images_meta INNER JOIN surv_ice_seals_2025.tbl_images USING (image_group)"
)

data_coded <- data %>%
  mutate(image_name = basename(image_name)) %>%
  left_join(altitude, by = "image_name") %>%
  mutate(alt_ft = round(ins_altitude * 3.28084, 2)) %>%
  mutate(image_number = as.integer(factor(image_name))) %>%
  mutate(hotspot_number = as.integer(factor(processed_detection_id))) %>%
  mutate(
    obs_id = ifelse(
      data$reviewer == "clc",
      1,
      ifelse(
        data$reviewer == "elr",
        2,
        ifelse(
          data$reviewer == "hlz",
          3,
          ifelse(
            data$reviewer == "smw",
            4,
            ifelse(
              data$reviewer == "jli",
              5,
              ifelse(
                data$reviewer == "spd",
                6,
                ifelse(data$reviewer == "gmb", 7, 0)
              )
            )
          )
        )
      )
    ),
    sp_id_conf = "x",
    age_class_conf = "x",
    original_sp_id_conf = "x",
    original_age_class_conf = "x",
    side = ifelse(
      grepl("_C_", image_name),
      1,
      ifelse(grepl("_L_", image_name), 2, 3)
    )
  ) %>%
  mutate(
    sp_id_conf = ifelse(
      detection_type == "ringed_seal",
      ifelse(
        species_confidence == "positive",
        "rd1",
        ifelse(
          species_confidence == "likely",
          "rd2",
          ifelse(species_confidence == "guess", "rd3", "rd?")
        )
      ),
      sp_id_conf
    )
  ) %>%
  mutate(
    sp_id_conf = ifelse(
      detection_type == "bearded_seal",
      ifelse(
        species_confidence == "positive",
        "bd1",
        ifelse(
          species_confidence == "likely",
          "bd2",
          ifelse(species_confidence == "guess", "bd3", "bd?")
        )
      ),
      sp_id_conf
    )
  ) %>%
  mutate(
    sp_id_conf = ifelse(
      detection_type == "spotted_seal",
      ifelse(
        species_confidence == "positive",
        "sd1",
        ifelse(
          species_confidence == "likely",
          "sd2",
          ifelse(species_confidence == "guess", "sd3", "sd?")
        )
      ),
      sp_id_conf
    )
  ) %>%
  mutate(
    sp_id_conf = ifelse(
      detection_type == "ribbon_seal",
      ifelse(
        species_confidence == "positive",
        "rn1",
        ifelse(
          species_confidence == "likely",
          "rn2",
          ifelse(species_confidence == "guess", "rn3", "rn?")
        )
      ),
      sp_id_conf
    )
  ) %>%
  mutate(
    sp_id_conf = ifelse(detection_type == "unknown_seal", "unk", sp_id_conf)
  ) %>%
  mutate(
    age_class_conf = ifelse(
      age_class == "nonpup",
      ifelse(
        age_class_confidence == "positive",
        "a1",
        ifelse(
          age_class_confidence == "likely",
          "a2",
          ifelse(age_class_confidence == "guess", "a3", "a?")
        )
      ),
      age_class_conf
    )
  ) %>%
  mutate(
    age_class_conf = ifelse(
      age_class == "pup",
      ifelse(
        age_class_confidence == "positive",
        "p1",
        ifelse(
          age_class_confidence == "likely",
          "p2",
          ifelse(age_class_confidence == "guess", "p3", "p?")
        )
      ),
      age_class_conf
    )
  ) %>%
  mutate(
    original_sp_id_conf = ifelse(
      original_detection_type == "ringed_seal",
      ifelse(
        original_species_confidence == "positive",
        "rd1",
        ifelse(
          original_species_confidence == "likely",
          "rd2",
          ifelse(original_species_confidence == "guess", "rd3", "rd?")
        )
      ),
      original_sp_id_conf
    )
  ) %>%
  mutate(
    original_sp_id_conf = ifelse(
      original_detection_type == "bearded_seal",
      ifelse(
        original_species_confidence == "positive",
        "bd1",
        ifelse(
          original_species_confidence == "likely",
          "bd2",
          ifelse(original_species_confidence == "guess", "bd3", "bd?")
        )
      ),
      original_sp_id_conf
    )
  ) %>%
  mutate(
    original_sp_id_conf = ifelse(
      original_detection_type == "spotted_seal",
      ifelse(
        original_species_confidence == "positive",
        "sd1",
        ifelse(
          original_species_confidence == "likely",
          "sd2",
          ifelse(original_species_confidence == "guess", "sd3", "sd?")
        )
      ),
      original_sp_id_conf
    )
  ) %>%
  mutate(
    original_sp_id_conf = ifelse(
      original_detection_type == "ribbon_seal",
      ifelse(
        original_species_confidence == "positive",
        "rn1",
        ifelse(
          original_species_confidence == "likely",
          "rn2",
          ifelse(original_species_confidence == "guess", "rn3", "rn?")
        )
      ),
      original_sp_id_conf
    )
  ) %>%
  mutate(
    original_sp_id_conf = ifelse(
      original_detection_type == "unknown_seal",
      "unk",
      original_sp_id_conf
    )
  ) %>%
  mutate(
    original_age_class_conf = ifelse(
      original_age_class == "nonpup",
      ifelse(
        original_age_class_confidence == "positive",
        "a1",
        ifelse(
          original_age_class_confidence == "likely",
          "a2",
          ifelse(original_age_class_confidence == "guess", "a3", "a?")
        )
      ),
      original_age_class_conf
    )
  ) %>%
  mutate(
    original_age_class_conf = ifelse(
      original_age_class == "pup",
      ifelse(
        original_age_class_confidence == "positive",
        "p1",
        ifelse(
          original_age_class_confidence == "likely",
          "p2",
          ifelse(original_age_class_confidence == "guess", "p3", "p?")
        )
      ),
      original_age_class_conf
    )
  ) %>%
  mutate(
    original_age_class_conf = ifelse(
      is.na(original_age_class),
      "unk",
      original_age_class_conf
    )
  ) %>%
  select(
    processed_detection_id,
    hotspot_number,
    image_name,
    image_number,
    obs_id,
    side,
    alt_ft,
    sp_id_conf,
    age_class_conf,
    original_sp_id_conf,
    original_age_class_conf
  )


codes <- rbind(
  (data_coded %>%
    select(hotspot_number, processed_detection_id) %>%
    unique() %>%
    mutate(type = "detection_id") %>%
    rename(description = processed_detection_id, id = hotspot_number) %>%
    select(type, id, description)),
  (data_coded %>%
    select(image_name, image_number) %>%
    unique() %>%
    mutate(type = "image_number") %>%
    rename(description = image_name, id = image_number) %>%
    select(type, id, description))
)


codes <- rbind(codes, c("obs_id", "1", "CLC"))
codes <- rbind(codes, c("obs_id", "2", "ELR"))
codes <- rbind(codes, c("obs_id", "3", "HLZ"))
codes <- rbind(codes, c("obs_id", "4", "SMW"))
codes <- rbind(codes, c("obs_id", "5", "JLi"))
codes <- rbind(codes, c("obs_id", "6", "SPD"))
codes <- rbind(codes, c("obs_id", "7", "GMB"))

codes <- rbind(codes, c("side", "1", "Center"))
codes <- rbind(codes, c("side", "2", "Left"))
codes <- rbind(codes, c("side", "3", "Right"))

codes <- rbind(codes, c("sp_id_conf", "rd1", "Ringed, Positive"))
codes <- rbind(codes, c("sp_id_conf", "rd2", "Ringed, Likely"))
codes <- rbind(codes, c("sp_id_conf", "rd3", "Ringed, Guess"))

codes <- rbind(codes, c("sp_id_conf", "bd1", "Bearded, Positive"))
codes <- rbind(codes, c("sp_id_conf", "bd2", "Bearded, Likely"))
codes <- rbind(codes, c("sp_id_conf", "bd3", "Bearded, Guess"))

codes <- rbind(codes, c("sp_id_conf", "unk", "Unknown Seal"))

codes <- rbind(codes, c("age_class_conf", "a1", "Non-Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "a2", "Non-Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "a3", "Non-Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "p1", "Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "p2", "Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "p3", "Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "unk", "Unknown Seal"))


# Export data
export <- data_coded %>%
  select(
    hotspot_number,
    image_number,
    obs_id,
    side,
    alt_ft,
    sp_id_conf,
    age_class_conf,
    original_sp_id_conf,
    original_age_class_conf
  )

write.csv(
  export,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass/IceSeals2025_CompiledSpeciesID_20260729.csv",
  row.names = FALSE
)
write.csv(
  codes,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass/IceSeals2025_CompiledSpeciesID_CodeKey_20260729.csv",
  row.names = FALSE
)

# Disconnect and clean up
RPostgreSQL::dbDisconnect(con)
rm(con, data)
