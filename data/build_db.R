# =============================================================================
# build_db.R — build and seed institute.sqlite
#
# The running database for the "SQL, step by step" app: a fictional research
# institute, The Institute for Highly Questionable Research, whose labs run the
# absurd studies students met in the PSY 302 app (seagull staring, peas in ears,
# Daves knowing Daves) plus a few new ones.
#
# Schema (7 tables):
#   labs           1───∞ researchers
#   labs           1───∞ studies
#   researchers    1───∞ studies            (as lead_researcher)
#   studies        ∞───∞ participants       (via enrollments)
#   enrollments    1───∞ measurements
#
# Design notes for teaching:
#   - researchers.lab_id has NULLs (a visiting fellow with no lab) → LEFT JOIN
#   - researchers.salary has NULLs (undisclosed)                   → NULL logic
#   - labs.building has a NULL (a lab between buildings)           → IS NULL
#   - participants.age has NULLs                                   → NULL logic
#   - many participants are nicknamed "Dave"                       → DISTINCT/COUNT
#   - measurements give repeated trials per enrollment             → window funcs
#
# Run from the project root:  Rscript data/build_db.R
# Produces: data/institute.sqlite
# =============================================================================

suppressMessages({
  library(DBI)
  library(RSQLite)
})

db_path <- file.path("data", "institute.sqlite")
if (file.exists(db_path)) unlink(db_path)
con <- dbConnect(RSQLite::SQLite(), db_path)
dbExecute(con, "PRAGMA foreign_keys = ON;")

# ---- Schema -----------------------------------------------------------------
dbExecute(con, "
CREATE TABLE labs (
  lab_id       INTEGER PRIMARY KEY,
  lab_name     TEXT    NOT NULL,
  building     TEXT,                 -- NULL allowed: a lab between buildings
  founded_year INTEGER NOT NULL
);")

dbExecute(con, "
CREATE TABLE researchers (
  researcher_id INTEGER PRIMARY KEY,
  full_name     TEXT    NOT NULL,
  lab_id        INTEGER,             -- NULL allowed: unaffiliated visiting fellow
  title         TEXT    NOT NULL,    -- 'PI','Postdoc','Grad Student','RA'
  salary        INTEGER,             -- NULL allowed: undisclosed
  start_year    INTEGER NOT NULL,
  FOREIGN KEY (lab_id) REFERENCES labs(lab_id)
);")

dbExecute(con, "
CREATE TABLE studies (
  study_id          INTEGER PRIMARY KEY,
  title             TEXT    NOT NULL,
  lab_id            INTEGER NOT NULL,
  lead_researcher_id INTEGER,
  year              INTEGER NOT NULL,
  status            TEXT    NOT NULL,  -- 'completed','ongoing','abandoned'
  sample_size       INTEGER NOT NULL,
  FOREIGN KEY (lab_id) REFERENCES labs(lab_id),
  FOREIGN KEY (lead_researcher_id) REFERENCES researchers(researcher_id)
);")

dbExecute(con, "
CREATE TABLE participants (
  participant_id INTEGER PRIMARY KEY,
  nickname       TEXT    NOT NULL,
  species        TEXT    NOT NULL,    -- 'human','seagull','cricket'
  age            INTEGER              -- NULL allowed
);")

dbExecute(con, "
CREATE TABLE enrollments (
  enrollment_id  INTEGER PRIMARY KEY,
  study_id       INTEGER NOT NULL,
  participant_id INTEGER NOT NULL,
  dropped_out    INTEGER NOT NULL DEFAULT 0,  -- 0/1
  FOREIGN KEY (study_id) REFERENCES studies(study_id),
  FOREIGN KEY (participant_id) REFERENCES participants(participant_id)
);")

dbExecute(con, "
CREATE TABLE measurements (
  measurement_id INTEGER PRIMARY KEY,
  enrollment_id  INTEGER NOT NULL,
  trial          INTEGER NOT NULL,
  value          REAL    NOT NULL,
  FOREIGN KEY (enrollment_id) REFERENCES enrollments(enrollment_id)
);")

# ---- Seed: labs -------------------------------------------------------------
labs <- data.frame(
  lab_id       = 1:6,
  lab_name     = c("Avian Intimidation Lab",
                   "Pediatric Otolaryngology Annex",
                   "Onomastic Density Group",
                   "Climate Affect Collective",
                   "Cinematic Dread Unit",
                   "Bioacoustics & Thermometry Lab"),
  building     = c("Wexler Hall", "Wexler Hall", "Pickering Building",
                   NA,  # between buildings
                   "Pickering Building", "Greenhouse Annex"),
  founded_year = c(1998, 1985, 2009, 2015, 2012, 1979),
  stringsAsFactors = FALSE
)
dbWriteTable(con, "labs", labs, append = TRUE)

# ---- Seed: researchers ------------------------------------------------------
# salary NULL = undisclosed; lab_id NULL = visiting fellow (researcher 12)
researchers <- data.frame(
  researcher_id = 1:12,
  full_name = c("Dr. Mona Albatross", "Wesley Tern", "Dr. Priya Eustachian",
                "Dr. Dave Davidson", "Dave Daveston", "Dr. Linnea Frost",
                "Dr. Cassius Gloom", "Bram Shudder", "Dr. Cricket Dolbear",
                "Esme Chirp", "Dr. Otto Kelvin", "Dr. Wanda Roams"),
  lab_id    = c(1, 1, 2, 3, 3, 4, 5, 5, 6, 6, 6, NA),
  title     = c("PI", "Grad Student", "PI", "PI", "Postdoc", "PI",
                "PI", "RA", "PI", "Grad Student", "Postdoc", "PI"),
  salary    = c(118000, 31000, 121000, 109000, 58000, 115000,
                104000, NA, 112000, 30000, NA, 99000),
  start_year= c(2001, 2019, 1990, 2010, 2017, 2016, 2013, 2021, 1995, 2020, 2014, 2008),
  stringsAsFactors = FALSE
)
dbWriteTable(con, "researchers", researchers, append = TRUE)

# ---- Seed: studies ----------------------------------------------------------
studies <- data.frame(
  study_id = 1:10,
  title = c("Do seagulls back off when stared at?",
            "Peas in babies' ears: a detection study",
            "Do Daves know more Daves than non-Daves?",
            "Climate concern across religious groups",
            "Ranking the Science of Scare horror films",
            "Cricket chirps predict temperature (Dolbear)",
            "Staring contests with herring gulls, replication",
            "Does naming a pet Dave increase recall?",
            "Dread habituation across repeated viewings",
            "Abandoned: telepathy in lab crickets"),
  lab_id = c(1, 2, 3, 4, 5, 6, 1, 3, 5, 6),
  lead_researcher_id = c(1, 3, 4, 6, 7, 9, 2, 5, 7, 9),
  year   = c(2019, 2017, 2018, 2021, 2022, 2016, 2023, 2020, 2022, 2014),
  status = c("completed", "completed", "completed", "completed", "completed",
             "completed", "ongoing", "ongoing", "ongoing", "abandoned"),
  sample_size = c(40, 24, 60, 180, 50, 32, 40, 45, 50, 8),
  stringsAsFactors = FALSE
)
dbWriteTable(con, "studies", studies, append = TRUE)

# ---- Seed: participants -----------------------------------------------------
# Lots of Daves; some seagulls and crickets; some NULL ages.
set.seed(302)
human_names <- c("Dave", "Dave", "Dave", "Dave", "Dave", "Dave",
                 "Marf", "Pim", "Ouida", "Bex", "Hollis", "Tam",
                 "Dave", "Dave", "Greer", "Lula", "Niamh", "Cor")
seagulls <- c("Steven Seagull", "Jonathan", "The Admiral")
crickets <- c("Chirpy", "Lord Stridulation", "Pip")
nicks   <- c(human_names, seagulls, crickets)
species <- c(rep("human", length(human_names)),
             rep("seagull", length(seagulls)),
             rep("cricket", length(crickets)))
ages <- c(34, 41, 29, 52, 23, 38, 45, 31, NA, 27, 60, 19,
          22, 48, NA, 36, 25, 33,        # humans (two NULL)
          NA, NA, NA,                    # seagulls: age unknown
          1, 1, NA)                      # crickets
participants <- data.frame(
  participant_id = seq_along(nicks),
  nickname = nicks,
  species  = species,
  age      = ages,
  stringsAsFactors = FALSE
)
dbWriteTable(con, "participants", participants, append = TRUE)

# ---- Seed: enrollments & measurements (frozen) ------------------------------
# These two tables were generated once with a fixed seed and frozen to CSV in
# data/frozen/, so this R builder and the reference build produce a byte-for-
# byte identical database (the app's expected-answer keys depend on the exact
# rows). To regenerate from scratch, see the project's build notes.
enr  <- read.csv(file.path("data", "frozen", "enrollments.csv"),  stringsAsFactors = FALSE)
meas <- read.csv(file.path("data", "frozen", "measurements.csv"), stringsAsFactors = FALSE)
dbWriteTable(con, "enrollments",  enr,  append = TRUE)
dbWriteTable(con, "measurements", meas, append = TRUE)

# ---- Helpful indexes (also referenced in the Tab 10 indexing aside) ---------
dbExecute(con, "CREATE INDEX idx_studies_lab     ON studies(lab_id);")
dbExecute(con, "CREATE INDEX idx_research_lab     ON researchers(lab_id);")
dbExecute(con, "CREATE INDEX idx_enroll_study     ON enrollments(study_id);")
dbExecute(con, "CREATE INDEX idx_meas_enrollment  ON measurements(enrollment_id);")

# ---- Report -----------------------------------------------------------------
for (t in c("labs","researchers","studies","participants","enrollments","measurements")) {
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", t))$n
  message(sprintf("  %-14s %4d rows", t, n))
}
dbDisconnect(con)
message("\n[OK] data/institute.sqlite built.")
