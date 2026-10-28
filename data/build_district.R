# =============================================================================
# build_district.R — build data/district.sqlite from the frozen CSVs
#
# The second running database, for the School district tabs: Prairie Lakes
# Public Schools, a FICTIONAL Minnesota-style district (9 schools, five school
# years, "today" = 2026-03-31). Its tables follow the shape of a real student
# information system (SIS):
#
#   schools ─┬─ enrollments ─── students ─┬─ programs
#            ├─ calendar_days             ├─ attendance (current year, exceptions only)
#            ├─ sections ─── roster ──────┤  attendance_year_summary (prior years)
#            │      └── courses ─ transcripts
#            └─ incidents ─ incident_students, resolutions
#   test_results, oct1_snapshot (frozen), state_oct1_return (the state's file)
#   lookups: grade_levels, status_codes, attendance_codes, resolution_codes
#   staff, user_access (who may see which school)
#
# Teaching notes:
#   - Dates are ISO text ('YYYY-MM-DD'), SQLite's convention; they sort and
#     compare correctly as text. end_date NULL = still enrolled.
#   - enrollments.grade is text ('KG', '01' .. '12'), as in most SIS. It has NO
#     foreign key to grade_levels, deliberately: one row holds an invalid '9'.
#   - attendance stores only exceptions. No row means present.
#   - Planted data-quality problems are listed in district_frozen/PLANTED_PROBLEMS.txt.
#
# Run from the project root:  Rscript data/build_district.R
# (data/generate_district.R regenerates the CSVs; only needed to change the data.)
# =============================================================================

suppressMessages({ library(DBI); library(RSQLite) })

SRC <- file.path("data", "district_frozen")
db_path <- file.path("data", "district.sqlite")
if (file.exists(db_path)) unlink(db_path)
con <- dbConnect(RSQLite::SQLite(), db_path)
rd <- function(name) read.csv(file.path(SRC, paste0(name, ".csv")), stringsAsFactors = FALSE,
                              na.strings = "", colClasses = "character")
num <- function(df, cols) { for (c in cols) df[[c]] <- as.numeric(df[[c]]); df }
int <- function(df, cols) { for (c in cols) df[[c]] <- as.integer(df[[c]]); df }

schema <- c("
CREATE TABLE schools (
  school_id           INTEGER PRIMARY KEY,
  school_name         TEXT    NOT NULL,
  school_type         TEXT    NOT NULL,   -- 'Elementary','Middle','High','Alternative'
  grade_low           TEXT    NOT NULL,
  grade_high          TEXT    NOT NULL,
  state_school_number TEXT    NOT NULL UNIQUE,
  opened_year         INTEGER
);", "
CREATE TABLE grade_levels (
  grade       TEXT    PRIMARY KEY,        -- 'KG', '01' .. '12'
  grade_order INTEGER NOT NULL UNIQUE,    -- 0 .. 12, for sorting
  grade_band  TEXT    NOT NULL            -- 'Elementary','Middle','High'
);", "
CREATE TABLE status_codes (
  code                TEXT    NOT NULL,
  code_type           TEXT    NOT NULL CHECK (code_type IN ('start', 'end')),
  description         TEXT    NOT NULL,
  leaves_district     INTEGER NOT NULL DEFAULT 0,
  documented_transfer INTEGER NOT NULL DEFAULT 0,  -- removes a student from a graduation cohort
  PRIMARY KEY (code, code_type)
);", "
CREATE TABLE attendance_codes (
  code_type   TEXT NOT NULL CHECK (code_type IN ('status', 'excuse')),
  code        TEXT NOT NULL,
  description TEXT NOT NULL,
  PRIMARY KEY (code_type, code)
);", "
CREATE TABLE resolution_codes (
  code         TEXT PRIMARY KEY,
  description  TEXT NOT NULL,
  removal_type TEXT NOT NULL              -- 'Out of school','In school','None'
);", "
CREATE TABLE students (
  student_id            INTEGER PRIMARY KEY,   -- local SIS number
  state_id              TEXT    UNIQUE,         -- 10-digit state number; NULL until assigned
  last_name             TEXT    NOT NULL,
  first_name            TEXT    NOT NULL,
  birth_date            TEXT    NOT NULL,
  gender                TEXT    NOT NULL CHECK (gender IN ('F', 'M', 'X')),
  hispanic_latino       INTEGER NOT NULL,
  race_american_indian  INTEGER NOT NULL,
  race_asian            INTEGER NOT NULL,
  race_black            INTEGER NOT NULL,
  race_pacific_islander INTEGER NOT NULL,
  race_white            INTEGER NOT NULL,
  home_language         TEXT    NOT NULL,
  entered_9th_year      INTEGER                 -- school year of first 9th-grade enrollment anywhere
);", "
CREATE TABLE enrollments (
  enrollment_id INTEGER PRIMARY KEY,
  student_id    INTEGER NOT NULL,
  school_id     INTEGER NOT NULL,
  school_year   INTEGER NOT NULL,   -- named by its spring: 2026 = 2025-26
  grade         TEXT    NOT NULL,
  start_date    TEXT    NOT NULL,
  end_date      TEXT,               -- NULL = still enrolled; otherwise the last day enrolled
  start_status  TEXT,
  end_status    TEXT,
  service_type  TEXT    NOT NULL CHECK (service_type IN ('P', 'S')),  -- primary, secondary
  FOREIGN KEY (student_id) REFERENCES students(student_id),
  FOREIGN KEY (school_id)  REFERENCES schools(school_id)
);", "
CREATE TABLE calendar_days (
  school_id     INTEGER NOT NULL,
  cal_date      TEXT    NOT NULL,
  school_year   INTEGER NOT NULL,
  instructional INTEGER NOT NULL,   -- 1 = a school day
  day_note      TEXT,
  PRIMARY KEY (school_id, cal_date),
  FOREIGN KEY (school_id) REFERENCES schools(school_id)
);", "
CREATE TABLE attendance (
  attendance_id INTEGER PRIMARY KEY,
  student_id    INTEGER NOT NULL,
  school_id     INTEGER NOT NULL,
  att_date      TEXT    NOT NULL,
  status        TEXT    NOT NULL,   -- 'A' absent all day, 'H' absent half the day, 'T' tardy
  excuse        TEXT    NOT NULL,   -- 'E' excused, 'U' unexcused, 'X' exempt (school activity), 'S' suspended
  FOREIGN KEY (student_id) REFERENCES students(student_id),
  FOREIGN KEY (school_id)  REFERENCES schools(school_id)
);", "
CREATE TABLE attendance_year_summary (
  school_year     INTEGER NOT NULL,
  student_id      INTEGER NOT NULL,
  school_id       INTEGER NOT NULL,
  days_membership INTEGER NOT NULL,
  days_absent     REAL    NOT NULL,
  PRIMARY KEY (school_year, student_id, school_id),
  FOREIGN KEY (student_id) REFERENCES students(student_id)
);", "
CREATE TABLE programs (
  program_id INTEGER PRIMARY KEY,
  student_id INTEGER NOT NULL,
  program    TEXT    NOT NULL,      -- 'EL','SPED','504','FRL','McKV','GT'
  start_date TEXT    NOT NULL,
  end_date   TEXT,
  FOREIGN KEY (student_id) REFERENCES students(student_id)
);", "
CREATE TABLE incidents (
  incident_id   INTEGER PRIMARY KEY,
  school_id     INTEGER NOT NULL,
  incident_date TEXT    NOT NULL,
  incident_type TEXT    NOT NULL,
  location      TEXT,
  FOREIGN KEY (school_id) REFERENCES schools(school_id)
);", "
CREATE TABLE incident_students (
  incident_id INTEGER NOT NULL,
  student_id  INTEGER NOT NULL,
  role        TEXT    NOT NULL,     -- 'Offender','Victim'
  PRIMARY KEY (incident_id, student_id),
  FOREIGN KEY (incident_id) REFERENCES incidents(incident_id),
  FOREIGN KEY (student_id)  REFERENCES students(student_id)
);", "
CREATE TABLE resolutions (
  resolution_id   INTEGER PRIMARY KEY,
  incident_id     INTEGER NOT NULL,
  student_id      INTEGER NOT NULL,
  resolution_code TEXT    NOT NULL,
  start_date      TEXT    NOT NULL,
  days_removed    INTEGER NOT NULL DEFAULT 0,
  FOREIGN KEY (incident_id)     REFERENCES incidents(incident_id),
  FOREIGN KEY (student_id)      REFERENCES students(student_id),
  FOREIGN KEY (resolution_code) REFERENCES resolution_codes(code)
);", "
CREATE TABLE courses (
  course_id         TEXT PRIMARY KEY,
  course_name       TEXT NOT NULL,
  subject           TEXT NOT NULL,
  state_course_code TEXT NOT NULL,   -- fictional codes in the style of a state course catalog
  credits_per_term  REAL,
  course_level      TEXT NOT NULL
);", "
CREATE TABLE staff (
  staff_id     INTEGER PRIMARY KEY,
  last_name    TEXT    NOT NULL,
  first_name   TEXT    NOT NULL,
  school_id    INTEGER NOT NULL,
  role         TEXT    NOT NULL,
  subject_area TEXT,
  FOREIGN KEY (school_id) REFERENCES schools(school_id)
);", "
CREATE TABLE sections (
  section_id  INTEGER PRIMARY KEY,
  course_id   TEXT    NOT NULL,
  school_id   INTEGER NOT NULL,
  school_year INTEGER NOT NULL,
  term        TEXT    NOT NULL,     -- 'S1','S2'
  period      INTEGER NOT NULL,     -- 0 = advisory
  teacher_id  INTEGER,
  FOREIGN KEY (course_id)  REFERENCES courses(course_id),
  FOREIGN KEY (school_id)  REFERENCES schools(school_id),
  FOREIGN KEY (teacher_id) REFERENCES staff(staff_id)
);", "
CREATE TABLE roster (
  roster_id  INTEGER PRIMARY KEY,
  section_id INTEGER NOT NULL,
  student_id INTEGER NOT NULL,
  start_date TEXT    NOT NULL,
  end_date   TEXT    NOT NULL,
  FOREIGN KEY (section_id) REFERENCES sections(section_id),
  FOREIGN KEY (student_id) REFERENCES students(student_id)
);", "
CREATE TABLE transcripts (
  transcript_id     INTEGER PRIMARY KEY,
  student_id        INTEGER NOT NULL,
  school_year       INTEGER NOT NULL,
  term              TEXT    NOT NULL,   -- 'S1','S2', or 'YR' for converted and transfer credit
  school_id         INTEGER,            -- NULL for credit earned elsewhere
  course_id         TEXT    NOT NULL,
  grade_level       TEXT,
  mark              TEXT    NOT NULL,   -- A-F, P (pass), W (withdrew)
  credits_attempted REAL    NOT NULL,
  credits_earned    REAL    NOT NULL,
  FOREIGN KEY (student_id) REFERENCES students(student_id),
  FOREIGN KEY (course_id)  REFERENCES courses(course_id)
);", "
CREATE TABLE test_results (
  test_id           INTEGER PRIMARY KEY,
  student_id        INTEGER NOT NULL,
  school_year       INTEGER NOT NULL,
  test_date         TEXT    NOT NULL,
  subject           TEXT    NOT NULL,   -- 'Reading','Mathematics'
  grade_tested      TEXT    NOT NULL,
  school_id         INTEGER NOT NULL,
  test_status       TEXT    NOT NULL,   -- 'Tested','Refused','Absent','Medically exempt'
  scale_score       INTEGER,            -- NULL unless tested
  achievement_level TEXT,               -- NULL unless tested
  FOREIGN KEY (student_id) REFERENCES students(student_id)
);", "
CREATE TABLE oct1_snapshot (
  school_year   INTEGER NOT NULL,
  snapshot_date TEXT    NOT NULL,
  student_id    INTEGER NOT NULL,
  school_id     INTEGER NOT NULL,
  grade         TEXT    NOT NULL,
  el            INTEGER NOT NULL,
  sped          INTEGER NOT NULL,
  frl           INTEGER NOT NULL,
  homeless      INTEGER NOT NULL,
  PRIMARY KEY (school_year, student_id)
);", "
CREATE TABLE state_oct1_return (
  school_year         INTEGER NOT NULL,
  snapshot_date       TEXT    NOT NULL,
  state_id            TEXT    NOT NULL,
  state_school_number TEXT    NOT NULL,
  grade               TEXT    NOT NULL,
  accepted            INTEGER NOT NULL
);", "
CREATE TABLE user_access (
  username  TEXT PRIMARY KEY,
  scope     TEXT NOT NULL CHECK (scope IN ('District', 'School')),
  school_id INTEGER,                 -- NULL when scope = 'District'
  role      TEXT NOT NULL
);")
for (s in schema) dbExecute(con, s)

ins <- function(tbl, df) dbAppendTable(con, tbl, df)

ins("schools", int(rd("schools"), c("school_id", "opened_year")))
ins("grade_levels", data.frame(grade = c("KG", sprintf("%02d", 1:12)), grade_order = 0:12,
    grade_band = c(rep("Elementary", 6), rep("Middle", 3), rep("High", 4))))
ins("status_codes", data.frame(
  code = c("CONT", "NEW", "TRNI", "OSIN", "INTR", "RTN",
           "EOY", "GRAD", "INTR", "TRMN", "TROS", "EMIG", "DROP", "UNKN", "NOSH"),
  code_type = c(rep("start", 6), rep("end", 9)),
  description = c("Continuing from the prior school year", "First enrollment in a Minnesota public school",
    "Transfer in from another Minnesota district", "Transfer in from out of state or out of country",
    "Transfer from another school in this district", "Returning after a withdrawal",
    "End of school year; expected to continue", "Graduated with a regular diploma",
    "Transfer to another school in this district", "Transfer to another Minnesota district; records requested",
    "Moved out of state; records requested", "Moved out of the country", "Dropped out",
    "Withdrew; whereabouts unknown", "No-show; never attended"),
  leaves_district = c(rep(0, 6), 0, 1, 0, 1, 1, 1, 1, 1, 1),
  documented_transfer = c(rep(0, 6), 0, 0, 0, 1, 1, 1, 0, 0, 0)))
ins("attendance_codes", data.frame(
  code_type = c("status", "status", "status", "excuse", "excuse", "excuse", "excuse"),
  code = c("A", "H", "T", "E", "U", "X", "S"),
  description = c("Absent the whole day", "Absent half the day", "Tardy (arrived late; counts as present)",
    "Excused absence", "Unexcused absence", "Exempt: school-sponsored activity (counts as present)",
    "Out-of-school suspension or expulsion")))
ins("resolution_codes", data.frame(
  code = c("OSS", "ISS", "EXP", "DET", "CONF"),
  description = c("Out-of-school suspension", "In-school suspension", "Expulsion", "Detention",
                  "Restorative conference"),
  removal_type = c("Out of school", "In school", "Out of school", "None", "None")))

stu <- int(rd("students"), c("student_id", "hispanic_latino", "race_american_indian", "race_asian",
  "race_black", "race_pacific_islander", "race_white", "entered_9th_year"))
ins("students", stu[order(stu$student_id), ])

enr <- int(rd("enrollments"), c("student_id", "school_id", "school_year"))
enr <- enr[order(enr$school_year, enr$school_id, enr$student_id, enr$start_date), ]
enr$enrollment_id <- seq_len(nrow(enr))
ins("enrollments", enr[, c("enrollment_id", "student_id", "school_id", "school_year", "grade",
  "start_date", "end_date", "start_status", "end_status", "service_type")])

cal <- int(rd("calendar_days"), c("school_id", "school_year", "instructional"))
ins("calendar_days", cal[, c("school_id", "cal_date", "school_year", "instructional", "day_note")])

att <- int(rd("attendance"), c("student_id", "school_id"))
att <- att[order(att$att_date, att$school_id, att$student_id), ]
att$attendance_id <- seq_len(nrow(att))
ins("attendance", att[, c("attendance_id", "student_id", "school_id", "att_date", "status", "excuse")])

ays <- num(int(rd("attendance_year_summary"), c("school_year", "student_id", "school_id", "days_membership")), "days_absent")
ays <- ays[order(ays$school_year, ays$student_id, ays$school_id), ]
ays <- ays[!duplicated(ays[, c("school_year", "student_id", "school_id")]), ]
ins("attendance_year_summary", ays)

pr <- int(rd("programs"), "student_id")
pr <- pr[order(pr$student_id, pr$program, pr$start_date), ]
pr$program_id <- seq_len(nrow(pr))
ins("programs", pr[, c("program_id", "student_id", "program", "start_date", "end_date")])

ins("incidents", int(rd("incidents"), c("incident_id", "school_id")))
ins("incident_students", int(rd("incident_students"), c("incident_id", "student_id")))
ins("resolutions", int(rd("resolutions"), c("resolution_id", "incident_id", "student_id", "days_removed")))
ins("courses", num(rd("courses"), "credits_per_term"))
ins("staff", int(rd("staff"), c("staff_id", "school_id")))
ins("sections", int(rd("sections"), c("section_id", "school_id", "school_year", "period", "teacher_id")))
ro <- int(rd("roster"), c("section_id", "student_id"))
ro <- ro[order(ro$section_id, ro$student_id), ]
ro$roster_id <- seq_len(nrow(ro))
ins("roster", ro[, c("roster_id", "section_id", "student_id", "start_date", "end_date")])
tr <- num(int(rd("transcripts"), c("student_id", "school_year", "school_id")), c("credits_attempted", "credits_earned"))
tr <- tr[order(tr$student_id, tr$school_year, tr$term, tr$course_id), ]
tr$transcript_id <- seq_len(nrow(tr))
ins("transcripts", tr[, c("transcript_id", "student_id", "school_year", "term", "school_id", "course_id",
  "grade_level", "mark", "credits_attempted", "credits_earned")])
te <- int(rd("test_results"), c("student_id", "school_year", "school_id", "scale_score"))
te <- te[order(te$school_year, te$subject, te$student_id), ]
te$test_id <- seq_len(nrow(te))
ins("test_results", te[, c("test_id", "student_id", "school_year", "test_date", "subject", "grade_tested",
  "school_id", "test_status", "scale_score", "achievement_level")])
ins("oct1_snapshot", int(rd("oct1_snapshot"), c("school_year", "student_id", "school_id", "el", "sped", "frl", "homeless")))
ins("state_oct1_return", int(rd("state_oct1_return"), c("school_year", "accepted")))
ins("user_access", int(rd("user_access"), "school_id"))

idx <- c(
  "CREATE INDEX idx_enr_student    ON enrollments (student_id);",
  "CREATE INDEX idx_enr_year       ON enrollments (school_year, school_id);",
  "CREATE INDEX idx_att_student    ON attendance (student_id, att_date);",
  "CREATE INDEX idx_att_school     ON attendance (school_id, att_date);",
  "CREATE INDEX idx_prog_student   ON programs (student_id, program);",
  "CREATE INDEX idx_trans_student  ON transcripts (student_id);",
  "CREATE INDEX idx_roster_student ON roster (student_id);",
  "CREATE INDEX idx_res_student    ON resolutions (student_id);")
for (s in idx) dbExecute(con, s)

dbExecute(con, "VACUUM;")
for (t in dbListTables(con)) cat(sprintf("%-24s %6d\n", t, dbGetQuery(con, paste("SELECT COUNT(*) FROM", t))[[1]]))
dbDisconnect(con)
cat("Wrote", db_path, sprintf("(%.1f MB)\n", file.size(db_path) / 1e6))
