# =============================================================================
# generate_district.R — simulate the synthetic school district (dev-only)
#
# Prairie Lakes Public Schools is a FICTIONAL district. Every student, name,
# number, and code here is invented; any resemblance to a real person or school
# is coincidence. The structure, though, is modeled on a real student
# information system (SIS): enrollment spells with start and end dates and
# status codes, an instructional calendar, attendance stored as exceptions,
# behavior events with participants and resolutions, program spells, course
# sections and rosters, transcripts, state-test results, a frozen October 1
# snapshot, and the state's return file for that snapshot.
#
# The simulation runs five school years (2021-22 to 2025-26) and stops "today",
# 2026-03-31, part-way through the current year. Prior years therefore exist
# as frozen, end-of-year data (attendance as a per-student summary); the
# current year has daily detail.
#
# This script WRITES CSVs to data/district_frozen/. data/build_district.R reads
# those CSVs and builds data/district.sqlite. Keeping the random step separate
# means the database rebuilds identically on any R version, which matters
# because the app's prose quotes numbers from it (tests/verify_claims.R checks
# them). Re-run this only if you mean to change the data, and then re-run the
# claims check and fix any sentence it flags.
#
# Deliberate data-quality problems are planted at the end (search "PLANT").
# They exist so the Data quality tab has real problems to find.
#
# Run from the project root:  Rscript data/generate_district.R
# =============================================================================

set.seed(20260331)
options(stringsAsFactors = FALSE)

OUT <- file.path("data", "district_frozen")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

YEARS  <- 2022:2026                      # school year, named by its spring
AS_OF  <- as.Date("2026-03-31")          # "today" in the current year
y_start <- as.Date(c("2021-09-07", "2022-09-06", "2023-09-05", "2024-09-03", "2025-09-02"))
y_end   <- as.Date(c("2022-06-09", "2023-06-08", "2024-06-06", "2025-06-05", "2026-06-04"))

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)
shuffle <- function(x) x[sample.int(length(x))]
pick  <- function(x, n = 1, prob = NULL) x[sample.int(length(x), n, replace = TRUE, prob = prob)]
fmt   <- function(d) format(as.Date(d, origin = "1970-01-01"), "%Y-%m-%d")

# ---- Calendar ---------------------------------------------------------------
nth_wday <- function(y, m, wday, n) {
  d <- seq(as.Date(sprintf("%d-%02d-01", y, m)), by = "day", length.out = 31)
  d <- d[as.integer(format(d, "%m")) == m & as.POSIXlt(d)$wday == wday]
  d[n]
}
build_calendar <- function(k) {
  Y <- YEARS[k]; F <- Y - 1
  days <- seq(y_start[k], y_end[k], by = "day")
  days <- days[!as.POSIXlt(days)$wday %in% c(0, 6)]
  note <- rep(NA_character_, length(days))
  mark <- function(d, label) note[days %in% d] <<- label
  mea <- nth_wday(F, 10, 4, 3); mark(c(mea, mea + 1), "MEA break")
  sd1 <- nth_wday(F, 11, 1, 1); mark(sd1, "Staff development")
  tg  <- nth_wday(F, 11, 4, 4); mark(c(tg - 1, tg, tg + 1), "Thanksgiving break")
  mark(seq(as.Date(sprintf("%d-12-23", F)), as.Date(sprintf("%d-01-01", Y)), by = "day"), "Winter break")
  mark(nth_wday(Y, 1, 1, 3), "MLK Day")
  s1_end <- nth_wday(Y, 1, 5, 4)
  mark(s1_end + 3, "Grading day")
  mark(nth_wday(Y, 2, 1, 3), "Presidents Day")
  sb <- nth_wday(Y, 3, 1, 4); mark(sb + 0:4, "Spring break")
  data.frame(school_year = Y, cal_date = days,
             instructional = as.integer(is.na(note)), day_note = note,
             s1_end = s1_end)
}
CAL <- lapply(seq_along(YEARS), build_calendar)
IDAYS <- lapply(CAL, function(c) c$cal_date[c$instructional == 1])
S1_END   <- vapply(CAL, function(c) c$s1_end[1], numeric(1)); class(S1_END) <- "Date"
S2_START <- S1_END + 4    # Friday + Grading Monday -> Tuesday

# ---- Schools ----------------------------------------------------------------
schools <- data.frame(
  school_id = c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L),
  school_name = c("Birchwood Elementary", "Cedar Park Elementary", "Riverbend Elementary",
                  "Prairie View Elementary", "Linden Middle School", "Oak Grove Middle School",
                  "Lakeview High School", "Riverside High School", "Horizon Area Learning Center"),
  school_type = c(rep("Elementary", 4), rep("Middle", 2), rep("High", 2), "Alternative"),
  grade_low = c(rep("KG", 4), "06", "06", "09", "09", "09"),
  grade_high = c(rep("05", 4), "08", "08", "12", "12", "12"),
  state_school_number = c("010", "020", "030", "040", "050", "060", "070", "080", "090"),
  opened_year = c(1958L, 1964L, 1971L, 2003L, 1966L, 1998L, 1924L, 1969L, 2011L)
)
school_for <- function(grade, zone, alc) {
  ifelse(grade <= 5, c(10L, 20L, 30L, 40L)[zone],
  ifelse(grade <= 8, ifelse(zone <= 2, 50L, 60L),
  ifelse(alc, 90L, ifelse(zone <= 2, 70L, 80L))))
}
level_of <- function(sid) ifelse(sid <= 40, "E", ifelse(sid <= 60, "M", ifelse(sid <= 80, "H", "A")))

# ---- Names ------------------------------------------------------------------
FIRST_F <- c("Amina","Sofia","Olivia","Hodan","Maya","Emma","Ava","Isabella","Nasra","Aaliyah","Mia","Hannah",
  "Paj","Kia","Mai","Lily","Zainab","Fatima","Camila","Valeria","Ximena","Grace","Ruth","Hawa","Ifrah","Sagal",
  "Chloe","Abigail","Nora","Ella","Harper","Zoe","Imani","Destiny","Nevaeh","Jasmine","Ayana","Leila","Yasmin",
  "Lucia","Daniela","Gabriela","Elena","Mariana","Clara","Ingrid","Astrid","Freya","Hana","Naw","Thu","Linh",
  "Anh","Winona","Aiyana","Tala","Sienna","Maren","Kaia","Brielle","Juniper","Ruby","Iris","Selam","Meron","Bethlehem")
FIRST_M <- c("Abdi","Mohamed","Liam","Noah","Mateo","Elijah","James","Ahmed","Yusuf","Hassan","Isaiah","Jayden",
  "Tou","Kou","Pao","Fue","Lucas","Ethan","Oliver","Henry","Santiago","Diego","Miguel","Luis","Carlos","Jose",
  "Omar","Ibrahim","Abdirahman","Hamza","Malik","Jalen","Darius","Andre","Marcus","Xavier","Owen","Gus","Leif",
  "Anders","Soren","Henrik","Minh","Bao","Duc","Eh","Saw","Tah","Kele","Ahanu","Chayton","Dakota","Theo","Felix",
  "Jonah","Caleb","Micah","Silas","Abel","Dawit","Yonas","Biruk","Tesfaye","Wyatt","Rowan","Emmett")
FIRST_X <- c("Avery","River","Sage","Quinn","Rowan","Jordan","Skyler","Kai","Ari","Remy")
LAST <- c("Ali","Hassan","Mohamed","Abdi","Omar","Warsame","Jama","Farah","Yusuf","Ahmed","Nguyen","Tran","Le",
  "Pham","Vang","Xiong","Yang","Lee","Thao","Moua","Her","Lor","Hernandez","Garcia","Martinez","Lopez","Rodriguez",
  "Gonzalez","Ramirez","Torres","Flores","Morales","Johnson","Anderson","Olson","Nelson","Peterson","Larson",
  "Swanson","Lindgren","Hanson","Carlson","Erickson","Johnson","Smith","Williams","Brown","Jones","Davis","Miller",
  "Wilson","Moore","Taylor","Thomas","Jackson","White","Harris","Martin","Thompson","Robinson","Walker","Young",
  "King","Wright","Scott","Green","Baker","Adams","Campbell","Mitchell","Roberts","Carter","Phillips","Evans",
  "Turner","Parker","Collins","Edwards","Stewart","Morris","Murphy","Cook","Rogers","Morgan","Bell","Bailey",
  "Cooper","Howard","Ward","Cox","Richardson","Wood","Watson","Brooks","Bennett","Gray","Hughes","Price","Sanders",
  "Bekele","Tadesse","Girma","Haile","Tesfaye","Kebede","Desta","Wolde","Htoo","Paw","Moo","Kaw","Begay","Morningstar",
  "Fairbanks","Whitebird","Thunder","Bellanger","Goodsky","Fineday","Jourdain","Kingbird","Novak","Kowalski","Schmidt",
  "Mueller","Becker","Hoffman","Sorensen","Jensen","Halvorson","Bergstrom","Dahl","Lund","Nyberg","Okafor","Mensah")

# ---- Student creation -------------------------------------------------------
ZONE_HISP <- c(0.22, 0.20, 0.12, 0.10)
ZONE_RACE <- list(  # amind, asian, black, nhpi, white, multi  (among non-Hispanic)
  c(.040, .070, .450, .005, .220, .055), c(.050, .100, .350, .005, .300, .050),
  c(.020, .050, .180, .003, .620, .060), c(.020, .060, .120, .003, .700, .060))
RACES <- c("amind", "asian", "black", "nhpi", "white")
ZONE_FRL <- c(0.66, 0.56, 0.34, 0.24)

next_id <- 100001L
used_state <- integer(0)
new_state_ids <- function(n) {
  out <- integer(0)
  while (length(out) < n) {
    cand <- sample(1000000:2999999999, n * 2)
    cand <- setdiff(unique(cand), c(used_state, out))
    out <- c(out, head(cand, n - length(out)))
  }
  used_state <<- c(used_state, out)
  sprintf("%010.0f", out)
}

make_students <- function(n, grade, zone, year_k) {
  F <- YEARS[year_k] - 1
  ids <- next_id:(next_id + n - 1L); next_id <<- next_id + n
  gender <- pick(c("F", "M", "X"), n, prob = c(.493, .497, .010))
  first <- ifelse(gender == "F", pick(FIRST_F, n), ifelse(gender == "M", pick(FIRST_M, n), pick(FIRST_X, n)))
  age_sep1 <- grade + 5
  bdate <- as.Date(sprintf("%d-09-01", F - age_sep1)) - sample(0:364, n, replace = TRUE)
  hisp <- rbinom(n, 1, ZONE_HISP[zone])
  flags <- matrix(0L, n, 5, dimnames = list(NULL, RACES))
  for (i in seq_len(n)) {
    if (hisp[i] == 1) {
      r <- pick(c("white", "amind", "black", "asian", "multi"), 1, prob = c(.70, .15, .07, .02, .06))
    } else {
      r <- pick(c(RACES, "multi"), 1, prob = ZONE_RACE[[zone[i]]])
    }
    if (r == "multi") {
      two <- sample(RACES, 2, prob = c(.30, .15, .30, .02, .40))
      flags[i, two] <- 1L
    } else flags[i, r] <- 1L
  }
  black_only <- flags[, "black"] == 1 & rowSums(flags) == 1 & hisp == 0
  asian_only <- flags[, "asian"] == 1 & rowSums(flags) == 1 & hisp == 0
  white_only <- flags[, "white"] == 1 & rowSums(flags) == 1 & hisp == 0
  lang <- rep("English", n)
  u <- runif(n)
  lang[hisp == 1 & u < .55] <- "Spanish"
  lang[black_only] <- ifelse(u[black_only] < .30, "Somali", ifelse(u[black_only] < .37, "Oromo",
                      ifelse(u[black_only] < .42, "Amharic", "English")))
  lang[asian_only] <- ifelse(u[asian_only] < .40, "Hmong", ifelse(u[asian_only] < .52, "Karen",
                      ifelse(u[asian_only] < .64, "Vietnamese", "English")))
  lang[white_only & u < .02] <- "Russian"
  # Last names loosely follow home language, as they would.
  last <- pick(LAST, n)
  last[lang == "Somali"]  <- pick(c("Ali","Hassan","Mohamed","Abdi","Omar","Warsame","Jama","Farah","Yusuf","Ahmed"), sum(lang == "Somali"))
  last[lang == "Hmong"]   <- pick(c("Vang","Xiong","Yang","Lee","Thao","Moua","Her","Lor"), sum(lang == "Hmong"))
  last[lang == "Spanish"] <- pick(c("Hernandez","Garcia","Martinez","Lopez","Rodriguez","Gonzalez","Ramirez","Torres","Flores","Morales"), sum(lang == "Spanish"))
  last[lang %in% c("Oromo","Amharic")] <- pick(c("Bekele","Tadesse","Girma","Haile","Tesfaye","Kebede","Desta","Wolde"), sum(lang %in% c("Oromo","Amharic")))
  last[lang == "Karen"]   <- pick(c("Htoo","Paw","Moo","Kaw"), sum(lang == "Karen"))
  last[lang == "Vietnamese"] <- pick(c("Nguyen","Tran","Le","Pham"), sum(lang == "Vietnamese"))
  first[lang == "Hmong" & gender == "F"] <- pick(c("Paj","Kia","Mai","Hana"), sum(lang == "Hmong" & gender == "F"))
  first[lang == "Hmong" & gender == "M"] <- pick(c("Tou","Kou","Pao","Fue"), sum(lang == "Hmong" & gender == "M"))
  first[lang == "Somali" & gender == "F"] <- pick(c("Amina","Hodan","Nasra","Hawa","Ifrah","Sagal","Zainab","Fatima"), sum(lang == "Somali" & gender == "F"))
  first[lang == "Somali" & gender == "M"] <- pick(c("Abdi","Mohamed","Ahmed","Yusuf","Hassan","Omar","Ibrahim","Abdirahman","Hamza"), sum(lang == "Somali" & gender == "M"))
  first[lang == "Spanish" & gender == "F"] <- pick(c("Sofia","Camila","Valeria","Ximena","Lucia","Daniela","Gabriela","Mariana"), sum(lang == "Spanish" & gender == "F"))
  first[lang == "Spanish" & gender == "M"] <- pick(c("Mateo","Santiago","Diego","Miguel","Luis","Carlos","Jose"), sum(lang == "Spanish" & gender == "M"))
  amind_any <- flags[, "amind"] == 1
  am_mask <- amind_any & runif(n) < .5
  last[am_mask] <- pick(c("Begay","Morningstar","Fairbanks","Whitebird","Thunder","Bellanger","Goodsky","Fineday","Jourdain","Kingbird"), sum(am_mask))
  if (length(last) != n) stop("name length")

  frl_p <- clamp(ZONE_FRL[zone] + 0.12 * (hisp == 1 | flags[, "black"] == 1 | amind_any) -
                 0.08 * white_only + rnorm(n, 0, .05), .05, .95)
  ability <- rnorm(n)
  # Chronic absence is more common among students from low-income families,
  # as it is in real data; the share prone to it rises with frl_p.
  frl_latent <- runif(n) < frl_p
  prone <- runif(n) < ifelse(frl_latent, .36, .13)
  absbase <- ifelse(prone, rbeta(n, 2.2, 11), rbeta(n, 2, 42))
  beh <- rgamma(n, shape = .55, rate = .55)
  data.frame(
    student_id = ids, state_id = new_state_ids(n), last_name = last, first_name = first,
    birth_date = bdate, gender = gender, hispanic_latino = hisp,
    race_american_indian = flags[, "amind"], race_asian = flags[, "asian"],
    race_black = flags[, "black"], race_pacific_islander = flags[, "nhpi"],
    race_white = flags[, "white"], home_language = lang,
    zone = zone, ability = ability - 0.35 * frl_latent - 0.25 * (lang != "English"),
    absbase = absbase, beh = beh, frl_p = frl_p, frl_latent = frl_latent,
    sped = as.integer(runif(n) < .135), s504 = as.integer(runif(n) < .03),
    gt = as.integer(runif(n) < .07),
    el_start = as.integer(lang != "English" & runif(n) < clamp(.88 - .045 * grade, .25, .9)),
    entered_9th_year = NA_integer_, credits = 0, alc = FALSE
  )
}

STU <- NULL   # registry of every student ever created
add_students <- function(df) STU <<- rbind(STU, df)

# ---- Holders ----------------------------------------------------------------
SPELLS <- list(); n_sp <- 0L
add_spell <- function(student_id, school_id, k, grade, start, end, sstat, estat, service = "P") {
  n_sp <<- n_sp + 1L
  SPELLS[[n_sp]] <<- data.frame(student_id = as.integer(student_id), school_id = as.integer(school_id),
    school_year = YEARS[k], grade = grade, start_date = as.Date(start, origin = "1970-01-01"),
    end_date = as.Date(end, origin = "1970-01-01"),
    start_status = as.character(sstat), end_status = as.character(estat), service_type = service)
}
TRANS <- list(); n_tr <- 0L
add_trans <- function(df) { n_tr <<- n_tr + 1L; TRANS[[n_tr]] <<- df }

GRADE_CODE <- function(g) ifelse(g == 0, "KG", sprintf("%02d", g))

# ---- Course catalog ---------------------------------------------------------
courses <- read.csv(text = "course_id,course_name,subject,state_course_code,credits_per_term,course_level
ENG9,English 9,English,01001,0.5,Regular
ENG10,English 10,English,01002,0.5,Regular
ENG11,English 11,English,01003,0.5,Regular
ENG12,English 12,English,01004,0.5,Regular
ENGH9,Honors English 9,English,01051,0.5,Honors
APLANG,AP English Language,English,01005,0.5,AP
CRENG,English Credit Recovery,English,01995,0.5,Credit recovery
ALG1,Algebra I,Mathematics,02052,0.5,Regular
GEOM,Geometry,Mathematics,02072,0.5,Regular
ALG2,Algebra II,Mathematics,02056,0.5,Regular
PRECALC,Precalculus,Mathematics,02110,0.5,Regular
STATS,Statistics,Mathematics,02201,0.5,Regular
APCALC,AP Calculus AB,Mathematics,02124,0.5,AP
CRMATH,Math Credit Recovery,Mathematics,02995,0.5,Credit recovery
EARTH,Earth and Space Science,Science,03001,0.5,Regular
BIO,Biology,Science,03051,0.5,Regular
CHEM,Chemistry,Science,03101,0.5,Regular
PHYS,Physics,Science,03151,0.5,Regular
APBIO,AP Biology,Science,03056,0.5,AP
WHIST,World History,Social Studies,04051,0.5,Regular
USHIST,U.S. History,Social Studies,04101,0.5,Regular
APUSH,AP U.S. History,Social Studies,04104,0.5,AP
GOVECON,Government and Economics,Social Studies,04151,0.5,Regular
SPAN1,Spanish I,World Languages,06101,0.5,Regular
SPAN2,Spanish II,World Languages,06102,0.5,Regular
SPAN3,Spanish III,World Languages,06103,0.5,Regular
SOMHL,Somali for Heritage Speakers,World Languages,06801,0.5,Regular
HEALTH,Health,Health and PE,08051,0.5,Regular
PE,Physical Education,Health and PE,08001,0.5,Regular
ART1,Art I,Fine Arts,05151,0.5,Regular
CERAM,Ceramics,Fine Arts,05155,0.5,Regular
BAND,Concert Band,Fine Arts,05101,0.5,Regular
CHOIR,Concert Choir,Fine Arts,05110,0.5,Regular
APCSP,AP Computer Science Principles,Computer Science,10019,0.5,AP
ENGR,Introduction to Engineering Design,Career and Technical Education,21004,0.5,CTE
CULIN,Culinary Arts,Career and Technical Education,16052,0.5,CTE
AUTO,Automotive Technology,Career and Technical Education,20104,0.5,CTE
ADVIS,Advisory,Advisory,22151,0.0,Non-credit
LEGACY,Credits converted from prior SIS,Transfer,99998,,Transfer
XFER,Transfer credit,Transfer,99999,,Transfer",
  colClasses = c(state_course_code = "character"))

pick_courses <- function(g, ab, alc, lang) {
  if (alc) {
    core <- c(if (ab < -0.6) "CRENG" else paste0("ENG", g), if (ab < -0.4) "CRMATH" else "ALG1",
              "BIO", c("WHIST", "USHIST", "GOVECON", "GOVECON")[g - 8], "HEALTH", "CULIN")
    return(list(year = core, s1 = "ART1", s2 = "AUTO"))
  }
  adv <- ab > 0.9
  eng  <- switch(as.character(g), "9" = if (adv) "ENGH9" else "ENG9", "10" = "ENG10",
                 "11" = if (adv) "APLANG" else "ENG11", "12" = "ENG12")
  math <- if (ab > 0.9) c("GEOM", "ALG2", "PRECALC", "APCALC")[g - 8] else
          if (ab > -0.3) c("ALG1", "GEOM", "ALG2", "PRECALC")[g - 8] else
                         c("ALG1", "ALG1", "GEOM", "STATS")[g - 8]
  sci  <- c("EARTH", "BIO", "CHEM", if (adv) "APBIO" else "PHYS")[g - 8]
  ss   <- c("WHIST", "WHIST", if (adv) "APUSH" else "USHIST", "GOVECON")[g - 8]
  lng  <- if (lang == "Somali" && runif(1) < .5) "SOMHL" else c("SPAN1", "SPAN2", "SPAN3", "SPAN3")[g - 8]
  el1  <- pick(c("BAND", "CHOIR", "ENGR", "APCSP", "ART1"), 1)
  sem  <- sample(c("HEALTH", "PE", "ART1", "CERAM", "CULIN", "AUTO"), 2)
  if (g == 9) sem <- c("HEALTH", "PE")
  list(year = c(eng, math, sci, ss, lng, el1), s1 = sem[1], s2 = sem[2])
}
mark_for <- function(ab, absr, level) {
  z <- 0.9 * ab - 5 * (absr - 0.06) + rnorm(1, 0, 0.8) + 0.5 - ifelse(level == "AP", 0.35, 0)
  pf <- plogis(-3.5 - 1.0 * ab + 10 * (absr - 0.07))
  if (runif(1) < pf) return("F")
  if (z > 0.9) "A" else if (z > 0.0) "B" else if (z > -0.9) "C" else "D"
}

# ---- Simulation -------------------------------------------------------------
LEAVE_CODES  <- c("TRMN", "TROS", "EMIG")
YEAR_MULT <- c(1.55, 1.38, 1.22, 1.10, 1.00)   # absence trend, pandemic recovery
SUMMARY <- list(); n_sum <- 0L
ATT <- list(); n_att <- 0L
TESTS <- list(); n_test <- 0L
active <- NULL        # data.frame(student_id, grade, school_id) at a year's start
exited_pool <- integer(0)

absrate_for <- function(sids, k, school) {
  i <- match(sids, STU$student_id)
  m <- c(E = 0.85, M = 1.05, H = 1.20, A = 2.10)[level_of(school)]
  clamp(STU$absbase[i] * YEAR_MULT[k] * m * ifelse(STU$sped[i] == 1, 1.15, 1), 0.004, 0.85)
}

for (k in seq_along(YEARS)) {
  Y <- YEARS[k]; F <- Y - 1
  days <- IDAYS[[k]]; ys <- days[1]; ye <- tail(days, 1)
  horizon <- if (Y == 2026) AS_OF else ye
  hdays <- days[days <= horizon]

  # -- who is here on day one --
  if (k == 1) {
    for (g in 0:12) {
      n <- round(rnorm(1, 104, 6))
      zone <- sample(1:4, n, replace = TRUE, prob = c(.27, .25, .25, .23))
      df <- make_students(n, g, zone, k)
      if (g >= 9) df$entered_9th_year <- Y - (g - 9) - as.integer(runif(n) < .03)
      df$alc <- g >= 9 & runif(n) < .07
      add_students(df)
      active <- rbind(active, data.frame(student_id = df$student_id, grade = g,
                                         sstat = if (g == 0) "NEW" else "CONT"))
    }
    # Legacy credits for students already in high school before the data window.
    hs <- STU[!is.na(STU$entered_9th_year) & STU$entered_9th_year < Y, ]
    for (j in seq_len(nrow(hs))) {
      yrs <- Y - hs$entered_9th_year[j]
      earned <- round(clamp(yrs * 6.5 + hs$ability[j] * 0.8 - 12 * (hs$absbase[j] - .07) + rnorm(1, 0, .7), 0, yrs * 7) * 2) / 2
      STU$credits[STU$student_id == hs$student_id[j]] <- earned
      add_trans(data.frame(student_id = hs$student_id[j], school_year = Y - 1L, term = "YR",
        school_id = NA_integer_, course_id = "LEGACY", mark = "P",
        credits_attempted = yrs * 7, credits_earned = earned, grade_level = NA_character_))
    }
  }
  i_act <- match(active$student_id, STU$student_id)
  active$zone <- STU$zone[i_act]
  active$school_id <- school_for(active$grade, active$zone, STU$alc[i_act])

  # -- new arrivals at the start of the year (k > 1 handled in transitions) --
  # -- events within the year --
  N <- nrow(active)
  roll <- runif(N)
  sids <- active$student_id
  lvl <- level_of(active$school_id)
  p_leave <- ifelse(lvl == "A", .12, .032)
  p_move  <- .02
  window_days <- hdays[hdays > ys + 20 & hdays < horizon - 5]
  year_spells <- list()
  active$gone <- FALSE
  for (j in seq_len(N)) {
    sid <- sids[j]; g <- active$grade[j]; sch <- active$school_id[j]; st <- active$sstat[j]
    gc <- GRADE_CODE(g)
    if (roll[j] < p_leave[j]) {
      d <- pick(window_days, 1)
      age16 <- g >= 10
      code <- if (age16 && runif(1) < ifelse(lvl[j] == "A", .45, .22)) "DROP" else pick(LEAVE_CODES, 1, prob = c(.66, .26, .08))
      add_spell(sid, sch, k, gc, ys, d, st, code)
      exited_pool <- c(exited_pool, sid)
      active$gone[j] <- TRUE
    } else if (roll[j] < p_leave[j] + p_move) {
      d <- pick(window_days, 1)
      if (g <= 5) { opts <- setdiff(c(10L, 20L, 30L, 40L), sch) }
      else if (g <= 8) { opts <- setdiff(c(50L, 60L), sch) }
      else { opts <- setdiff(c(70L, 80L, if (STU$absbase[STU$student_id == sid] > .12) 90L), sch) }
      new_sch <- pick(opts, 1)
      nd <- days[match(d, days) + 1]
      add_spell(sid, sch, k, gc, ys, d, st, "INTR")
      add_spell(sid, new_sch, k, gc, nd, NA, "INTR", NA)
      STU$alc[STU$student_id == sid] <- new_sch == 90L
      if (new_sch %in% c(10L, 20L, 30L, 40L)) STU$zone[STU$student_id == sid] <- match(new_sch, c(10L, 20L, 30L, 40L))
      if (new_sch %in% c(50L, 60L, 70L, 80L)) STU$zone[STU$student_id == sid] <- if (new_sch %in% c(50L, 70L)) sample(1:2, 1) else sample(3:4, 1)
      active$gone[j] <- FALSE
    } else {
      add_spell(sid, sch, k, gc, ys, NA, st, NA)
      active$gone[j] <- FALSE
    }
  }
  # mid-year arrivals
  n_arr <- rpois(1, 0.04 * N * as.numeric(horizon - ys) / as.numeric(ye - ys))
  if (n_arr > 0) {
    g_arr <- sample(0:12, n_arr, replace = TRUE)
    zone <- sample(1:4, n_arr, replace = TRUE)
    df <- make_students(n_arr, g_arr, zone, k)
    df$alc <- g_arr >= 9 & runif(n_arr) < .06
    df$entered_9th_year <- ifelse(g_arr >= 9, Y - (g_arr - 9), NA)
    df$credits <- ifelse(g_arr >= 9, pmax(0, (g_arr - 9) * 6.3 + rnorm(n_arr, 0, 1.2)), 0)
    df$credits <- round(df$credits * 2) / 2
    add_students(df)
    for (j in seq_len(n_arr)) {
      d <- pick(window_days, 1)
      sch <- school_for(g_arr[j], zone[j], df$alc[j])
      add_spell(df$student_id[j], sch, k, GRADE_CODE(g_arr[j]), d, NA,
                pick(c("TRNI", "OSIN", "NEW"), 1, prob = c(.62, .30, .08)), NA)
      if (g_arr[j] >= 10 && df$credits[j] > 0)
        add_trans(data.frame(student_id = df$student_id[j], school_year = Y, term = "YR",
          school_id = NA_integer_, course_id = "XFER", mark = "P",
          credits_attempted = df$credits[j], credits_earned = df$credits[j], grade_level = NA_character_))
      active <- rbind(active, data.frame(student_id = df$student_id[j], grade = g_arr[j],
                      sstat = "x", zone = zone[j], school_id = sch, gone = FALSE))
    }
  }
  # No-shows: enrolled for the first day and never came; caught late (PLANT for snapshot drift)
  if (k >= 3) {
    n_ns <- 4
    g_ns <- sample(1:12, n_ns, replace = TRUE)
    df <- make_students(n_ns, g_ns, sample(1:4, n_ns, replace = TRUE), k)
    add_students(df)
    for (j in seq_len(n_ns)) {
      sch <- school_for(g_ns[j], df$zone[j], FALSE)
      add_spell(df$student_id[j], sch, k, GRADE_CODE(g_ns[j]), ys, days[8], "TRNI", "NOSH")
    }
  }

  # ---- finish the year's spells: close open ones (past years), compute attendance
  this_idx <- which(vapply(SPELLS, function(s) s$school_year[1] == Y, TRUE))

  # Attendance
  for (ix in this_idx) {
    s <- SPELLS[[ix]]
    if (s$service_type != "P") next
    end <- if (is.na(s$end_date)) horizon else min(s$end_date, horizon)
    md <- days[days >= s$start_date & days <= end]
    if (length(md) == 0) next
    if (identical(s$end_status, "NOSH")) {
      if (Y == 2026) { n_att <- n_att + 1L
        ATT[[n_att]] <- data.frame(student_id = s$student_id, school_id = s$school_id, att_date = md,
                                   status = "A", excuse = "U") }
      next
    }
    p <- absrate_for(s$student_id, k, s$school_id)
    if (Y < 2026) {
      if (is.na(s$end_date)) md <- days[days >= s$start_date]   # whole year for summary
      a_full <- rbinom(1, length(md), p * .9); a_half <- rbinom(1, length(md), p * .2)
      n_sum <- n_sum + 1L
      SUMMARY[[n_sum]] <- data.frame(school_year = Y, student_id = s$student_id, school_id = s$school_id,
        days_membership = length(md), days_absent = min(length(md), a_full + 0.5 * a_half))
    } else {
      u <- runif(length(md)); v <- runif(length(md))
      status <- ifelse(u < p * .88, "A", ifelse(u < p, "H", ifelse(u < p + .035, "T", NA)))
      excuse <- ifelse(status == "T", ifelse(v < .25, "E", "U"),
                ifelse(v < .50, "E", "U"))
      ex <- runif(length(md)) < .006 & is.na(status)
      status[ex] <- "A"; excuse[ex] <- "X"
      keep <- !is.na(status)
      if (any(keep)) { n_att <- n_att + 1L
        ATT[[n_att]] <- data.frame(student_id = s$student_id, school_id = s$school_id,
          att_date = md[keep], status = status[keep], excuse = excuse[keep]) }
    }
  }

  # Spring state tests (2023-2025): enrolled in a tested grade on April 15
  if (Y %in% 2023:2025) {
    td <- days[days >= as.Date(sprintf("%d-04-15", Y))][1]
    for (ix in this_idx) {
      s <- SPELLS[[ix]]
      if (s$service_type != "P" || s$start_date > td || (!is.na(s$end_date) && s$end_date < td)) next
      g <- if (s$grade == "KG") 0 else as.integer(s$grade)
      subj <- c(if (g %in% c(3:8, 10)) "Reading", if (g %in% c(3:8, 11)) "Mathematics")
      i <- match(s$student_id, STU$student_id)
      for (sb in subj) {
        st <- pick(c("Tested", "Refused", "Absent", "Medically exempt"), 1,
                   prob = if (g == 11) c(.86, .11, .025, .005) else c(.955, .028, .012, .005))
        z <- STU$ability[i] + rnorm(1, 0, .55) - ifelse(sb == "Reading" & STU$el_start[i] == 1, .35, 0) - ifelse(STU$sped[i] == 1, .55, 0)
        sc <- if (st == "Tested") as.integer(clamp(round(g * 100 + 50 + 13 * (z + 0.05)), g * 100 + 1, g * 100 + 99)) else NA_integer_
        lv <- if (is.na(sc)) NA_character_ else if (sc >= g * 100 + 65) "Exceeds" else if (sc >= g * 100 + 50) "Meets" else
              if (sc >= g * 100 + 40) "Partially Meets" else "Does Not Meet"
        n_test <- n_test + 1L
        TESTS[[n_test]] <- data.frame(student_id = s$student_id, school_year = Y, test_date = td,
          subject = sb, grade_tested = s$grade, school_id = s$school_id, test_status = st,
          scale_score = sc, achievement_level = lv)
      }
    }
  }

  # Transcripts: high school semesters
  sems <- list(S1 = c(ys, S1_END[k]), S2 = c(S2_START[k], ye))
  if (Y == 2026) sems <- sems["S1"]
  for (ix in this_idx) {
    s <- SPELLS[[ix]]
    if (!s$school_id %in% c(70L, 80L, 90L) || s$service_type != "P" || identical(s$end_status, "NOSH")) next
    g <- as.integer(s$grade)
    i <- match(s$student_id, STU$student_id)
    cs <- pick_courses(g, STU$ability[i], s$school_id == 90L, STU$home_language[i])
    p <- absrate_for(s$student_id, k, s$school_id)
    for (tm in names(sems)) {
      w <- sems[[tm]]
      if (s$start_date > w[2] - 30) next                       # arrived too late for this term
      if (!is.na(s$end_date) && s$end_date < w[1]) next         # gone before it began
      left_early <- !is.na(s$end_date) && s$end_date < w[2]
      if (left_early && s$end_status %in% "INTR") next           # the new school grades it
      cl <- c(cs$year, if (tm == "S1") cs$s1 else cs$s2)
      lv <- courses$course_level[match(cl, courses$course_id)]
      mk <- if (left_early) rep("W", length(cl)) else vapply(seq_along(cl), function(q) mark_for(STU$ability[i], p, lv[q]), "")
      earned <- ifelse(mk %in% c("F", "W"), 0, 0.5)
      STU$credits[i] <- STU$credits[i] + sum(earned)
      add_trans(data.frame(student_id = s$student_id, school_year = Y, term = tm,
        school_id = s$school_id, course_id = cl, mark = mk, credits_attempted = 0.5,
        credits_earned = earned, grade_level = s$grade))
    }
  }

  if (Y == 2026) break

  # ---- year-end transitions ----
  still <- which(vapply(SPELLS, function(s) s$school_year[1] == Y && is.na(s$end_date), TRUE))
  nxt <- NULL
  for (ix in still) {
    s <- SPELLS[[ix]]
    if (s$service_type != "P") { SPELLS[[ix]]$end_date <- ye; SPELLS[[ix]]$end_status <- "EOY"; next }
    sid <- s$student_id; i <- match(sid, STU$student_id)
    g <- if (s$grade == "KG") 0L else as.integer(s$grade)
    SPELLS[[ix]]$end_date <- ye
    if (g == 12) {
      if (STU$credits[i] >= 21.5) { SPELLS[[ix]]$end_status <- "GRAD"; next }
      if (runif(1) < .5) { SPELLS[[ix]]$end_status <- "EOY"; nxt <- rbind(nxt, data.frame(student_id = sid, grade = 12L, sstat = "CONT")); next }
      SPELLS[[ix]]$end_status <- pick(c("DROP", "UNKN", "TRMN"), 1, prob = c(.55, .2, .25))
      exited_pool <- c(exited_pool, sid); next
    }
    if (runif(1) < .05) {
      SPELLS[[ix]]$end_status <- if (g >= 10 && runif(1) < .15) "DROP" else pick(LEAVE_CODES, 1, prob = c(.65, .25, .10))
      exited_pool <- c(exited_pool, sid); next
    }
    SPELLS[[ix]]$end_status <- "EOY"
    retain <- if (g <= 8) runif(1) < .01 else {
      behind <- STU$credits[i] < (g - 8) * 5.5
      runif(1) < ifelse(behind, .45, .01) }
    ng <- if (retain) g else g + 1L
    if (ng == 9 && is.na(STU$entered_9th_year[i])) STU$entered_9th_year[i] <- Y + 1L
    if (ng >= 9 && g <= 8 && STU$absbase[i] > .16 && runif(1) < .3) STU$alc[i] <- TRUE
    if (ng >= 10 && !STU$alc[i] && STU$absbase[i] > .14 && runif(1) < .15) STU$alc[i] <- TRUE
    nxt <- rbind(nxt, data.frame(student_id = sid, grade = ng, sstat = "CONT"))
  }
  # Close any spell that ended with a mid-year exit but has no code yet (none expected)
  # New kindergarten class and year-start transfers in
  nk <- round(rnorm(1, 103, 6))
  dfk <- make_students(nk, rep(0L, nk), sample(1:4, nk, replace = TRUE, prob = c(.27, .25, .25, .23)), k + 1)
  add_students(dfk)
  nxt <- rbind(nxt, data.frame(student_id = dfk$student_id, grade = 0L, sstat = "NEW"))
  n_in <- round(0.05 * nrow(nxt))
  g_in <- sample(1:12, n_in, replace = TRUE)
  dfi <- make_students(n_in, g_in, sample(1:4, n_in, replace = TRUE), k + 1)
  dfi$alc <- g_in >= 9 & runif(n_in) < .05
  dfi$entered_9th_year <- ifelse(g_in >= 9, Y + 1L - (g_in - 9), NA)
  dfi$credits <- ifelse(g_in >= 10, round(pmax(0, (g_in - 9) * 6.3 + rnorm(n_in, 0, 1.2)) * 2) / 2, 0)
  add_students(dfi)
  for (j in which(g_in >= 10 & dfi$credits > 0))
    add_trans(data.frame(student_id = dfi$student_id[j], school_year = Y + 1L, term = "YR",
      school_id = NA_integer_, course_id = "XFER", mark = "P",
      credits_attempted = dfi$credits[j], credits_earned = dfi$credits[j], grade_level = NA_character_))
  nxt <- rbind(nxt, data.frame(student_id = dfi$student_id, grade = g_in,
                               sstat = pick(c("TRNI", "OSIN"), n_in, prob = c(.7, .3))))
  # A few students come back after leaving
  back <- head(shuffle(setdiff(exited_pool, nxt$student_id)), 3)
  for (b in back) {
    last <- tail(Filter(function(s) s$student_id == b, SPELLS), 1)[[1]]
    g <- if (last$grade == "KG") 1L else min(12L, as.integer(last$grade) + 1L)
    if (last$end_status %in% c("GRAD", "NOSH")) next
    nxt <- rbind(nxt, data.frame(student_id = b, grade = g, sstat = "RTN"))
  }
  active <- nxt
}

# ---- Assemble enrollments ---------------------------------------------------
enr <- do.call(rbind, SPELLS)
enr <- enr[order(enr$school_year, enr$student_id, enr$start_date), ]

# Secondary enrollments: evening credit recovery at Horizon ALC for some HS students
hs_now <- enr[enr$school_id %in% c(70L, 80L) & enr$service_type == "P", ]
pickhs <- hs_now[sample(nrow(hs_now), round(.035 * nrow(hs_now))), ]
for (j in seq_len(nrow(pickhs))) {
  k <- match(pickhs$school_year[j], YEARS)
  d <- IDAYS[[k]]
  st <- max(pickhs$start_date[j], S2_START[k])
  en <- if (is.na(pickhs$end_date[j])) NA else pickhs$end_date[j]
  if (!is.na(en) && en <= st) next
  if (pickhs$school_year[j] == 2026 && st > AS_OF) next
  add_spell(pickhs$student_id[j], 90L, k, pickhs$grade[j], d[d >= st][1],
            if (pickhs$school_year[j] == 2026) NA else en,
            "CONT", if (pickhs$school_year[j] == 2026) NA else "EOY", service = "S")
}
enr <- do.call(rbind, SPELLS)
enr <- enr[order(enr$school_year, enr$student_id, enr$start_date, enr$service_type), ]
enr$start_status[enr$start_status == "x"] <- "TRNI"

att <- do.call(rbind, ATT)
ays <- do.call(rbind, SUMMARY)
tests <- do.call(rbind, TESTS)
trans <- do.call(rbind, TRANS)

# ---- Behavior: 2024-25 and 2025-26 -----------------------------------------
inc_rows <- list(); part_rows <- list(); res_rows <- list()
inc_id <- 5000L; res_id <- 80000L
TYPES <- list(
  E = c("Disruptive behavior" = .38, "Physical aggression" = .30, "Insubordination" = .15, "Bullying" = .10, "Property damage" = .07),
  M = c("Disruptive behavior" = .30, "Fighting" = .20, "Insubordination" = .18, "Bullying" = .12, "Electronic device violation" = .10, "Threat" = .05, "Theft" = .05),
  H = c("Disruptive behavior" = .22, "Fighting" = .15, "Insubordination" = .15, "Attendance policy" = .15, "Electronic device violation" = .12, "Vaping or tobacco" = .12, "Threat" = .05, "Theft" = .04),
  A = c("Disruptive behavior" = .25, "Fighting" = .15, "Insubordination" = .15, "Attendance policy" = .20, "Vaping or tobacco" = .15, "Threat" = .10))
RES_FOR <- function(type, lvl) {
  if (type %in% c("Fighting", "Physical aggression")) return(pick(c("OSS", "ISS", "CONF"), 1, prob = c(.58, .22, .20)))
  if (type == "Threat") return(pick(c("OSS", "EXP", "CONF"), 1, prob = c(.75, .05, .20)))
  if (type %in% c("Attendance policy", "Electronic device violation")) return(pick(c("DET", "CONF"), 1, prob = c(.75, .25)))
  pick(c("DET", "ISS", "OSS", "CONF"), 1, prob = if (lvl == "E") c(.35, .30, .12, .23) else c(.30, .30, .20, .20))
}
RACE_MULT <- function(i) {
  b <- STU$race_black[i] == 1; a <- STU$race_american_indian[i] == 1; h <- STU$hispanic_latino[i] == 1
  ifelse(a, 1.9, ifelse(b, 2.1, ifelse(h, 1.2, 1)))
}
for (Y in c(2025, 2026)) {
  k <- match(Y, YEARS); days <- IDAYS[[k]]
  horizon <- if (Y == 2026) AS_OF else tail(days, 1)
  e <- enr[enr$school_year == Y & enr$service_type == "P" & !enr$end_status %in% "NOSH", ]
  for (j in seq_len(nrow(e))) {
    i <- match(e$student_id[j], STU$student_id)
    end <- if (is.na(e$end_date[j])) horizon else min(e$end_date[j], horizon)
    md <- days[days >= e$start_date[j] & days <= end]
    if (length(md) < 3) next
    lvl <- level_of(e$school_id[j])
    base <- c(E = .10, M = .42, H = .30, A = .75)[lvl]
    lam <- base * STU$beh[i] * ifelse(STU$sped[i] == 1, 1.6, 1) * RACE_MULT(i) * length(md) / 170
    n <- rpois(1, lam)
    for (q in seq_len(n)) {
      inc_id <- inc_id + 1L
      d <- pick(md[-length(md)], 1)
      tp <- pick(names(TYPES[[lvl]]), 1, prob = TYPES[[lvl]])
      inc_rows[[length(inc_rows) + 1]] <- data.frame(incident_id = inc_id, school_id = e$school_id[j],
        incident_date = d, incident_type = tp, location = pick(c("Classroom", "Hallway", "Cafeteria", "Bus", "Playground", "Restroom", "Gym"), 1,
          prob = if (lvl == "E") c(.4, .1, .15, .1, .2, .02, .03) else c(.45, .25, .12, .06, 0, .08, .04)))
      offenders <- e$student_id[j]
      peers <- e$student_id[e$school_id == e$school_id[j] & e$start_date <= d & (is.na(e$end_date) | e$end_date >= d)]
      peers <- setdiff(peers, offenders)
      if (tp %in% c("Fighting", "Physical aggression") && runif(1) < .55 && length(peers)) offenders <- c(offenders, pick(peers, 1))
      vp <- setdiff(peers, offenders)
      victims <- if (tp %in% c("Bullying", "Threat", "Physical aggression") && runif(1) < .7 && length(vp)) pick(vp, 1) else integer(0)
      part_rows[[length(part_rows) + 1]] <- data.frame(incident_id = inc_id,
        student_id = c(offenders, victims), role = c(rep("Offender", length(offenders)), rep("Victim", length(victims))))
      for (o in offenders) {
        rc <- RES_FOR(tp, lvl)
        nd <- days[match(d, days) + 1]
        dur <- switch(rc, OSS = if (tp %in% c("Fighting", "Threat")) sample(2:5, 1) else sample(1:3, 1),
                      ISS = sample(1:2, 1), EXP = 45, DET = 0, CONF = 0)
        res_id <- res_id + 1L
        res_rows[[length(res_rows) + 1]] <- data.frame(resolution_id = res_id, incident_id = inc_id,
          student_id = o, resolution_code = rc, start_date = nd, days_removed = dur)
        if (rc != "CONF" && runif(1) < .10) {
          res_id <- res_id + 1L
          res_rows[[length(res_rows) + 1]] <- data.frame(resolution_id = res_id, incident_id = inc_id,
            student_id = o, resolution_code = "CONF", start_date = nd, days_removed = 0)
        }
      }
    }
  }
}
incidents <- do.call(rbind, inc_rows)
incident_students <- do.call(rbind, part_rows)
incident_students <- incident_students[!duplicated(incident_students[, c("incident_id", "student_id")]), ]
resolutions <- do.call(rbind, res_rows)

# OSS and expulsion days in the current year become attendance rows coded S.
k5 <- match(2026, YEARS); d5 <- IDAYS[[k5]]
rem <- resolutions[resolutions$resolution_code %in% c("OSS", "EXP") & resolutions$start_date >= y_start[k5], ]
for (j in seq_len(nrow(rem))) {
  sid <- rem$student_id[j]
  sp <- enr[enr$student_id == sid & enr$school_year == 2026 & enr$service_type == "P" &
            enr$start_date <= rem$start_date[j] & (is.na(enr$end_date) | enr$end_date >= rem$start_date[j]), ]
  if (!nrow(sp)) next
  ds <- d5[d5 >= rem$start_date[j]][seq_len(rem$days_removed[j])]
  ds <- ds[!is.na(ds) & ds <= AS_OF & (is.na(sp$end_date[1]) | ds <= sp$end_date[1])]
  if (!length(ds)) next
  att <- att[!(att$student_id == sid & att$att_date %in% ds), ]
  att <- rbind(att, data.frame(student_id = sid, school_id = sp$school_id[1], att_date = ds, status = "A", excuse = "S"))
}

# ---- Programs ---------------------------------------------------------------
first_start <- tapply(as.numeric(enr$start_date), enr$student_id, min); fs <- as.Date(first_start, origin = "1970-01-01")
names(fs) <- names(first_start)
last_end <- tapply(as.numeric(enr$end_date), enr$student_id, function(x) if (any(is.na(x))) NA else max(x))
le <- as.Date(last_end, origin = "1970-01-01"); names(le) <- names(last_end)
prog <- list()
addp <- function(sid, p, s, e) prog[[length(prog) + 1]] <<- data.frame(student_id = sid, program = p, start_date = s, end_date = e)
for (i in seq_len(nrow(STU))) {
  sid <- STU$student_id[i]; key <- as.character(sid)
  if (!key %in% names(fs)) next
  s0 <- fs[[key]]; e0 <- le[[key]]
  pre <- s0 - sample(200:1500, 1)
  later <- function(x) if (!is.na(e0) && x > e0) s0 else x
  if (STU$sped[i] == 1) addp(sid, "SPED", if (runif(1) < .6) pre else later(s0 + sample(30:400, 1)), e0)
  if (STU$s504[i] == 1) addp(sid, "504", later(s0 + sample(0:300, 1)), e0)
  if (STU$gt[i] == 1) addp(sid, "GT", s0, e0)
  if (STU$el_start[i] == 1) {
    ex <- NA
    for (Y in YEARS) {
      ye <- y_end[match(Y, YEARS)]
      if (ye < s0 || Y == 2026) next
      if (!is.na(e0) && ye > e0) break
      if (runif(1) < .17) { ex <- ye; break }
    }
    addp(sid, "EL", min(s0, pre + 150), if (!is.na(ex)) ex else e0)
  }
}
# FRL eligibility by school year; homeless (McKinney-Vento) spells
ey <- unique(enr[enr$service_type == "P", c("student_id", "school_year")])
for (j in seq_len(nrow(ey))) {
  sid <- ey$student_id[j]; Y <- ey$school_year[j]; i <- match(sid, STU$student_id)
  sp <- enr[enr$student_id == sid & enr$school_year == Y & enr$service_type == "P", ]
  s <- min(sp$start_date); k <- match(Y, YEARS)
  e <- if (any(is.na(sp$end_date))) as.Date(sprintf("%d-06-30", Y)) else max(sp$end_date)
  if (xor(STU$frl_latent[i], runif(1) < .08)) addp(sid, "FRL", s, e)
  if (runif(1) < .022) {
    hd <- IDAYS[[k]]; hd <- hd[hd >= s & hd <= min(e, if (Y == 2026) AS_OF else e)]
    if (length(hd) > 10) addp(sid, "McKV", pick(hd, 1), if (Y == 2026) NA else e)
  }
}
programs <- do.call(rbind, prog)
programs <- programs[order(programs$student_id, programs$program, programs$start_date), ]

# ---- Staff, sections, roster (2025-26 high schools) --------------------------
SUBJ_TEACH <- c(English = 4, Mathematics = 4, Science = 3, `Social Studies` = 3, `World Languages` = 2,
                `Health and PE` = 2, `Fine Arts` = 2, `Computer Science` = 1, `Career and Technical Education` = 2)
staff <- data.frame(); st_id <- 7000L
add_staff <- function(sch, role, subj = NA) {
  st_id <<- st_id + 1L
  g <- sample(c("F", "M"), 1)
  staff <<- rbind(staff, data.frame(staff_id = st_id, last_name = pick(LAST, 1),
    first_name = if (g == "F") pick(FIRST_F, 1) else pick(FIRST_M, 1), school_id = sch,
    role = role, subject_area = subj))
  st_id
}
for (sch in schools$school_id) add_staff(sch, "Principal")
for (sch in schools$school_id) add_staff(sch, "Counselor")
for (sch in c(70L, 80L)) for (sb in names(SUBJ_TEACH)) for (q in seq_len(SUBJ_TEACH[[sb]])) add_staff(sch, "Teacher", sb)
for (sb in c("English", "Mathematics", "Science", "Social Studies", "Health and PE", "Fine Arts", "Career and Technical Education")) add_staff(90L, "Teacher", sb)

t26 <- trans[trans$school_year == 2026 & trans$term == "S1" & !trans$course_id %in% c("LEGACY", "XFER"), ]
cur_hs <- enr[enr$school_year == 2026 & enr$school_id %in% c(70L, 80L, 90L) & enr$service_type == "P" &
              !enr$end_status %in% "NOSH", ]
sections <- data.frame(); roster <- data.frame(); sec_id <- 30000L
for (sch in c(70L, 80L, 90L)) {
  for (tm in c("S1", "S2")) {
    k <- 5; w <- if (tm == "S1") c(y_start[k], S1_END[k]) else c(S2_START[k], y_end[k])
    sp <- cur_hs[cur_hs$school_id == sch & cur_hs$start_date <= w[2] & (is.na(cur_hs$end_date) | cur_hs$end_date >= w[1]), ]
    if (!nrow(sp)) next
    # course lists: reuse S1 transcripts where present, otherwise draw fresh
    req <- list()
    for (j in seq_len(nrow(sp))) {
      sid <- sp$student_id[j]; i <- match(sid, STU$student_id)
      cs <- pick_courses(as.integer(sp$grade[j]), STU$ability[i], sch == 90L, STU$home_language[i])
      cl <- c(cs$year, if (tm == "S1") cs$s1 else cs$s2, "ADVIS")
      tc <- t26$course_id[t26$student_id == sid]
      if (tm == "S1" && length(tc)) cl <- unique(c(tc, "ADVIS"))
      req[[j]] <- data.frame(student_id = sid, course_id = cl,
        start = max(sp$start_date[j], w[1]), end = if (is.na(sp$end_date[j])) w[2] else min(sp$end_date[j], w[2]))
    }
    req <- do.call(rbind, req)
    for (cid in unique(req$course_id)) {
      r <- req[req$course_id == cid, ]
      r <- r[sample(nrow(r)), ]
      nsec <- ceiling(nrow(r) / 30)
      subj <- courses$subject[courses$course_id == cid]
      tpool <- staff$staff_id[staff$school_id == sch & staff$role == "Teacher" & staff$subject_area %in% subj]
      if (!length(tpool)) tpool <- staff$staff_id[staff$school_id == sch & staff$role == "Teacher"]
      for (q in seq_len(nsec)) {
        sec_id <- sec_id + 1L
        sections <- rbind(sections, data.frame(section_id = sec_id, course_id = cid, school_id = sch,
          school_year = 2026L, term = tm, period = if (cid == "ADVIS") 0L else sample(1:7, 1), teacher_id = pick(tpool, 1)))
        mem <- r[seq(q, nrow(r), by = nsec), ]
        roster <- rbind(roster, data.frame(section_id = sec_id, student_id = mem$student_id,
          start_date = mem$start, end_date = mem$end))
      }
    }
  }
}

# ---- oct1 snapshot (frozen as submitted) -----------------------------------
oct1 <- list()
for (k in seq_along(YEARS)) {
  Y <- YEARS[k]; od <- as.Date(sprintf("%d-10-01", Y - 1))
  e <- enr[enr$school_year == Y & enr$service_type == "P" & enr$start_date <= od &
           (is.na(enr$end_date) | enr$end_date >= od | enr$end_status %in% "NOSH"), ]
  e <- e[!duplicated(e$student_id), ]
  flag <- function(p) as.integer(e$student_id %in% programs$student_id[programs$program == p &
          programs$start_date <= od & (is.na(programs$end_date) | programs$end_date >= od)])
  oct1[[k]] <- data.frame(school_year = Y, snapshot_date = od, student_id = e$student_id,
    school_id = e$school_id, grade = e$grade, el = flag("EL"), sped = flag("SPED"),
    frl = flag("FRL"), homeless = flag("McKV"))
}
oct1 <- do.call(rbind, oct1)

# =========================== PLANT: data-quality problems ====================
plant_log <- character()
cur <- which(enr$school_year == 2026 & enr$service_type == "P")

# 1. Overlapping primary enrollments: intra-district moves where the old school
#    never ended the enrollment. Teachers at the old school keep marking absent.
movers <- unique(enr$student_id[cur][enr$end_status[cur] %in% "INTR"])
ov <- head(shuffle(movers), 4)
for (sid in ov) {
  r <- which(enr$student_id == sid & enr$school_year == 2026 & enr$end_status %in% "INTR")[1]
  moved <- enr$end_date[r]
  enr$end_date[r] <- NA; enr$end_status[r] <- NA
  ds <- d5[d5 > moved & d5 <= AS_OF]
  att <- rbind(att, data.frame(student_id = sid, school_id = enr$school_id[r], att_date = ds, status = "A", excuse = "U"))
  plant_log <- c(plant_log, sprintf("overlap: student %d", sid))
}
# 2. Gaps: a move recorded with several school days missing between spells
gp <- head(shuffle(setdiff(movers, ov)), 3)
for (sid in gp) {
  r <- which(enr$student_id == sid & enr$school_year == 2026 & enr$start_status %in% "INTR")[1]
  if (is.na(r)) next
  nd <- d5[match(enr$start_date[r], d5) + 6]
  att <- att[!(att$student_id == sid & att$school_id == enr$school_id[r] & att$att_date < nd), ]
  enr$start_date[r] <- nd
  plant_log <- c(plant_log, sprintf("gap: student %d", sid))
}
# 3. Ghosts: stopped coming in late January; enrollment still open
open_now <- enr[cur, ][is.na(enr$end_date[cur]) & enr$school_id[cur] %in% c(50L, 60L, 70L, 80L), ]
gh <- head(shuffle(setdiff(open_now$student_id, c(ov, gp))), 4)
for (sid in gh) {
  sch <- open_now$school_id[open_now$student_id == sid][1]
  ds <- d5[d5 >= as.Date("2026-02-02") & d5 <= AS_OF]
  att <- att[!(att$student_id == sid & att$att_date %in% ds), ]
  att <- rbind(att, data.frame(student_id = sid, school_id = sch, att_date = ds, status = "A", excuse = "U"))
  roster <- roster  # still scheduled, which is realistic
  plant_log <- c(plant_log, sprintf("ghost: student %d", sid))
}
# 4. Closed enrollments with no end status
cl <- which(!is.na(enr$end_date) & enr$end_status %in% c("TRMN", "TROS") & enr$school_year >= 2024)
ns <- head(shuffle(cl), 4); enr$end_status[ns] <- NA
plant_log <- c(plant_log, sprintf("no end_status: enrollment row %s", paste(ns, collapse = ",")))
# 5. Invalid grade code
r <- pick(which(enr$school_year == 2026 & enr$grade == "09" & enr$service_type == "P" & is.na(enr$end_date) &
               enr$start_date == y_start[5]), 1)
enr$grade[r] <- "9"; bad_grade_sid <- enr$student_id[r]
oct1$grade[oct1$school_year == 2026 & oct1$student_id == bad_grade_sid] <- "9"   # it was wrong when sent
plant_log <- c(plant_log, sprintf("grade '9': student %d", bad_grade_sid))
# 6. End date before start date (a mistyped year)
r <- pick(which(enr$school_year == 2025 & !is.na(enr$end_date) & enr$end_status %in% c("TRMN", "TROS") &
                  format(enr$end_date, "%m") %in% c("01", "02", "03")), 1)
enr$end_date[r] <- as.Date(sub("^2025", "2024", fmt(enr$end_date[r])))
plant_log <- c(plant_log, sprintf("end before start: student %d", enr$student_id[r]))
# 7. Duplicate person: left in December, came back in January under a new record
cand <- enr[enr$school_year == 2026 & enr$service_type == "P" & is.na(enr$end_date) &
            enr$start_date == y_start[5] & enr$school_id <= 60L & !enr$student_id %in% c(ov, gp, gh), ]
dup_src <- head(shuffle(cand$student_id), 2)
for (sid in dup_src) {
  r <- which(enr$student_id == sid & enr$school_year == 2026)[1]
  enr$end_date[r] <- as.Date("2025-12-19"); enr$end_status[r] <- "TRMN"
  att <- att[!(att$student_id == sid & att$att_date > as.Date("2025-12-19")), ]
  i <- match(sid, STU$student_id)
  clone <- STU[i, ]; clone$student_id <- next_id; next_id <- next_id + 1L; clone$state_id <- NA
  STU <- rbind(STU, clone)
  newsch <- pick(setdiff(if (enr$school_id[r] <= 40L) c(10L, 20L, 30L, 40L) else c(50L, 60L), enr$school_id[r]), 1)
  enr <- rbind(enr, data.frame(student_id = clone$student_id, school_id = newsch, school_year = 2026L,
    grade = enr$grade[r], start_date = as.Date("2026-01-06"), end_date = as.Date(NA), start_status = "NEW",
    end_status = NA_character_, service_type = "P"))
  p <- absrate_for(sid, 5, newsch)
  ds <- d5[d5 >= as.Date("2026-01-06") & d5 <= AS_OF]
  u <- runif(length(ds)); k <- u < p
  if (any(k)) att <- rbind(att, data.frame(student_id = clone$student_id, school_id = newsch, att_date = ds[k], status = "A", excuse = "U"))
  plant_log <- c(plant_log, sprintf("duplicate person: %d -> %d", sid, clone$student_id))
}
# 8. Two brand-new arrivals with no state ID yet
arr <- cur[enr$start_status[cur] %in% c("NEW", "TRNI", "OSIN")]
r <- tail(arr[order(enr$start_date[arr])], 2)
STU$state_id[STU$student_id %in% enr$student_id[r]] <- NA
plant_log <- c(plant_log, sprintf("no state_id: %s", paste(enr$student_id[r], collapse = ",")))
# 9. Attendance recorded on a non-instructional day (MEA break)
mea <- CAL[[5]]$cal_date[CAL[[5]]$day_note %in% "MEA break"]
oc <- enr[enr$school_year == 2026 & enr$service_type == "P" & enr$start_date <= mea[1] & (is.na(enr$end_date) | enr$end_date >= mea[2]), ]
bad <- oc[sample(nrow(oc), 5), ]
att <- rbind(att, data.frame(student_id = bad$student_id, school_id = bad$school_id,
  att_date = pick(mea, 5), status = "A", excuse = "U"))
# 10. Duplicate open EL spells
el_open <- unique(programs$student_id[programs$program == "EL" & is.na(programs$end_date)])
for (sid in head(shuffle(el_open), 2)) {
  r <- which(programs$student_id == sid & programs$program == "EL")[1]
  programs <- rbind(programs, data.frame(student_id = sid, program = "EL",
    start_date = as.Date("2025-09-15"), end_date = NA))
}
# 11. Roster rows left open for students who withdrew (S2 schedules never dropped)
gone <- enr[enr$school_year == 2026 & enr$school_id %in% c(70L, 80L) & !is.na(enr$end_date) &
            enr$end_date > S2_START[5] & enr$end_status %in% c("TRMN", "TROS", "DROP"), ]
if (nrow(gone)) {
  hit <- head(gone$student_id, 3)
  sel <- roster$student_id %in% hit & roster$start_date >= S2_START[5]
  roster$end_date[sel] <- y_end[5]
  plant_log <- c(plant_log, sprintf("open roster after withdrawal: %s", paste(hit, collapse = ",")))
}

# ---- State's Oct 1 return for 2025-26 ---------------------------------------
snap <- oct1[oct1$school_year == 2026, ]
snap$state_id <- STU$state_id[match(snap$student_id, STU$student_id)]
# The state rejected records with an invalid grade, and one student it found
# enrolled full-time in another district on October 1.
rej_other <- pick(snap$student_id[snap$grade %in% c("03", "04")], 1)
state_ret <- snap[!is.na(snap$state_id) & snap$grade %in% c("KG", sprintf("%02d", 1:12)) &
                  snap$student_id != rej_other, ]
plant_log <- c(plant_log, sprintf("state rejected (other district): %d", rej_other))
state_ret <- merge(state_ret, schools[, c("school_id", "state_school_number")], by = "school_id")
# The district corrected two grade levels after submitting.
fix <- head(shuffle(which(state_ret$grade == "10")), 2)
for (f in fix) {
  sid <- state_ret$student_id[f]
  enr$grade[enr$student_id == sid & enr$school_year == 2026 & enr$grade == "10"] <- "11"
  oct1$grade[oct1$student_id == sid & oct1$school_year == 2026] <- "10"   # snapshot keeps what was sent
}
state_ret <- data.frame(school_year = 2026L, snapshot_date = as.Date("2025-10-01"),
  state_id = state_ret$state_id, state_school_number = state_ret$state_school_number,
  grade = state_ret$grade, accepted = 1L)
state_ret <- state_ret[order(state_ret$state_school_number, state_ret$state_id), ]

# ---- Users for row-level access --------------------------------------------
user_access <- data.frame(
  username = c("district.analyst", "research.director", "p.birchwood", "p.cedarpark", "p.riverbend",
               "p.prairieview", "p.linden", "p.oakgrove", "p.lakeview", "p.riverside", "p.horizon",
               "c.lakeview", "c.riverside", "attendance.team"),
  scope = c("District", "District", rep("School", 11), "District"),
  school_id = c(NA, NA, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 70L, 80L, NA),
  role = c("Analyst", "Director", rep("Principal", 9), "Counselor", "Counselor", "Attendance staff"))

# ---- Students table (public columns only) ----------------------------------
stu_out <- STU[STU$student_id %in% enr$student_id, c("student_id", "state_id", "last_name", "first_name",
  "birth_date", "gender", "hispanic_latino", "race_american_indian", "race_asian", "race_black",
  "race_pacific_islander", "race_white", "home_language", "entered_9th_year")]
stu_out$entered_9th_year[is.na(stu_out$entered_9th_year)] <- NA

# ---- Write -----------------------------------------------------------------
cal_out <- do.call(rbind, lapply(CAL, function(c) c[, c("school_year", "cal_date", "instructional", "day_note")]))
cal_out <- merge(data.frame(school_id = schools$school_id), cal_out)   # one calendar row per school
cal_out <- cal_out[order(cal_out$school_id, cal_out$cal_date), ]

att <- att[order(att$att_date, att$student_id), ]
w <- function(df, name) {
  for (nm in names(df)) if (inherits(df[[nm]], "Date")) df[[nm]] <- fmt(df[[nm]])
  write.csv(df, file.path(OUT, paste0(name, ".csv")), row.names = FALSE, na = "")
}
w(schools, "schools"); w(stu_out, "students"); w(enr, "enrollments"); w(cal_out, "calendar_days")
w(att, "attendance"); w(ays, "attendance_year_summary"); w(programs, "programs")
w(incidents, "incidents"); w(incident_students, "incident_students"); w(resolutions, "resolutions")
w(courses, "courses"); w(sections, "sections"); w(roster, "roster"); w(trans, "transcripts")
w(staff, "staff"); w(tests, "test_results"); w(oct1, "oct1_snapshot"); w(state_ret, "state_oct1_return")
w(user_access, "user_access")
writeLines(plant_log, file.path(OUT, "PLANTED_PROBLEMS.txt"))
cat("students:", nrow(stu_out), " enrollments:", nrow(enr), " attendance:", nrow(att),
    " transcripts:", nrow(trans), " incidents:", nrow(incidents), "\n")
cat(plant_log, sep = "\n")
