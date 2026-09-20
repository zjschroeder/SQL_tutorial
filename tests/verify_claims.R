# =============================================================================
# tests/verify_claims.R — every number the prose states, checked by a query
#
# Run from the project root:   Rscript tests/verify_claims.R
# Each entry: where the claim is made, what it says, the query, the value the
# prose gives. If the data changes (build_db.R), this says which sentences to
# rewrite. Add a row whenever a step quotes a figure.
# =============================================================================
suppressMessages({ library(DBI); library(RSQLite) })
con <- dbConnect(SQLite(), "data/institute.sqlite", flags = SQLITE_RO)
one <- function(sql) { r <- dbGetQuery(con, sql); if (nrow(r) == 1 && ncol(r) == 1) r[[1]] else nrow(r) }
claims <- list(
  # tab, claim, query (scalar, or a result whose ROW COUNT is the claim), stated value
  c("start",  "six tables",                         "SELECT COUNT(*) FROM sqlite_master WHERE type='table'", 6),
  c("select", "twelve researchers",                 "SELECT COUNT(*) FROM researchers", 12),
  c("select", "four job titles",                    "SELECT COUNT(DISTINCT title) FROM researchers", 4),
  c("select", "ten studies, seven columns",         "SELECT COUNT(*) FROM pragma_table_info('studies')", 7),
  c("where",  "three labs founded before 2000",     "SELECT COUNT(*) FROM labs WHERE founded_year < 2000", 3),
  c("where",  "seven PIs",                          "SELECT COUNT(*) FROM researchers WHERE title='PI'", 7),
  c("where",  "four senior PIs (start < 2010)",     "SELECT COUNT(*) FROM researchers WHERE title='PI' AND start_year < 2010", 4),
  c("where",  "title = \"title\" returns all twelve","SELECT COUNT(*) FROM researchers WHERE title = \"title\"", 12),
  c("where",  "title = 'pi' returns none",          "SELECT COUNT(*) FROM researchers WHERE title = 'pi'", 0),
  c("where",  "LIKE 'd%' finds the eight Daves",    "SELECT COUNT(*) FROM participants WHERE nickname LIKE 'd%'", 8),
  c("where",  "two undisclosed salaries",           "SELECT COUNT(*) FROM researchers WHERE salary IS NULL", 2),
  c("order",  "two studies from 2022",              "SELECT COUNT(*) FROM studies WHERE year = 2022", 2),
  c("order",  "two crickets aged 1",                "SELECT COUNT(*) FROM participants WHERE age = 1 AND species='cricket'", 2),
  c("order",  "six participants with NULL age",     "SELECT COUNT(*) FROM participants WHERE age IS NULL", 6),
  c("order",  "all three seagulls have NULL age",   "SELECT COUNT(*) FROM participants WHERE species='seagull' AND age IS NULL", 3),
  c("order",  "largest study has 180",              "SELECT MAX(sample_size) FROM studies", 180),
  c("group",  "payroll is 897000",                  "SELECT SUM(salary) FROM researchers", 897000),
  c("group",  "COUNT(salary) is 10",                "SELECT COUNT(salary) FROM researchers", 10),
  c("group",  "18 human participants",              "SELECT COUNT(*) FROM participants WHERE species='human'", 18),
  c("group",  "11 distinct human nicknames",        "SELECT COUNT(DISTINCT nickname) FROM participants WHERE species='human'", 11),
  c("group",  "since 2018: completed has four",     "SELECT COUNT(*) FROM studies WHERE year >= 2018 AND status='completed'", 4),
  c("group",  "since 2018: ongoing has three",      "SELECT COUNT(*) FROM studies WHERE year >= 2018 AND status='ongoing'", 3),
  c("group",  "abandoned study is from 2014",       "SELECT year FROM studies WHERE status='abandoned'", 2014),
  c("func",   "LENGTH('Lord Stridulation') = 17",   "SELECT LENGTH(nickname) FROM participants WHERE nickname='Lord Stridulation'", 17),
  c("func",   "seagull study enrolled 40 (sample_size)", "SELECT sample_size FROM studies WHERE study_id=1", 40),
  c("func",   "40 / 12 = 3",                        "SELECT 40 / 12", 3),
  c("func",   "mean sample size 52.9",              "SELECT AVG(sample_size) FROM studies", 52.9),
  c("func",   "ROUND(AVG(sample_size)) = 53",       "SELECT ROUND(AVG(sample_size)) FROM studies", 53),
  c("func",   "two small studies (<30)",            "SELECT COUNT(*) FROM studies WHERE sample_size < 30", 2),
  c("func",   "six medium (30-59)",                 "SELECT COUNT(*) FROM studies WHERE sample_size >= 30 AND sample_size < 60", 6),
  c("func",   "two large (>=60)",                   "SELECT COUNT(*) FROM studies WHERE sample_size >= 60", 2),
  c("func",   "cricket telepathy n = 8",            "SELECT sample_size FROM studies WHERE study_id=10", 8),
  c("func",   "CAST(3.9 AS INTEGER) = 3",           "SELECT CAST(3.9 AS INTEGER)", 3),
  c("func",   "x / 0 is NULL in SQLite",            "SELECT (1/0) IS NULL", 1),
  c("func",   "zero-filling drags the mean down by a sixth", "SELECT ROUND(1 - (SUM(salary)/12.0) / AVG(salary), 3) FROM researchers", round(1/6, 3)),
  c("func",   "six completed",                      "SELECT COUNT(*) FROM studies WHERE status='completed'", 6),
  c("func",   "three ongoing",                      "SELECT COUNT(*) FROM studies WHERE status='ongoing'", 3),
  c("func",   "eight participants under 30",        "SELECT COUNT(*) FROM participants WHERE age < 30", 8),
  c("func",   "ten at 30 and over",                 "SELECT COUNT(*) FROM participants WHERE age >= 30", 10),
  c("func",   "Eustachian has the earliest start_year", "SELECT researcher_id FROM researchers ORDER BY start_year LIMIT 1", 3),
  c("joins",  "comma join without WHERE: 60 rows",  "SELECT COUNT(*) FROM studies, labs", 60),
  c("joins",  "studies NATURAL JOIN researchers: 0 rows", "SELECT COUNT(*) FROM studies NATURAL JOIN researchers", 0),
  c("joins",  "89 enrollments",                     "SELECT COUNT(*) FROM enrollments", 89),
  c("joins",  "FULL OUTER JOIN researchers/labs: 12 rows", "SELECT COUNT(*) FROM researchers r FULL OUTER JOIN labs l ON r.lab_id=l.lab_id", 12),
  c("joins",  "15 lab pairings",                    "SELECT COUNT(*) FROM labs a CROSS JOIN labs b WHERE a.lab_id < b.lab_id", 15),
  c("joins",  "89 x 289 = 25721",                   "SELECT (SELECT COUNT(*) FROM enrollments) * (SELECT COUNT(*) FROM measurements)", 25721),
  c("joins",  "self-join without <: 23 rows",       "SELECT COUNT(*) FROM researchers a JOIN researchers b ON a.lab_id=b.lab_id", 23),
  c("joins",  "eleven researchers have a lab",      "SELECT COUNT(lab_id) FROM researchers", 11),
  c("joins",  "six real same-lab pairs",            "SELECT COUNT(*) FROM researchers a JOIN researchers b ON a.lab_id=b.lab_id AND a.researcher_id<b.researcher_id", 6),
  c("union",  "36 stacked names",                   "SELECT COUNT(*) FROM (SELECT full_name FROM researchers UNION ALL SELECT nickname FROM participants)", 36),
  c("union",  "study 1 enrolled 10",                "SELECT COUNT(*) FROM enrollments WHERE study_id=1", 10),
  c("union",  "study 7 enrolled 8",                 "SELECT COUNT(*) FROM enrollments WHERE study_id=7", 8),
  c("union",  "UNION of the two: 15",               "SELECT COUNT(*) FROM (SELECT participant_id FROM enrollments WHERE study_id=1 UNION SELECT participant_id FROM enrollments WHERE study_id=7)", 15),
  c("union",  "three in both",                      "SELECT COUNT(*) FROM (SELECT participant_id FROM enrollments WHERE study_id=1 INTERSECT SELECT participant_id FROM enrollments WHERE study_id=7)", 3),
  c("union",  "participant 4 dropped out of study 1", "SELECT dropped_out FROM enrollments WHERE study_id=1 AND participant_id=4", 1),
  c("union",  "nickname-only INTERSECT still gives 3", "SELECT COUNT(*) FROM (SELECT p.nickname FROM enrollments e JOIN participants p USING(participant_id) WHERE study_id=1 INTERSECT SELECT p.nickname FROM enrollments e JOIN participants p USING(participant_id) WHERE study_id=7)", 3),
  c("union",  "seven in study 1 only",              "SELECT COUNT(*) FROM (SELECT participant_id FROM enrollments WHERE study_id=1 EXCEPT SELECT participant_id FROM enrollments WHERE study_id=7)", 7),
  c("union",  "five in study 7 only",               "SELECT COUNT(*) FROM (SELECT participant_id FROM enrollments WHERE study_id=7 EXCEPT SELECT participant_id FROM enrollments WHERE study_id=1)", 5),
  c("union",  "nickname-only EXCEPT gives four",    "SELECT COUNT(*) FROM (SELECT p.nickname FROM enrollments e JOIN participants p USING(participant_id) WHERE study_id=1 EXCEPT SELECT p.nickname FROM enrollments e JOIN participants p USING(participant_id) WHERE study_id=7)", 4),
  c("union",  "three Daves only in study 1",        "SELECT COUNT(*) FROM enrollments e JOIN participants p USING(participant_id) WHERE study_id=1 AND nickname='Dave' AND participant_id NOT IN (SELECT participant_id FROM enrollments WHERE study_id=7)", 3),
  c("union",  "21 distinct years",                  "SELECT COUNT(*) FROM (SELECT founded_year FROM labs UNION SELECT start_year FROM researchers UNION SELECT year FROM studies)", 21),
  c("union",  "SQLite: 1 UNION 2 INTERSECT 2 is left-to-right (1 row)", "SELECT COUNT(*) FROM (SELECT 1 UNION SELECT 2 INTERSECT SELECT 2)", 1),
  c("subq",   "seven above-average earners",        "SELECT COUNT(*) FROM researchers WHERE salary > (SELECT AVG(salary) FROM researchers)", 7),
  c("subq",   "nine studies with an enrollee",      "SELECT COUNT(DISTINCT study_id) FROM enrollments", 9),
  c("subq",   "avg researchers per lab 1.83",       "SELECT ROUND(AVG(n),2) FROM (SELECT COUNT(*) n FROM researchers WHERE lab_id IS NOT NULL GROUP BY lab_id)", 1.83),
  c("subq",   "2015 is the one year (2014-2023) with no study", "WITH RECURSIVE y(y) AS (SELECT 2014 UNION ALL SELECT y+1 FROM y WHERE y<2023) SELECT y FROM y WHERE y NOT IN (SELECT year FROM studies)", 2015),
  c("windows","enrollment 2, 3-trial average at trial 3 = 6.41", "SELECT ROUND(AVG(value),2) FROM measurements WHERE enrollment_id=2 AND trial<=3", 6.41),
  c("windows","... at trial 4 = 6.18",              "SELECT ROUND(AVG(value),2) FROM measurements WHERE enrollment_id=2 AND trial BETWEEN 2 AND 4", 6.18),
  c("windows","default-frame running total reads 489 for both 2022 studies", "SELECT SUM(sample_size) FROM studies WHERE year <= 2022", 489),
  c("windows","ROWS frame gives 439 for the first 2022 study", "SELECT MIN(t) FROM (SELECT year, SUM(sample_size) OVER (ORDER BY year ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS t FROM studies) WHERE year = 2022", 439),
  c("windows","no enrollment repeats a trial number","SELECT COUNT(*) FROM (SELECT 1 FROM measurements GROUP BY enrollment_id, trial HAVING COUNT(*)>1)", 0),
  c("windows","seven top-earner rows",              "SELECT COUNT(*) FROM (SELECT lab_id FROM researchers WHERE salary IS NOT NULL GROUP BY lab_id)", 7),
  c("windows","Tern's gap to lab top = -87000",     "SELECT gap FROM (SELECT full_name, salary - FIRST_VALUE(salary) OVER (PARTITION BY lab_id ORDER BY salary DESC) AS gap FROM researchers WHERE salary IS NOT NULL) WHERE full_name = 'Wesley Tern'", -87000),
  c("changing","289 measurements",                  "SELECT COUNT(*) FROM measurements", 289),
  c("tables", "lab 6 has three researchers",        "SELECT COUNT(*) FROM researchers WHERE lab_id=6", 3),
  c("tables", "lab 6 has two studies",              "SELECT COUNT(*) FROM studies WHERE lab_id=6", 2),
  c("tables", "no duplicate (study, participant) enrollments", "SELECT COUNT(*) FROM (SELECT 1 FROM enrollments GROUP BY study_id, participant_id HAVING COUNT(*)>1)", 0),
  c("capstone","three labs founded before 2000 (old_labs task)", "SELECT COUNT(*) FROM labs WHERE founded_year < 2000", 3)
)
bad <- 0
for (cl in claims) {
  got <- tryCatch(one(cl[3]), error = function(e) paste("ERROR:", conditionMessage(e)))
  okk <- isTRUE(all.equal(as.numeric(got), as.numeric(cl[4]), tolerance = 1e-6))
  if (!okk) { bad <- bad + 1; cat(sprintf("MISMATCH [%s] %s: prose says %s, database says %s\n", cl[1], cl[2], cl[4], format(got))) }
}
dbDisconnect(con)
cat(sprintf("%d claims checked, %d mismatch(es)\n", length(claims), bad))
quit(status = if (bad) 1 else 0)
