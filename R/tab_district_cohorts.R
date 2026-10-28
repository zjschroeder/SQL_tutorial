# =============================================================================
# tab_district_cohorts.R — "Cohorts and outcomes" (School district, 5 of 7)
#
# Seven gated steps: defining a cohort; each student's last enrollment; the
# outcome from the exit code; the four-year graduation rate; credits and being
# on track; test results and their denominator; following the same students.
# Then a 10-question quest.
#
# COHORT_CTE is the class-of-2025 outcome computation the steps build.
# =============================================================================

COHORT_CTE <- "WITH cohort AS (                  -- entered 9th grade in 2021-22: the class of 2025
  SELECT student_id
  FROM students
  WHERE entered_9th_year = 2022
),
last_spell AS (                     -- each cohort member's most recent enrollment, through 2024-25
  SELECT e.student_id, e.school_year, e.end_status,
         ROW_NUMBER() OVER (PARTITION BY e.student_id
                            ORDER BY e.start_date DESC, e.enrollment_id DESC) AS rn
  FROM enrollments AS e
  JOIN cohort AS c ON c.student_id = e.student_id
  WHERE e.service_type = 'P'
    AND e.school_year <= 2025
),
outcome AS (                        -- what that last exit code means for the cohort
  SELECT l.student_id, l.end_status,
         CASE WHEN l.end_status = 'GRAD'        THEN 'Graduated'
              WHEN sc.documented_transfer = 1   THEN 'Transferred out (removed)'
              ELSE 'Did not graduate in 4 years'
         END AS outcome
  FROM last_spell AS l
  LEFT JOIN status_codes AS sc
         ON sc.code = l.end_status AND sc.code_type = 'end'
  WHERE l.rn = 1
)"

coh_sql <- function(tail) paste0(COHORT_CTE, "\n", tail)

DCOH_STEP_TITLES <- c(
  "Defining a cohort",
  "Each student's last enrollment",
  "From exit code to outcome",
  "The four-year graduation rate",
  "Credits and being on track",
  "Test results and their denominator",
  "Following the same students"
)

DCOH_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("cohort"), " is a fixed set of units defined by a shared ",
        "starting event, followed over time. Membership is decided once; outcomes are ",
        "attached to members, never the reverse."),
      plain = tagList("A graduating class is everyone who started 9th grade in the same year. ",
        "Students who transfer in later join the class they would have been in; nobody ",
        "leaves it except in specific, documented ways."),
      district = tagList(kbd("students.entered_9th_year"), " records the school year each ",
        "student first entered 9th grade, anywhere. The 128 students with ",
        kbd("entered_9th_year = 2022"), " are the class of 2025, including students who ",
        "transferred in during high school.")),
    dsql_code("SELECT entered_9th_year, COUNT(*) AS students\nFROM students\nWHERE entered_9th_year BETWEEN 2019 AND 2025\nGROUP BY entered_9th_year;"),
    dsql_editor("dzdcoh1", "SELECT entered_9th_year, COUNT(*) AS students\nFROM students\nWHERE entered_9th_year BETWEEN 2019 AND 2025\nGROUP BY entered_9th_year;"),
    callout_warm(tags$b("Why the cohort is set by the first year of 9th grade. "),
      "A student who repeats 9th grade is still in the class that started with them. If ",
      "cohorts were defined by “in 12th grade this year,” students who fell behind ",
      "would drop out of the denominator before anyone counted them, and the graduation rate ",
      "would rise for the worst possible reason."),
    help_details("Where this field comes from",
      tagList("Some SIS products store a graduation cohort year or a 9th-grade entry year; ",
        "others leave you to derive it from the first 9th-grade enrollment, which fails for ",
        "students who started 9th grade in another district. For an official rate, the state ",
        "assigns the cohort using statewide records. A district's own version is for ",
        "monitoring, and it will differ from the state's in explainable ways."))
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList(kbd("ROW_NUMBER()"), " over a partition by student, ordered by date ",
        "descending with a unique tiebreaker, numbers each student's enrollments newest first; ",
        "keeping row 1 selects exactly one per student."),
      plain = tagList("Each student has several enrollment rows. The one that says how their ",
        "story ended (for now) is the latest. Number each student's rows from newest to oldest, ",
        "then keep number 1."),
      district = tagList("Through 2024-25, the class of 2025's last enrollments end in ",
        kbd("GRAD"), ", in a transfer code, in a dropout code, or in ", kbd("EOY"), " (end of ",
        "year, still enrolled): 13 members of the class are back for a fifth year.")),
    dsql_code("SELECT e.student_id, e.school_year, e.end_status,\n       ROW_NUMBER() OVER (PARTITION BY e.student_id\n                          ORDER BY e.start_date DESC, e.enrollment_id DESC) AS rn\nFROM enrollments AS e\nWHERE e.student_id IN (SELECT student_id FROM students WHERE entered_9th_year = 2022)\n  AND e.service_type = 'P'\n  AND e.school_year <= 2025\nORDER BY e.student_id, rn\nLIMIT 12;"),
    dsql_editor("dzdcoh2", "SELECT e.student_id, e.school_year, e.end_status,\n       ROW_NUMBER() OVER (PARTITION BY e.student_id\n                          ORDER BY e.start_date DESC, e.enrollment_id DESC) AS rn\nFROM enrollments AS e\nWHERE e.student_id IN (SELECT student_id FROM students WHERE entered_9th_year = 2022)\n  AND e.service_type = 'P'\n  AND e.school_year <= 2025\nORDER BY e.student_id, rn\nLIMIT 12;", height = "200px"),
    callout_warm(tags$b("The tiebreaker is not decoration. "),
      "Two enrollments can start on the same day (a correction, a same-day transfer). Without ",
      kbd("enrollment_id DESC"), " the database may return either as row 1, and may choose ",
      "differently on another day. A cohort file that changes when nobody changed the data ",
      "is very hard to explain to a board."),
    help_details("Why not MAX(start_date)?",
      tagList(kbd("MAX"), " gives the latest date but not the rest of that row; joining back ",
        "on the date returns two rows when two enrollments share it. ", kbd("ROW_NUMBER"),
        " keeps the whole row and guarantees one."))
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("Outcomes are classified by a lookup on the exit code. The lookup ",
        "table, not the query, holds the business rule (which exits remove a student from the ",
        "cohort), so a rule change is a data change."),
      plain = tagList("Graduated is easy. For everything else, the question is whether the ",
        "student left with evidence of going somewhere else: a records request from another ",
        "district, a move out of state or country. Those students leave the cohort. Dropouts, ",
        "unknown exits, and students still enrolled stay in it, as non-graduates."),
      district = tagList("Class of 2025: 81 graduated; 27 left with a documented transfer ",
        "(18 to another Minnesota district, 8 out of state, 1 out of the country); 20 did not ",
        "graduate in four years (13 still enrolled, 6 dropped out, 1 whereabouts unknown).")),
    dsql_code(coh_sql("SELECT outcome, end_status, COUNT(*) AS students\nFROM outcome\nGROUP BY outcome, end_status\nORDER BY outcome, end_status;")),
    dsql_editor("dzdcoh3", coh_sql("SELECT outcome, end_status, COUNT(*) AS students\nFROM outcome\nGROUP BY outcome, end_status\nORDER BY outcome, end_status;"), height = "340px"),
    p(style = "line-height:1.6;",
      "The join to ", kbd("status_codes"), " matches both key columns. ", kbd("INTR"),
      " exists as a start code and as an end code, and a join on ", kbd("code"), " alone would ",
      "turn one row into two for any student whose last code is ", kbd("INTR"), ". It is a ",
      kbd("LEFT JOIN"), " so that a missing end code (NULL) still produces a row, classified ",
      "as not graduating, which is what an unresolved exit is until someone resolves it."),
    help_details("UNKN versus DROP",
      tagList("A student who stops attending and whose new school never requests records is ",
        "“whereabouts unknown.” For the graduation rate that is the same as a ",
        "dropout. Finding where these students went (a records request from another district ",
        "turns up months later) is some of the most valuable data work a district does, ",
        "because each one found moves from the numerator's shortfall to the documented-transfer ",
        "column."))
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("The ", tags$b("adjusted cohort graduation rate"), " (ACGR) divides ",
        "graduates with a regular diploma within four years by the cohort, adjusted: plus ",
        "transfers in, minus documented transfers out, emigrants, and deaths."),
      plain = tagList("Graduates divided by the class, after taking out the students who left ",
        "with somewhere to go. Everyone else stays in the bottom of the fraction."),
      district = tagList("Class of 2025: 81 ÷ (128 − 27) = 81 ÷ 101 = 80.2%. ",
        "The class of 2024, computed the same way through 2023-24, was 78.9%.")),
    dsql_code(coh_sql("SELECT SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END) AS graduates,\n       SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END) AS adjusted_cohort,\n       ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3) AS four_year_rate\nFROM outcome;")),
    dsql_editor("dzdcoh4", coh_sql("SELECT SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END) AS graduates,\n       SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END) AS adjusted_cohort,\n       ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3) AS four_year_rate\nFROM outcome;"), height = "360px"),
    callout_warm(tags$b("Thirteen students still in school count against the four-year rate. "),
      "That is by design: the rate measures graduating on time. Minnesota also publishes ",
      "extended rates (five years and more) that credit those students when they finish. The ",
      "class of 2025's five-year outcome will be known after June 2026."),
    tsql_note("SELECT CAST(SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END) AS decimal(9,4))\n       / NULLIF(SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 0) AS four_year_rate\nFROM outcome;",
      kbd("NULLIF(x, 0)"), " turns a zero denominator into NULL, so an empty group returns NULL ",
      "instead of an error. SQLite returns NULL for division by zero on its own; SQL Server ",
      "stops the query.")
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("Leading indicators measured early in a cohort's course (credits earned ",
        "and course failures in 9th grade) predict graduation well enough to direct support ",
        "while it can still change the outcome."),
      plain = tagList("A four-year rate arrives four years too late to help anyone in it. ",
        "Whether a 9th grader finished the year with enough credits is known in June of 9th ",
        "grade, and it is one of the strongest early signals there is."),
      district = tagList("Graduation takes 21.5 credits: 5.375 a year on average. Of the 110 ",
        "students who started 9th grade in 2024-25 and were still enrolled in June, 98 earned ",
        "5.5 credits or more that year (89.1%).")),
    dsql_code("WITH ninth_graders AS (\n  SELECT s.student_id\n  FROM students AS s\n  WHERE s.entered_9th_year = 2025\n    AND EXISTS (SELECT 1 FROM enrollments AS e\n                WHERE e.student_id = s.student_id AND e.school_year = 2025\n                  AND e.service_type = 'P' AND e.end_status = 'EOY')\n),\ncredits AS (\n  SELECT n.student_id, COALESCE(SUM(t.credits_earned), 0) AS credits\n  FROM ninth_graders AS n\n  LEFT JOIN transcripts AS t\n         ON t.student_id = n.student_id AND t.school_year = 2025\n  GROUP BY n.student_id\n)\nSELECT COUNT(*) AS students,\n       SUM(CASE WHEN credits >= 5.5 THEN 1 ELSE 0 END) AS on_track,\n       ROUND(AVG(CASE WHEN credits >= 5.5 THEN 1.0 ELSE 0 END), 3) AS share_on_track\nFROM credits;"),
    dsql_editor("dzdcoh5", "SELECT t.student_id, t.school_year,\n       SUM(t.credits_earned) AS credits_that_year,\n       SUM(SUM(t.credits_earned)) OVER (PARTITION BY t.student_id\n                                        ORDER BY t.school_year) AS cumulative\nFROM transcripts AS t\nJOIN students AS s ON s.student_id = t.student_id\nWHERE s.entered_9th_year = 2023\nGROUP BY t.student_id, t.school_year\nORDER BY t.student_id, t.school_year\nLIMIT 20;", height = "220px"),
    p(style = "line-height:1.6;",
      "The editor's query is the running total of credits per student, by year: a window ",
      "function over a grouped result. ", kbd("SUM(SUM(credits_earned)) OVER (...)"), " looks ",
      "odd but reads in order: the inner ", kbd("SUM"), " totals each year (the GROUP BY), the ",
      "outer one accumulates those totals across years."),
    help_details("Transfer and converted credit",
      tagList("Transcripts hold rows with ", kbd("school_id"), " NULL: credit earned in another ",
        "district (", kbd("XFER"), ") and credit carried over when the district changed SIS ",
        "products (", kbd("LEGACY"), "). They count toward graduation. Leave them out of a ",
        "school's pass rates, which should describe the school's own courses."))
  ),

  # ---- 6 ----
  tagList(
    district_triad(
      formal = tagList("A proficiency rate's denominator decides what it measures: among ",
        "students tested, it describes performance; among all students expected to test, ",
        "it also reflects participation."),
      plain = tagList("Students who refused or missed the test have rows with no score. Leave ",
        "them out and the rate describes the students who tested. Count them as not proficient, ",
        "as Minnesota's accountability reports do, and the rate goes down."),
      district = tagList("Spring 2025: Mathematics 44.6% proficient among students tested, ",
        "41.6% counting all 753 records. Reading 39.0% against 36.9%.")),
    dsql_code("SELECT subject,\n       COUNT(*) AS records,\n       SUM(CASE WHEN test_status = 'Tested' THEN 1 ELSE 0 END) AS tested,\n       ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN test_status = 'Tested' THEN 1 ELSE 0 END), 3) AS pct_of_tested,\n       ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END)\n                 / COUNT(*), 3) AS pct_of_all\nFROM test_results\nWHERE school_year = 2025\nGROUP BY subject;"),
    dsql_editor("dzdcoh6", "SELECT grade_tested, test_status, COUNT(*) AS students\nFROM test_results\nWHERE school_year = 2025\n  AND subject = 'Mathematics'\nGROUP BY grade_tested, test_status\nORDER BY grade_tested, test_status;"),
    callout_warm(tags$b("AVG of a NULL-filled column quietly changes the denominator. "),
      kbd("AVG(scale_score)"), " averages only the scores that exist, which is right for an ",
      "average score. ", kbd("AVG(CASE WHEN achievement_level IN (...) THEN 1.0 ELSE 0 END)"),
      " counts a non-tested student as 0, because ", kbd("CASE"), " turns their NULL into the ",
      "ELSE branch. Same table, two denominators, and both can be correct if they are labeled."),
    help_details("Refusals by grade",
      tagList("The editor's query shows where the non-tested records are. Refusals cluster in ",
        "the oldest grade tested, which is common in real data too, so a high school's ",
        "all-students rate can fall well below its tested-students rate. When a principal ",
        "disputes a proficiency figure, the denominator is the first thing to check."))
  ),

  # ---- 7 ----
  tagList(
    district_triad(
      formal = tagList(tags$b("Matched"), " (longitudinal) comparisons restrict to units observed ",
        "at both times, so change is within-student. ", tags$b("Cross-sectional"),
        " comparisons contrast different students in the same grade across years."),
      plain = tagList("“This year's 5th graders did better than last year's” compares ",
        "two groups of children. “These students improved from 4th to 5th grade” ",
        "follows the same children. Only the second is growth."),
      district = tagList("Scale scores here are built so that grade × 100 + 50 is the ",
        "proficiency cut, so subtracting grade × 100 puts two grades on the same footing. ",
        "Matching reading results from 2024 to 2025 by student shows the average change by ",
        "starting grade.")),
    dsql_code("SELECT a.grade_tested AS grade_2024,\n       COUNT(*) AS students,\n       ROUND(AVG((b.scale_score - CAST(b.grade_tested AS INTEGER) * 100)\n               - (a.scale_score - CAST(a.grade_tested AS INTEGER) * 100)), 2) AS avg_change\nFROM test_results AS a\nJOIN test_results AS b\n  ON b.student_id = a.student_id\n AND b.subject = a.subject\n AND b.school_year = a.school_year + 1\nWHERE a.subject = 'Reading'\n  AND a.school_year = 2024\n  AND a.test_status = 'Tested'\n  AND b.test_status = 'Tested'\nGROUP BY a.grade_tested;"),
    dsql_editor("dzdcoh7", "SELECT a.grade_tested AS grade_2024,\n       COUNT(*) AS students,\n       ROUND(AVG((b.scale_score - CAST(b.grade_tested AS INTEGER) * 100)\n               - (a.scale_score - CAST(a.grade_tested AS INTEGER) * 100)), 2) AS avg_change\nFROM test_results AS a\nJOIN test_results AS b\n  ON b.student_id = a.student_id\n AND b.subject = a.subject\n AND b.school_year = a.school_year + 1\nWHERE a.subject = 'Reading'\n  AND a.school_year = 2024\n  AND a.test_status = 'Tested'\n  AND b.test_status = 'Tested'\nGROUP BY a.grade_tested;", height = "260px"),
    p(style = "line-height:1.6;",
      "A self-join on student, subject, and consecutive years. Grade 8 has only a couple of ",
      "matches because reading is not tested in 9th grade: those students have no 2025 row. ",
      "Students who left, arrived, or did not test either year are missing too, and the ",
      "students who remain are not a random sample of the ones who started."),
    interview_note(
      p(style = "margin-bottom:6px;", "“How would you build a dataset to track each fall's ",
        "9th graders through graduation?” One row per student, keyed by the state ID, with ",
        "the cohort fixed at entry and attributes as of entry (school, program status), then ",
        "yearly status from each year's frozen snapshot, credits from transcripts, and a final ",
        "outcome from the exit code via a lookup of which codes are documented transfers."),
      p(style = "margin-bottom:0;", "Say what you would document: the cohort rule, the source ",
        "of each column, the as-of date, and why the result will differ from the state's rate ",
        "(the state can see students who enrolled in other districts; you cannot)."))
  )
)

DCOH_DQUEST <- list(
  list(prompt = "Size of each entering 9th-grade cohort, 2019 through 2025: return entered_9th_year and students.",
       ref = "SELECT entered_9th_year, COUNT(*) AS students FROM students WHERE entered_9th_year BETWEEN 2019 AND 2025 GROUP BY entered_9th_year;",
       solution = "SELECT entered_9th_year, COUNT(*) AS students\nFROM students\nWHERE entered_9th_year BETWEEN 2019 AND 2025\nGROUP BY entered_9th_year;"),
  list(prompt = paste0("For each member of the class of 2025 (entered_9th_year = 2022), return their most ",
                       "recent primary enrollment through school_year 2025: student_id, school_year, ",
                       "end_status. Break ties on start_date with the larger enrollment_id."),
       ref = "SELECT student_id, school_year, end_status FROM (SELECT e.student_id, e.school_year, e.end_status, ROW_NUMBER() OVER (PARTITION BY e.student_id ORDER BY e.start_date DESC, e.enrollment_id DESC) AS rn FROM enrollments e JOIN students s ON s.student_id = e.student_id WHERE s.entered_9th_year = 2022 AND e.service_type = 'P' AND e.school_year <= 2025) WHERE rn = 1;",
       solution = "SELECT student_id, school_year, end_status\nFROM (\n  SELECT e.student_id, e.school_year, e.end_status,\n         ROW_NUMBER() OVER (PARTITION BY e.student_id\n                            ORDER BY e.start_date DESC, e.enrollment_id DESC) AS rn\n  FROM enrollments AS e\n  JOIN students AS s ON s.student_id = e.student_id\n  WHERE s.entered_9th_year = 2022\n    AND e.service_type = 'P'\n    AND e.school_year <= 2025\n)\nWHERE rn = 1;"),
  list(prompt = "Using COHORT_CTE (step 3), count the class of 2025 by outcome: return outcome and students.",
       ref = coh_sql("SELECT outcome, COUNT(*) AS students FROM outcome GROUP BY outcome;"),
       solution = coh_sql("SELECT outcome, COUNT(*) AS students\nFROM outcome\nGROUP BY outcome;")),
  list(prompt = paste0("The class of 2025's four-year adjusted cohort graduation rate, rounded to 3 ",
                       "decimals. One column, four_year_rate."),
       ref = coh_sql("SELECT ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END) / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3) AS four_year_rate FROM outcome;"),
       solution = coh_sql("SELECT ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3)\n       AS four_year_rate\nFROM outcome;")),
  list(prompt = paste0("The same rate for the class of 2024: students with entered_9th_year = 2021, ",
                       "enrollments through school_year 2024. One column, four_year_rate, rounded to 3."),
       ref = sub("school_year <= 2025", "school_year <= 2024", sub("entered_9th_year = 2022", "entered_9th_year = 2021", coh_sql("SELECT ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END) / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3) AS four_year_rate FROM outcome;"), fixed = TRUE), fixed = TRUE),
       solution = sub("school_year <= 2025", "school_year <= 2024", sub("entered_9th_year = 2022", "entered_9th_year = 2021", coh_sql("SELECT ROUND(1.0 * SUM(CASE WHEN outcome = 'Graduated' THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN outcome <> 'Transferred out (removed)' THEN 1 ELSE 0 END), 3)\n       AS four_year_rate\nFROM outcome;"), fixed = TRUE), fixed = TRUE)),
  list(prompt = paste0("Credits earned in 2024-25 (school_year 2025) by every student with ",
                       "entered_9th_year = 2025, with 0 for students who earned none: return student_id ",
                       "and credits."),
       ref = "SELECT s.student_id, COALESCE(SUM(t.credits_earned), 0) AS credits FROM students s LEFT JOIN transcripts t ON t.student_id = s.student_id AND t.school_year = 2025 WHERE s.entered_9th_year = 2025 GROUP BY s.student_id;",
       solution = "SELECT s.student_id, COALESCE(SUM(t.credits_earned), 0) AS credits\nFROM students AS s\nLEFT JOIN transcripts AS t\n       ON t.student_id = s.student_id\n      AND t.school_year = 2025\nWHERE s.entered_9th_year = 2025\nGROUP BY s.student_id;"),
  list(prompt = paste0("Of the students who entered 9th grade in 2024-25 and ended that year enrolled ",
                       "(a 2025 primary enrollment with end_status 'EOY'), what share earned 5.5 credits or ",
                       "more that year? One column, share_on_track, rounded to 3."),
       ref = "WITH n AS (SELECT s.student_id FROM students s WHERE s.entered_9th_year = 2025 AND EXISTS (SELECT 1 FROM enrollments e WHERE e.student_id = s.student_id AND e.school_year = 2025 AND e.service_type = 'P' AND e.end_status = 'EOY')), c AS (SELECT n.student_id, COALESCE(SUM(t.credits_earned), 0) AS credits FROM n LEFT JOIN transcripts t ON t.student_id = n.student_id AND t.school_year = 2025 GROUP BY n.student_id) SELECT ROUND(AVG(CASE WHEN credits >= 5.5 THEN 1.0 ELSE 0 END), 3) AS share_on_track FROM c;",
       solution = "WITH ninth_graders AS (\n  SELECT s.student_id\n  FROM students AS s\n  WHERE s.entered_9th_year = 2025\n    AND EXISTS (SELECT 1 FROM enrollments AS e\n                WHERE e.student_id = s.student_id AND e.school_year = 2025\n                  AND e.service_type = 'P' AND e.end_status = 'EOY')\n),\ncredits AS (\n  SELECT n.student_id, COALESCE(SUM(t.credits_earned), 0) AS credits\n  FROM ninth_graders AS n\n  LEFT JOIN transcripts AS t\n         ON t.student_id = n.student_id AND t.school_year = 2025\n  GROUP BY n.student_id\n)\nSELECT ROUND(AVG(CASE WHEN credits >= 5.5 THEN 1.0 ELSE 0 END), 3) AS share_on_track\nFROM credits;"),
  list(prompt = paste0("For students who entered 9th grade in 2023, return student_id, school_year, ",
                       "credits_that_year (sum of credits_earned), and cumulative (running total across ",
                       "years, per student)."),
       ref = "SELECT t.student_id, t.school_year, SUM(t.credits_earned) AS credits_that_year, SUM(SUM(t.credits_earned)) OVER (PARTITION BY t.student_id ORDER BY t.school_year) AS cumulative FROM transcripts t JOIN students s ON s.student_id = t.student_id WHERE s.entered_9th_year = 2023 GROUP BY t.student_id, t.school_year;",
       solution = "SELECT t.student_id, t.school_year,\n       SUM(t.credits_earned) AS credits_that_year,\n       SUM(SUM(t.credits_earned)) OVER (PARTITION BY t.student_id\n                                        ORDER BY t.school_year) AS cumulative\nFROM transcripts AS t\nJOIN students AS s ON s.student_id = t.student_id\nWHERE s.entered_9th_year = 2023\nGROUP BY t.student_id, t.school_year;"),
  list(prompt = paste0("Spring 2025 proficiency (Meets or Exceeds) by subject, both ways: return subject, ",
                       "pct_of_tested, and pct_of_all, each rounded to 3."),
       ref = "SELECT subject, ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END) / SUM(CASE WHEN test_status = 'Tested' THEN 1 ELSE 0 END), 3) AS pct_of_tested, ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END) / COUNT(*), 3) AS pct_of_all FROM test_results WHERE school_year = 2025 GROUP BY subject;",
       solution = "SELECT subject,\n       ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END)\n                 / SUM(CASE WHEN test_status = 'Tested' THEN 1 ELSE 0 END), 3) AS pct_of_tested,\n       ROUND(1.0 * SUM(CASE WHEN achievement_level IN ('Meets', 'Exceeds') THEN 1 ELSE 0 END)\n                 / COUNT(*), 3) AS pct_of_all\nFROM test_results\nWHERE school_year = 2025\nGROUP BY subject;"),
  list(prompt = paste0("Matched growth in Mathematics from 2024 to 2025, as in step 7: by 2024 grade, ",
                       "return grade_2024, students, and avg_change rounded to 2."),
       ref = "SELECT a.grade_tested AS grade_2024, COUNT(*) AS students, ROUND(AVG((b.scale_score - CAST(b.grade_tested AS INTEGER) * 100) - (a.scale_score - CAST(a.grade_tested AS INTEGER) * 100)), 2) AS avg_change FROM test_results a JOIN test_results b ON b.student_id = a.student_id AND b.subject = a.subject AND b.school_year = a.school_year + 1 WHERE a.subject = 'Mathematics' AND a.school_year = 2024 AND a.test_status = 'Tested' AND b.test_status = 'Tested' GROUP BY a.grade_tested;",
       solution = "SELECT a.grade_tested AS grade_2024,\n       COUNT(*) AS students,\n       ROUND(AVG((b.scale_score - CAST(b.grade_tested AS INTEGER) * 100)\n               - (a.scale_score - CAST(a.grade_tested AS INTEGER) * 100)), 2) AS avg_change\nFROM test_results AS a\nJOIN test_results AS b\n  ON b.student_id = a.student_id\n AND b.subject = a.subject\n AND b.school_year = a.school_year + 1\nWHERE a.subject = 'Mathematics'\n  AND a.school_year = 2024\n  AND a.test_status = 'Tested'\n  AND b.test_status = 'Tested'\nGROUP BY a.grade_tested;")
)

source_district_cohorts_server <- district_tab_server(
  key = "dcoh",
  step_titles = DCOH_STEP_TITLES,
  step_bodies = DCOH_STEPS,
  n_demo = 7,
  header = function() district_header(
    "Cohorts and outcomes", eyebrow = "School district · 5 of 7",
    p(style = "margin-bottom:6px;",
      "Outcomes need a population fixed in advance and followed over time. This tab builds ",
      "the four-year graduation rate the way the federal definition does (a cohort set by the ",
      "first year of 9th grade, adjusted only for documented transfers), then the early signal ",
      "that predicts it (credits in 9th grade), and finishes with test results: what the ",
      "denominator does to a proficiency rate, and the difference between comparing grades and ",
      "following students."),
    p(style = "margin-bottom:0;",
      "Uses window functions (", kbd("ROW_NUMBER"), ", a running ", kbd("SUM"), "), CTEs, a ",
      "lookup join on a two-column key, and a self-join.")),
  quest = DCOH_DQUEST,
  quest_title = "Ten cohort and outcome questions",
  quest_intro = "Fix the population first, then attach outcomes to it. Never the other way around."
)
