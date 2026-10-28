# =============================================================================
# tab_district_attendance.R — "Attendance" (School district, 3 of 7)
#
# Seven gated steps: from exceptions to school days (the calendar join); what
# counts as absent; one row per student; three district numbers (attendance
# rate, consistent attendance, chronic absence); the line at 90%; groups and
# years; students who have stopped coming. Then a 10-question quest.
#
# ATT_CTE is the computation the steps build up, written once here so every
# step and solution shows exactly the same SQL (tests/verify_claims.R checks the
# quoted numbers with the same text).
# =============================================================================

ATT_CTE <- "WITH enrolled_days AS (        -- one row per student per school per school day enrolled
  SELECT e.student_id, e.school_id, c.cal_date
  FROM enrollments AS e
  JOIN calendar_days AS c
    ON c.school_id = e.school_id
   AND c.instructional = 1
   AND c.cal_date >= e.start_date
   AND c.cal_date <= COALESCE(e.end_date, '2026-03-31')
  WHERE e.school_year = 2026
    AND e.service_type = 'P'
    AND c.cal_date <= '2026-03-31'
),
absence AS (                        -- how much of a day each attendance row costs
  SELECT student_id, school_id, att_date,
         CASE WHEN excuse = 'X' THEN 0      -- school activity: present
              WHEN status = 'A' THEN 1.0
              WHEN status = 'H' THEN 0.5
              ELSE 0                         -- tardy: present
         END AS days_absent
  FROM attendance
),
per_student AS (                    -- one row per student per school
  SELECT d.student_id, d.school_id,
         COUNT(*)                         AS days_enrolled,
         COALESCE(SUM(a.days_absent), 0)  AS days_absent
  FROM enrolled_days AS d
  LEFT JOIN absence AS a
    ON a.student_id = d.student_id
   AND a.school_id  = d.school_id
   AND a.att_date   = d.cal_date
  GROUP BY d.student_id, d.school_id
)"

att_sql <- function(tail) paste0(ATT_CTE, "\n", tail)

DATT_STEP_TITLES <- c(
  "From exceptions to school days",
  "What counts as absent",
  "One row per student",
  "Three numbers, one district",
  "The line at 90 percent",
  "Groups and years",
  "Students who have stopped coming"
)

DATT_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("range join"), " matches each enrollment interval to every ",
        "calendar row inside it, expanding intervals into one row per day. The result is the ",
        "denominator population for any per-day measure."),
      plain = tagList("Attendance rows say when a student was missing. To know how many days a ",
        "student ", tags$em("could"), " have been there, list the school days inside each ",
        "enrollment spell: join enrollments to the calendar on school and date range."),
      district = tagList("This year through today, the join produces 178,524 rows: one per ",
        "student per school per school day enrolled. A full-year student has 129 of them.")),
    dsql_code("SELECT e.student_id, e.school_id, c.cal_date\nFROM enrollments AS e\nJOIN calendar_days AS c\n  ON c.school_id = e.school_id\n AND c.instructional = 1\n AND c.cal_date >= e.start_date\n AND c.cal_date <= COALESCE(e.end_date, '2026-03-31')\nWHERE e.student_id = 100330\n  AND e.school_year = 2026\n  AND e.service_type = 'P'\n  AND c.cal_date <= '2026-03-31';"),
    dsql_editor("dzdatt1", "SELECT COUNT(*) AS days_enrolled\nFROM enrollments AS e\nJOIN calendar_days AS c\n  ON c.school_id = e.school_id\n AND c.instructional = 1\n AND c.cal_date >= e.start_date\n AND c.cal_date <= COALESCE(e.end_date, '2026-03-31')\nWHERE e.student_id = 100330\n  AND e.school_year = 2026\n  AND e.service_type = 'P'\n  AND c.cal_date <= '2026-03-31';"),
    p(style = "line-height:1.6;",
      kbd("COALESCE(e.end_date, '2026-03-31')"), " stands an open enrollment's missing end ",
      "date in with today, so the range stops at today instead of matching nothing (a ",
      "comparison with NULL is never true). The last condition stops a closed enrollment's ",
      "range at today too, for spells that end in the future (none do here, but schedules ",
      "do)."),
    tsql_note("SELECT COUNT(*) AS days_enrolled\nFROM enrollments AS e\nJOIN calendar_days AS c\n  ON c.school_id = e.school_id\n AND c.instructional = 1\n AND c.cal_date BETWEEN e.start_date AND COALESCE(e.end_date, @as_of)\nWHERE e.student_id = 100330 AND c.cal_date <= @as_of;",
      "Identical apart from the variable. ", kbd("BETWEEN"), " is inclusive at both ends in ",
      "both dialects, which matches inclusive end dates.")
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList("Each exception row is mapped to a quantity of absence by a ",
        kbd("CASE"), " expression. Business rules decide the mapping; the order of the ",
        kbd("WHEN"), " branches decides which rule wins when two apply."),
      plain = tagList("Not every attendance row is an absence. A tardy student was there. A ",
        "student at a school-sponsored event was there. A half-day absence is half a day. ",
        "Write those rules down once, in one place."),
      district = tagList("Student 100330 has 20 attendance rows this year: 13 tardies, one ",
        "school activity, one half day, and five full-day absences. By the rules, that is ",
        "5.5 days absent, not 20.")),
    dsql_code("SELECT att_date, status, excuse,\n       CASE WHEN excuse = 'X' THEN 0\n            WHEN status = 'A' THEN 1.0\n            WHEN status = 'H' THEN 0.5\n            ELSE 0\n       END AS days_absent\nFROM attendance\nWHERE student_id = 100330\nORDER BY att_date;"),
    dsql_editor("dzdatt2", "SELECT att_date, status, excuse,\n       CASE WHEN excuse = 'X' THEN 0\n            WHEN status = 'A' THEN 1.0\n            WHEN status = 'H' THEN 0.5\n            ELSE 0\n       END AS days_absent\nFROM attendance\nWHERE student_id = 100330\nORDER BY att_date;"),
    callout_warm(tags$b("The order of the branches is a rule. "),
      "The exempt test comes first because an exempt row may still carry status ", kbd("A"),
      ". Put ", kbd("WHEN status = 'A'"), " first and the field trip counts as an absence. ",
      kbd("CASE"), " stops at the first true branch."),
    help_details("Excused absences count",
      tagList("Minnesota's attendance measures count excused and unexcused absences alike, ",
        "and count out-of-school suspension as absence. Excused versus unexcused matters for ",
        "truancy, a separate legal process with its own counts of unexcused absences. If a ",
        "report filters to unexcused absences only, it is a truancy report, whatever it is called."))
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("Left-joining the per-day absence quantities to the per-day population ",
        "and aggregating by student yields enrolled days and absent days at the student ",
        "grain. Days with no exception row contribute 0 through ", kbd("COALESCE"), "."),
      plain = tagList("Attach each day's absence (if any) to each day enrolled, then add up by ",
        "student. A LEFT JOIN keeps the days with no attendance row, which are the days the ",
        "student was present."),
      district = tagList("The result has 1,461 rows: one per student per school. A student ",
        "who changed schools has a row at each, with that school's days, which is how ",
        "attendance is reported by school.")),
    dsql_code(att_sql("SELECT *\nFROM per_student\nWHERE school_id = 30\nORDER BY days_absent DESC\nLIMIT 10;")),
    dsql_editor("dzdatt3", att_sql("SELECT *\nFROM per_student\nWHERE school_id = 30\nORDER BY days_absent DESC\nLIMIT 10;"), height = "300px"),
    callout_warm(tags$b("Why LEFT JOIN, and why COALESCE. "),
      "An inner join to ", kbd("absence"), " would keep only days with an attendance row, so ",
      kbd("COUNT(*)"), " would count absences instead of days enrolled, and a student with ",
      "perfect attendance would vanish. With the LEFT JOIN, that student's ",
      kbd("SUM(a.days_absent)"), " is NULL (a sum of nothing), which ", kbd("COALESCE"),
      " turns into 0."),
    help_details("Reading the three CTEs",
      tagList(kbd("enrolled_days"), " is the population, at one row per day. ", kbd("absence"),
        " is the rulebook from step 2. ", kbd("per_student"), " combines them. Every question ",
        "in the rest of this tab is a different SELECT on ", kbd("per_student"), ", so the ",
        "editors below start with the three CTEs already written."))
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("The ", tags$b("average daily attendance rate"), " pools days: total days ",
        "present over total days enrolled. ", tags$b("Consistent attendance"), " and ",
        tags$b("chronic absence"), " are proportions of students, each classified by their own ",
        "rate. Pooling weights students by days; proportions weight them equally."),
      plain = tagList("Asking “what share of seats were filled?” and “what share ",
        "of students came regularly?” gives different answers, because a few students ",
        "missing a lot barely move the first and fully count in the second."),
      district = tagList("This year so far: 91.9% of student-days attended. 74.8% of students ",
        "attended more than 90% of their days (consistent attendance). 25.2% missed 10% or ",
        "more (chronic absence): 368 student-school records.")),
    dsql_code(att_sql("SELECT ROUND(1 - SUM(days_absent) / SUM(days_enrolled), 3)                   AS attendance_rate,\n       ROUND(AVG(CASE WHEN (days_enrolled - days_absent) * 1.0 / days_enrolled > 0.9\n                      THEN 1.0 ELSE 0 END), 3)                                   AS consistent_attendance,\n       ROUND(AVG(CASE WHEN days_absent * 1.0 / days_enrolled >= 0.1\n                      THEN 1.0 ELSE 0 END), 3)                                   AS chronic_absence\nFROM per_student;")),
    dsql_editor("dzdatt4", att_sql("SELECT school_id,\n       ROUND(1 - SUM(days_absent) / SUM(days_enrolled), 3) AS attendance_rate,\n       ROUND(AVG(CASE WHEN days_absent * 1.0 / days_enrolled >= 0.1\n                      THEN 1.0 ELSE 0 END), 3)        AS chronic_absence\nFROM per_student\nGROUP BY school_id;"), height = "300px"),
    p(style = "line-height:1.6;",
      "A 92% attendance rate sounds fine and a quarter of students chronically absent does not, ",
      "and they describe the same days. That is why Minnesota's accountability system, like ",
      "most states', measures students rather than seats."),
    interview_note(
      p(style = "margin-bottom:6px;", "“Our attendance rate is 92%, but the state says a ",
        "quarter of our students are chronically absent. Which is wrong?” Neither. Explain ",
        "with numbers: eight students who come 98% of the time and two who come 60% of the time ",
        "pool to 90.4%, yet 20% of them are chronically absent."),
      p(style = "margin-bottom:0;", "Then say which you would put in front of principals: the ",
        "student-level measure, with the list of students behind it, because it points at ",
        "children who need a call."))
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("Threshold classifications are sensitive at the boundary: ",
        kbd(">"), " versus ", kbd(">="), ", rounding before comparison, and the minimum ",
        "exposure (days enrolled) required for a rate to be classified at all."),
      plain = tagList("“More than 90%” is not “at least 90%.” A rate rounded ",
        "to 90.0% before the test can cross the line. And a student enrolled for a week who ",
        "missed one day is “80% attendance” on almost no evidence."),
      district = tagList("Student 102332 arrived on March 4: 15 days enrolled, 1.5 absent. ",
        "That is exactly 10%: chronically absent under “10% or more,” and not ",
        "consistently attending, since 90% is not more than 90%. Fourteen student-school ",
        "records have fewer than 20 days enrolled.")),
    dsql_code(att_sql("SELECT student_id, school_id, days_enrolled, days_absent,\n       days_absent * 1.0 / days_enrolled AS share_missed\nFROM per_student\nWHERE days_enrolled < 20\nORDER BY share_missed DESC, student_id;")),
    dsql_editor("dzdatt5", att_sql("SELECT student_id, school_id, days_enrolled, days_absent,\n       days_absent * 1.0 / days_enrolled AS share_missed\nFROM per_student\nWHERE days_enrolled < 20\nORDER BY share_missed DESC, student_id;"), height = "300px"),
    callout_warm(tags$b("Compare unrounded values. "),
      "Round for display, never before a test. ", kbd("ROUND(rate, 2) > 0.90"), " and ",
      kbd("rate > 0.90"), " disagree for every rate between 0.895 and 0.900."),
    help_details("Matching an official figure",
      tagList("A state's published rates come with business rules this tab simplifies: ",
        "a minimum number of days enrolled before a student counts, which enrollments count ",
        "at which school, and how part-time students are handled. Before you promise to ",
        "reproduce an official number, get the technical documentation for it, then expect ",
        "small differences that you can explain one rule at a time."))
  ),

  # ---- 6 ----
  tagList(
    district_triad(
      formal = tagList("Disaggregation joins a group attribute, evaluated at a stated reference ",
        "date, to the per-student rows, then aggregates within groups. Trends need the same ",
        "definition in every period; comparing a partial year with full years confounds ",
        "seasonality with change."),
      plain = tagList("To compare groups, attach each student's group, then compute the ",
        "same rate inside each group. To compare years, make sure each year was counted the same ",
        "way, and remember that a year in progress is not a whole year."),
      district = tagList("Chronic absence this year: 31.2% among students eligible for free or ",
        "reduced-price meals, 19.8% among students who are not. Over the prior four years ",
        "it fell from 39.8% (2021-22) to 26.9% (2024-25).")),
    dsql_code(att_sql(",\nfrl_now AS (\n  SELECT DISTINCT student_id\n  FROM programs\n  WHERE program = 'FRL'\n    AND start_date <= '2026-03-31'\n    AND (end_date IS NULL OR end_date >= '2026-03-31')\n)\nSELECT CASE WHEN f.student_id IS NOT NULL THEN 'FRL' ELSE 'Not FRL' END AS frl,\n       COUNT(*) AS students,\n       ROUND(AVG(CASE WHEN s.days_absent * 1.0 / s.days_enrolled >= 0.1\n                      THEN 1.0 ELSE 0 END), 3) AS chronic_absence\nFROM per_student AS s\nLEFT JOIN frl_now AS f ON f.student_id = s.student_id\nGROUP BY frl;")),
    dsql_editor("dzdatt6", "SELECT school_year,\n       COUNT(*) AS students,\n       ROUND(AVG(CASE WHEN days_absent * 1.0 / days_membership >= 0.1\n                      THEN 1.0 ELSE 0 END), 3) AS chronic_absence\nFROM attendance_year_summary\nGROUP BY school_year;"),
    callout_warm(tags$b("This year is not finished. "),
      "Absence usually rises through the winter and again in spring, so a rate through March ",
      "is not comparable with a full year's. Compare it with the same weeks of last year, ",
      "which needs last year's daily data, or wait for June. The year-end summaries here are ",
      "comparable with one another."),
    help_details("A gap is a finding about a system",
      tagList("A difference between groups tells you where to look, not why it is there. ",
        "Report it with counts, with the comparison group named, and next to the context ",
        "(school, grade, transportation, health) that the people acting on it will ask about. ",
        "The Discipline and groups tab has more on comparing groups fairly."))
  ),

  # ---- 7 ----
  tagList(
    district_triad(
      formal = tagList("For each open enrollment, the most recent date present is the maximum ",
        "enrolled day whose absence quantity is below 1. Open enrollments whose last date ",
        "present is old are candidates for unrecorded exits."),
      plain = tagList("A student who moved away, or stopped coming, still shows as enrolled ",
        "until someone ends the enrollment, and meanwhile is marked absent every day. Find the ",
        "open enrollments where the last day the student was actually there is weeks ago."),
      district = tagList("Eight open enrollments have no day present since 2026-03-12. Four ",
        "belong to students last present on 2026-01-30. The other four belong to students who ",
        "moved to another Prairie Lakes school; the old school never ended their enrollments and ",
        "has marked them absent every day since.")),
    dsql_code(att_sql(",\npresent_days AS (\n  SELECT d.student_id, d.school_id, d.cal_date\n  FROM enrolled_days AS d\n  LEFT JOIN absence AS a\n    ON a.student_id = d.student_id AND a.school_id = d.school_id\n   AND a.att_date = d.cal_date\n  WHERE COALESCE(a.days_absent, 0) < 1\n)\nSELECT e.student_id, e.school_id, MAX(p.cal_date) AS last_present\nFROM enrollments AS e\nLEFT JOIN present_days AS p\n  ON p.student_id = e.student_id AND p.school_id = e.school_id\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.end_date IS NULL\nGROUP BY e.student_id, e.school_id\nHAVING last_present IS NULL OR last_present < '2026-03-13'\nORDER BY last_present, e.student_id;")),
    dsql_editor("dzdatt7", att_sql(",\npresent_days AS (\n  SELECT d.student_id, d.school_id, d.cal_date\n  FROM enrolled_days AS d\n  LEFT JOIN absence AS a\n    ON a.student_id = d.student_id AND a.school_id = d.school_id\n   AND a.att_date = d.cal_date\n  WHERE COALESCE(a.days_absent, 0) < 1\n)\nSELECT e.student_id, e.school_id, MAX(p.cal_date) AS last_present\nFROM enrollments AS e\nLEFT JOIN present_days AS p\n  ON p.student_id = e.student_id AND p.school_id = e.school_id\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.end_date IS NULL\nGROUP BY e.student_id, e.school_id\nHAVING last_present IS NULL OR last_present < '2026-03-13'\nORDER BY last_present, e.student_id;"), height = "320px"),
    p(style = "line-height:1.6;",
      "Since 2026-03-13 there have been eight school days; spring break took the week of ",
      "March 23, which is why the cutoff comes from the calendar and not from counting back ",
      "on a wall calendar. The list is a to-do list for school staff: confirm where ",
      "each student is, then end the enrollment with the right code or reach the family."),
    interview_note(p(style = "margin-bottom:0;",
      "“How would you find students who are enrolled but no longer attending?” Describe ",
      "this query in words: days enrolled from the calendar, days present as days without a ",
      "full-day absence, the last such day per open enrollment, and a threshold in school days. ",
      "Then mention why it matters beyond attendance: these students inflate enrollment and ",
      "chronic absence counts, and some have left the district without anyone knowing where they went."))
  )
)

DATT_DQUEST <- list(
  list(prompt = paste0("How many school days has student 100330 been enrolled this year, through today ",
                       "(2026-03-31)? Return one column, days_enrolled."),
       ref = "SELECT COUNT(*) AS days_enrolled FROM enrollments e JOIN calendar_days c ON c.school_id = e.school_id AND c.instructional = 1 AND c.cal_date >= e.start_date AND c.cal_date <= COALESCE(e.end_date, '2026-03-31') WHERE e.student_id = 100330 AND e.school_year = 2026 AND e.service_type = 'P' AND c.cal_date <= '2026-03-31';",
       solution = "SELECT COUNT(*) AS days_enrolled\nFROM enrollments AS e\nJOIN calendar_days AS c\n  ON c.school_id = e.school_id\n AND c.instructional = 1\n AND c.cal_date >= e.start_date\n AND c.cal_date <= COALESCE(e.end_date, '2026-03-31')\nWHERE e.student_id = 100330\n  AND e.school_year = 2026\n  AND e.service_type = 'P'\n  AND c.cal_date <= '2026-03-31';"),
  list(prompt = paste0("By the rules in step 2, how many days has student 100330 been absent? ",
                       "Return one column, days_absent."),
       ref = "SELECT SUM(CASE WHEN excuse = 'X' THEN 0 WHEN status = 'A' THEN 1.0 WHEN status = 'H' THEN 0.5 ELSE 0 END) AS days_absent FROM attendance WHERE student_id = 100330;",
       solution = "SELECT SUM(CASE WHEN excuse = 'X' THEN 0\n                WHEN status = 'A' THEN 1.0\n                WHEN status = 'H' THEN 0.5\n                ELSE 0 END) AS days_absent\nFROM attendance\nWHERE student_id = 100330;"),
  list(prompt = paste0("Count full-day absences that are not exempt (status 'A', excuse other than 'X') ",
                       "at each school this year. Return school_id and absences."),
       ref = "SELECT school_id, COUNT(*) AS absences FROM attendance WHERE status = 'A' AND excuse <> 'X' GROUP BY school_id;",
       solution = "SELECT school_id, COUNT(*) AS absences\nFROM attendance\nWHERE status = 'A'\n  AND excuse <> 'X'\nGROUP BY school_id;"),
  list(prompt = paste0("Using the three CTEs, list every student at Riverbend Elementary (school_id 30) ",
                       "with days_enrolled and days_absent, including students with no absences. Return ",
                       "student_id, days_enrolled, days_absent."),
       ref = att_sql("SELECT student_id, days_enrolled, days_absent FROM per_student WHERE school_id = 30;"),
       solution = att_sql("SELECT student_id, days_enrolled, days_absent\nFROM per_student\nWHERE school_id = 30;")),
  list(prompt = paste0("What is the district's pooled attendance rate this year: 1 minus total days ",
                       "absent over total days enrolled, rounded to 3 decimals? One column, attendance_rate."),
       ref = att_sql("SELECT ROUND(1 - SUM(days_absent) / SUM(days_enrolled), 3) AS attendance_rate FROM per_student;"),
       solution = att_sql("SELECT ROUND(1 - SUM(days_absent) / SUM(days_enrolled), 3) AS attendance_rate\nFROM per_student;")),
  list(prompt = paste0("Consistent attendance by school: the share of student records attending more ",
                       "than 90% of their enrolled days, rounded to 3 decimals. Return school_id and ",
                       "consistent_attendance."),
       ref = att_sql("SELECT school_id, ROUND(AVG(CASE WHEN (days_enrolled - days_absent) * 1.0 / days_enrolled > 0.9 THEN 1.0 ELSE 0 END), 3) AS consistent_attendance FROM per_student GROUP BY school_id;"),
       solution = att_sql("SELECT school_id,\n       ROUND(AVG(CASE WHEN (days_enrolled - days_absent) * 1.0 / days_enrolled > 0.9\n                      THEN 1.0 ELSE 0 END), 3) AS consistent_attendance\nFROM per_student\nGROUP BY school_id;")),
  list(prompt = paste0("How many chronically absent student records (10% or more of enrolled days missed) ",
                       "does each school have? Return school_id and chronic."),
       ref = att_sql("SELECT school_id, SUM(CASE WHEN days_absent * 1.0 / days_enrolled >= 0.1 THEN 1 ELSE 0 END) AS chronic FROM per_student GROUP BY school_id;"),
       solution = att_sql("SELECT school_id,\n       SUM(CASE WHEN days_absent * 1.0 / days_enrolled >= 0.1\n                THEN 1 ELSE 0 END) AS chronic\nFROM per_student\nGROUP BY school_id;")),
  list(prompt = paste0("Chronic absence by FRL status as of today, as in step 6: return frl ('FRL' or ",
                       "'Not FRL'), students, and chronic_absence rounded to 3 decimals."),
       ref = att_sql(", frl_now AS (SELECT DISTINCT student_id FROM programs WHERE program = 'FRL' AND start_date <= '2026-03-31' AND (end_date IS NULL OR end_date >= '2026-03-31')) SELECT CASE WHEN f.student_id IS NOT NULL THEN 'FRL' ELSE 'Not FRL' END AS frl, COUNT(*) AS students, ROUND(AVG(CASE WHEN s.days_absent * 1.0 / s.days_enrolled >= 0.1 THEN 1.0 ELSE 0 END), 3) AS chronic_absence FROM per_student s LEFT JOIN frl_now f ON f.student_id = s.student_id GROUP BY frl;"),
       solution = att_sql(",\nfrl_now AS (\n  SELECT DISTINCT student_id\n  FROM programs\n  WHERE program = 'FRL'\n    AND start_date <= '2026-03-31'\n    AND (end_date IS NULL OR end_date >= '2026-03-31')\n)\nSELECT CASE WHEN f.student_id IS NOT NULL THEN 'FRL' ELSE 'Not FRL' END AS frl,\n       COUNT(*) AS students,\n       ROUND(AVG(CASE WHEN s.days_absent * 1.0 / s.days_enrolled >= 0.1\n                      THEN 1.0 ELSE 0 END), 3) AS chronic_absence\nFROM per_student AS s\nLEFT JOIN frl_now AS f ON f.student_id = s.student_id\nGROUP BY frl;")),
  list(prompt = paste0("From attendance_year_summary, the chronic absence rate for each prior year by ",
                       "grade band of the school: return school_year, school_type, and chronic_absence ",
                       "(share of rows with days_absent / days_membership >= 0.1), rounded to 3 decimals. ",
                       "Join schools for school_type."),
       ref = "SELECT y.school_year, s.school_type, ROUND(AVG(CASE WHEN y.days_absent * 1.0 / y.days_membership >= 0.1 THEN 1.0 ELSE 0 END), 3) AS chronic_absence FROM attendance_year_summary y JOIN schools s ON s.school_id = y.school_id GROUP BY y.school_year, s.school_type;",
       solution = "SELECT y.school_year, s.school_type,\n       ROUND(AVG(CASE WHEN y.days_absent * 1.0 / y.days_membership >= 0.1\n                      THEN 1.0 ELSE 0 END), 3) AS chronic_absence\nFROM attendance_year_summary AS y\nJOIN schools AS s ON s.school_id = y.school_id\nGROUP BY y.school_year, s.school_type;"),
  list(prompt = paste0("Open enrollments (this year, primary, end_date NULL) whose last day present is ",
                       "before 2026-03-13, or who have never been present: return student_id, school_id, ",
                       "and last_present, as in step 7."),
       ref = att_sql(", present_days AS (SELECT d.student_id, d.school_id, d.cal_date FROM enrolled_days d LEFT JOIN absence a ON a.student_id = d.student_id AND a.school_id = d.school_id AND a.att_date = d.cal_date WHERE COALESCE(a.days_absent, 0) < 1) SELECT e.student_id, e.school_id, MAX(p.cal_date) AS last_present FROM enrollments e LEFT JOIN present_days p ON p.student_id = e.student_id AND p.school_id = e.school_id WHERE e.school_year = 2026 AND e.service_type = 'P' AND e.end_date IS NULL GROUP BY e.student_id, e.school_id HAVING last_present IS NULL OR last_present < '2026-03-13';"),
       solution = att_sql(",\npresent_days AS (\n  SELECT d.student_id, d.school_id, d.cal_date\n  FROM enrolled_days AS d\n  LEFT JOIN absence AS a\n    ON a.student_id = d.student_id AND a.school_id = d.school_id\n   AND a.att_date = d.cal_date\n  WHERE COALESCE(a.days_absent, 0) < 1\n)\nSELECT e.student_id, e.school_id, MAX(p.cal_date) AS last_present\nFROM enrollments AS e\nLEFT JOIN present_days AS p\n  ON p.student_id = e.student_id AND p.school_id = e.school_id\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.end_date IS NULL\nGROUP BY e.student_id, e.school_id\nHAVING last_present IS NULL OR last_present < '2026-03-13';"))
)

source_district_attendance_server <- district_tab_server(
  key = "datt",
  step_titles = DATT_STEP_TITLES,
  step_bodies = DATT_STEPS,
  n_demo = 7,
  header = function() district_header(
    "Attendance: from absences to rates", eyebrow = "School district · 3 of 7",
    p(style = "margin-bottom:6px;",
      "Attendance is the measure a district data team computes most often and argues about ",
      "most. The data makes it harder than it looks: the attendance table records only ",
      "absences, so the number of days each student could have attended has to be built from ",
      "enrollments and the calendar. This tab builds it, one CTE at a time, then computes the ",
      "three measures people quote, finds where they disagree, and uses the same machinery to ",
      "find students who have quietly stopped coming."),
    p(style = "margin-bottom:0;",
      "Uses ", kbd("JOIN"), " on a date range, ", kbd("CASE"), ", ", kbd("LEFT JOIN"),
      " with ", kbd("COALESCE"), ", and CTEs (", kbd("WITH"), "). The Subqueries tab teaches CTEs ",
      "if they are new.")),
  quest = DATT_DQUEST,
  quest_title = "Ten attendance questions",
  quest_intro = tagList("Questions 4 to 8 and 10 build on the three CTEs. Copy them from step 3 ",
    "into the editor and write the final SELECT; that is how a reporting team works too, with ",
    "the shared definition at the top and the question at the bottom.")
)
