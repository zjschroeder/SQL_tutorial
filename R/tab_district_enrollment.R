# =============================================================================
# tab_district_enrollment.R — "Enrollment counts" (School district, 2 of 7)
#
# Seven gated steps: point-in-time counts; windows and cumulative counts;
# primary enrollments and DISTINCT; zero cells (scaffolds); live data versus a
# frozen snapshot; student groups as of a date; from headcount to ADM. Then a
# 10-question quest on the district database.
# =============================================================================

DEN_STEP_TITLES <- c(
  "Who was enrolled on a date",
  "Over a window of time: cumulative counts",
  "Count students, not rows",
  "Zero is a number: keeping empty groups",
  "Live data and the frozen snapshot",
  "Student groups as of a date",
  "From headcount to ADM"
)

DEN_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("point-in-time"), " count selects the intervals that ",
        "contain a given date: ", kbd("start <= d"), " and (", kbd("end IS NULL"), " or ",
        kbd("end >= d"), "). The NULL test is required; without it every open interval fails ",
        "the comparison and drops out."),
      plain = tagList("To count who was here on October 1, keep each spell that had started ",
        "by then and had not yet ended. A spell with no end date has not ended, which SQL ",
        "will only understand if you say so."),
      district = tagList("Minnesota districts report enrollment as of October 1. On ",
        "2025-10-01, Prairie Lakes had 1,386 students enrolled, by its current data.")),
    dsql_code("SELECT school_id, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2025-10-01'\n  AND (end_date IS NULL OR end_date >= '2025-10-01')\nGROUP BY school_id;"),
    dsql_editor("dzden1", "SELECT school_id, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2025-10-01'\n  AND (end_date IS NULL OR end_date >= '2025-10-01')\nGROUP BY school_id;"),
    callout_warm(tags$b("The parentheses are load-bearing. "),
      "Without them, ", kbd("... AND end_date IS NULL OR end_date >= '2025-10-01'"),
      " groups as (everything before the OR) OR (", kbd("end_date >= '2025-10-01'"),
      "), which lets in every closed enrollment from any school, any year, that ended after ",
      "October 1. The count jumps, and nothing errors. Try removing them."),
    tsql_note("DECLARE @count_date date = '2025-10-01';\n\nSELECT school_id, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE service_type = 'P'\n  AND start_date <= @count_date\n  AND (end_date IS NULL OR end_date >= @count_date)\nGROUP BY school_id;",
      "The date appears twice, so it goes in a variable: change it once and the report ",
      "counts a different day. In real SQL Server tables the dates are ", kbd("date"),
      " columns, not text, but the comparisons read the same.")
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList("Two intervals [", kbd("s1"), ", ", kbd("e1"), "] and [", kbd("s2"), ", ",
        kbd("e2"), "] overlap when ", kbd("s1 <= e2"), " and ", kbd("s2 <= e1"),
        ". A point-in-time test is the special case where the second interval is one day long."),
      plain = tagList("“Who was enrolled at any point this year?” keeps every spell ",
        "that touched the window: started before the window closed, and ended (if it ended) ",
        "after the window opened."),
      district = tagList("1,386 were enrolled on October 1. 1,437 different students were ",
        "enrolled at some point between the first day of school and today. A count with no ",
        "date in its definition is almost always the larger one.")),
    dsql_code("SELECT COUNT(DISTINCT student_id) AS students_this_year\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2026-03-31'                        -- started before the window closed\n  AND (end_date IS NULL OR end_date >= '2025-09-02');   -- and was still open when it began"),
    dsql_editor("dzden2", "SELECT COUNT(DISTINCT student_id) AS students_this_year\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2026-03-31'\n  AND (end_date IS NULL OR end_date >= '2025-09-02');"),
    term_table(head = c("Count", "Definition"), rows = list(
      c("Snapshot (point in time)", "Enrolled on one date. Official counts, staffing, comparisons across years."),
      c("Cumulative", "Enrolled at any time in a window. How many students a school served; always at least as large."),
      c("Average (ADM)", "Membership days divided by days in the year. Drives funding; step 7."))),
    help_details("Why a school's cumulative count matters",
      tagList("A school with 200 students on October 1 and 260 over the year has turnover of ",
        "a quarter of its seats. Every one of the 60 arrived or left mid-year, with records to ",
        "transfer and catch-up to do. Reporting only the snapshot hides that work, which is why ",
        "mobility is usually reported beside it."))
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("A count's unit must match the question's unit. When the table's grain ",
        "is finer than the unit (spells versus students), ", kbd("COUNT(DISTINCT key)"),
        " restores the unit, and filters remove rows that are not instances of it."),
      plain = tagList("Two things inflate a student count: counting a student once per row, ",
        "and counting enrollments that are not the student's real one."),
      district = tagList("Today (2026-03-31) the table holds 1,399 open enrollment rows for ",
        "1,377 students. The 22 extra rows are 18 secondary enrollments (part-time credit ",
        "recovery) and four students with two primary enrollments at once, a data error the ",
        "Data quality tab tracks down.")),
    dsql_code("SELECT COUNT(*)                   AS rows_today,\n       COUNT(DISTINCT student_id) AS students_today,\n       SUM(service_type = 'S')    AS secondary_rows\nFROM enrollments\nWHERE school_year = 2026\n  AND start_date <= '2026-03-31'\n  AND (end_date IS NULL OR end_date >= '2026-03-31');"),
    dsql_editor("dzden3", "SELECT COUNT(*)                   AS rows_today,\n       COUNT(DISTINCT student_id) AS students_today,\n       SUM(service_type = 'S')    AS secondary_rows\nFROM enrollments\nWHERE school_year = 2026\n  AND start_date <= '2026-03-31'\n  AND (end_date IS NULL OR end_date >= '2026-03-31');"),
    callout_warm(tags$b("DISTINCT fixes the total but not the breakdown. "),
      "A student with two open primary enrollments, at two schools, is counted once ",
      "district-wide and once at each school. The school counts then add up to more than the ",
      "district total. When per-school numbers do not sum to the total, look for students in ",
      "two places at once."),
    help_details("SUM(service_type = 'S') ?",
      tagList("In SQLite a comparison is a number, 1 for true and 0 for false, so summing it ",
        "counts the rows where it holds. SQL Server does not allow this; write ",
        kbd("SUM(CASE WHEN service_type = 'S' THEN 1 ELSE 0 END)"), ", which works everywhere."))
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("Aggregation produces groups only for values present in the input. ",
        "To report every member of a domain, drive the query from a table of the domain ",
        "(a ", tags$b("scaffold"), ") and ", kbd("LEFT JOIN"), " the facts to it, counting a ",
        "column from the facts side."),
      plain = tagList("If no student arrived in grade 3, a grouped query has no grade 3 row ",
        "at all, and a reader cannot tell a zero from a missing value. Start from the list of ",
        "all grades (or schools), then attach the data."),
      district = tagList("Horizon had no new arrivals from out of state this year. Grouping ",
        "the arrivals leaves Horizon out; starting from ", kbd("schools"), " shows it with 0.")),
    dsql_code("SELECT s.school_name,\n       COUNT(DISTINCT e.student_id) AS arrived_from_out_of_state\nFROM schools AS s\nLEFT JOIN enrollments AS e\n       ON e.school_id = s.school_id\n      AND e.school_year = 2026\n      AND e.service_type = 'P'\n      AND e.start_status = 'OSIN'\nGROUP BY s.school_name;"),
    dsql_editor("dzden4", "SELECT s.school_name,\n       COUNT(DISTINCT e.student_id) AS arrived_from_out_of_state\nFROM schools AS s\nLEFT JOIN enrollments AS e\n       ON e.school_id = s.school_id\n      AND e.school_year = 2026\n      AND e.service_type = 'P'\n      AND e.start_status = 'OSIN'\nGROUP BY s.school_name;"),
    p(style = "line-height:1.6;",
      "Two details carry the weight. The filters on ", kbd("e"), " sit in the ", kbd("ON"),
      " clause; in ", kbd("WHERE"), " they would discard Horizon's NULL row and undo the ",
      "LEFT JOIN. And the count is of ", kbd("e.student_id"), ", which is NULL for Horizon, ",
      "so it counts 0; ", kbd("COUNT(*)"), " would count Horizon's single empty row as 1."),
    help_details("A two-way scaffold with CROSS JOIN",
      tagList("For a school-by-grade table where every high school shows all four grades, ",
        "build the grid first with ", kbd("CROSS JOIN"), " (every school paired with every ",
        "grade), keep the pairs that make sense, then LEFT JOIN the counts:",
        dsql_code("SELECT s.school_id, g.grade, COUNT(DISTINCT e.student_id) AS students\nFROM schools AS s\nCROSS JOIN grade_levels AS g\nLEFT JOIN enrollments AS e\n       ON e.school_id = s.school_id AND e.grade = g.grade\n      AND e.school_year = 2026 AND e.service_type = 'P'\n      AND e.start_date <= '2025-10-01'\n      AND (e.end_date IS NULL OR e.end_date >= '2025-10-01')\nWHERE s.school_type IN ('High', 'Alternative')\n  AND g.grade_band = 'High'\nGROUP BY s.school_id, g.grade;"))),
    interview_note(p(style = "margin-bottom:0;",
      "“A principal says grade 2 is missing from your report.” A good answer separates ",
      "two possibilities: there were no grade 2 students in the group (so the report should show ",
      "0), or the data has a problem. Then it fixes the first for good, with a scaffold, so ",
      "the question cannot come up again."))
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("Operational data is mutable: records are corrected after the fact. ",
        "A ", tags$b("snapshot"), " preserves the state at a moment so that published figures ",
        "remain reproducible."),
      plain = tagList("The SIS changes the past. Fix an enrollment in November and October 1 ",
        "looks different than it did in October. The number the district reported in October ",
        "is kept separately, as it was, in a frozen copy."),
      district = tagList(kbd("oct1_snapshot"), " says 1,390 students were enrolled on ",
        "2025-10-01. The live data now says 1,386. The difference is four no-shows: students ",
        "enrolled for the first day who never came, recorded (late) with end code ",
        kbd("NOSH"), " and end date 2025-09-11.")),
    dsql_code("SELECT s.student_id, s.school_id, s.grade\nFROM oct1_snapshot AS s\nWHERE s.school_year = 2026\n  AND NOT EXISTS (\n        SELECT 1 FROM enrollments AS e\n        WHERE e.student_id = s.student_id\n          AND e.service_type = 'P'\n          AND e.start_date <= '2025-10-01'\n          AND (e.end_date IS NULL OR e.end_date >= '2025-10-01'));"),
    dsql_editor("dzden5", "SELECT school_year, COUNT(*) AS in_snapshot\nFROM oct1_snapshot\nGROUP BY school_year;"),
    callout_warm(tags$b("Which number is right? "),
      "Both, for different questions. To compare this year with last year, use the snapshots: ",
      "each is what was reported, and each was frozen the same way. To answer “who was ",
      "really here on October 1,” use the corrected live data. What you must not do is ",
      "compare last year's snapshot with this year's live data and call the difference a trend."),
    help_details("Snapshots in practice",
      tagList("Districts typically freeze a copy at each state reporting deadline and keep ",
        "it alongside the live system, often in a reporting database or warehouse. When a ",
        "board member quotes last October's enrollment and your query says something else, ",
        "the first question is which of the two you each used."))
  ),

  # ---- 6 ----
  tagList(
    district_triad(
      formal = tagList("Group membership with start and end dates must be evaluated at the ",
        "same reference date as the population, by applying the interval test to both ",
        "tables in the join."),
      plain = tagList("“How many English learners were enrolled on October 1?” has ",
        "two dates in it, and they are the same date: enrolled on October 1, and EL on ",
        "October 1. Students exit EL services; a student who was EL last year may not be now."),
      district = tagList("On 2025-10-01: 177 English learners, 201 students receiving special ",
        "education, 684 eligible for free or reduced-price meals, 2 identified as homeless. ",
        "The snapshot's flags agree, because they were taken the same way.")),
    dsql_code("SELECT p.program, COUNT(DISTINCT p.student_id) AS students\nFROM enrollments AS e\nJOIN programs AS p\n  ON p.student_id = e.student_id\n AND p.start_date <= '2025-10-01'\n AND (p.end_date IS NULL OR p.end_date >= '2025-10-01')\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.start_date <= '2025-10-01'\n  AND (e.end_date IS NULL OR e.end_date >= '2025-10-01')\nGROUP BY p.program;"),
    dsql_editor("dzden6", "SELECT p.program, COUNT(DISTINCT p.student_id) AS students\nFROM enrollments AS e\nJOIN programs AS p\n  ON p.student_id = e.student_id\n AND p.start_date <= '2025-10-01'\n AND (p.end_date IS NULL OR p.end_date >= '2025-10-01')\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.start_date <= '2025-10-01'\n  AND (e.end_date IS NULL OR e.end_date >= '2025-10-01')\nGROUP BY p.program;"),
    help_details("“EL this year” is three different questions",
      tagList("EL on a census date (October 1); EL at any point during the year; ever EL. ",
        "Each gives a larger number than the one before. Services planning wants the current ",
        "status; some outcome reports follow students who exited EL for a few years after. ",
        "Ask which one the requester means, then put the definition in the report."))
  ),

  # ---- 7 ----
  tagList(
    district_triad(
      formal = tagList(tags$b("Average daily membership (ADM)"), " is the sum of membership ",
        "days divided by the number of days in the school year: a full-year student counts ",
        "1.0, one enrolled for half the year 0.5."),
      plain = tagList("Funding follows students, and a student here for half a year brings ",
        "half a year's funding. ADM is the count that does that arithmetic."),
      district = tagList("For 2024-25 (176 school days), ", kbd("attendance_year_summary"),
        " holds each student's membership days per school. Summing and dividing gives each ",
        "school's ADM: Lakeview about 210, Horizon about 22.")),
    dsql_code("SELECT school_id,\n       ROUND(SUM(days_membership) / 176.0, 1) AS adm\nFROM attendance_year_summary\nWHERE school_year = 2025\nGROUP BY school_id;"),
    dsql_editor("dzden7", "SELECT school_id,\n       ROUND(SUM(days_membership) / 176.0, 1) AS adm\nFROM attendance_year_summary\nWHERE school_year = 2025\nGROUP BY school_id;"),
    callout_warm(tags$b("176.0, not 176. "),
      kbd("SUM(days_membership)"), " is an integer, and an integer divided by an integer is ",
      "an integer in SQLite and SQL Server alike: the decimals are thrown away before ",
      kbd("ROUND"), " sees them. One decimal point in the divisor makes it real division."),
    help_details("The state's version is more detailed",
      tagList("Minnesota computes ADM in its student accounting system (MARSS) with rules ",
        "this tab skips: part-time students count by their percent enrolled, some programs ",
        "carry extra weights, and grade levels are weighted differently in the funding ",
        "formula. The shape is the same: membership days over days in session."))
  )
)

DEN_DQUEST <- list(
  list(prompt = paste0("Count students enrolled (primary enrollment) on 2025-10-01 at each school. ",
                       "Return school_id and students."),
       ref = "SELECT school_id, COUNT(DISTINCT student_id) AS students FROM enrollments WHERE school_year = 2026 AND service_type = 'P' AND start_date <= '2025-10-01' AND (end_date IS NULL OR end_date >= '2025-10-01') GROUP BY school_id;",
       solution = "SELECT school_id, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2025-10-01'\n  AND (end_date IS NULL OR end_date >= '2025-10-01')\nGROUP BY school_id;"),
  list(prompt = paste0("Same date, district-wide, by grade: return grade and students for primary ",
                       "enrollments open on 2025-10-01."),
       ref = "SELECT grade, COUNT(DISTINCT student_id) AS students FROM enrollments WHERE school_year = 2026 AND service_type = 'P' AND start_date <= '2025-10-01' AND (end_date IS NULL OR end_date >= '2025-10-01') GROUP BY grade;",
       solution = "SELECT grade, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date <= '2025-10-01'\n  AND (end_date IS NULL OR end_date >= '2025-10-01')\nGROUP BY grade;"),
  list(prompt = paste0("How many students has each school served this year: distinct students with any ",
                       "primary enrollment in school_year 2026? Return school_id and students_served."),
       ref = "SELECT school_id, COUNT(DISTINCT student_id) AS students_served FROM enrollments WHERE school_year = 2026 AND service_type = 'P' GROUP BY school_id;",
       solution = "SELECT school_id, COUNT(DISTINCT student_id) AS students_served\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\nGROUP BY school_id;"),
  list(prompt = paste0("How many students are enrolled at Lakeview High School (school_id 70) today, ",
                       "2026-03-31, as their primary school? One column, students."),
       ref = "SELECT COUNT(DISTINCT student_id) AS students FROM enrollments WHERE school_id = 70 AND service_type = 'P' AND start_date <= '2026-03-31' AND (end_date IS NULL OR end_date >= '2026-03-31');",
       solution = "SELECT COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_id = 70\n  AND service_type = 'P'\n  AND start_date <= '2026-03-31'\n  AND (end_date IS NULL OR end_date >= '2026-03-31');"),
  list(prompt = paste0("Arrivals since the count date: for primary enrollments in school_year 2026 that ",
                       "started after 2025-10-01 with a start_status of NEW, TRNI, OSIN, or RTN (arrivals ",
                       "from outside the district, not moves within it), count distinct students by ",
                       "start_status. Return start_status and students."),
       ref = "SELECT start_status, COUNT(DISTINCT student_id) AS students FROM enrollments WHERE school_year = 2026 AND service_type = 'P' AND start_date > '2025-10-01' AND start_status IN ('NEW', 'TRNI', 'OSIN', 'RTN') GROUP BY start_status;",
       solution = "SELECT start_status, COUNT(DISTINCT student_id) AS students\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND start_date > '2025-10-01'\n  AND start_status IN ('NEW', 'TRNI', 'OSIN', 'RTN')\nGROUP BY start_status;"),
  list(prompt = paste0("For every school, including any with none, count this year's (2026) primary ",
                       "enrollments that started with OSIN (arrived from out of state or country). ",
                       "Return school_name and arrivals."),
       ref = "SELECT s.school_name, COUNT(e.enrollment_id) AS arrivals FROM schools s LEFT JOIN enrollments e ON e.school_id = s.school_id AND e.school_year = 2026 AND e.service_type = 'P' AND e.start_status = 'OSIN' GROUP BY s.school_name;",
       solution = "SELECT s.school_name, COUNT(e.enrollment_id) AS arrivals\nFROM schools AS s\nLEFT JOIN enrollments AS e\n       ON e.school_id = s.school_id\n      AND e.school_year = 2026\n      AND e.service_type = 'P'\n      AND e.start_status = 'OSIN'\nGROUP BY s.school_name;"),
  list(prompt = paste0("How many students does each year's frozen October 1 snapshot hold? Return ",
                       "school_year and in_snapshot."),
       ref = "SELECT school_year, COUNT(*) AS in_snapshot FROM oct1_snapshot GROUP BY school_year;",
       solution = "SELECT school_year, COUNT(*) AS in_snapshot\nFROM oct1_snapshot\nGROUP BY school_year;"),
  list(prompt = paste0("Which students are in the 2026 snapshot but, by the live data, were not enrolled ",
                       "(primary) on 2025-10-01? Return student_id and school_id from the snapshot."),
       ref = "SELECT s.student_id, s.school_id FROM oct1_snapshot s WHERE s.school_year = 2026 AND NOT EXISTS (SELECT 1 FROM enrollments e WHERE e.student_id = s.student_id AND e.service_type = 'P' AND e.start_date <= '2025-10-01' AND (e.end_date IS NULL OR e.end_date >= '2025-10-01'));",
       solution = "SELECT s.student_id, s.school_id\nFROM oct1_snapshot AS s\nWHERE s.school_year = 2026\n  AND NOT EXISTS (\n        SELECT 1 FROM enrollments AS e\n        WHERE e.student_id = s.student_id\n          AND e.service_type = 'P'\n          AND e.start_date <= '2025-10-01'\n          AND (e.end_date IS NULL OR e.end_date >= '2025-10-01'));"),
  list(prompt = paste0("English learners on the count date, by school: among primary enrollments open on ",
                       "2025-10-01, count distinct students who also had an EL program spell covering ",
                       "2025-10-01. Return school_id and el_students."),
       ref = "SELECT e.school_id, COUNT(DISTINCT e.student_id) AS el_students FROM enrollments e JOIN programs p ON p.student_id = e.student_id AND p.program = 'EL' AND p.start_date <= '2025-10-01' AND (p.end_date IS NULL OR p.end_date >= '2025-10-01') WHERE e.school_year = 2026 AND e.service_type = 'P' AND e.start_date <= '2025-10-01' AND (e.end_date IS NULL OR e.end_date >= '2025-10-01') GROUP BY e.school_id;",
       solution = "SELECT e.school_id, COUNT(DISTINCT e.student_id) AS el_students\nFROM enrollments AS e\nJOIN programs AS p\n  ON p.student_id = e.student_id\n AND p.program = 'EL'\n AND p.start_date <= '2025-10-01'\n AND (p.end_date IS NULL OR p.end_date >= '2025-10-01')\nWHERE e.school_year = 2026\n  AND e.service_type = 'P'\n  AND e.start_date <= '2025-10-01'\n  AND (e.end_date IS NULL OR e.end_date >= '2025-10-01')\nGROUP BY e.school_id;"),
  list(prompt = paste0("ADM for 2023-24 (school_year 2024, which had 178 school days): from ",
                       "attendance_year_summary, return school_id and adm, the sum of days_membership ",
                       "divided by 178, rounded to one decimal place."),
       ref = "SELECT school_id, ROUND(SUM(days_membership) / 178.0, 1) AS adm FROM attendance_year_summary WHERE school_year = 2024 GROUP BY school_id;",
       solution = "SELECT school_id,\n       ROUND(SUM(days_membership) / 178.0, 1) AS adm\nFROM attendance_year_summary\nWHERE school_year = 2024\nGROUP BY school_id;")
)

source_district_enrollment_server <- district_tab_server(
  key = "den",
  step_titles = DEN_STEP_TITLES,
  step_bodies = DEN_STEPS,
  n_demo = 7,
  header = function() district_header(
    "Enrollment counts: who was here, when", eyebrow = "School district · 2 of 7",
    p(style = "margin-bottom:6px;",
      "“How many students do we have?” is the most common question a district data ",
      "team gets, and it has no answer until someone says ", tags$em("when"), " and ",
      tags$em("which"), ". This tab works through the decisions behind an enrollment count: ",
      "the date, the window, the unit, empty groups, corrected versus reported data, groups ",
      "that change over time, and the funding count, ADM."),
    p(style = "margin-bottom:0;",
      "The queries use ", kbd("WHERE"), ", ", kbd("GROUP BY"), ", ", kbd("JOIN"), ", and in ",
      "two places ", kbd("NOT EXISTS"), ". If any of those is new, the matching institute tab ",
      "comes first.")),
  quest = DEN_DQUEST,
  quest_title = "Ten enrollment counts",
  quest_intro = "Every one of these has a date or a window in it. Say it in the WHERE clause."
)
