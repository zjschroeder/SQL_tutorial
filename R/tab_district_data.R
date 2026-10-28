# =============================================================================
# tab_district_data.R — "District data": how a student information system
# looks from SQL (first of the School district tabs)
#
# Six gated steps: the grain of each table; the three kinds of student ID;
# enrollment spells and status codes; the calendar and the school year;
# exceptions-only tables; SQLite here versus T-SQL at work. Then a 10-question
# quest on the district database (data/district.sqlite).
# =============================================================================



DD_STEP_TITLES <- c(
  "One row is ... : the grain of each table",
  "Three kinds of student ID",
  "Enrollment spells and status codes",
  "The calendar and the school year",
  "Tables that store only exceptions",
  "SQLite here, SQL Server at work"
)

DD_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("The ", tags$b("grain"), " of a table is what one row represents. ",
        "Every aggregate depends on it: a ", kbd("COUNT(*)"), " counts rows at the table's ",
        "grain, whatever you meant to count."),
      plain = tagList("Before you count anything, finish the sentence “one row of this ",
        "table is …”. If the answer is not “one student,” then ",
        kbd("COUNT(*)"), " is not a count of students."),
      district = tagList("In ", kbd("students"), ", one row is a person. In ", kbd("enrollments"),
        ", one row is a ", tags$em("spell"), ": one student at one school in one grade for one ",
        "stretch of dates. A student who changes schools in March has two rows that year.")),
    p(style = "line-height:1.6;",
      "This year's enrollment table has 1,479 rows but only 1,437 different students. ",
      "Run the query and compare the two numbers:"),
    dsql_code("SELECT COUNT(*)                   AS rows_this_year,\n       COUNT(DISTINCT student_id) AS students_this_year\nFROM enrollments\nWHERE school_year = 2026;"),
    dsql_editor("dzdd1", "SELECT COUNT(*)                   AS rows_this_year,\n       COUNT(DISTINCT student_id) AS students_this_year\nFROM enrollments\nWHERE school_year = 2026;"),
    term_table(head = c("Table", "One row is ..."), rows = list(
      c("students", "a person (who may appear in many years)"),
      c("enrollments", "a student at a school, in a grade, for a span of dates"),
      c("calendar_days", "a weekday at a school, flagged school day or not"),
      c("attendance", "an absence or tardy on one day (current year only)"),
      c("attendance_year_summary", "a student's year at one school, prior years"),
      c("programs", "a span of time in one program (EL, special education, ...)"),
      c("incidents / incident_students / resolutions", "an event / a person in that event / a consequence for that person"),
      c("transcripts", "a final mark in one course for one term"),
      c("oct1_snapshot", "a student as counted on October 1 of a year, frozen"))),
    callout_warm(tags$b("The habit that prevents most wrong numbers. "),
      "Write the grain at the top of a query as a comment: ",
      kbd("-- one row per student per school"), ". When the output's grain is not ",
      "what the comment says, something joined or grouped differently than you intended.")
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("surrogate key"), " is an identifier with no meaning of ",
        "its own, assigned by a system. A ", tags$b("natural"), " or ", tags$b("external key"),
        " comes from outside it. Systems that exchange data join on the key they share."),
      plain = tagList("The district numbers its students. The state numbers them too, ",
        "differently, and the state's number follows a child between districts. Each system ",
        "has its own. Joining two systems means finding the number both of them store."),
      district = tagList(kbd("student_id"), " is Prairie Lakes' number. ", kbd("state_id"),
        " is the state's ten-digit number, the one on the state's files. Four students ",
        "have none: two arrived in March and are waiting for the state to assign one, and two ",
        "are duplicate records the Data quality tab tracks down.")),
    p(style = "line-height:1.6;",
      kbd("state_id"), " is stored as ", tags$b("text"), ", even though it is all digits. ",
      "763 of them begin with a zero. Store them as numbers and the zeros vanish, and so ",
      "does every match against the state's file:"),
    dsql_code("SELECT state_id,\n       CAST(state_id AS INTEGER) AS as_a_number\nFROM students\nWHERE state_id LIKE '00%'\nLIMIT 5;"),
    dsql_editor("dzdd2", "SELECT state_id,\n       CAST(state_id AS INTEGER) AS as_a_number\nFROM students\nWHERE state_id LIKE '00%'\nLIMIT 5;"),
    help_details("Where leading zeros go missing in real life",
      tagList("Most often in a spreadsheet. Excel reads 0003276861 as the number 3276861 ",
        "unless the column is imported as text, and a file saved from that sheet carries the ",
        "damage to the next system. When a join on an ID matches far fewer rows than it ",
        "should, compare ", kbd("LENGTH(id)"), " on both sides before anything else.")),
    tsql_note("SELECT state_id,\n       CAST(state_id AS BIGINT) AS as_a_number   -- zeros lost here too\nFROM students\nWHERE state_id LIKE '00%';\n\n-- In SQL Server the column would be declared VARCHAR(10) or CHAR(10).",
      "Same idea. SQL Server is strict about types: comparing a VARCHAR column with a number ",
      "makes it convert every row of the column, which is slow and fails outright on a value ",
      "like 'PENDING'. Compare text with text: ", kbd("state_id = '0003276861'"), ".")
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("An enrollment is an ", tags$b("interval"), ": [", kbd("start_date"),
        ", ", kbd("end_date"), "], both ends inclusive here, with a NULL end meaning the ",
        "interval is still open. Status codes record why each interval began and ended."),
      plain = tagList("Each spell says when a student arrived, when they left, and why. ",
        "A blank leaving date means they have not left. To know who was here on a given ",
        "day, you ask which spells contain that day."),
      district = tagList("Student 100063 moved from Prairie View to Cedar Park in February: one spell ",
        "ends 2026-02-18 with code ", kbd("INTR"), " (transfer within the district); the next ",
        "starts 2026-02-19 with code ", kbd("INTR"), ". The codes live in ", kbd("status_codes"), ".")),
    dsql_code("SELECT enrollment_id, school_id, grade, start_date, end_date,\n       start_status, end_status, service_type\nFROM enrollments\nWHERE student_id = 100063\n  AND school_year = 2026;"),
    dsql_editor("dzdd3", "SELECT enrollment_id, school_id, grade, start_date, end_date,\n       start_status, end_status, service_type\nFROM enrollments\nWHERE student_id = 100063\n  AND school_year = 2026;"),
    p(style = "line-height:1.6;",
      kbd("status_codes"), " has a two-column primary key, ", kbd("(code, code_type)"),
      ", because ", kbd("INTR"), " is both a start code and an end code with different ",
      "meanings. A join to it must match both columns, or each ", kbd("INTR"),
      " row matches twice:"),
    dsql_code("SELECT code, code_type, description, leaves_district, documented_transfer\nFROM status_codes\nORDER BY code_type, code;"),
    term_table(head = c("Field", "What to know"), rows = list(
      c("end_date", "NULL = still enrolled. Otherwise the last day the student was enrolled (inclusive)."),
      c("service_type", "P = primary enrollment, the one that counts the student. S = a secondary, part-time enrollment (18 this year, evening credit recovery at Horizon). Counts of students use P only."),
      c("school_year", "Named by its spring: 2026 means 2025-26."),
      c("documented_transfer", "1 for the exits that remove a student from a graduation cohort: TRMN, TROS, EMIG. A dropout (DROP) or an unknown exit (UNKN) does not."))),
    help_details("Inclusive or exclusive end dates?",
      tagList("Systems differ. Some store the last day attended; some store the first day ",
        "gone. Here it is the last day enrolled, so a student enrolled on date ", tags$em("d"),
        " satisfies ", kbd("start_date <= d AND (end_date IS NULL OR end_date >= d)"),
        ". With exclusive end dates the second test would be ", kbd("end_date > d"),
        ". Check which convention a system uses before writing any date logic against it; ",
        "an off-by-one here shifts every withdrawn student's membership by a day."))
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("calendar table"), " materializes each date with its ",
        "attributes, so date logic becomes a join instead of arithmetic. Dates here are ISO ",
        "text, ", kbd("'YYYY-MM-DD'"), ", which sorts and compares correctly as text."),
      plain = tagList("Not every weekday is a school day. Rather than compute which ones are, ",
        "the district lists them: one row per weekday per school, flagged. Counting school ",
        "days becomes counting rows."),
      district = tagList("2025-26 runs from 2025-09-02 to 2026-06-04: 176 school days. ",
        "Through today, 2026-03-31, there have been 129. The 22 weekdays off have a ",
        kbd("day_note"), ": MEA break, Thanksgiving, winter break, and so on.")),
    dsql_code("SELECT COUNT(*) AS school_days_so_far\nFROM calendar_days\nWHERE school_id = 10\n  AND school_year = 2026\n  AND instructional = 1\n  AND cal_date <= '2026-03-31';"),
    dsql_editor("dzdd4", "SELECT cal_date, day_note\nFROM calendar_days\nWHERE school_id = 10\n  AND school_year = 2026\n  AND instructional = 0;"),
    callout_warm(tags$b("“Today” is a fixed date. "),
      "The data stops on 2026-03-31, and every question here treats that as today. SQLite's ",
      kbd("date('now')"), " would return the real date, which moves, and a query whose ",
      "answer depends on when it runs is a query nobody can check. Reporting teams handle ",
      "this the same way: an ", tags$em("as-of date"), " is a parameter of the report, ",
      "written down, not whatever the clock says."),
    help_details("Why the calendar is per school",
      tagList("Every Prairie Lakes school follows the same calendar, so the table repeats it ",
        "nine times. Real districts differ: an alternative school may run extra days, a ",
        "school may close for a boiler failure. Keying the calendar by school costs a ",
        "little space and means the join is right when that happens."))
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("An ", tags$b("exceptions-only"), " table records deviations from a ",
        "default state. The default is implied by absence of a row, so measures of the ",
        "default state require the full population from another source."),
      plain = tagList("Teachers mark who is missing, not who is there. So the attendance ",
        "table holds absences and tardies only, and a student with perfect attendance has ",
        "no rows at all. Counting rows tells you about absences, never about presence."),
      district = tagList("This year's ", kbd("attendance"), " table has 22,651 rows. ",
        "A day present is a school day inside an enrollment spell with no row. Prior years ",
        "are already summarized, one row per student per school, in ",
        kbd("attendance_year_summary"), ".")),
    term_table(head = c("Code", "Meaning for a rate"), rows = list(
      c("status A", "absent the whole day: 1 day absent"),
      c("status H", "absent half the day: 0.5 day absent"),
      c("status T", "tardy: present (a tardy is not an absence)"),
      c("excuse X", "exempt, at a school-sponsored activity: present, whatever the status says"),
      c("excuse S", "out-of-school suspension: absent (Minnesota counts suspensions toward chronic absence)"),
      c("excuse E / U", "excused / unexcused: both absent; the excuse matters for truancy, not for attendance rates"))),
    dsql_code("SELECT status, excuse, COUNT(*) AS n\nFROM attendance\nGROUP BY status, excuse\nORDER BY status, excuse;"),
    dsql_editor("dzdd5", "SELECT status, excuse, COUNT(*) AS n\nFROM attendance\nGROUP BY status, excuse\nORDER BY status, excuse;"),
    p(style = "line-height:1.6;",
      "Turning exceptions into rates takes a join to the calendar, so it waits for the ",
      "Attendance tab. The point for now: the denominator lives in a different table from ",
      "the numerator.")
  ),

  # ---- 6 ----
  tagList(
    p(style = "line-height:1.6;",
      "This app runs SQLite because SQLite runs in a browser. District reporting databases ",
      "are usually Microsoft SQL Server, whose dialect is called ", tags$b("T-SQL"),
      ". Everything you learn here carries over; the spelling of some functions does not. ",
      "The pink boxes on these tabs show the T-SQL version where it differs."),
    term_table(head = c("Task", "SQLite (here) → T-SQL (SQL Server)"), rows = list(
      c("First N rows", "... LIMIT 10   →   SELECT TOP 10 ..."),
      c("Today's date", "date('now')   →   CAST(GETDATE() AS date)"),
      c("Days between", "julianday(b) - julianday(a)   →   DATEDIFF(day, a, b)"),
      c("Add days", "date(d, '+7 days')   →   DATEADD(day, 7, d)"),
      c("Year of a date", "strftime('%Y', d)   →   YEAR(d)"),
      c("Join text", "first_name || ' ' || last_name   →   CONCAT(first_name, ' ', last_name)"),
      c("Replace NULL", "COALESCE(x, 0)   →   COALESCE(x, 0) or ISNULL(x, 0)"),
      c("Integer division", "5 / 2 = 2 in both; write 5 * 1.0 / 2 for 2.5"),
      c("A named parameter", "WITH params AS (SELECT '2026-03-31' AS as_of)   →   DECLARE @as_of date = '2026-03-31';"),
      c("Temporary table", "CREATE TEMP TABLE t AS SELECT ...   →   SELECT ... INTO #t FROM ..."),
      c("Text types", "TEXT   →   VARCHAR(n) / NVARCHAR(n)"),
      c("Booleans", "1 and 0 in an INTEGER   →   BIT"))),
    tsql_note("DECLARE @as_of date = '2026-03-31';\n\nSELECT TOP 10 student_id, school_id, start_date\nFROM enrollments\nWHERE start_date <= @as_of\n  AND (end_date IS NULL OR end_date >= @as_of)\nORDER BY start_date DESC;",
      "A variable holds the as-of date once, and every filter uses it. SQLite has no ",
      "variables; the nearest equivalent is a one-row CTE (", kbd("WITH params AS (...)"),
      "), which the Subqueries tab teaches."),
    interview_note(
      p(style = "margin-bottom:6px;", "Expect a question like “what SQL have you used?” ",
        "If your experience is in another dialect, say so and name the differences you know: ",
        kbd("TOP"), " versus ", kbd("LIMIT"), ", ", kbd("DATEDIFF"), ", variables, temp tables. ",
        "Interviewers mainly want to hear that you reason about grain, NULLs, and dates; ",
        "dialect is the part anyone can look up."),
      p(style = "margin-bottom:0;", "If the role uses Infinite Campus, PowerSchool, Skyward, or ",
        "another SIS, its reporting tables will have their own names, but the shapes on this ",
        "tab (people, enrollment spells, calendars, exceptions, program spells, frozen ",
        "snapshots) are the shapes you will find."))
  )
)

DD_DQUEST <- list(
  list(prompt = paste0("How many enrollment rows does 2025-26 (school_year 2026) have, and how many ",
                       "different students? Return two columns, n_rows and n_students."),
       ref = "SELECT COUNT(*) AS n_rows, COUNT(DISTINCT student_id) AS n_students FROM enrollments WHERE school_year = 2026;",
       solution = "SELECT COUNT(*) AS n_rows,\n       COUNT(DISTINCT student_id) AS n_students\nFROM enrollments\nWHERE school_year = 2026;"),
  list(prompt = "Return student_id, last_name, and first_name of the students who have no state_id yet.",
       ref = "SELECT student_id, last_name, first_name FROM students WHERE state_id IS NULL;",
       solution = "SELECT student_id, last_name, first_name\nFROM students\nWHERE state_id IS NULL;"),
  list(prompt = "Return school_name and state_school_number for every school.",
       ref = "SELECT school_name, state_school_number FROM schools;",
       solution = "SELECT school_name, state_school_number\nFROM schools;"),
  list(prompt = paste0("How many school days (instructional = 1) has Birchwood Elementary (school_id 10) had ",
                       "in 2025-26 up to and including today, 2026-03-31? Name the column school_days."),
       ref = "SELECT COUNT(*) AS school_days FROM calendar_days WHERE school_id = 10 AND school_year = 2026 AND instructional = 1 AND cal_date <= '2026-03-31';",
       solution = "SELECT COUNT(*) AS school_days\nFROM calendar_days\nWHERE school_id = 10\n  AND school_year = 2026\n  AND instructional = 1\n  AND cal_date <= '2026-03-31';"),
  list(prompt = paste0("Which end codes count as a documented transfer (documented_transfer = 1)? ",
                       "Return code and description."),
       ref = "SELECT code, description FROM status_codes WHERE code_type = 'end' AND documented_transfer = 1;",
       solution = "SELECT code, description\nFROM status_codes\nWHERE code_type = 'end'\n  AND documented_transfer = 1;"),
  list(prompt = paste0("List this year's secondary enrollments (service_type 'S'): enrollment_id, ",
                       "student_id, school_id, start_date."),
       ref = "SELECT enrollment_id, student_id, school_id, start_date FROM enrollments WHERE school_year = 2026 AND service_type = 'S';",
       solution = "SELECT enrollment_id, student_id, school_id, start_date\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'S';"),
  list(prompt = paste0("List the weekdays in 2025-26 when Birchwood (school_id 10) had no school ",
                       "(instructional = 0): cal_date and day_note."),
       ref = "SELECT cal_date, day_note FROM calendar_days WHERE school_id = 10 AND school_year = 2026 AND instructional = 0;",
       solution = "SELECT cal_date, day_note\nFROM calendar_days\nWHERE school_id = 10\n  AND school_year = 2026\n  AND instructional = 0;"),
  list(prompt = paste0("Who was marked absent or tardy at Lakeview High School (school_id 70) on ",
                       "2026-03-02? Return student_id, status, and excuse."),
       ref = "SELECT student_id, status, excuse FROM attendance WHERE school_id = 70 AND att_date = '2026-03-02';",
       solution = "SELECT student_id, status, excuse\nFROM attendance\nWHERE school_id = 70\n  AND att_date = '2026-03-02';"),
  list(prompt = paste0("How many different students had an FRL (free/reduced-price meals) program spell ",
                       "that covered 2025-10-01? A spell covers a date if it started on or before it and ",
                       "had not ended before it. Name the column n_frl."),
       ref = "SELECT COUNT(DISTINCT student_id) AS n_frl FROM programs WHERE program = 'FRL' AND start_date <= '2025-10-01' AND (end_date IS NULL OR end_date >= '2025-10-01');",
       solution = "SELECT COUNT(DISTINCT student_id) AS n_frl\nFROM programs\nWHERE program = 'FRL'\n  AND start_date <= '2025-10-01'\n  AND (end_date IS NULL OR end_date >= '2025-10-01');"),
  list(prompt = paste0("Who is enrolled at Horizon (school_id 90) right now as their primary school? ",
                       "Return student_id, grade, and start_date for this year's primary enrollments there ",
                       "that have not ended."),
       ref = "SELECT student_id, grade, start_date FROM enrollments WHERE school_year = 2026 AND school_id = 90 AND service_type = 'P' AND end_date IS NULL;",
       solution = "SELECT student_id, grade, start_date\nFROM enrollments\nWHERE school_year = 2026\n  AND school_id = 90\n  AND service_type = 'P'\n  AND end_date IS NULL;")
)

source_district_data_server <- district_tab_server(
  key = "dd",
  step_titles = DD_STEP_TITLES,
  step_bodies = DD_STEPS,
  n_demo = 5,
  header = function() district_header(
    "A student information system, as SQL sees it", eyebrow = "School district · 1 of 7",
    p(style = "margin-bottom:6px;",
      "These tabs run on a second database: ", tags$b(DISTRICT_NAME), ", a fictional Minnesota ",
      "district with nine schools and five school years of data (2021-22 to 2025-26). ",
      "Every person and number in it is invented. Its tables, though, have the shape of a real ",
      "student information system (SIS): enrollment spells with dates and status codes, a ",
      "calendar of school days, attendance stored as exceptions, program spells, behavior ",
      "events, transcripts, test results, and the frozen files a district sends the state."),
    p(style = "margin-bottom:0;",
      "Today, in the data, is ", tags$b("2026-03-31"), ". The district has also been left with ",
      "the kinds of problems real SIS data has (a few overlapping enrollments, a missing code, ",
      "a duplicated student), which the Data quality tab hunts down. This tab is the tour."),
    help_details("The district's tables", district_schema_explorer()),
    district_relationship_map()),
  quest = DD_DQUEST,
  quest_title = "Ten queries to find your way around the district",
  quest_intro = "Single-table queries, each pointed at one of the shapes this tab describes."
)
