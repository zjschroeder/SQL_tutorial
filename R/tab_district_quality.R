# =============================================================================
# tab_district_quality.R — "Data quality" (School district, 6 of 7)
#
# Seven gated steps, each a check a district runs on its SIS data: students in
# two places at once; gaps between enrollments; duplicate people; codes that
# are not codes (and dates that cannot be); records in the wrong place;
# reconciling with the state's file; making the checks routine (an exception
# report, and filtering it by who is allowed to see what). Then a 10-question
# quest. Every problem found here was planted by data/generate_district.R and is
# listed in data/district_frozen/PLANTED_PROBLEMS.txt.
# =============================================================================

OCT1_LIVE_STATE <- "WITH live AS (                    -- our October 1 count, in the state's terms
  SELECT s.state_id, sc.state_school_number, e.grade
  FROM enrollments AS e
  JOIN students AS s ON s.student_id = e.student_id
  JOIN schools  AS sc ON sc.school_id = e.school_id
  WHERE e.service_type = 'P'
    AND e.start_date <= '2025-10-01'
    AND (e.end_date IS NULL OR e.end_date >= '2025-10-01')
),
state AS (                          -- what the state accepted
  SELECT state_id, state_school_number, grade
  FROM state_oct1_return
  WHERE school_year = 2026
)"

DQA_STEP_TITLES <- c(
  "Students in two places at once",
  "Gaps between enrollments",
  "The same person twice",
  "Codes that are not codes",
  "Records in the wrong place",
  "Reconciling with the state",
  "Making the checks routine"
)

DQA_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("A ", tags$b("self-join"), " pairs rows of one table. Joining ",
        "enrollments to themselves on student, with the interval-overlap condition and ",
        kbd("a.enrollment_id < b.enrollment_id"), " to list each pair once, finds concurrent ",
        "primary enrollments."),
      plain = tagList("A student can have one primary school at a time. Pair each of a ",
        "student's primary enrollments with each other one; any pair whose dates overlap is a ",
        "problem."),
      district = tagList("Four students are enrolled at two schools at once. Each moved within ",
        "the district; the new school entered the enrollment, and the old school never ended its own.")),
    dsql_code("SELECT a.student_id,\n       a.enrollment_id AS first_id,  a.school_id AS first_school,\n       b.enrollment_id AS second_id, b.school_id AS second_school\nFROM enrollments AS a\nJOIN enrollments AS b\n  ON b.student_id = a.student_id\n AND a.enrollment_id < b.enrollment_id\nWHERE a.service_type = 'P'\n  AND b.service_type = 'P'\n  AND a.start_date <= COALESCE(b.end_date, '9999-12-31')\n  AND b.start_date <= COALESCE(a.end_date, '9999-12-31');"),
    dsql_editor("dzdqa1", "SELECT a.student_id,\n       a.enrollment_id AS first_id,  a.school_id AS first_school,\n       b.enrollment_id AS second_id, b.school_id AS second_school\nFROM enrollments AS a\nJOIN enrollments AS b\n  ON b.student_id = a.student_id\n AND a.enrollment_id < b.enrollment_id\nWHERE a.service_type = 'P'\n  AND b.service_type = 'P'\n  AND a.start_date <= COALESCE(b.end_date, '9999-12-31')\n  AND b.start_date <= COALESCE(a.end_date, '9999-12-31');", height = "260px"),
    p(style = "line-height:1.6;",
      kbd("COALESCE(end_date, '9999-12-31')"), " treats an open enrollment as running ",
      "forever, which is what it claims. Without it, every comparison with an open ",
      "enrollment's end date is NULL and the overlap test cannot be true: the query would ",
      "miss exactly the cases it exists to find, since in each of them both enrollments are open."),
    callout_warm(tags$b("What these four cost. "),
      "Each is counted at both schools in school-level enrollment, marked absent every day at the ",
      "old school (so it looks like chronic absence), and double-counted in membership days, ",
      "which is money. The Attendance tab's “stopped coming” list found the same four ",
      "from the other direction.")
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList(kbd("LEAD(start_date)"), " over each student's enrollments in date ",
        "order gives the next start beside each end. Counting calendar school days strictly ",
        "between the two measures the gap in the unit that matters."),
      plain = tagList("When a student moves schools, the new enrollment should start on the next ",
        "school day after the old one ends. Put each enrollment's end next to the following ",
        "start and count the school days in between."),
      district = tagList("Three students this year lost six school days each between schools: ",
        "days they were not enrolled anywhere, so not counted, not marked absent, and not funded. ",
        "Calendar days are the wrong measure: a Friday end and a Monday start is no gap at all.")),
    dsql_code("WITH spells AS (\n  SELECT student_id, school_id, end_date,\n         LEAD(start_date) OVER (PARTITION BY student_id\n                                ORDER BY start_date, enrollment_id) AS next_start\n  FROM enrollments\n  WHERE school_year = 2026 AND service_type = 'P'\n)\nSELECT s.student_id, s.end_date, s.next_start,\n       (SELECT COUNT(*) FROM calendar_days AS c\n        WHERE c.school_id = s.school_id\n          AND c.instructional = 1\n          AND c.cal_date > s.end_date\n          AND c.cal_date < s.next_start) AS school_days_missed\nFROM spells AS s\nWHERE s.end_date IS NOT NULL\n  AND s.next_start IS NOT NULL\n  AND (SELECT COUNT(*) FROM calendar_days AS c\n       WHERE c.school_id = s.school_id AND c.instructional = 1\n         AND c.cal_date > s.end_date AND c.cal_date < s.next_start) > 0;"),
    dsql_editor("dzdqa2", "SELECT student_id, school_id, start_date, end_date,\n       LEAD(start_date) OVER (PARTITION BY student_id\n                              ORDER BY start_date, enrollment_id) AS next_start\nFROM enrollments\nWHERE school_year = 2026\n  AND service_type = 'P'\n  AND student_id IN (100694, 102161, 102255);", height = "180px"),
    help_details("Why the count is written twice",
      tagList("A column alias from ", kbd("SELECT"), " cannot be used in ", kbd("WHERE"),
        " in standard SQL, because ", kbd("WHERE"), " runs first. SQLite would let ",
        kbd("WHERE school_days_missed > 0"), " through; SQL Server would not. Repeating the ",
        "expression runs everywhere. Wrapping the query in a CTE and filtering outside is the ",
        "tidier fix."))
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("Candidate duplicates share identifying attributes (name, birth date) ",
        "under different keys. Grouping on those attributes with ", kbd("HAVING COUNT(*) > 1"),
        " finds candidates; other evidence decides."),
      plain = tagList("A child re-enrolls under a new record when the office cannot find the old ",
        "one. Look for the same name and birth date on two student numbers, then check the rest ",
        "before merging anything."),
      district = tagList("Three pairs share a name and birth date. Two are duplicates: in each, ",
        "the newer record has no state ID and starts the week the older one ends. The third pair ",
        "(two boys named Jose Gonzalez, born the same day) were in kindergarten at two schools ",
        "at the same time, with different state IDs: two children.")),
    dsql_code("SELECT last_name, first_name, birth_date,\n       COUNT(*) AS records,\n       GROUP_CONCAT(student_id) AS student_ids\nFROM students\nGROUP BY last_name, first_name, birth_date\nHAVING COUNT(*) > 1;"),
    dsql_editor("dzdqa3", "SELECT s.student_id, s.state_id, e.school_id, e.school_year,\n       e.start_date, e.end_date, e.start_status, e.end_status\nFROM students AS s\nJOIN enrollments AS e ON e.student_id = s.student_id\nWHERE s.student_id IN (101783, 102341, 101537, 101544)\nORDER BY s.last_name, e.start_date;", height = "180px"),
    callout_warm(tags$b("A match is a lead, not a verdict. "),
      "Merging two students' records is hard to undo and affects everything attached to them. ",
      "Evidence that supports a duplicate: one record missing a state ID, enrollments that ",
      "hand off from one record to the other, the same address or guardian. Evidence against: ",
      "overlapping enrollments, two different state IDs. The decision belongs to whoever owns ",
      "student records; the report's job is to make it easy."),
    tsql_note("SELECT last_name, first_name, birth_date,\n       COUNT(*) AS records,\n       STRING_AGG(CAST(student_id AS varchar(10)), ',') AS student_ids\nFROM students\nGROUP BY last_name, first_name, birth_date\nHAVING COUNT(*) > 1;",
      kbd("STRING_AGG"), " (SQL Server 2017 and later) is the T-SQL ", kbd("GROUP_CONCAT"), ".")
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("An ", tags$b("anti-join"), " against a lookup table finds values outside ",
        "the domain. Rows that violate rules no constraint enforces (an end before a start, a ",
        "closed spell with no exit code) are found with plain predicates."),
      plain = tagList("Every grade should be in the list of grades and every exit should have a ",
        "code. LEFT JOIN each value to its list and keep the ones with no match; then ask the ",
        "questions the database never asked when the row was typed."),
      district = tagList("One enrollment has grade ", kbd("'9'"), " instead of ", kbd("'09'"),
        ". Four closed enrollments have no end code. One ends before it starts: its end date ",
        "was typed with the wrong year.")),
    dsql_code("SELECT e.enrollment_id, e.student_id, e.grade\nFROM enrollments AS e\nLEFT JOIN grade_levels AS g ON g.grade = e.grade\nWHERE g.grade IS NULL;"),
    dsql_code("SELECT enrollment_id, student_id, school_year, end_date\nFROM enrollments\nWHERE end_date IS NOT NULL\n  AND end_status IS NULL;"),
    dsql_editor("dzdqa4", "SELECT enrollment_id, student_id, start_date, end_date\nFROM enrollments\nWHERE end_date < start_date;"),
    p(style = "line-height:1.6;",
      "Grade ", kbd("'9'"), " is a small typo with a long reach. It sorts after ", kbd("'12'"),
      ", it matches no grade-level join, so the student falls out of every report broken down by ",
      "grade, and the state's system rejected the record, so the student was not counted on ",
      "October 1 (step 6)."),
    help_details("Why not a CHECK constraint?",
      tagList("A ", kbd("FOREIGN KEY"), " from ", kbd("enrollments.grade"), " to ",
        kbd("grade_levels"), " would have refused the typo, and a ", kbd("CHECK (end_date >= start_date)"),
        " the reversed dates. Production SIS databases often lack such constraints, or the ",
        "reporting copy does, or the data arrived through an import that skipped them. Checks ",
        "in queries are how you find what the database let through. The Tables tab shows how ",
        "constraints are declared."))
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("Referential checks across tables: each attendance row should fall on an ",
        "instructional day inside one of the student's enrollments at that school; each open ",
        "roster row should belong to a currently enrolled student."),
      plain = tagList("Data can be valid on its own and wrong in context: an absence recorded on ",
        "a holiday, a schedule left in place for a student who has withdrawn."),
      district = tagList("Five absences are recorded on MEA break, when there was no school. ",
        "Three students who withdrew still have current class schedules, so they appear on ",
        "teachers' rosters and in section counts.")),
    dsql_code("SELECT a.attendance_id, a.student_id, a.att_date, c.day_note\nFROM attendance AS a\nJOIN calendar_days AS c\n  ON c.school_id = a.school_id AND c.cal_date = a.att_date\nWHERE c.instructional = 0;"),
    dsql_editor("dzdqa5", "SELECT DISTINCT r.student_id\nFROM roster AS r\nWHERE r.end_date >= '2026-03-31'\n  AND NOT EXISTS (\n        SELECT 1 FROM enrollments AS e\n        WHERE e.student_id = r.student_id\n          AND e.service_type = 'P'\n          AND e.start_date <= '2026-03-31'\n          AND (e.end_date IS NULL OR e.end_date >= '2026-03-31'));"),
    help_details("Absences on a non-school day change the rates",
      tagList("The Attendance tab's calculation would ignore them, because it starts from school ",
        "days and attaches absences to those. A calculation that starts from the attendance ",
        "table (counting absence rows per student) would count them. Two reports, two numbers, ",
        "and the explanation is five stray rows. That is the general pattern: a data problem ",
        "becomes a reporting discrepancy only for the method it touches."))
  ),

  # ---- 6 ----
  tagList(
    district_triad(
      formal = tagList(kbd("EXCEPT"), " in both directions yields the symmetric difference of two ",
        "result sets on all selected columns. Comparing on the external key (state ID, state ",
        "school number) aligns the district's records with the state's."),
      plain = tagList("Put your list and the state's list in the same shape, then ask: what do we ",
        "have that they don't, and what do they have that we don't? Each row on either side has ",
        "a reason, and the job is to find it."),
      district = tagList("Our October 1 count is 1,386; the state accepted 1,388. Four rows are ",
        "ours only and six are theirs only, from eight students: the four no-shows (the state has ",
        "them, our corrected data does not), two grade corrections made after the submission ",
        "(each appears once on each side), the student with grade ", kbd("'9'"), " (rejected), and ",
        "one student the state found enrolled full-time in another district.")),
    dsql_code(paste0(OCT1_LIVE_STATE, "\nSELECT 'ours only' AS side, * FROM (SELECT * FROM live EXCEPT SELECT * FROM state)\nUNION ALL\nSELECT 'state only', * FROM (SELECT * FROM state EXCEPT SELECT * FROM live);")),
    dsql_editor("dzdqa6", paste0(OCT1_LIVE_STATE, "\nSELECT 'ours only' AS side, * FROM (SELECT * FROM live EXCEPT SELECT * FROM state)\nUNION ALL\nSELECT 'state only', * FROM (SELECT * FROM state EXCEPT SELECT * FROM live);"), height = "360px"),
    callout_warm(tags$b("Fix the source, not the file. "),
      "If a difference is our error, correct it in the SIS so every later report agrees, rather ",
      "than editing an extract. If it is a timing difference (a correction made after the ",
      "deadline), document it. If it is the state's decision (the rejected records), follow the ",
      "state's process. Then write down what each difference was: next year's analyst will ask."),
    interview_note(p(style = "margin-bottom:0;",
      "“The state's enrollment number for a school is lower than ours. How would you ",
      "reconcile it?” Student-level lists from both sides in the same shape, compared on the ",
      "state's identifiers in both directions, then each difference classified: rejected by the ",
      "state, timing of the snapshot, corrected since, counted at another district, or ours to ",
      "fix. The words “in both directions” matter."))
  ),

  # ---- 7 ----
  tagList(
    district_triad(
      formal = tagList("An ", tags$b("exception report"), " stacks each validation rule's ",
        "violations into one shape (rule, record, detail) with ", kbd("UNION ALL"), ". Stored as ",
        "a view and run on a schedule, it turns validation into a routine. Joining it to an ",
        "entitlements table restricts each user to the rows they may see."),
      plain = tagList("Write each check once, make them all return the same columns, stack ",
        "them, and send each school its own list every week. The people who can fix the data ",
        "get the list; nobody gets a list they should not see."),
      district = tagList(kbd("user_access"), " says which school each user may see. Lakeview's ",
        "principal (", kbd("p.lakeview"), ") should get Lakeview's rows; the district analyst ",
        "gets all of them.")),
    dsql_code("WITH exceptions AS (\n  SELECT 'Two open primary enrollments' AS rule, a.school_id, a.student_id\n  FROM enrollments AS a\n  JOIN enrollments AS b\n    ON b.student_id = a.student_id AND b.enrollment_id <> a.enrollment_id\n  WHERE a.service_type = 'P' AND b.service_type = 'P'\n    AND a.end_date IS NULL AND b.end_date IS NULL\n  UNION ALL\n  SELECT 'Grade code not in grade_levels', e.school_id, e.student_id\n  FROM enrollments AS e\n  LEFT JOIN grade_levels AS g ON g.grade = e.grade\n  WHERE g.grade IS NULL\n  UNION ALL\n  SELECT 'Closed enrollment with no end code', school_id, student_id\n  FROM enrollments\n  WHERE end_date IS NOT NULL AND end_status IS NULL\n  UNION ALL\n  SELECT 'Absence on a non-school day', a.school_id, a.student_id\n  FROM attendance AS a\n  JOIN calendar_days AS c ON c.school_id = a.school_id AND c.cal_date = a.att_date\n  WHERE c.instructional = 0\n)\nSELECT x.rule, x.school_id, x.student_id\nFROM exceptions AS x\nJOIN user_access AS u\n  ON u.scope = 'District' OR u.school_id = x.school_id\nWHERE u.username = 'p.lakeview'\nORDER BY x.rule, x.student_id;"),
    dsql_editor("dzdqa7", "SELECT username, scope, school_id, role\nFROM user_access\nORDER BY scope, school_id;"),
    p(style = "line-height:1.6;",
      "Change ", kbd("'p.lakeview'"), " to ", kbd("'district.analyst'"), " and the same query ",
      "returns every school's exceptions. That join is row-level security in miniature: the data ",
      "and the rule about who sees it are both in the database, and the report applies them."),
    tsql_note("CREATE VIEW dbo.v_enrollment_exceptions AS\n  SELECT ... ;   -- the UNION ALL above\n\nSELECT x.*\nFROM dbo.v_enrollment_exceptions AS x\nJOIN dbo.user_access AS u\n  ON u.scope = 'District' OR u.school_id = x.school_id\nWHERE u.username = SUSER_SNAME();   -- the signed-in user",
      "In SQL Server the view holds the rules, a scheduled job (SQL Server Agent) or the ",
      "reporting tool runs it, and ", kbd("SUSER_SNAME()"), " supplies the user, so nobody types a ",
      "username. SQL Server also has built-in row-level security policies that attach the filter ",
      "to the table itself. Tableau and Power BI do the same with a user filter or role on a ",
      "published data source."),
    interview_note(p(style = "margin-bottom:0;",
      "“What data quality issues would you expect in an SIS, and how would you catch them ",
      "before they reach a report?” This tab is the answer: overlapping and gapped ",
      "enrollments, duplicates, invalid or missing codes, records in the wrong place, and ",
      "disagreement with the state; caught by scheduled exception reports routed to the people ",
      "who can fix them, plus reconciliation before every submission."))
  )
)

DQA_DQUEST <- list(
  list(prompt = paste0("Find students with two primary enrollments that overlap in time (any year): return ",
                       "student_id, first_id, and second_id (the two enrollment_ids, smaller first)."),
       ref = "SELECT a.student_id, a.enrollment_id AS first_id, b.enrollment_id AS second_id FROM enrollments a JOIN enrollments b ON b.student_id = a.student_id AND a.enrollment_id < b.enrollment_id WHERE a.service_type = 'P' AND b.service_type = 'P' AND a.start_date <= COALESCE(b.end_date, '9999-12-31') AND b.start_date <= COALESCE(a.end_date, '9999-12-31');",
       solution = "SELECT a.student_id,\n       a.enrollment_id AS first_id,\n       b.enrollment_id AS second_id\nFROM enrollments AS a\nJOIN enrollments AS b\n  ON b.student_id = a.student_id\n AND a.enrollment_id < b.enrollment_id\nWHERE a.service_type = 'P'\n  AND b.service_type = 'P'\n  AND a.start_date <= COALESCE(b.end_date, '9999-12-31')\n  AND b.start_date <= COALESCE(a.end_date, '9999-12-31');"),
  list(prompt = paste0("For 2026 primary enrollments, return student_id, end_date, and next_start (the ",
                       "student's next enrollment start, via LEAD) for enrollments that have both an end ",
                       "date and a next start."),
       ref = "SELECT student_id, end_date, next_start FROM (SELECT student_id, end_date, LEAD(start_date) OVER (PARTITION BY student_id ORDER BY start_date, enrollment_id) AS next_start FROM enrollments WHERE school_year = 2026 AND service_type = 'P') WHERE end_date IS NOT NULL AND next_start IS NOT NULL;",
       solution = "SELECT student_id, end_date, next_start\nFROM (\n  SELECT student_id, end_date,\n         LEAD(start_date) OVER (PARTITION BY student_id\n                                ORDER BY start_date, enrollment_id) AS next_start\n  FROM enrollments\n  WHERE school_year = 2026 AND service_type = 'P'\n)\nWHERE end_date IS NOT NULL\n  AND next_start IS NOT NULL;"),
  list(prompt = paste0("Candidate duplicate students: return last_name, first_name, birth_date, and ",
                       "records for each combination shared by more than one student_id."),
       ref = "SELECT last_name, first_name, birth_date, COUNT(*) AS records FROM students GROUP BY last_name, first_name, birth_date HAVING COUNT(*) > 1;",
       solution = "SELECT last_name, first_name, birth_date, COUNT(*) AS records\nFROM students\nGROUP BY last_name, first_name, birth_date\nHAVING COUNT(*) > 1;"),
  list(prompt = "Enrollments whose grade is not in grade_levels: return enrollment_id, student_id, grade.",
       ref = "SELECT e.enrollment_id, e.student_id, e.grade FROM enrollments e LEFT JOIN grade_levels g ON g.grade = e.grade WHERE g.grade IS NULL;",
       solution = "SELECT e.enrollment_id, e.student_id, e.grade\nFROM enrollments AS e\nLEFT JOIN grade_levels AS g ON g.grade = e.grade\nWHERE g.grade IS NULL;"),
  list(prompt = paste0("Enrollments with an end_status that is not an end code in status_codes (including ",
                       "closed enrollments whose end_status is NULL): return enrollment_id and end_status. ",
                       "Open enrollments (end_date NULL) are fine and should not appear."),
       ref = "SELECT e.enrollment_id, e.end_status FROM enrollments e LEFT JOIN status_codes sc ON sc.code = e.end_status AND sc.code_type = 'end' WHERE e.end_date IS NOT NULL AND sc.code IS NULL;",
       solution = "SELECT e.enrollment_id, e.end_status\nFROM enrollments AS e\nLEFT JOIN status_codes AS sc\n       ON sc.code = e.end_status\n      AND sc.code_type = 'end'\nWHERE e.end_date IS NOT NULL\n  AND sc.code IS NULL;"),
  list(prompt = paste0("Attendance rows dated on a day that was not a school day at that school: return ",
                       "attendance_id, student_id, att_date, day_note."),
       ref = "SELECT a.attendance_id, a.student_id, a.att_date, c.day_note FROM attendance a JOIN calendar_days c ON c.school_id = a.school_id AND c.cal_date = a.att_date WHERE c.instructional = 0;",
       solution = "SELECT a.attendance_id, a.student_id, a.att_date, c.day_note\nFROM attendance AS a\nJOIN calendar_days AS c\n  ON c.school_id = a.school_id\n AND c.cal_date = a.att_date\nWHERE c.instructional = 0;"),
  list(prompt = paste0("Students with a roster row running through today (end_date on or after ",
                       "2026-03-31) but no primary enrollment open today: return student_id, once each."),
       ref = "SELECT DISTINCT r.student_id FROM roster r WHERE r.end_date >= '2026-03-31' AND NOT EXISTS (SELECT 1 FROM enrollments e WHERE e.student_id = r.student_id AND e.service_type = 'P' AND e.start_date <= '2026-03-31' AND (e.end_date IS NULL OR e.end_date >= '2026-03-31'));",
       solution = "SELECT DISTINCT r.student_id\nFROM roster AS r\nWHERE r.end_date >= '2026-03-31'\n  AND NOT EXISTS (\n        SELECT 1 FROM enrollments AS e\n        WHERE e.student_id = r.student_id\n          AND e.service_type = 'P'\n          AND e.start_date <= '2026-03-31'\n          AND (e.end_date IS NULL OR e.end_date >= '2026-03-31'));"),
  list(prompt = paste0("Rows in our October 1, 2025 count that are not in the state's return: using the ",
                       "live and state CTEs from step 6, return state_id, state_school_number, grade."),
       ref = paste0(OCT1_LIVE_STATE, " SELECT * FROM live EXCEPT SELECT * FROM state;"),
       solution = paste0(OCT1_LIVE_STATE, "\nSELECT * FROM live\nEXCEPT\nSELECT * FROM state;")),
  list(prompt = paste0("And the other direction: rows in the state's return that are not in our October 1 ",
                       "count. Return state_id, state_school_number, grade."),
       ref = paste0(OCT1_LIVE_STATE, " SELECT * FROM state EXCEPT SELECT * FROM live;"),
       solution = paste0(OCT1_LIVE_STATE, "\nSELECT * FROM state\nEXCEPT\nSELECT * FROM live;")),
  list(prompt = paste0("An exception report for Riverside High School's principal (username 'p.riverside'): ",
                       "students at schools that user may see who have a closed enrollment with no end code, ",
                       "or an enrollment grade not in grade_levels. Return rule ('No end code' or 'Bad grade'), ",
                       "school_id, student_id."),
       ref = "WITH x AS (SELECT 'No end code' AS rule, school_id, student_id FROM enrollments WHERE end_date IS NOT NULL AND end_status IS NULL UNION ALL SELECT 'Bad grade', e.school_id, e.student_id FROM enrollments e LEFT JOIN grade_levels g ON g.grade = e.grade WHERE g.grade IS NULL) SELECT x.rule, x.school_id, x.student_id FROM x JOIN user_access u ON u.scope = 'District' OR u.school_id = x.school_id WHERE u.username = 'p.riverside';",
       solution = "WITH exceptions AS (\n  SELECT 'No end code' AS rule, school_id, student_id\n  FROM enrollments\n  WHERE end_date IS NOT NULL AND end_status IS NULL\n  UNION ALL\n  SELECT 'Bad grade', e.school_id, e.student_id\n  FROM enrollments AS e\n  LEFT JOIN grade_levels AS g ON g.grade = e.grade\n  WHERE g.grade IS NULL\n)\nSELECT x.rule, x.school_id, x.student_id\nFROM exceptions AS x\nJOIN user_access AS u\n  ON u.scope = 'District' OR u.school_id = x.school_id\nWHERE u.username = 'p.riverside';")
)

source_district_quality_server <- district_tab_server(
  key = "dqa",
  step_titles = DQA_STEP_TITLES,
  step_bodies = DQA_STEPS,
  n_demo = 7,
  header = function() district_header(
    "Data quality: finding what is wrong before a report does", eyebrow = "School district · 6 of 7",
    p(style = "margin-bottom:6px;",
      "Prairie Lakes' data has been seeded with the problems real SIS data has: overlapping and ",
      "gapped enrollments, duplicate students, a bad code and missing ones, reversed dates, ",
      "records in the wrong place, and a state file that disagrees with the district's. Each ",
      "step writes the check that finds one kind, and the last turns the checks into a weekly ",
      "exception report filtered by who is allowed to see what."),
    p(style = "margin-bottom:0;",
      "Uses self-joins, ", kbd("LEAD"), ", anti-joins, ", kbd("EXCEPT"), ", ", kbd("UNION ALL"),
      ", and correlated subqueries.")),
  quest = DQA_DQUEST,
  quest_title = "Ten data-quality checks",
  quest_intro = "Every one of these returns rows. A check that returns nothing on clean data is the goal; on this data, it is a bug in the check."
)
