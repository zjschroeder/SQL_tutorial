# =============================================================================
# tab_district_discipline.R — "Discipline and groups" (School district, 4 of 7)
#
# Seven gated steps: three tables, three units; the fan-out; unduplicated
# students and rates; who is in which group (federal and Minnesota race and
# ethnicity); comparing groups (risk ratios); small numbers and suppression;
# this year so far. Then a 10-question quest.
#
# RACE_CASE derives the federal seven-category race/ethnicity from the flag
# columns; it is shown in step 4 and reused by the later steps and the quest.
# =============================================================================

RACE_CASE <- "CASE WHEN s.hispanic_latino = 1 THEN 'Hispanic or Latino'
     WHEN s.race_american_indian + s.race_asian + s.race_black
          + s.race_pacific_islander + s.race_white > 1 THEN 'Two or more races'
     WHEN s.race_american_indian = 1  THEN 'American Indian or Alaska Native'
     WHEN s.race_asian = 1            THEN 'Asian'
     WHEN s.race_black = 1            THEN 'Black or African American'
     WHEN s.race_pacific_islander = 1 THEN 'Native Hawaiian or Pacific Islander'
     WHEN s.race_white = 1            THEN 'White'
END"

# Students enrolled in 2024-25 and whether each was suspended out of school
# (OSS or expulsion) for an incident that year. Shared by steps 5-6 and the quest.
SUSP_CTE <- paste0("WITH served AS (            -- everyone enrolled at any point in 2024-25
  SELECT DISTINCT student_id
  FROM enrollments
  WHERE school_year = 2025 AND service_type = 'P'
),
removed AS (                        -- suspended out of school or expelled that year
  SELECT DISTINCT r.student_id
  FROM resolutions AS r
  JOIN incidents AS i ON i.incident_id = r.incident_id
  WHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'
    AND r.resolution_code IN ('OSS', 'EXP')
),
by_group AS (
  SELECT ", gsub("\n", "\n         ", RACE_CASE), " AS race_ethnicity,
         COUNT(*)                                        AS students,
         SUM(CASE WHEN x.student_id IS NOT NULL THEN 1 ELSE 0 END) AS suspended
  FROM served AS v
  JOIN students AS s ON s.student_id = v.student_id
  LEFT JOIN removed AS x ON x.student_id = v.student_id
  GROUP BY race_ethnicity
)")

DDIS_STEP_TITLES <- c(
  "Three tables, three units",
  "The fan-out, again",
  "Students, not suspensions",
  "Who is in which group",
  "Comparing groups: risk ratios",
  "Small numbers and suppression",
  "This year so far"
)

DDIS_STEPS <- function() list(
  # ---- 1 ----
  tagList(
    district_triad(
      formal = tagList("Behavior data is a many-to-many structure: ", kbd("incidents"),
        " (events), ", kbd("incident_students"), " (event × person, with a role), and ",
        kbd("resolutions"), " (event × person × consequence). Each answers counts at a ",
        "different grain."),
      plain = tagList("One fight is one incident. It involves two students. Each gets a ",
        "consequence, and one of them gets two. Ask “how many?” and the answer is ",
        "1, 2, or 3 depending on which table you count."),
      district = tagList("In 2024-25: 501 incidents, 609 student involvements (560 as ",
        "offender, 49 as victim), and 606 resolutions.")),
    dsql_code("SELECT COUNT(*) AS incidents\nFROM incidents\nWHERE incident_date BETWEEN '2024-07-01' AND '2025-06-30';"),
    dsql_editor("dzddis1", "SELECT role, COUNT(*) AS involvements\nFROM incident_students AS p\nJOIN incidents AS i ON i.incident_id = p.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\nGROUP BY role;"),
    term_table(head = c("Question", "Count this"), rows = list(
      c("How much behavior activity was there?", "incidents"),
      c("How many students were involved?", "DISTINCT student_id in incident_students"),
      c("How many suspensions were issued?", "resolutions with OSS"),
      c("How many students were suspended?", "DISTINCT student_id in resolutions with OSS"),
      c("How much class time was lost?", "SUM(days_removed) for removals"))),
    help_details("Dates for a school year",
      tagList("Behavior events are dated, not tagged with a school year, so a year is a date ",
        "window: July 1 to June 30 here. ", kbd("BETWEEN"), " includes both ends. With dates ",
        "stored as text without a time part that is exactly right; with a datetime column, ",
        "an event at 2:00 p.m. on June 30 is after '2025-06-30' and falls out, so use ",
        kbd("< '2025-07-01'"), " instead."))
  ),

  # ---- 2 ----
  tagList(
    district_triad(
      formal = tagList("Joining two child tables to the same parent on the parent's key alone ",
        "produces their cross product within each parent: the row count is the product of ",
        "the children's counts, per parent."),
      plain = tagList("A fight with two students and three consequences, joined on the ",
        "incident only, gives six rows: every student paired with every consequence, including ",
        "the other student's. Every count and sum over those rows is wrong."),
      district = tagList("Joined on ", kbd("incident_id"), " alone, 2024-25 gives 792 rows. ",
        "There are 606 resolutions. Joining resolutions on ", kbd("(incident_id, student_id)"),
        " gives 606.")),
    dsql_code("-- Wrong: every person in the incident meets every consequence in it\nSELECT COUNT(*) AS rows_returned\nFROM incidents AS i\nJOIN incident_students AS p ON p.incident_id = i.incident_id\nJOIN resolutions AS r       ON r.incident_id = i.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30';"),
    dsql_code("-- Right: a consequence belongs to one person in one incident\nSELECT COUNT(*) AS rows_returned\nFROM incidents AS i\nJOIN incident_students AS p ON p.incident_id = i.incident_id\nJOIN resolutions AS r       ON r.incident_id = p.incident_id\n                           AND r.student_id  = p.student_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30';"),
    dsql_editor("dzddis2", "SELECT COUNT(*) AS rows_returned\nFROM incidents AS i\nJOIN incident_students AS p ON p.incident_id = i.incident_id\nJOIN resolutions AS r       ON r.incident_id = i.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30';"),
    callout_warm(tags$b("A check worth automating. "),
      "Before trusting a join, count its rows and compare with the table you meant to count. ",
      "If you are counting resolutions and the join returns more rows than ", kbd("resolutions"),
      " has, something multiplied. The same applies to every join to ", kbd("programs"),
      ": a student with three program spells appears three times.")
  ),

  # ---- 3 ----
  tagList(
    district_triad(
      formal = tagList("An ", tags$b("unduplicated count"), " counts each person once ",
        "regardless of events. A rate needs a denominator at the same unit and over the same ",
        "period: here, students enrolled at any point in the year."),
      plain = tagList("“152 suspensions” and “129 students suspended” ",
        "describe the same year. The second, divided by the number of students the district ",
        "served, is the suspension rate."),
      district = tagList("In 2024-25: 152 out-of-school suspensions and one expulsion, given to ",
        "129 different students, out of 1,434 enrolled during the year: 9.0%. Those removals ",
        "cost 481 days of school.")),
    dsql_code(paste0(SUSP_CTE, "\nSELECT SUM(students) AS served, SUM(suspended) AS suspended,\n       ROUND(100.0 * SUM(suspended) / SUM(students), 1) AS pct_suspended\nFROM by_group;")),
    dsql_editor("dzddis3", "SELECT resolution_code,\n       COUNT(*)                   AS resolutions,\n       COUNT(DISTINCT r.student_id) AS students,\n       SUM(days_removed)          AS days_removed\nFROM resolutions AS r\nJOIN incidents AS i ON i.incident_id = r.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\nGROUP BY resolution_code;"),
    help_details("Which denominator?",
      tagList("Students enrolled at any time in the year (cumulative enrollment) covers the same ",
        "period the suspensions do, which is why it is used here. October 1 enrollment is ",
        "smaller and makes rates look higher. Either is defensible if it is stated; mixing them ",
        "across reports is not, and federal and state collections each specify their own.")),
    help_details("SUSP_CTE: the CTEs used for the next steps",
      tagList("The query above starts with three CTEs: ", kbd("served"), " (students ",
        "enrolled in 2024-25), ", kbd("removed"), " (students suspended out of school or ",
        "expelled that year), and ", kbd("by_group"), ", which counts both by race and ",
        "ethnicity, built in the next step. Steps 5 and 6 query ", kbd("by_group"), "."))
  ),

  # ---- 4 ----
  tagList(
    district_triad(
      formal = tagList("Federal reporting collects ethnicity (Hispanic or Latino, yes or no) and ",
        "race (any of five) separately, then reports seven mutually exclusive categories: ",
        "Hispanic or Latino of any race, five single-race categories, and two or more races."),
      plain = tagList("The flags allow any combination. A report needs each student in exactly ",
        "one box, so the rules are applied in order: Hispanic first, then more than one race, ",
        "then the one race marked. The order is part of the definition."),
      district = tagList("The flags live in ", kbd("students"), ". Minnesota reports American ",
        "Indian students differently: a student who marks American Indian counts as American ",
        "Indian in state accountability reports, whatever else they mark. On 2025-10-01 that ",
        "is 119 Prairie Lakes students; the federal category holds 41.")),
    dsql_code(paste0("SELECT ", RACE_CASE, " AS race_ethnicity,\n       COUNT(*) AS students\nFROM students AS s\nWHERE s.student_id IN (\n  SELECT student_id FROM enrollments\n  WHERE service_type = 'P'\n    AND start_date <= '2025-10-01'\n    AND (end_date IS NULL OR end_date >= '2025-10-01'))\nGROUP BY race_ethnicity;")),
    dsql_editor("dzddis4", "SELECT SUM(race_american_indian) AS mn_american_indian,\n       SUM(CASE WHEN hispanic_latino = 0\n                 AND race_american_indian = 1\n                 AND race_asian + race_black + race_pacific_islander + race_white = 0\n                THEN 1 ELSE 0 END) AS federal_american_indian\nFROM students\nWHERE student_id IN (\n  SELECT student_id FROM enrollments\n  WHERE service_type = 'P'\n    AND start_date <= '2025-10-01'\n    AND (end_date IS NULL OR end_date >= '2025-10-01'));", height = "240px"),
    callout_warm(tags$b("Same students, different number. "),
      "If your report says 41 American Indian students and the state's says 119, neither is ",
      "an error. State the definition next to the number, every time."),
    help_details("The CASE order, line by line",
      tagList("The Hispanic test comes first because federal rules put a Hispanic student in ",
        "that category whatever races are marked. The sum of the five race flags comes next: ",
        "more than one flag is two or more races. Only then does a single flag decide. Swap the ",
        "first two lines and every Hispanic student who marked two races moves categories."))
  ),

  # ---- 5 ----
  tagList(
    district_triad(
      formal = tagList("The ", tags$b("risk"), " of an outcome in a group is the share of the ",
        "group with it. The ", tags$b("risk ratio"), " divides one group's risk by a comparison ",
        "group's; the ", tags$b("risk difference"), " subtracts them. Both depend on the ",
        "comparison group, which must be named."),
      plain = tagList("10.6% of Black students were suspended and 6.8% of white students were. ",
        "The ratio, 1.56, says Black students were about one and a half times as likely to be ",
        "suspended. The difference, 3.8 percentage points, says how much more often."),
      district = tagList("2024-25, out-of-school suspension or expulsion, students enrolled at ",
        "any point in the year, white students as the comparison group.")),
    dsql_code(paste0(SUSP_CTE, "\nSELECT g.race_ethnicity, g.students, g.suspended,\n       ROUND(100.0 * g.suspended / g.students, 1) AS pct,\n       ROUND((g.suspended * 1.0 / g.students) / (w.suspended * 1.0 / w.students), 2) AS risk_ratio_vs_white\nFROM by_group AS g\nCROSS JOIN (SELECT students, suspended FROM by_group WHERE race_ethnicity = 'White') AS w;")),
    dsql_editor("dzddis5", paste0(SUSP_CTE, "\nSELECT g.race_ethnicity, g.students, g.suspended,\n       ROUND(100.0 * g.suspended / g.students, 1) AS pct,\n       ROUND((g.suspended * 1.0 / g.students) / (w.suspended * 1.0 / w.students), 2) AS risk_ratio_vs_white\nFROM by_group AS g\nCROSS JOIN (SELECT students, suspended FROM by_group WHERE race_ethnicity = 'White') AS w;"), height = "360px"),
    p(style = "line-height:1.6;",
      "The one-row subquery holds the comparison group's numbers, and ", kbd("CROSS JOIN"),
      " attaches them to every group's row so the division can happen row by row."),
    help_details("Which comparison group?",
      tagList("White students, the largest group here, is one common choice. “All other ",
        "students” is another, and it is what federal special education rules use. The two ",
        "give different ratios for the same data. Pick one before you look at the results, ",
        "and say which in the report.")),
    callout_warm(tags$b("A ratio says where to look, not why. "),
      "These are synthetic data with disparities built in to resemble the patterns national data ",
      "show. Real disparities come from policies, practices, and circumstances that a ",
      "suspension table does not record. Report the numbers with the people who will act on ",
      "them, and with the context they will ask for: which schools, which incident types, which ",
      "consequences for the same incident type.")
  ),

  # ---- 6 ----
  tagList(
    district_triad(
      formal = tagList(tags$b("Small-cell suppression"), " withholds statistics computed on ",
        "fewer than a minimum number of students, to protect privacy and to avoid publishing ",
        "unstable rates. ", tags$b("Complementary suppression"), " withholds enough other cells ",
        "that the hidden one cannot be recovered from totals."),
      plain = tagList("Three Pacific Islander students, none suspended: publishing “0%” ",
        "tells everyone in the school that those three were not suspended, and one suspension ",
        "would have made it 33%. Hide counts under a threshold, and hide a second number if ",
        "the first can be worked out by subtraction."),
      district = tagList("With a threshold of 10, the Native Hawaiian or Pacific Islander row is ",
        "withheld. If the table also shows a total, one more row must be withheld or the hidden ",
        "count is the total minus the rest.")),
    dsql_code(paste0(SUSP_CTE, "\nSELECT race_ethnicity,\n       CASE WHEN students < 10 THEN '*' ELSE CAST(students  AS TEXT) END AS students,\n       CASE WHEN students < 10 THEN '*' ELSE CAST(suspended AS TEXT) END AS suspended,\n       CASE WHEN students < 10 THEN '*'\n            ELSE CAST(ROUND(100.0 * suspended / students, 1) AS TEXT) END AS pct\nFROM by_group;")),
    dsql_editor("dzddis6", paste0(SUSP_CTE, "\nSELECT race_ethnicity,\n       CASE WHEN students < 10 THEN '*' ELSE CAST(students  AS TEXT) END AS students,\n       CASE WHEN students < 10 THEN '*' ELSE CAST(suspended AS TEXT) END AS suspended,\n       CASE WHEN students < 10 THEN '*'\n            ELSE CAST(ROUND(100.0 * suspended / students, 1) AS TEXT) END AS pct\nFROM by_group;"), height = "340px"),
    p(style = "line-height:1.6;",
      "Every branch of a ", kbd("CASE"), " must produce the same type in SQL Server, which is ",
      "why the numbers are cast to text beside the ", kbd("'*'"), ". SQLite would let the ",
      "column hold a mix; most tools reading the result would not."),
    tsql_note("CASE WHEN students < 10 THEN '*'\n     ELSE CAST(suspended AS varchar(10)) END AS suspended",
      "Same rule, with SQL Server's type names. ", kbd("FORMAT(x, 'N1')"), " is the T-SQL way to ",
      "show one decimal place as text."),
    help_details("Thresholds in practice",
      tagList("Thresholds vary by agency and report: 10 is common for public tables, and some ",
        "use different minimums for counts and for rates. Suppression also applies to ",
        "percentages near 0 and 100, which reveal every student's outcome, and to any ",
        "combination of filters a dashboard allows. Use your organization's rule; this tab ",
        "uses 10 as an example."))
  ),

  # ---- 7 ----
  tagList(
    district_triad(
      formal = tagList("Partial-period counts grow with exposure; a rate for an incomplete year ",
        "is not comparable with a full-year rate unless the same window is used for both."),
      plain = tagList("By March 31 this year has had 396 incidents. Last year had 501 in all. ",
        "Fewer so far is not fewer this year; compare the same months."),
      district = tagList("Compare September through March of each year instead: 391 incidents ",
        "last year, 396 this year. About the same, which is a different story from ",
        "“396 against 501.”")),
    dsql_code("SELECT CASE WHEN incident_date < '2025-07-01' THEN '2024-25' ELSE '2025-26' END AS school_year,\n       COUNT(*) AS incidents_sep_to_mar\nFROM incidents\nWHERE incident_date BETWEEN '2024-09-01' AND '2025-03-31'\n   OR incident_date BETWEEN '2025-09-01' AND '2026-03-31'\nGROUP BY school_year;"),
    dsql_editor("dzddis7", "SELECT CASE WHEN incident_date < '2025-07-01' THEN '2024-25' ELSE '2025-26' END AS school_year,\n       COUNT(*) AS incidents_sep_to_mar\nFROM incidents\nWHERE incident_date BETWEEN '2024-09-01' AND '2025-03-31'\n   OR incident_date BETWEEN '2025-09-01' AND '2026-03-31'\nGROUP BY school_year;"),
    interview_note(
      p(style = "margin-bottom:6px;", "“We want to look at suspension disproportionality ",
        "across schools. How would you approach it?” A strong answer names: unduplicated ",
        "students as the unit; cumulative enrollment as the denominator, stated; the race ",
        "definition used; risk ratios with a named comparison group, plus risk differences and ",
        "counts; suppression for small groups; and comparing like with like (incident types, ",
        "schools, same months)."),
      p(style = "margin-bottom:0;", "Then mention the data checks: coding practices differ across ",
        "schools, so a school with a low rate may be recording fewer incidents, not having fewer."))
  )
)

DDIS_DQUEST <- list(
  list(prompt = paste0("Count incidents by school year, labeling each by date: incident_date before ",
                       "'2025-07-01' is '2024-25', otherwise '2025-26'. Return school_year and incidents."),
       ref = "SELECT CASE WHEN incident_date < '2025-07-01' THEN '2024-25' ELSE '2025-26' END AS school_year, COUNT(*) AS incidents FROM incidents GROUP BY school_year;",
       solution = "SELECT CASE WHEN incident_date < '2025-07-01' THEN '2024-25' ELSE '2025-26' END AS school_year,\n       COUNT(*) AS incidents\nFROM incidents\nGROUP BY school_year;"),
  list(prompt = "Incidents by type in 2024-25 (2024-07-01 to 2025-06-30): return incident_type and incidents.",
       ref = "SELECT incident_type, COUNT(*) AS incidents FROM incidents WHERE incident_date BETWEEN '2024-07-01' AND '2025-06-30' GROUP BY incident_type;",
       solution = "SELECT incident_type, COUNT(*) AS incidents\nFROM incidents\nWHERE incident_date BETWEEN '2024-07-01' AND '2025-06-30'\nGROUP BY incident_type;"),
  list(prompt = paste0("For 2024-25 incidents, how many different students received each resolution ",
                       "code? Return resolution_code and students."),
       ref = "SELECT r.resolution_code, COUNT(DISTINCT r.student_id) AS students FROM resolutions r JOIN incidents i ON i.incident_id = r.incident_id WHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30' GROUP BY r.resolution_code;",
       solution = "SELECT r.resolution_code, COUNT(DISTINCT r.student_id) AS students\nFROM resolutions AS r\nJOIN incidents AS i ON i.incident_id = r.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\nGROUP BY r.resolution_code;"),
  list(prompt = paste0("Days of school lost to out-of-school removals (OSS and EXP) in 2024-25, by the ",
                       "school where the incident happened: return school_id and days_removed."),
       ref = "SELECT i.school_id, SUM(r.days_removed) AS days_removed FROM resolutions r JOIN incidents i ON i.incident_id = r.incident_id WHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30' AND r.resolution_code IN ('OSS', 'EXP') GROUP BY i.school_id;",
       solution = "SELECT i.school_id, SUM(r.days_removed) AS days_removed\nFROM resolutions AS r\nJOIN incidents AS i ON i.incident_id = r.incident_id\nWHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\n  AND r.resolution_code IN ('OSS', 'EXP')\nGROUP BY i.school_id;"),
  list(prompt = paste0("Which 2024-25 incidents had more than one offender? Return incident_id and ",
                       "offenders."),
       ref = "SELECT p.incident_id, COUNT(*) AS offenders FROM incident_students p JOIN incidents i ON i.incident_id = p.incident_id WHERE p.role = 'Offender' AND i.incident_date BETWEEN '2024-07-01' AND '2025-06-30' GROUP BY p.incident_id HAVING COUNT(*) > 1;",
       solution = "SELECT p.incident_id, COUNT(*) AS offenders\nFROM incident_students AS p\nJOIN incidents AS i ON i.incident_id = p.incident_id\nWHERE p.role = 'Offender'\n  AND i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\nGROUP BY p.incident_id\nHAVING COUNT(*) > 1;"),
  list(prompt = paste0("Suspension rate by school for 2024-25: for each school, students served ",
                       "(distinct students with a primary enrollment there in school_year 2025), students ",
                       "suspended out of school or expelled for an incident at that school in 2024-25 (0 if ",
                       "none), and pct = 100 * suspended / served, rounded to 1. Return school_id, served, ",
                       "suspended, pct."),
       ref = "WITH served AS (SELECT school_id, COUNT(DISTINCT student_id) AS served FROM enrollments WHERE school_year = 2025 AND service_type = 'P' GROUP BY school_id), susp AS (SELECT i.school_id, COUNT(DISTINCT r.student_id) AS suspended FROM resolutions r JOIN incidents i ON i.incident_id = r.incident_id WHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30' AND r.resolution_code IN ('OSS', 'EXP') GROUP BY i.school_id) SELECT v.school_id, v.served, COALESCE(x.suspended, 0) AS suspended, ROUND(100.0 * COALESCE(x.suspended, 0) / v.served, 1) AS pct FROM served v LEFT JOIN susp x ON x.school_id = v.school_id;",
       solution = "WITH served AS (\n  SELECT school_id, COUNT(DISTINCT student_id) AS served\n  FROM enrollments\n  WHERE school_year = 2025 AND service_type = 'P'\n  GROUP BY school_id\n),\nsusp AS (\n  SELECT i.school_id, COUNT(DISTINCT r.student_id) AS suspended\n  FROM resolutions AS r\n  JOIN incidents AS i ON i.incident_id = r.incident_id\n  WHERE i.incident_date BETWEEN '2024-07-01' AND '2025-06-30'\n    AND r.resolution_code IN ('OSS', 'EXP')\n  GROUP BY i.school_id\n)\nSELECT v.school_id, v.served,\n       COALESCE(x.suspended, 0) AS suspended,\n       ROUND(100.0 * COALESCE(x.suspended, 0) / v.served, 1) AS pct\nFROM served AS v\nLEFT JOIN susp AS x ON x.school_id = v.school_id;"),
  list(prompt = paste0("Federal race/ethnicity of students enrolled (primary) on 2025-10-01, using the ",
                       "CASE from step 4: return race_ethnicity and students."),
       ref = paste0("SELECT ", RACE_CASE, " AS race_ethnicity, COUNT(*) AS students FROM students s WHERE s.student_id IN (SELECT student_id FROM enrollments WHERE service_type = 'P' AND start_date <= '2025-10-01' AND (end_date IS NULL OR end_date >= '2025-10-01')) GROUP BY race_ethnicity;"),
       solution = paste0("SELECT ", RACE_CASE, " AS race_ethnicity,\n       COUNT(*) AS students\nFROM students AS s\nWHERE s.student_id IN (\n  SELECT student_id FROM enrollments\n  WHERE service_type = 'P'\n    AND start_date <= '2025-10-01'\n    AND (end_date IS NULL OR end_date >= '2025-10-01'))\nGROUP BY race_ethnicity;")),
  list(prompt = paste0("Using the CTEs from step 3 (SUSP_CTE), return race_ethnicity, students, ",
                       "suspended, and pct (100 * suspended / students, rounded to 1) for every group."),
       ref = paste0(SUSP_CTE, " SELECT race_ethnicity, students, suspended, ROUND(100.0 * suspended / students, 1) AS pct FROM by_group;"),
       solution = paste0(SUSP_CTE, "\nSELECT race_ethnicity, students, suspended,\n       ROUND(100.0 * suspended / students, 1) AS pct\nFROM by_group;")),
  list(prompt = paste0("Risk ratio for Black or African American students compared with white students, ",
                       "2024-25 out-of-school removals, rounded to 2 decimals. One column, risk_ratio."),
       ref = paste0(SUSP_CTE, " SELECT ROUND((b.suspended * 1.0 / b.students) / (w.suspended * 1.0 / w.students), 2) AS risk_ratio FROM by_group b, by_group w WHERE b.race_ethnicity = 'Black or African American' AND w.race_ethnicity = 'White';"),
       solution = paste0(SUSP_CTE, "\nSELECT ROUND((b.suspended * 1.0 / b.students)\n             / (w.suspended * 1.0 / w.students), 2) AS risk_ratio\nFROM by_group AS b, by_group AS w\nWHERE b.race_ethnicity = 'Black or African American'\n  AND w.race_ethnicity = 'White';")),
  list(prompt = paste0("Write the suppressed version for publication: for every group, return ",
                       "race_ethnicity, and suspended shown as text, or '*' when the group has fewer than ",
                       "10 students. Name the second column suspended."),
       ref = paste0(SUSP_CTE, " SELECT race_ethnicity, CASE WHEN students < 10 THEN '*' ELSE CAST(suspended AS TEXT) END AS suspended FROM by_group;"),
       solution = paste0(SUSP_CTE, "\nSELECT race_ethnicity,\n       CASE WHEN students < 10 THEN '*'\n            ELSE CAST(suspended AS TEXT) END AS suspended\nFROM by_group;"))
)

source_district_discipline_server <- district_tab_server(
  key = "ddis",
  step_titles = DDIS_STEP_TITLES,
  step_bodies = DDIS_STEPS,
  n_demo = 7,
  header = function() district_header(
    "Discipline and student groups", eyebrow = "School district · 4 of 7",
    p(style = "margin-bottom:6px;",
      "Behavior data is where a careless join does the most damage, and where group ",
      "comparisons draw the most scrutiny. This tab separates the three things people mean by ",
      "“suspensions,” builds unduplicated counts and rates, assigns each student to ",
      "one race and ethnicity category under federal and Minnesota rules, compares groups with ",
      "risk ratios, and suppresses the small numbers before anything is published."),
    p(style = "margin-bottom:0;",
      "The data are synthetic. The patterns in them were built to resemble the ones real ",
      "discipline data show, so that the methods have something to find; they are not a ",
      "description of any real district.")),
  quest = DDIS_DQUEST,
  quest_title = "Ten discipline and group questions",
  quest_intro = "Watch the unit in every question: incidents, students, resolutions, or days."
)
