# =============================================================================
# tab_capstone.R — "Put it together" capstone (Tab 10)
#
# No new query syntax: 20 mixed challenges drawing on every query tab (graded by
# result-set comparison against the read-only DB), a free SELECT sandbox with
# the schema on hand, a keyword index (every keyword the tutorial teaches, by
# tab), a note on what other dialects add, a note on running SQL from R, and a
# short indexing / performance aside with a runnable EXPLAIN QUERY PLAN demo.
#
# KEYWORD_INDEX is checked by the project's verification script: every entry
# must actually appear in the tab file it is listed under. If you remove a
# topic from a tab, remove it here.
# =============================================================================

CAPSTONE_QUEST <- list(
  # --- Warm-up: one idea each ---
  list(prompt = paste0("Return each researcher's full_name and a salary_band: 'undisclosed' when the ",
                       "salary is NULL, 'six figures' at 100000 and above, otherwise 'five figures'."),
       ref = "SELECT full_name, CASE WHEN salary IS NULL THEN 'undisclosed' WHEN salary >= 100000 THEN 'six figures' ELSE 'five figures' END AS salary_band FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name,\n       CASE WHEN salary IS NULL THEN 'undisclosed'\n            WHEN salary >= 100000 THEN 'six figures'\n            ELSE 'five figures'\n       END AS salary_band\nFROM researchers;"),
  list(prompt = "Return the lab_name of every lab with no building on file (building is NULL).",
       ref = "SELECT lab_name FROM labs WHERE building IS NULL;", ordered = FALSE,
       solution = "SELECT lab_name FROM labs WHERE building IS NULL;"),
  list(prompt = paste0("Return the title and year of the three most recent studies, newest first, ",
                       "breaking ties in year by title (A to Z). Graded on order."),
       ref = "SELECT title, year FROM studies ORDER BY year DESC, title LIMIT 3;", ordered = TRUE,
       solution = "SELECT title, year FROM studies ORDER BY year DESC, title LIMIT 3;"),
  list(prompt = "For each study status, return the status and the number of studies as n.",
       ref = "SELECT status, COUNT(*) AS n FROM studies GROUP BY status;", ordered = FALSE,
       solution = "SELECT status, COUNT(*) AS n FROM studies GROUP BY status;"),
  list(prompt = "Return each researcher title held by more than one person, with its count as n.",
       ref = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;",
       ordered = FALSE,
       solution = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;"),
  list(prompt = paste0("Return each lab_id that has both at least one researcher and at least one ",
                       "study. Use a set operator."),
       ref = "SELECT lab_id FROM researchers INTERSECT SELECT lab_id FROM studies;", ordered = FALSE,
       solution = "SELECT lab_id FROM researchers\nINTERSECT\nSELECT lab_id FROM studies;"),
  list(prompt = "Return the nickname of every participant whose nickname starts with D. You know who they are.",
       ref = "SELECT nickname FROM participants WHERE nickname LIKE 'D%';", ordered = FALSE,
       solution = "SELECT nickname FROM participants WHERE nickname LIKE 'D%';"),
  # --- Combining tables ---
  list(prompt = "Return each study's title alongside its lab's lab_name.",
       ref = "SELECT s.title, l.lab_name FROM studies s JOIN labs l ON s.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT s.title, l.lab_name\nFROM studies s JOIN labs l ON s.lab_id = l.lab_id;"),
  list(prompt = "Return the title of every study that has no enrollments.",
       ref = "SELECT s.title FROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id WHERE e.enrollment_id IS NULL;",
       ordered = FALSE,
       solution = "SELECT s.title\nFROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id\nWHERE e.enrollment_id IS NULL;"),
  list(prompt = paste0("For every study, return its title and the number of enrollments as ",
                       "n_enrollments (include studies with zero, which requires a LEFT JOIN)."),
       ref = "SELECT s.title, COUNT(e.enrollment_id) AS n_enrollments FROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id GROUP BY s.study_id, s.title;",
       ordered = FALSE,
       solution = "SELECT s.title, COUNT(e.enrollment_id) AS n_enrollments\nFROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id\nGROUP BY s.study_id, s.title;"),
  list(prompt = "Return the title and the participant nickname for every enrollment (a three-table join).",
       ref = "SELECT s.title, p.nickname FROM enrollments e JOIN studies s ON e.study_id = s.study_id JOIN participants p ON e.participant_id = p.participant_id;",
       ordered = FALSE,
       solution = "SELECT s.title, p.nickname\nFROM enrollments e\nJOIN studies s ON e.study_id = s.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
  list(prompt = paste0("Return each lab_name and its number of studies as n_studies, for labs ",
                       "running more than one study."),
       ref = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies FROM labs l JOIN studies s ON l.lab_id = s.lab_id GROUP BY l.lab_id, l.lab_name HAVING COUNT(s.study_id) > 1;",
       ordered = FALSE,
       solution = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies\nFROM labs l JOIN studies s ON l.lab_id = s.lab_id\nGROUP BY l.lab_id, l.lab_name\nHAVING COUNT(s.study_id) > 1;"),
  # --- Subqueries, CTEs, windows ---
  list(prompt = "Return the full_name of every researcher earning above the overall average salary.",
       ref = "SELECT full_name FROM researchers WHERE salary > (SELECT AVG(salary) FROM researchers);",
       ordered = FALSE,
       solution = "SELECT full_name\nFROM researchers\nWHERE salary > (SELECT AVG(salary) FROM researchers);"),
  list(prompt = "Return the full_name of researchers whose lab is in 'Wexler Hall' (use a subquery).",
       ref = "SELECT full_name FROM researchers WHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');",
       ordered = FALSE,
       solution = "SELECT full_name\nFROM researchers\nWHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');"),
  list(prompt = "Return the lab_name of every lab that has at least one study (use EXISTS).",
       ref = "SELECT lab_name FROM labs l WHERE EXISTS (SELECT 1 FROM studies s WHERE s.lab_id = l.lab_id);",
       ordered = FALSE,
       solution = "SELECT lab_name\nFROM labs l\nWHERE EXISTS (SELECT 1 FROM studies s WHERE s.lab_id = l.lab_id);"),
  list(prompt = paste0("Using a CTE (WITH), count researchers per lab, then return each lab_name ",
                       "with its researcher count as n."),
       ref = "WITH rc AS (SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id) SELECT l.lab_name, rc.n FROM labs l JOIN rc ON l.lab_id = rc.lab_id;",
       ordered = FALSE,
       solution = "WITH rc AS (\n  SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id\n)\nSELECT l.lab_name, rc.n\nFROM labs l JOIN rc ON l.lab_id = rc.lab_id;"),
  list(prompt = paste0("For each lab that runs studies, return its lab_name, its number of studies as ",
                       "n_studies, and how many of those are completed as n_completed."),
       ref = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies, SUM(CASE WHEN s.status = 'completed' THEN 1 ELSE 0 END) AS n_completed FROM labs l JOIN studies s ON s.lab_id = l.lab_id GROUP BY l.lab_id, l.lab_name;",
       ordered = FALSE,
       solution = "SELECT l.lab_name,\n       COUNT(s.study_id) AS n_studies,\n       SUM(CASE WHEN s.status = 'completed' THEN 1 ELSE 0 END) AS n_completed\nFROM labs l JOIN studies s ON s.lab_id = l.lab_id\nGROUP BY l.lab_id, l.lab_name;"),
  list(prompt = paste0("Among researchers with a known salary, return full_name, salary, and ",
                       "their salary rank (highest = 1) as salary_rank."),
       ref = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank FROM researchers WHERE salary IS NOT NULL;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
  list(prompt = "Return each researcher's full_name and their lab-average salary as lab_avg.",
       ref = "SELECT full_name, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg\nFROM researchers;"),
  list(prompt = paste0("For enrollment_id 1, return trial, value, and the running total of value ",
                       "ordered by trial, as running_total."),
       ref = "SELECT trial, value, SUM(value) OVER (ORDER BY trial) AS running_total FROM measurements WHERE enrollment_id = 1;",
       ordered = FALSE,
       solution = "SELECT trial, value, SUM(value) OVER (ORDER BY trial) AS running_total\nFROM measurements\nWHERE enrollment_id = 1;")
)

# Every keyword the tutorial teaches, by the tab that teaches it. Rendered as
# the "Keyword index" and machine-checked against the tab files.
KEYWORD_INDEX <- list(
  "Start here"    = c("statement", "clause", "keyword", "identifier", "literal", "expression",
                      "predicate", "schema", "Primary key", "Foreign key", "DQL", "DML", "DDL", "TCL", "DCL"),
  "SELECT"        = c("SELECT", "FROM", "*", "AS", "DISTINCT"),
  "WHERE"         = c("WHERE", "=", "<>", "!=", "AND", "OR", "NOT", "BETWEEN", "IN", "LIKE",
                      "ESCAPE", "GLOB", "IS NULL", "IS NOT NULL"),
  "ORDER BY"      = c("ORDER BY", "ASC", "DESC", "NULLS FIRST", "NULLS LAST", "LIMIT", "OFFSET"),
  "GROUP BY"      = c("COUNT", "SUM", "AVG", "MIN", "MAX", "COUNT(DISTINCT", "GROUP_CONCAT",
                      "GROUP BY", "HAVING"),
  "Functions"     = c("||", "UPPER", "LOWER", "LENGTH", "SUBSTR", "TRIM", "REPLACE", "INSTR",
                      "ROUND", "ABS", "CAST", "CASE", "WHEN", "THEN", "ELSE", "END",
                      "COALESCE", "IFNULL", "NULLIF", "FILTER", "date(", "datetime(",
                      "strftime(", "julianday("),
  "JOINs"         = c("JOIN", "INNER JOIN", "ON", "LEFT JOIN", "RIGHT JOIN", "FULL OUTER JOIN",
                      "CROSS JOIN", "USING", "NATURAL JOIN", "self-join"),
  "UNION"         = c("UNION", "UNION ALL", "INTERSECT", "EXCEPT"),
  "Subqueries"    = c("scalar subquery", "IN", "NOT IN", "EXISTS", "NOT EXISTS", "WITH",
                      "derived table", "WITH RECURSIVE"),
  "Windows"       = c("OVER", "PARTITION BY", "ROW_NUMBER", "RANK", "DENSE_RANK", "NTILE",
                      "LAG", "LEAD", "FIRST_VALUE", "LAST_VALUE", "ROWS BETWEEN",
                      "UNBOUNDED PRECEDING", "CURRENT ROW", "FOLLOWING", "WINDOW"),
  "Changing data" = c("INSERT INTO", "VALUES", "UPDATE", "SET", "DELETE FROM", "ON CONFLICT",
                      "DO UPDATE", "DO NOTHING", "INSERT OR REPLACE", "RETURNING",
                      "BEGIN", "COMMIT", "ROLLBACK", "SAVEPOINT"),
  "Tables"        = c("CREATE TABLE", "INTEGER", "REAL", "TEXT", "BLOB", "PRIMARY KEY",
                      "NOT NULL", "UNIQUE", "CHECK", "DEFAULT", "FOREIGN KEY", "REFERENCES",
                      "ON DELETE CASCADE", "PRAGMA", "STRICT", "IF NOT EXISTS", "ALTER TABLE",
                      "ADD COLUMN", "RENAME COLUMN", "DROP COLUMN", "DROP TABLE",
                      "CREATE VIEW", "DROP VIEW", "CREATE INDEX"),
  "Put it together" = c("CREATE INDEX", "EXPLAIN QUERY PLAN")
)

# The School district tabs teach ideas rather than keywords; this lists them by
# tab, rendered under the keyword index and checked against the tab files the
# same way (tests/verify_content.R, section 9).
DISTRICT_TOPIC_INDEX <- list(
  "District data"         = c("grain", "state_id", "service_type", "calendar_days", "exceptions",
                              "T-SQL", "DECLARE", "TOP"),
  "Enrollment counts"     = c("point-in-time", "cumulative", "COUNT(DISTINCT", "scaffold",
                              "CROSS JOIN", "snapshot", "ADM"),
  "Attendance"            = c("enrolled_days", "COALESCE", "attendance rate", "consistent attendance",
                              "chronic absence", "last_present"),
  "Discipline and groups" = c("fan-out", "unduplicated", "Two or more races", "risk ratio",
                              "risk difference", "suppression"),
  "Cohorts and outcomes"  = c("cohort", "ROW_NUMBER", "documented_transfer",
                              "adjusted cohort graduation rate", "on track", "denominator", "Matched"),
  "Data quality"          = c("self-join", "LEAD", "GROUP_CONCAT", "anti-join", "EXCEPT",
                              "exception report", "user_access", "SUSER_SNAME"),
  "District challenges"   = c("As of when", "grain", "odbc", "Trusted_Connection")
)

source_capstone_server <- function(input, output, session) {

  wire_quest_editors(CAPSTONE_QUEST, "capq", input, output, session)
  wire_quest_editors(CAPSTONE_DQUEST, "dzcapq", input, output, session, con_fun = district_con_ro)

  # Free sandbox + the EXPLAIN demo (both read-only).
  sql_editor_server("sandbox", input, output, session, con_fun = institute_con_ro)
  sql_editor_server("explain_demo", input, output, session, con_fun = institute_con_ro)

  output$capstone_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1100px; padding-top: 16px;",

      scenario_card(
        "Put it together",
        eyebrow = "The capstone",
        p(style = "margin-bottom:6px;",
          "There is no new syntax on this tab. Instead, ", tags$b("twenty challenges"),
          " mix everything you have seen (filtering, sorting, grouping, ", kbd("CASE"),
          ", joining, set operators, subqueries, windows), so that the work is choosing ",
          "the tool rather than learning it. After that, a ", tags$b("free sandbox"),
          " to ask the institute whatever you like, and a short note on why some ",
          "queries are fast and others are not."),
        p(style = "margin-bottom:0;",
          "Stuck on one? ", tags$b("Show a solution"),
          " reveals a correct query. Try first, though. In my experience this is the ",
          "tab where the clauses begin to feel like one language rather than seven ",
          "rules, and that only happens if you reach for the answer before you reveal it.")
      ),

      quest_section(
        "Twenty mixed challenges",
        intro = p(style = "line-height:1.6; margin-bottom:0;",
          "Work them in any order. Each is graded against the live database. The ",
          "full schema is in the sandbox below if you need a reminder of column names, ",
          "which everyone does."),
        quest_editors_ui(CAPSTONE_QUEST, "capq")
      ),

      # ---- In a school district ----
      dsec_capstone(),

      # ---- Free sandbox ----
      div(class = "mt-5 mb-3",
        h3("Free sandbox", style = "margin-bottom:6px;"),
        p(style = "line-height:1.6; max-width:820px;",
          "Ask the institute anything. This editor runs any read-only query against ",
          "the full database, with no grading and no goal. Which lab has the highest ",
          "dropout rate? Which participant has signed up for the most studies, and is ",
          "it a Dave? Nobody has checked."),
        sql_editor("sandbox",
                   starter = "SELECT *\nFROM studies\nORDER BY year DESC;",
                   height = "150px"),
        div(class = "mt-4",
          div(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-bottom:10px;",
              "Schema reference"),
          schema_explorer())
      ),

      # ---- Keyword index ----
      div(class = "mt-5 mb-3",
        h3("Keyword index", style = "margin-bottom:6px;"),
        p(style = "line-height:1.6; max-width:820px;",
          "Every keyword and function this tutorial teaches, by the tab that teaches ",
          "it. If a term is listed here, that tab has a step or a collapsible on it; if ",
          "you are looking for one that is not, the section after this one may say why."),
        term_table(head = c("Tab", "Teaches"),
          rows = lapply(names(KEYWORD_INDEX), function(tab)
            c(tab, paste(KEYWORD_INDEX[[tab]], collapse = "  \u00b7  ")))),
        p(style = "line-height:1.6; max-width:820px; margin-top:12px;",
          "The School district tabs teach ideas about data more than new keywords. Their ",
          "topics, by tab:"),
        term_table(head = c("District tab", "Topics"),
          rows = lapply(names(DISTRICT_TOPIC_INDEX), function(tab)
            c(tab, paste(DISTRICT_TOPIC_INDEX[[tab]], collapse = "  \u00b7  "))))
      ),

      # ---- Other dialects ----
      div(class = "mt-5 mb-3",
        h3("What you will meet elsewhere", style = "margin-bottom:6px;"),
        p(style = "line-height:1.6; max-width:820px;",
          "SQLite is small on purpose. The larger databases (PostgreSQL, MySQL, SQL ",
          "Server, Oracle) share everything above, give or take a spelling, and add ",
          "things SQLite leaves out. None of these is hard once the rest is in place, ",
          "and it helps to know the names when they turn up in a job description."),
        term_table(head = c("Elsewhere", "What it is"), rows = list(
          c("TOP n · FETCH FIRST n ROWS ONLY", "LIMIT, as spelled by SQL Server and by the SQL standard respectively."),
          c("ILIKE", "PostgreSQL's case-insensitive LIKE. (Its plain LIKE is case-sensitive; SQLite's is not.)"),
          c("CONCAT() · LEN() · SUBSTRING()", "MySQL's and SQL Server's spellings of ||, LENGTH, and SUBSTR."),
          c("DATE · TIMESTAMP · INTERVAL · EXTRACT()", "Real date and time types, with arithmetic, where SQLite has text and strftime()."),
          c("BOOLEAN · NUMERIC(p, s) · VARCHAR(n)", "Enforced column types. SQLite accepts the words and enforces nothing unless the table is STRICT."),
          c("ANY · ALL · SOME", "Comparison against every row of a subquery: salary > ALL (SELECT …). SQLite lacks them; MAX(), MIN(), and EXISTS cover the same ground."),
          c("ROLLUP · CUBE · GROUPING SETS", "Subtotals and grand totals from a single GROUP BY."),
          c("LATERAL · CROSS APPLY", "A subquery in FROM that may refer to the tables before it, row by row."),
          c("MERGE · ON DUPLICATE KEY UPDATE", "Upserts, as spelled by SQL Server and Oracle, and by MySQL."),
          c("TRUNCATE TABLE", "A fast DELETE of every row."),
          c("GRANT · REVOKE · CREATE USER", "Permissions (DCL). SQLite has none; access to the file is access to everything."),
          c("CREATE SCHEMA · CREATE DATABASE", "Namespaces for tables. A SQLite database is one file with one namespace."),
          c("MATERIALIZED VIEW", "A view whose result is stored and refreshed on request: a view that trades freshness for speed."),
          c("Stored procedures · functions · triggers", "Code kept inside the database. SQLite has triggers only, and this tutorial does not cover them.")))
      ),

      # ---- From R ----
      div(class = "mt-5 mb-3",
        h3("Running SQL from R", style = "margin-bottom:6px;"),
        p(style = "line-height:1.65; max-width:840px;",
          "Everything in this tutorial ran through two R packages, ", kbd("DBI"),
          " and ", kbd("RSQLite"), ", and the same two lines will run it for you: ",
          kbd("con <- DBI::dbConnect(RSQLite::SQLite(), \"institute.sqlite\")"),
          " and then ", kbd("DBI::dbGetQuery(con, \"SELECT …\")"),
          ", which hands back a data frame. For another database, swap the first ",
          "argument (", kbd("RPostgres::Postgres()"), ", ", kbd("RMariaDB::MariaDB()"),
          ", ", kbd("odbc::odbc()"), ") and leave the rest alone. ", kbd("dbExecute()"),
          " is the counterpart for statements that change things and return no rows."),
        sql_code("library(DBI)\ncon <- dbConnect(RSQLite::SQLite(), \"data/institute.sqlite\")\n\n# Never paste a value into the SQL string. Pass it as a parameter:\ndbGetQuery(con,\n  \"SELECT title, year FROM studies WHERE status = ? AND year >= ?\",\n  params = list(\"ongoing\", 2020))\n\ndbDisconnect(con)"),
        callout_warm(tags$b("Pass values as parameters. "),
          "Building a query with ", kbd("paste0(\"… WHERE nickname = '\", x, \"'\")"),
          " works until ", kbd("x"), " contains an apostrophe, at which point it is a ",
          "syntax error if you are lucky and, if ", kbd("x"), " came from a web form, a ",
          "SQL injection if you are not: the text after the apostrophe gets run as SQL. ",
          "A ", kbd("?"), " placeholder with ", kbd("params ="), " sends the value ",
          "separately from the statement, so it can never be mistaken for code. (",
          kbd("glue::glue_sql()"), " does the same job with nicer syntax.) And if you ",
          "would rather keep writing dplyr, ", kbd("dbplyr"), " translates it: ",
          kbd("tbl(con, \"studies\") |> filter(year >= 2020) |> show_query()"),
          " prints the SQL it generated, which, after this tutorial, you can read.")
      ),

      # ---- Indexing / performance aside ----
      div(class = "mt-5 mb-4 p-4",
        style = "background:#f6fafb; border:1px solid #e0e6ea; border-radius:12px;",
        div(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#0072B2; margin-bottom:8px;",
            "One last idea: why some queries are slow"),
        h3("Indexes, in ninety seconds", style = "margin-top:0; margin-bottom:10px;"),
        p(style = "line-height:1.65; max-width:840px;",
          "To find every study in lab 3 without help, the database reads ",
          tags$b("every row"), " and checks each one, a “full table scan.” On ten ",
          "studies this takes no time at all. On ten million it takes long enough ",
          "to make coffee. An ", tags$b("index"), " is a pre-sorted lookup structure ",
          "on a column, and the comparison everyone makes is the right one: it is ",
          "the index at the back of a book. Rather than skimming all 300 pages for ",
          "“seagull,” you turn to the entry and go straight to page 212. ",
          kbd("CREATE INDEX idx_name ON studies(lab_id)"), " builds one."),
        p(style = "line-height:1.65; max-width:840px;",
          "Indexes pay off on columns you ", tags$b("filter"), " (", kbd("WHERE"),
          "), ", tags$b("join"), " on, or ", tags$b("sort"), " by, especially ",
          "foreign keys. They are not free: each one takes space and makes ",
          kbd("INSERT"), "/", kbd("UPDATE"), "/", kbd("DELETE"),
          " a little slower, since the index has to be kept current. So you index ",
          "the columns that searches actually use, not every column."),
        p(style = "line-height:1.65; max-width:840px; margin-bottom:6px;",
          "This database already ships with indexes on the busy foreign keys (",
          kbd("studies.lab_id"), ", ", kbd("researchers.lab_id"), ", ",
          kbd("enrollments.study_id"), ", ", kbd("measurements.enrollment_id"),
          "). You can ", tags$b("see the planner's choice"), " by putting ",
          kbd("EXPLAIN QUERY PLAN"), " in front of any query. Run the example, ",
          "then try one that filters on a non-indexed column like ", kbd("studies.year"),
          " and compare “USING INDEX” with “SCAN.”"),
        sql_editor("explain_demo",
                   starter = "EXPLAIN QUERY PLAN\nSELECT * FROM studies WHERE lab_id = 3;",
                   height = "110px"),
        callout_warm(tags$b("Don't optimize what isn't slow. "),
          "On six tiny tables like these, every query is instant and an index ",
          "changes nothing you could measure. Indexing matters at scale, in the ",
          "millions of rows, where a scan is the difference between milliseconds ",
          "and minutes. Reach for it when a real query is slow, and let ",
          kbd("EXPLAIN QUERY PLAN"), " tell you where, rather than guessing.")
      ),

      div(style = "text-align:center; color:#6b7c85; font-size:14px; padding:8px 0 28px 0;",
          "That is the language, at least as far as this institute needs it. ",
          tags$b("You can now read and write real SQL."), " The sandbox stays open.")
    )
  })
}
