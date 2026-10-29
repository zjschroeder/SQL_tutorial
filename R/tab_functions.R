# =============================================================================
# tab_functions.R — "Functions and CASE" walkthrough
#
# Sits after GROUP BY and before JOINs. Six gated steps (text functions and ||;
# numbers, integer division, CAST; CASE; COALESCE / IFNULL / NULLIF;
# conditional aggregation with SUM(CASE ...) and FILTER; dates) then a
# 10-question quest.
#
# Placement follows the sequenced courses (Mode puts CASE directly after
# aggregation, because conditional aggregation needs both), and it lets the
# JOINs tab use COALESCE on the NULLs a LEFT JOIN produces.
#
# Nothing graded here uses 'now' or any other clock-dependent value, and no
# graded item uses the math extension (sqrt, power, ...), which is a
# compile-time option that not every SQLite build, webR's included, is
# guaranteed to have. ROUND and ABS are core and safe.
# =============================================================================

FUNC_QUEST <- list(
  list(prompt = "Return every researcher's full_name in capitals, as a column named shouting.",
       ref = "SELECT UPPER(full_name) AS shouting FROM researchers;", ordered = FALSE,
       solution = "SELECT UPPER(full_name) AS shouting FROM researchers;"),
  list(prompt = "Return each participant's nickname and the number of characters in it as n_chars.",
       ref = "SELECT nickname, LENGTH(nickname) AS n_chars FROM participants;", ordered = FALSE,
       solution = "SELECT nickname, LENGTH(nickname) AS n_chars FROM participants;"),
  list(prompt = paste0("Return one column, label, that reads like Wesley Tern (Grad Student): the ",
                       "researcher's full_name, a space, and the title in parentheses."),
       ref = "SELECT full_name || ' (' || title || ')' AS label FROM researchers;", ordered = FALSE,
       solution = "SELECT full_name || ' (' || title || ')' AS label FROM researchers;"),
  list(prompt = paste0("Return each study's title and its sample_size expressed in dozens, as dozens, ",
                       "rounded to one decimal place. (Divide by 12.0, not 12; step 2 says why.)"),
       ref = "SELECT title, ROUND(sample_size / 12.0, 1) AS dozens FROM studies;", ordered = FALSE,
       solution = "SELECT title, ROUND(sample_size / 12.0, 1) AS dozens FROM studies;"),
  list(prompt = paste0("Return each study's title, sample_size, and a size_band: 'small' below 30, ",
                       "'medium' from 30 to 59, 'large' at 60 and above."),
       ref = "SELECT title, sample_size, CASE WHEN sample_size < 30 THEN 'small' WHEN sample_size < 60 THEN 'medium' ELSE 'large' END AS size_band FROM studies;",
       ordered = FALSE,
       solution = "SELECT title, sample_size,\n       CASE WHEN sample_size < 30 THEN 'small'\n            WHEN sample_size < 60 THEN 'medium'\n            ELSE 'large'\n       END AS size_band\nFROM studies;"),
  list(prompt = paste0("Return each researcher's full_name and a pay_status that reads 'undisclosed' ",
                       "when the salary is NULL and 'disclosed' otherwise."),
       ref = "SELECT full_name, CASE WHEN salary IS NULL THEN 'undisclosed' ELSE 'disclosed' END AS pay_status FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name,\n       CASE WHEN salary IS NULL THEN 'undisclosed' ELSE 'disclosed' END AS pay_status\nFROM researchers;"),
  list(prompt = paste0("Return each lab's lab_name and its building, as building, with the text ",
                       "'between buildings' standing in wherever the building is NULL. Use COALESCE."),
       ref = "SELECT lab_name, COALESCE(building, 'between buildings') AS building FROM labs;", ordered = FALSE,
       solution = "SELECT lab_name, COALESCE(building, 'between buildings') AS building FROM labs;"),
  list(prompt = paste0("In a single row, return how many studies are completed, ongoing, and ",
                       "abandoned, as columns n_completed, n_ongoing, and n_abandoned."),
       ref = "SELECT SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) AS n_completed, SUM(CASE WHEN status = 'ongoing' THEN 1 ELSE 0 END) AS n_ongoing, SUM(CASE WHEN status = 'abandoned' THEN 1 ELSE 0 END) AS n_abandoned FROM studies;",
       ordered = FALSE,
       solution = "SELECT SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) AS n_completed,\n       SUM(CASE WHEN status = 'ongoing'   THEN 1 ELSE 0 END) AS n_ongoing,\n       SUM(CASE WHEN status = 'abandoned' THEN 1 ELSE 0 END) AS n_abandoned\nFROM studies;"),
  list(prompt = paste0("For each lab_id in the researchers table, return the lab_id, the head count ",
                       "as n, and the number of PIs among them as n_pis."),
       ref = "SELECT lab_id, COUNT(*) AS n, SUM(CASE WHEN title = 'PI' THEN 1 ELSE 0 END) AS n_pis FROM researchers GROUP BY lab_id;",
       ordered = FALSE,
       solution = "SELECT lab_id,\n       COUNT(*) AS n,\n       SUM(CASE WHEN title = 'PI' THEN 1 ELSE 0 END) AS n_pis\nFROM researchers\nGROUP BY lab_id;"),
  list(prompt = paste0("Sort the participants into an age_band ('unknown' when age is NULL, 'under 30', ",
                       "or '30 and over') and return each age_band with its head count as n."),
       ref = "SELECT CASE WHEN age IS NULL THEN 'unknown' WHEN age < 30 THEN 'under 30' ELSE '30 and over' END AS age_band, COUNT(*) AS n FROM participants GROUP BY 1;",
       ordered = FALSE,
       solution = "SELECT CASE WHEN age IS NULL THEN 'unknown'\n            WHEN age < 30    THEN 'under 30'\n            ELSE '30 and over'\n       END AS age_band,\n       COUNT(*) AS n\nFROM participants\nGROUP BY age_band;")
)

FUNC_STEP_TITLES <- c(
  "Text: ||, UPPER, LENGTH, SUBSTR",
  "Numbers: ROUND, division, CAST",
  "If-then logic with CASE",
  "Standing in for NULL: COALESCE",
  "Counting with conditions",
  "Dates and times"
)

source_functions_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$func_step <- reactive(walk$step)
  outputOptions(output, "func_step", suspendWhenHidden = FALSE)

  observeEvent(input$func_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$func_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$func_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$func_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$func_continue5, walk$step <- max(walk$step, 6))

  output$func_toc <- renderUI(
    walkthrough_toc(walk$step, FUNC_STEP_TITLES, "func-step"))

  wire_demo_editors("fsed", 6, input, output, session)
  wire_quest_editors(FUNC_QUEST, "funq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzfun", 1, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(FUNC_DQUEST, "dzfunq", input, output, session, con_fun = district_con_ro)

  output$functions_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("func_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Functions and CASE — computing values row by row",
            eyebrow = "Expressions",
            p(style = "margin-bottom:6px;",
              "Recall the SELECT tab's last step, where ", kbd("salary * 1.03"),
              " produced a column that no table contains. Arithmetic is the smallest ",
              "case of something general: anywhere SQL expects a value, you can put an ",
              tags$b("expression"), " that computes one, and SQL supplies functions for ",
              "text, numbers, and dates, plus ", kbd("CASE"), " for if-then logic. None of ",
              "this adds a clause. It changes what you can write inside the clauses ",
              "you already have."),
            p(style = "margin-bottom:0;",
              "For an R user: these are ", kbd("toupper()"), ", ", kbd("nchar()"), ", ",
              kbd("substr()"), ", ", kbd("paste0()"), ", ", kbd("round()"), ", and ",
              kbd("dplyr::case_when()"), " and ", kbd("coalesce()"),
              ", under slightly different names. The one habit to unlearn is ",
              kbd("/"), ", which in SQL does something R's never would (step 2).")
          ),

          # ===== STEP 1 =====
          step_container(1, "Text: ||, UPPER, LENGTH, SUBSTR",
            explanation_triad(
              formal = tagList(
                "Scalar functions take one row's values and return one value. For text, ",
                "the operator ", tags$code("||"), " concatenates; ", tags$code("UPPER"),
                " and ", tags$code("LOWER"), " change case; ", tags$code("LENGTH"),
                " counts characters; ", tags$code("SUBSTR(x, start, n)"),
                " extracts a substring, counting from 1; ", tags$code("TRIM"),
                " removes surrounding spaces; ", tags$code("REPLACE(x, from, to)"),
                " substitutes."),
              plain = tagList(
                "A function goes wherever a column name could go, and it is applied to ",
                "every row separately. ", kbd("||"), " glues pieces of text end to end, ",
                "and the pieces can be columns, quoted text, or both. Positions in a ",
                "string start at 1, which will feel like home."),
              lab = tagList(
                kbd("full_name || ' (' || title || ')'"), " turns two columns into ",
                "“Wesley Tern (Grad Student).” ", kbd("LENGTH(nickname)"),
                " is 4 for each of the Daves and 17 for Lord Stridulation, a cricket.")
            ),
            sql_code("SELECT full_name || ' (' || title || ')' AS label,\n       UPPER(title)               AS title_caps,\n       LENGTH(full_name)          AS n_chars,\n       REPLACE(full_name, 'Dr. ', '') AS no_honorific\nFROM researchers;"),
            sql_editor("fsed1", starter = "SELECT full_name || ' (' || title || ')' AS label,\n       UPPER(title)               AS title_caps,\n       LENGTH(full_name)          AS n_chars,\n       REPLACE(full_name, 'Dr. ', '') AS no_honorific\nFROM researchers;", height = "150px"),
            term_table(list(
              c("a || b", "a and b joined end to end. If either is NULL, so is the result."),
              c("UPPER(x), LOWER(x)", "x in capitals, or in lower case. (SQLite changes only the letters A to Z.)"),
              c("LENGTH(x)", "The number of characters in x."),
              c("SUBSTR(x, 5, 3)", "Three characters of x, starting at the fifth. Leave off the 3 to run to the end."),
              c("TRIM(x)", "x with the spaces at both ends removed. LTRIM and RTRIM do one end each."),
              c("REPLACE(x, 'a', 'b')", "x with every 'a' changed to 'b'."),
              c("INSTR(x, 'a')", "The position of the first 'a' in x, or 0 if there is none."))),
            callout_warm(tags$b("Function names are the least portable part of SQL. "),
              "Everything above works in SQLite and PostgreSQL. MySQL and SQL Server ",
              "glue text with ", kbd("CONCAT(a, b)"), " (SQL Server also uses ", kbd("+"),
              "), SQL Server spells ", kbd("LENGTH"), " as ", kbd("LEN"), " and ",
              kbd("SUBSTR"), " as ", kbd("SUBSTRING"), ", and so on. The ideas carry ",
              "over intact; when you change databases, expect to look up the spellings."),
            continue_button("func_continue1"),
            id_prefix = "func-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.func_step >= 2",
          step_container(2, "Numbers: ROUND, division, CAST",
            explanation_triad(
              formal = tagList(
                "The arithmetic operators are ", tags$code("+"), ", ", tags$code("-"), ", ",
                tags$code("*"), ", ", tags$code("/"), ", and ", tags$code("%"),
                " (remainder). Division of one integer by another yields an integer, ",
                "truncated toward zero. ", tags$code("ROUND(x, d)"), " rounds to ",
                tags$code("d"), " decimal places and ", tags$code("ABS(x)"),
                " gives the absolute value. ", tags$code("CAST(x AS type)"),
                " converts a value to another type."),
              plain = tagList(
                kbd("5 / 2"), " is ", kbd("2"), ". Both numbers are whole, so SQL gives a ",
                "whole answer and discards the rest without comment. Make either side a ",
                "decimal, ", kbd("5 / 2.0"), ", and you get ", kbd("2.5"),
                ". This is the single most common way to get a wrong number out of a ",
                "correct-looking query."),
              lab = tagList(
                "The seagull study enrolled 40. ", kbd("sample_size / 12"),
                " says that is 3 dozen; ", kbd("sample_size / 12.0"),
                " says 3.33. The mean sample size across the ten studies is 52.9, and ",
                kbd("ROUND(AVG(sample_size))"), " reports it as 53.")
            ),
            sql_code("SELECT title,\n       sample_size,\n       sample_size / 12            AS dozens_truncated,\n       sample_size / 12.0          AS dozens,\n       ROUND(sample_size / 12.0, 1) AS dozens_rounded,\n       sample_size % 12            AS left_over\nFROM studies;"),
            sql_editor("fsed2", starter = "SELECT title,\n       sample_size,\n       sample_size / 12            AS dozens_truncated,\n       sample_size / 12.0          AS dozens,\n       ROUND(sample_size / 12.0, 1) AS dozens_rounded,\n       sample_size % 12            AS left_over\nFROM studies;", height = "170px"),
            help_details("CAST, and when both sides are columns",
              tagList("Writing ", kbd("12.0"), " works when one side is a number you typed. ",
                "When both sides are integer columns, convert one: ",
                kbd("CAST(dropped AS REAL) / enrolled"), ", or multiply by ", kbd("1.0"),
                " first, which does the same job with less ceremony. ", kbd("CAST"),
                " also goes the other way: ", kbd("CAST('42' AS INTEGER)"), " is the number 42, and ",
                kbd("CAST(3.9 AS INTEGER)"), " is 3, because casting to an integer truncates ",
                "and does not round. (", kbd("AVG"), " always returns a decimal, so an ",
                "average never needs this treatment.)")),
            help_details("Dividing by zero",
              tagList("SQLite returns ", kbd("NULL"), " for ", kbd("x / 0"),
                " and carries on. PostgreSQL and SQL Server stop the whole query with an ",
                "error. Step 4 has the portable way to get SQLite's behavior everywhere: ",
                kbd("x / NULLIF(y, 0)"), ".")),
            continue_button("func_continue2"),
            id_prefix = "func-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.func_step >= 3",
          step_container(3, "If-then logic with CASE",
            explanation_triad(
              formal = tagList(
                "A ", tags$code("CASE"), " expression evaluates its ", tags$code("WHEN"),
                " conditions in order and returns the ", tags$code("THEN"),
                " value of the first that is true, or the ", tags$code("ELSE"),
                " value if none is. With no ", tags$code("ELSE"), ", it returns ",
                tags$code("NULL"), ". It is an expression: it yields a value, and may ",
                "appear wherever a value can."),
              plain = tagList(
                "SQL's if / else-if / else. The conditions are tried from the top, the ",
                "first one that holds wins, and the rest are never looked at, so order ",
                "them from most specific to least. Finish with ", kbd("END"),
                " and, nearly always, an ", kbd("AS"), " to name the result."),
              lab = tagList(
                "Banding the ten studies at 30 and 60 participants gives two small, six ",
                "medium, and two large. The cricket telepathy study (n = 8) is small. ",
                "The climate study (n = 180) is large, and is also the only study at the ",
                "institute whose sample size a reviewer would not query.")
            ),
            sql_code("SELECT title, sample_size,\n       CASE WHEN sample_size < 30 THEN 'small'\n            WHEN sample_size < 60 THEN 'medium'\n            ELSE 'large'\n       END AS size_band\nFROM studies;"),
            sql_editor("fsed3", starter = "SELECT title, sample_size,\n       CASE WHEN sample_size < 30 THEN 'small'\n            WHEN sample_size < 60 THEN 'medium'\n            ELSE 'large'\n       END AS size_band\nFROM studies;", height = "160px"),
            callout_warm(tags$b("Leave off ELSE and the leftovers become NULL. "),
              "Delete the ", kbd("ELSE 'large'"), " line above and run it again: the two ",
              "large studies get a ", kbd("NULL"), " band, with no warning. A ", kbd("CASE"),
              " that does not mention a possibility has an answer for it anyway, and the ",
              "answer is NULL. I write the ", kbd("ELSE"), " every time, even when I am ",
              "sure the conditions cover everything, since being sure is how the NULLs get in."),
            help_details("The short form: CASE x WHEN …",
              tagList("When every condition tests the same column for equality, there is a ",
                "shorthand: ", kbd("CASE status WHEN 'completed' THEN 'done' WHEN 'ongoing' THEN 'running' ELSE 'shelved' END"),
                ". It can only test equality, so it cannot do ranges, and it cannot catch ",
                "NULL (", kbd("WHEN NULL"), " is an ", kbd("= NULL"),
                " test, which is never true). For those, use the long form.")),
            help_details("CASE outside the select list",
              tagList(kbd("CASE"), " is a value, so it can go in ", kbd("WHERE"), ", ",
                kbd("GROUP BY"), ", or ", kbd("ORDER BY"), " too. The ORDER BY use is the ",
                "one worth knowing: ", kbd("ORDER BY CASE status WHEN 'ongoing' THEN 1 WHEN 'completed' THEN 2 ELSE 3 END"),
                " sorts in an order you define, where alphabetical would have put ",
                "“abandoned” first.")),
            continue_button("func_continue3"),
            id_prefix = "func-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.func_step >= 4",
          step_container(4, "Standing in for NULL: COALESCE",
            explanation_triad(
              formal = tagList(
                tags$code("COALESCE(a, b, …)"), " returns its first non-NULL argument, or ",
                tags$code("NULL"), " if all are NULL. ", tags$code("IFNULL(a, b)"),
                " is SQLite's two-argument form of the same function. ",
                tags$code("NULLIF(a, b)"), " is the reverse operation: it returns ",
                tags$code("NULL"), " when ", tags$code("a = b"), " and ", tags$code("a"),
                " otherwise."),
              plain = tagList(
                kbd("COALESCE(building, 'between buildings')"), " reads as “the building, ",
                "or failing that, this.” It is how you keep a NULL from wiping out a ",
                "calculation or leaving a hole in a report. ", kbd("NULLIF"),
                " goes the other way and turns a placeholder value back into an honest NULL."),
              lab = tagList(
                "The Climate Affect Collective has no building. ",
                kbd("lab_name || ' in ' || building"), " therefore comes back NULL for that ",
                "lab, whole sentence and all, because anything glued to NULL is NULL. Wrap ",
                "the column in ", kbd("COALESCE"), " and the sentence survives.")
            ),
            sql_code("SELECT lab_name || ' in ' || building AS fragile,\n       lab_name || ' in ' || COALESCE(building, 'no fixed abode') AS sturdy\nFROM labs;"),
            sql_editor("fsed4", starter = "SELECT lab_name || ' in ' || building AS fragile,\n       lab_name || ' in ' || COALESCE(building, 'no fixed abode') AS sturdy\nFROM labs;"),
            callout_warm(tags$b("A stand-in is a claim about the data. "),
              kbd("COALESCE(salary, 0)"), " makes the arithmetic run, and it also asserts ",
              "that the two researchers who declined to state a salary earn nothing, which ",
              "will drag an average down by a sixth. Substituting a label for a missing ",
              "building is harmless. Substituting a number for a missing number is an ",
              "analytic decision, and it should be one you made on purpose. (R users have ",
              "had this argument before, about ", kbd("na.rm = TRUE"), ".)"),
            help_details("NULLIF, and the division-by-zero idiom",
              tagList("Some datasets record “missing” as 0, or -99, or an empty string. ",
                kbd("NULLIF(age, 0)"), " converts such a value back to NULL so that ",
                kbd("AVG"), " and ", kbd("COUNT"), " skip it as they should. Its other ",
                "standard use is as a guard: ", kbd("dropped / NULLIF(enrolled, 0)"),
                " returns NULL when nobody enrolled, in every database, where a bare ",
                "division would be an error in most of them.")),
            continue_button("func_continue4"),
            id_prefix = "func-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.func_step >= 5",
          step_container(5, "Counting with conditions",
            explanation_triad(
              formal = tagList(
                "Placing a ", tags$code("CASE"), " expression inside an aggregate restricts ",
                "what that aggregate counts or sums, independently of the other ",
                "aggregates in the same query (conditional aggregation). The standard ",
                tags$code("FILTER (WHERE …)"), " clause, written after an aggregate, ",
                "expresses the same restriction directly."),
              plain = tagList(
                kbd("WHERE"), " filters the whole query, so it can give you the completed ",
                "studies or the ongoing ones, not both counts side by side. ",
                kbd("SUM(CASE WHEN … THEN 1 ELSE 0 END)"), " gives each column its own ",
                "private filter: score a row 1 if it qualifies and 0 if not, then add up ",
                "the scores. It is how one query returns several differently-filtered ",
                "totals, and how rows get pivoted into columns."),
              lab = tagList(
                "One row, three counts: six studies completed, three ongoing, one ",
                "abandoned. With ", kbd("GROUP BY status"), " you would get the same ",
                "numbers as three rows; this form puts them in three columns, which is ",
                "what a table in a paper usually wants.")
            ),
            sql_code("SELECT COUNT(*) AS n_studies,\n       SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) AS n_completed,\n       SUM(CASE WHEN status = 'ongoing'   THEN 1 ELSE 0 END) AS n_ongoing,\n       COUNT(*) FILTER (WHERE status = 'abandoned')          AS n_abandoned\nFROM studies;"),
            sql_editor("fsed5", starter = "SELECT COUNT(*) AS n_studies,\n       SUM(CASE WHEN status = 'completed' THEN 1 ELSE 0 END) AS n_completed,\n       SUM(CASE WHEN status = 'ongoing'   THEN 1 ELSE 0 END) AS n_ongoing,\n       COUNT(*) FILTER (WHERE status = 'abandoned')          AS n_abandoned\nFROM studies;", height = "150px"),
            callout_warm(tags$b("SUM(CASE …) or FILTER? "),
              kbd("FILTER"), " is easier to read, and SQLite and PostgreSQL both have it. ",
              "MySQL and SQL Server do not, so ", kbd("SUM(CASE …)"), " is the form you ",
              "will meet in other people's queries, and the one that runs everywhere. ",
              "Add a ", kbd("GROUP BY lab_id"), " to either and you get the same set of ",
              "conditional counts once per lab."),
            help_details("Grouping by a CASE",
              tagList("A ", kbd("CASE"), " can also define the groups. ",
                kbd("SELECT CASE WHEN age IS NULL THEN 'unknown' WHEN age < 30 THEN 'under 30' ELSE '30 and over' END AS age_band, COUNT(*) AS n FROM participants GROUP BY age_band"),
                " sorts the 24 participants into three bands that exist nowhere in the ",
                "table (eight under 30, ten at 30 and over, six unknown). SQLite lets ",
                kbd("GROUP BY"), " use the alias. Databases that do not will take ",
                kbd("GROUP BY 1"), ", meaning “the first output column,” or the ",
                kbd("CASE"), " written out a second time.")),
            continue_button("func_continue5"),
            id_prefix = "func-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.func_step >= 6",
          step_container(6, "Dates and times",
            explanation_triad(
              formal = tagList(
                "SQLite has no date type. Dates are stored as ISO-8601 text (",
                tags$code("'2024-03-15'"), "), which sorts and compares correctly as ",
                "text, and are manipulated with ", tags$code("date()"), ", ",
                tags$code("datetime()"), ", and ", tags$code("strftime()"),
                ". Each takes a time value followed by optional modifiers such as ",
                tags$code("'+1 month'"), " or ", tags$code("'start of year'"),
                "; the special value ", tags$code("'now'"), " is the current time in UTC."),
              plain = tagList(
                "Write dates year-first, ", kbd("'2024-03-15'"), ", and the ordinary ",
                "comparisons work on them: ", kbd("<"), ", ", kbd("BETWEEN"), ", ",
                kbd("ORDER BY"), ". Date arithmetic goes through ", kbd("date()"),
                " with a modifier. Pulling out a part (the year, the month) goes through ",
                kbd("strftime()"), ", whose format codes (", kbd("%Y"), ", ", kbd("%m"),
                ", ", kbd("%d"), ") will look familiar from R's ", kbd("format()"), "."),
              lab = tagList(
                "The institute, characteristically, recorded the year of everything and ",
                "the date of nothing, so the example computes from ", kbd("'now'"),
                ": each researcher's years of service, from ", kbd("start_year"),
                ". Dr. Priya Eustachian, who started in 1990, has the most.")
            ),
            sql_code("SELECT full_name,\n       start_year,\n       CAST(strftime('%Y', 'now') AS INTEGER) - start_year AS years_here\nFROM researchers\nORDER BY years_here DESC;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then replace the whole query with ",
              kbd("SELECT date('now'), date('now', '+30 days'), date('now', 'start of month');"),
              " to see the modifiers at work:"),
            sql_editor("fsed6", starter = "SELECT full_name,\n       start_year,\n       CAST(strftime('%Y', 'now') AS INTEGER) - start_year AS years_here\nFROM researchers\nORDER BY years_here DESC;", height = "140px"),
            term_table(list(
              c("date('now')", "Today, as 'YYYY-MM-DD'. datetime('now') adds the time of day."),
              c("date(d, '+1 month')", "d moved by an interval: days, months, years, hours, and so on; minus signs work too."),
              c("date(d, 'start of year')", "d snapped back to January 1. Also 'start of month' and 'start of day'."),
              c("strftime('%Y', d)", "A piece of d, as text: %Y year, %m month, %d day, %H hour, %w weekday (0 is Sunday)."),
              c("julianday(a) - julianday(b)", "The number of days from b to a, fractions included."))),
            callout_warm(tags$b("This step is the most SQLite-specific in the tutorial. "),
              "PostgreSQL, MySQL, and SQL Server have real ", kbd("DATE"), " and ",
              kbd("TIMESTAMP"), " types, and their own function names: ", kbd("CURRENT_DATE"),
              ", ", kbd("EXTRACT(YEAR FROM d)"), ", ", kbd("d + INTERVAL '1 month'"),
              ", ", kbd("DATEADD"), ", ", kbd("DATEDIFF"), ". What transfers is the habit ",
              "of storing dates year-first and the shape of the three operations: get ",
              "the current date, shift a date, take a date apart."),
            help_details("Why strftime() needed a CAST",
              tagList(kbd("strftime()"), " returns text, so ", kbd("strftime('%Y', 'now')"),
                " is the string ", kbd("'2026'"), " and not the number. SQLite will ",
                "convert it for you in arithmetic, but a comparison such as ",
                kbd("strftime('%Y', d) = 2026"), " is comparing text with a number and is ",
                "quietly false for every row. ", kbd("CAST(… AS INTEGER)"),
                " removes the question.")),
            continue_button("func_done", "I've got functions ↓"),
            id_prefix = "func-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.func_step >= 6",
          quest_section(
            "Ten queries to make expressions automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("||"), ", ", kbd("UPPER"), ", ", kbd("LENGTH"), ", ",
              kbd("ROUND"), ", ", kbd("CASE"), ", ", kbd("COALESCE"),
              ", and conditional aggregation. Nothing here is graded on dates, since ",
              "an answer that depends on today would be wrong by tomorrow. Use the ",
              "exact output column names each prompt asks for. Tables: ", kbd("labs"),
              ", ", kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), "."),
            quest_editors_ui(FUNC_QUEST, "funq")
          ),
          dsec_functions())
        )
      )
    )
  })
}
