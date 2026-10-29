# =============================================================================
# tab_subqueries.R — "Subqueries and CTEs" walkthrough (Tab 7)
#
# Seven gated steps (scalar subquery; subquery with IN; EXISTS / correlated; the
# NOT IN + NULL trap; CTEs with WITH; subqueries in FROM and in SELECT;
# recursive CTEs) then a 10-question quest.
# =============================================================================

SUBQ_QUEST <- list(
  list(prompt = paste0("Return the full_name and salary of researchers who earn more than the ",
                       "overall average salary. Use a subquery."),
       ref = "SELECT full_name, salary FROM researchers WHERE salary > (SELECT AVG(salary) FROM researchers);",
       ordered = FALSE,
       solution = "SELECT full_name, salary\nFROM researchers\nWHERE salary > (SELECT AVG(salary) FROM researchers);"),
  list(prompt = paste0("Return the title of every study run by the 'Avian Intimidation Lab'. ",
                       "Look up its lab_id with a subquery rather than by peeking at the table."),
       ref = "SELECT title FROM studies WHERE lab_id = (SELECT lab_id FROM labs WHERE lab_name = 'Avian Intimidation Lab');",
       ordered = FALSE,
       solution = "SELECT title\nFROM studies\nWHERE lab_id = (SELECT lab_id FROM labs WHERE lab_name = 'Avian Intimidation Lab');"),
  list(prompt = "Return the full_name of researchers whose lab is in 'Wexler Hall'. Use a subquery with IN.",
       ref = "SELECT full_name FROM researchers WHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');",
       ordered = FALSE,
       solution = "SELECT full_name\nFROM researchers\nWHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');"),
  list(prompt = paste0("Return the average number of researchers per lab, as avg_per_lab, leaving out ",
                       "the researcher who has no lab. Count per lab in a subquery in FROM, then ",
                       "average the counts. (An average of counts cannot be done in one level; step 6.)"),
       ref = "SELECT AVG(n) AS avg_per_lab FROM (SELECT COUNT(*) AS n FROM researchers WHERE lab_id IS NOT NULL GROUP BY lab_id) AS per_lab;",
       ordered = FALSE,
       solution = "SELECT AVG(n) AS avg_per_lab\nFROM (\n  SELECT COUNT(*) AS n\n  FROM researchers\n  WHERE lab_id IS NOT NULL\n  GROUP BY lab_id\n) AS per_lab;"),
  list(prompt = "Return the nickname of participants enrolled in no study. Use NOT IN. (Safe here; step 4 explains when it isn't.)",
       ref = "SELECT nickname FROM participants WHERE participant_id NOT IN (SELECT participant_id FROM enrollments);",
       ordered = FALSE,
       solution = "SELECT nickname\nFROM participants\nWHERE participant_id NOT IN (SELECT participant_id FROM enrollments);"),
  list(prompt = "Return the title of studies that have at least one enrollment. Use EXISTS.",
       ref = "SELECT s.title FROM studies s WHERE EXISTS (SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id);",
       ordered = FALSE,
       solution = "SELECT s.title\nFROM studies s\nWHERE EXISTS (SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id);"),
  list(prompt = "Return the title of studies with no enrollments. Use NOT EXISTS.",
       ref = "SELECT s.title FROM studies s WHERE NOT EXISTS (SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id);",
       ordered = FALSE,
       solution = "SELECT s.title\nFROM studies s\nWHERE NOT EXISTS (SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id);"),
  list(prompt = paste0("Return the lab_name of labs that have more than one researcher. Use a ",
                       "subquery (GROUP BY ... HAVING) with IN."),
       ref = "SELECT lab_name FROM labs WHERE lab_id IN (SELECT lab_id FROM researchers GROUP BY lab_id HAVING COUNT(*) > 1);",
       ordered = FALSE,
       solution = "SELECT lab_name\nFROM labs\nWHERE lab_id IN (\n  SELECT lab_id FROM researchers GROUP BY lab_id HAVING COUNT(*) > 1\n);"),
  list(prompt = paste0("Using a CTE (WITH), compute the number of researchers per lab, then return ",
                       "each lab_name with its researcher count as n."),
       ref = "WITH lab_counts AS (SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id) SELECT l.lab_name, c.n FROM labs l JOIN lab_counts c ON l.lab_id = c.lab_id;",
       ordered = FALSE,
       solution = "WITH lab_counts AS (\n  SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id\n)\nSELECT l.lab_name, c.n\nFROM labs l\nJOIN lab_counts c ON l.lab_id = c.lab_id;"),
  list(prompt = paste0("Using a recursive CTE to generate the years, return every year from 2014 to ",
                       "2023 as year, with the number of researchers who started that year as ",
                       "n_starts (0 for the years when nobody did)."),
       ref = "WITH RECURSIVE years(year) AS (SELECT 2014 UNION ALL SELECT year + 1 FROM years WHERE year < 2023) SELECT years.year, COUNT(r.researcher_id) AS n_starts FROM years LEFT JOIN researchers r ON r.start_year = years.year GROUP BY years.year;",
       ordered = FALSE,
       solution = "WITH RECURSIVE years(year) AS (\n  SELECT 2014\n  UNION ALL\n  SELECT year + 1 FROM years WHERE year < 2023\n)\nSELECT years.year, COUNT(r.researcher_id) AS n_starts\nFROM years\nLEFT JOIN researchers r ON r.start_year = years.year\nGROUP BY years.year;")
)

SUBQ_STEP_TITLES <- c(
  "Scalar subqueries",
  "Subqueries with IN",
  "EXISTS and correlated subqueries",
  "The NOT IN + NULL trap",
  "Readable queries with CTEs",
  "Subqueries in FROM and in SELECT",
  "Recursive CTEs"
)

source_subqueries_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$subq_step <- reactive(walk$step)
  outputOptions(output, "subq_step", suspendWhenHidden = FALSE)

  observeEvent(input$subq_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$subq_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$subq_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$subq_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$subq_continue5, walk$step <- max(walk$step, 6))
  observeEvent(input$subq_continue6, walk$step <- max(walk$step, 7))

  output$subq_toc <- renderUI(
    walkthrough_toc(walk$step, SUBQ_STEP_TITLES, "subq-step"))

  wire_demo_editors("qsed", 8, input, output, session)
  wire_quest_editors(SUBQ_QUEST, "subqq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzsub", 2, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(SUBQ_DQUEST, "dzsubq", input, output, session, con_fun = district_con_ro)

  output$subqueries_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("subq_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Subqueries and CTEs — queries inside queries",
            eyebrow = "Composing questions",
            p(style = "margin-bottom:6px;",
              "Who earns more than average? Which studies belong to the labs in ",
              "Wexler Hall? Both questions have a number or a list buried inside ",
              "them (", tags$em("the average"), ", ", tags$em("the labs in Wexler Hall"),
              ") that is itself the answer to a smaller query. A ", tags$b("subquery"),
              " computes that inner answer first, in place; a ", tags$b("CTE"),
              " gives it a name so the whole thing reads top to bottom rather than ",
              "inside out."),
            p(style = "margin-bottom:0;",
              "For an R user: a subquery is an inline computed value or vector, the ",
              "SQL equivalent of writing ", kbd("x[x > mean(x)]"), " instead of ",
              "computing the mean on one line and using it on the next. A CTE (",
              kbd("WITH"), ") is the second style: assign the intermediate result a ",
              "name, then use the name.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Scalar subqueries",
            explanation_triad(
              formal = tagList(
                "A scalar subquery returns a single row and column, usable anywhere ",
                "a value is expected, typically the right side of a comparison in ",
                tags$code("WHERE"), ". It is evaluated and substituted as that value."),
              plain = tagList(
                "Put a small query in parentheses where a number would go. SQL runs ",
                "it, gets one value, and compares against it. In practical terms, the ",
                "threshold is now computed from the data rather than typed in."),
              lab = tagList(
                kbd("WHERE salary > (SELECT AVG(salary) FROM researchers)"),
                " keeps the seven above-average earners. The threshold is computed ",
                "from the data, so if the institute ever pays anyone more, the query ",
                "adjusts without being edited.")
            ),
            sql_code("SELECT full_name, salary\nFROM researchers\nWHERE salary > (SELECT AVG(salary) FROM researchers);"),
            sql_editor("qsed1", starter = "SELECT full_name, salary\nFROM researchers\nWHERE salary > (SELECT AVG(salary) FROM researchers);"),
            callout_warm(tags$b("One value only. "),
              "A scalar subquery must return exactly one value. If it could return ",
              "several rows, use ", kbd("IN"), " (next step) rather than ", kbd("="),
              ". SQLite, being permissive, will take the first row and say nothing; ",
              "other databases will refuse. Neither outcome is what you wanted."),
            continue_button("subq_continue1"),
            id_prefix = "subq-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.subq_step >= 2",
          step_container(2, "Subqueries with IN",
            explanation_triad(
              formal = tagList(
                "A subquery returning a single column can feed ", tags$code("IN"),
                ", testing membership of the outer value in that set, which amounts ",
                "to a join-like filter without an explicit join."),
              plain = tagList(
                "Let the inner query produce a list of ids, then keep the outer rows ",
                "whose id appears in that list. It reads almost exactly the way you ",
                "would say it aloud."),
              lab = tagList(
                kbd("WHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall')"),
                " keeps the researchers whose lab sits in Wexler Hall, without your ",
                "having to know that those labs are numbers 1 and 2.")
            ),
            sql_code("SELECT full_name\nFROM researchers\nWHERE lab_id IN (\n  SELECT lab_id FROM labs WHERE building = 'Wexler Hall'\n);"),
            sql_editor("qsed2", starter = "SELECT full_name\nFROM researchers\nWHERE lab_id IN (\n  SELECT lab_id FROM labs WHERE building = 'Wexler Hall'\n);"),
            help_details("Subquery or JOIN?",
              tagList("Many ", kbd("IN"), "-subqueries can be rewritten as joins and ",
                "vice versa, and the database will often execute them identically. ",
                "Use whichever reads more clearly to a person; for “does a match ",
                "exist?” questions, ", kbd("EXISTS"), " (next) is usually the cleanest ",
                "of the three.")),
            continue_button("subq_continue2"),
            id_prefix = "subq-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.subq_step >= 3",
          step_container(3, "EXISTS and correlated subqueries",
            explanation_triad(
              formal = tagList(
                "A correlated subquery references the outer row, so it is ",
                "re-evaluated per outer row. ", tags$code("EXISTS (…)"),
                " is true if that subquery yields at least one row, a pure ",
                "existence test that ignores the selected values."),
              plain = tagList(
                "For each outer row, ask “is there a matching row over there?” ",
                kbd("EXISTS"), " answers yes or no and does not care what the ",
                "matching row contains. It is the right tool for “studies that have ",
                "any enrollment at all.”"),
              lab = tagList(
                kbd("WHERE EXISTS (SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id)"),
                " keeps the nine studies with at least one enrollee. The inner ",
                kbd("e.study_id = s.study_id"), " is the correlation: it is what ",
                "makes the subquery about ", tags$em("this"), " study.")
            ),
            sql_code("SELECT s.title\nFROM studies s\nWHERE EXISTS (\n  SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id\n);"),
            sql_editor("qsed3", starter = "SELECT s.title\nFROM studies s\nWHERE EXISTS (\n  SELECT 1 FROM enrollments e WHERE e.study_id = s.study_id\n);"),
            callout_warm(tags$b("SELECT 1 is idiomatic. "),
              kbd("EXISTS"), " only cares whether a row comes back, not what is in ",
              "it, so people write ", kbd("SELECT 1"), " to signal “the contents do ",
              "not matter.” ", kbd("SELECT *"), " works just as well; ", kbd("SELECT 1"),
              " merely tells the next reader you knew that. ", kbd("NOT EXISTS"),
              " flips it to “no match,” the cleanest way to ask “studies with nobody enrolled.”"),
            continue_button("subq_continue3"),
            id_prefix = "subq-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.subq_step >= 4",
          step_container(4, "The NOT IN + NULL trap",
            explanation_triad(
              formal = tagList(
                "If a ", tags$code("NOT IN"), " subquery returns even one ",
                tags$code("NULL"), ", the predicate becomes ", tags$em("unknown"),
                " for every outer row, so the whole query returns nothing. ",
                tags$code("NOT EXISTS"), " does not suffer this and is the safe ",
                "default."),
              plain = tagList(
                "A single NULL inside a ", kbd("NOT IN"),
                " list poisons the whole list, and you get zero rows and no error. ",
                "This is three-valued logic again (recall the WHERE tab). When in ",
                "doubt, reach for ", kbd("NOT EXISTS"), "."),
              lab = tagList(
                kbd("participant_id"), " in enrollments is never NULL, so ",
                kbd("NOT IN"), " is safe here. But if you ran ",
                kbd("WHERE lab_id NOT IN (SELECT lab_id FROM researchers)"),
                ", Dr. Roams's NULL lab_id would wipe the result out entirely. One ",
                "unaffiliated researcher, and every lab disappears.")
            ),
            sql_code("-- Safe here (no NULLs in the subquery):\nSELECT nickname\nFROM participants\nWHERE participant_id NOT IN (SELECT participant_id FROM enrollments);"),
            sql_editor("qsed4", starter = "SELECT nickname\nFROM participants\nWHERE participant_id NOT IN (SELECT participant_id FROM enrollments);"),
            help_details("Why one NULL ruins NOT IN",
              tagList(kbd("x NOT IN (1, NULL)"), " means ",
                kbd("x <> 1 AND x <> NULL"), ". That second test is ",
                tags$em("unknown"), " for any x, and “true AND unknown” is unknown, ",
                "so no row qualifies. The logic is airtight; it is only the outcome ",
                "that is useless. ", kbd("NOT EXISTS"), " sidesteps it entirely, ",
                "because it never compares against the NULL; it only asks whether ",
                "a matching row exists.")),
            continue_button("subq_continue4"),
            id_prefix = "subq-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.subq_step >= 5",
          step_container(5, "Readable queries with CTEs",
            explanation_triad(
              formal = tagList(
                "A common table expression (", tags$code("WITH name AS (…)"),
                ") names a subquery, which can then be referenced by name, often ",
                "several times, in the main query. It improves readability and ",
                "lets you build a query in stages."),
              plain = tagList(
                "Define a temporary, named result up front, then use that name ",
                "below. Same power as a nested subquery, but it reads top to bottom ",
                "instead of inside out, which is the order people read in."),
              lab = tagList(
                kbd("WITH lab_counts AS (...)"), " computes researchers-per-lab once, ",
                "then the main query joins it to ", kbd("labs"), " so the output has ",
                "names instead of ids.")
            ),
            sql_code("WITH lab_counts AS (\n  SELECT lab_id, COUNT(*) AS n\n  FROM researchers\n  GROUP BY lab_id\n)\nSELECT l.lab_name, c.n\nFROM labs l\nJOIN lab_counts c ON l.lab_id = c.lab_id;"),
            sql_editor("qsed5", starter = "WITH lab_counts AS (\n  SELECT lab_id, COUNT(*) AS n\n  FROM researchers\n  GROUP BY lab_id\n)\nSELECT l.lab_name, c.n\nFROM labs l\nJOIN lab_counts c ON l.lab_id = c.lab_id;"),
            callout_warm(tags$b("CTEs make long queries sane. "),
              "When a query grows, name its pieces with ", kbd("WITH"),
              ". You can chain several CTEs, each building on the last, which is the ",
              "readable alternative to a subquery nested inside a subquery nested ",
              "inside a subquery. Six months from now, the CTE version is the one ",
              "you will still understand."),
            continue_button("subq_continue5"),
            id_prefix = "subq-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.subq_step >= 6",
          step_container(6, "Subqueries in FROM and in SELECT",
            explanation_triad(
              formal = tagList(
                "A subquery in the ", tags$code("FROM"), " clause (a ",
                tags$em("derived table"), ") is treated as a table for the duration of ",
                "the query, and should be given an alias. A scalar subquery may also ",
                "appear in the select list, where, if correlated, it is evaluated once ",
                "per output row."),
              plain = tagList(
                "A query's result is a table, so it can stand wherever a table can, ",
                "including after ", kbd("FROM"), ". This is the CTE of the previous step ",
                "without the name up front, and it is how most SQL written before CTEs ",
                "did the job, so you will meet it constantly in other people's queries. ",
                "Its main use is a second round of aggregation on the first round's output."),
              lab = tagList(
                "How many researchers does a lab have, on average? ",
                kbd("AVG(COUNT(*))"), " is an error (“misuse of aggregate”), since an ",
                "aggregate cannot contain another. Count per lab in an inner query, then ",
                "average those counts in the outer one: 1.83, the institute's eleven ",
                "affiliated researchers spread over six labs.")
            ),
            sql_code("SELECT AVG(n) AS avg_per_lab\nFROM (\n  SELECT lab_id, COUNT(*) AS n\n  FROM researchers\n  WHERE lab_id IS NOT NULL\n  GROUP BY lab_id\n) AS per_lab;"),
            p(style = "margin:8px 0 4px 0;", "Run it. Then delete everything except the ",
              "inner query and run that, to see the table the outer one was averaging:"),
            sql_editor("qsed6", starter = "SELECT AVG(n) AS avg_per_lab\nFROM (\n  SELECT lab_id, COUNT(*) AS n\n  FROM researchers\n  WHERE lab_id IS NOT NULL\n  GROUP BY lab_id\n) AS per_lab;", height = "160px"),
            p(style = "margin:14px 0 4px 0;", "And a correlated subquery in the select ",
              "list, which looks up one number per output row:"),
            sql_code("SELECT l.lab_name,\n       (SELECT COUNT(*) FROM studies s WHERE s.lab_id = l.lab_id) AS n_studies\nFROM labs l;"),
            sql_editor("qsed7", starter = "SELECT l.lab_name,\n       (SELECT COUNT(*) FROM studies s WHERE s.lab_id = l.lab_id) AS n_studies\nFROM labs l;"),
            callout_warm(tags$b("Always alias a derived table. "),
              "SQLite does not insist on the ", kbd("AS per_lab"), ". PostgreSQL (before ",
              "version 16), MySQL, and SQL Server all refuse to run the query without it. ",
              "Writing the alias costs nothing here and saves an error there."),
            help_details("Derived table, CTE, or join?",
              tagList("The second query could have been a ", kbd("LEFT JOIN"), " with ",
                kbd("GROUP BY"), ", and the first could have been a CTE. They return the ",
                "same rows, and the database will often run them the same way. My own ",
                "habit: a CTE when the inner query is more than three lines or is used ",
                "twice; a derived table when it is short and used once; a select-list ",
                "subquery when I want a single extra number and do not want the join to ",
                "multiply my rows (recall the JOINs tab, step 5).")),
            continue_button("subq_continue6"),
            id_prefix = "subq-step"
          )),

          # ===== STEP 7 =====
          conditionalPanel("output.subq_step >= 7",
          step_container(7, "Recursive CTEs",
            explanation_triad(
              formal = tagList(
                "A recursive CTE (", tags$code("WITH RECURSIVE"), ") has two parts joined ",
                "by ", tags$code("UNION ALL"), ": an anchor query that supplies the ",
                "starting rows, and a recursive query that refers to the CTE's own name ",
                "and is re-run on the rows most recently produced until it produces no more."),
              plain = tagList(
                "A loop, in a language that has none. Start with one row; keep deriving ",
                "the next row from the last; stop when the ", kbd("WHERE"),
                " says so. Its two everyday uses are generating a series (every year, ",
                "every date, the numbers 1 to n) and walking a hierarchy (a manager's ",
                "reports, and their reports, and so on down)."),
              lab = tagList(
                "How many studies did the institute run in each year from 2014 to 2023? ",
                kbd("GROUP BY year"), " on ", kbd("studies"), " cannot report a year with ",
                "no studies, there being no row to count. Generate the ten years, ",
                kbd("LEFT JOIN"), " the studies to them, and 2015 appears with its zero.")
            ),
            sql_code("WITH RECURSIVE years(y) AS (\n  SELECT 2014                             -- the anchor: where to start\n  UNION ALL\n  SELECT y + 1 FROM years WHERE y < 2023  -- the step, and when to stop\n)\nSELECT y AS year, COUNT(s.study_id) AS n_studies\nFROM years\nLEFT JOIN studies s ON s.year = y\nGROUP BY y;"),
            sql_editor("qsed8", starter = "WITH RECURSIVE years(y) AS (\n  SELECT 2014\n  UNION ALL\n  SELECT y + 1 FROM years WHERE y < 2023\n)\nSELECT y AS year, COUNT(s.study_id) AS n_studies\nFROM years\nLEFT JOIN studies s ON s.year = y\nGROUP BY y;", height = "200px"),
            callout_warm(tags$b("The stopping condition is your responsibility. "),
              "Leave off ", kbd("WHERE y < 2023"), " and the CTE counts upward for as long ",
              "as it is allowed to, which in this browser tab means until you lose ",
              "patience and reload the page. If you want to experiment, add a ",
              kbd("LIMIT 50"), " to the final query first; SQLite stops the recursion ",
              "once the limit is met."),
            help_details("Why the zero needed all that",
              tagList("This is the general form of a problem from the GROUP BY tab: a group ",
                "with no rows does not exist, so it cannot be counted. The remedy is ",
                "always the same. Get the complete list of groups from somewhere (a ",
                "lookup table, a ", kbd("CROSS JOIN"), ", or a generated series like this ",
                "one), put it on the left of a ", kbd("LEFT JOIN"), ", and count a column ",
                "from the right side, so that an unmatched row counts as 0 and not as 1. ",
                "PostgreSQL has ", kbd("generate_series()"), " for the series; elsewhere, ",
                "this CTE is the portable way.")),
            continue_button("subq_done", "I've got subqueries \u2193"),
            id_prefix = "subq-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.subq_step >= 7",
          quest_section(
            "Ten queries to make subqueries automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use scalar subqueries, ", kbd("IN"), "/", kbd("NOT IN"), ", ",
              kbd("EXISTS"), "/", kbd("NOT EXISTS"), ", a subquery in ", kbd("FROM"),
              ", a ", kbd("WITH"), " CTE, and a recursive one. Tables: ", kbd("labs"), ", ", kbd("researchers"), ", ",
              kbd("studies"), ", ", kbd("participants"), ", ", kbd("enrollments"), "."),
            quest_editors_ui(SUBQ_QUEST, "subqq")
          ),
          dsec_subqueries())
        )
      )
    )
  })
}
