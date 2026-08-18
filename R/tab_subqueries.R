# =============================================================================
# tab_subqueries.R — "Subqueries and CTEs" walkthrough (Tab 7)
#
# Five gated steps (scalar subquery; subquery with IN; EXISTS / correlated; the
# NOT IN + NULL trap; CTEs with WITH) then a 10-question quest.
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
  list(prompt = "Return the nickname of participants enrolled in at least one study. Use IN with a subquery.",
       ref = "SELECT nickname FROM participants WHERE participant_id IN (SELECT participant_id FROM enrollments);",
       ordered = FALSE,
       solution = "SELECT nickname\nFROM participants\nWHERE participant_id IN (SELECT participant_id FROM enrollments);"),
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
  list(prompt = paste0("Return the full_name and salary of the researcher(s) with the highest ",
                       "salary. Use a scalar subquery."),
       ref = "SELECT full_name, salary FROM researchers WHERE salary = (SELECT MAX(salary) FROM researchers);",
       ordered = FALSE,
       solution = "SELECT full_name, salary\nFROM researchers\nWHERE salary = (SELECT MAX(salary) FROM researchers);")
)

SUBQ_STEP_TITLES <- c(
  "Scalar subqueries",
  "Subqueries with IN",
  "EXISTS and correlated subqueries",
  "The NOT IN + NULL trap",
  "Readable queries with CTEs"
)

source_subqueries_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$subq_step <- reactive(walk$step)
  outputOptions(output, "subq_step", suspendWhenHidden = FALSE)

  observeEvent(input$subq_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$subq_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$subq_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$subq_continue4, walk$step <- max(walk$step, 5))

  output$subq_toc <- renderUI(
    walkthrough_toc(walk$step, SUBQ_STEP_TITLES, "subq-step"))

  wire_demo_editors("qsed", 5, input, output, session)
  wire_quest_editors(SUBQ_QUEST, "subqq", input, output, session)

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
              formal = tagList(tags$b("Academic. "),
                "A scalar subquery returns a single row and column, usable anywhere ",
                "a value is expected, typically the right side of a comparison in ",
                tags$code("WHERE"), ". It is evaluated and substituted as that value."),
              plain = tagList(tags$b("In human words. "),
                "Put a small query in parentheses where a number would go. SQL runs ",
                "it, gets one value, and compares against it. In practical terms, the ",
                "threshold is now computed from the data rather than typed in."),
              lab = tagList(tags$b("Lab database. "),
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
              formal = tagList(tags$b("Academic. "),
                "A subquery returning a single column can feed ", tags$code("IN"),
                ", testing membership of the outer value in that set, which amounts ",
                "to a join-like filter without an explicit join."),
              plain = tagList(tags$b("In human words. "),
                "Let the inner query produce a list of ids, then keep the outer rows ",
                "whose id appears in that list. It reads almost exactly the way you ",
                "would say it aloud."),
              lab = tagList(tags$b("Lab database. "),
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
              formal = tagList(tags$b("Academic. "),
                "A correlated subquery references the outer row, so it is ",
                "re-evaluated per outer row. ", tags$code("EXISTS (…)"),
                " is true if that subquery yields at least one row, a pure ",
                "existence test that ignores the selected values."),
              plain = tagList(tags$b("In human words. "),
                "For each outer row, ask “is there a matching row over there?” ",
                kbd("EXISTS"), " answers yes or no and does not care what the ",
                "matching row contains. It is the right tool for “studies that have ",
                "any enrollment at all.”"),
              lab = tagList(tags$b("Lab database. "),
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
              formal = tagList(tags$b("Academic. "),
                "If a ", tags$code("NOT IN"), " subquery returns even one ",
                tags$code("NULL"), ", the predicate becomes ", tags$em("unknown"),
                " for every outer row, so the whole query returns nothing. ",
                tags$code("NOT EXISTS"), " does not suffer this and is the safe ",
                "default."),
              plain = tagList(tags$b("In human words. "),
                "A single NULL inside a ", kbd("NOT IN"),
                " list poisons the whole list, and you get zero rows and no error. ",
                "This is three-valued logic again (recall the WHERE tab). When in ",
                "doubt, reach for ", kbd("NOT EXISTS"), "."),
              lab = tagList(tags$b("Lab database. "),
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
              formal = tagList(tags$b("Academic. "),
                "A common table expression (", tags$code("WITH name AS (…)"),
                ") names a subquery, which can then be referenced by name, often ",
                "several times, in the main query. It improves readability and ",
                "lets you build a query in stages."),
              plain = tagList(tags$b("In human words. "),
                "Define a temporary, named result up front, then use that name ",
                "below. Same power as a nested subquery, but it reads top to bottom ",
                "instead of inside out, which is the order people read in."),
              lab = tagList(tags$b("Lab database. "),
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
            continue_button("subq_done", "I've got subqueries ↓"),
            id_prefix = "subq-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.subq_step >= 5",
          quest_section(
            "Ten queries to make subqueries automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use scalar subqueries, ", kbd("IN"), "/", kbd("NOT IN"), ", ",
              kbd("EXISTS"), "/", kbd("NOT EXISTS"), ", and a ", kbd("WITH"),
              " CTE. Tables: ", kbd("labs"), ", ", kbd("researchers"), ", ",
              kbd("studies"), ", ", kbd("participants"), ", ", kbd("enrollments"), "."),
            quest_editors_ui(SUBQ_QUEST, "subqq")
          ))
        )
      )
    )
  })
}
