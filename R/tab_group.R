# =============================================================================
# tab_group.R — "GROUP BY" aggregation walkthrough (Tab 5)
#
# Six gated steps (whole-table aggregates; GROUP BY; COUNT(*) vs COUNT(col) and
# NULLs; HAVING vs WHERE; multi-column grouping; SUM / COUNT(DISTINCT) /
# GROUP_CONCAT) then a 10-question quest. No graded item uses GROUP_CONCAT: the
# order it concatenates in is arbitrary, so its output cannot be graded by
# result-set comparison.
# Prompts name the required output column (e.g. "as n") so the result-set
# grader's column-name check is unambiguous.
# =============================================================================

GROUP_QUEST <- list(
  list(prompt = "How many researchers are there? Return a single column named n.",
       ref = "SELECT COUNT(*) AS n FROM researchers;", ordered = FALSE,
       solution = "SELECT COUNT(*) AS n FROM researchers;"),
  list(prompt = "Return the average researcher salary as a column named avg_salary. (Among the ten who disclosed one; two salaries are NULL.)",
       ref = "SELECT AVG(salary) AS avg_salary FROM researchers;", ordered = FALSE,
       solution = "SELECT AVG(salary) AS avg_salary FROM researchers;"),
  list(prompt = "For each title, return the title and the number of researchers holding it as n.",
       ref = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title;", ordered = FALSE,
       solution = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title;"),
  list(prompt = paste0("For each lab_id in the studies table, return the lab_id and the total ",
                       "sample_size across its studies as total_n."),
       ref = "SELECT lab_id, SUM(sample_size) AS total_n FROM studies GROUP BY lab_id;", ordered = FALSE,
       solution = "SELECT lab_id, SUM(sample_size) AS total_n FROM studies GROUP BY lab_id;"),
  list(prompt = "For each status, return the status and the average sample_size as avg_n.",
       ref = "SELECT status, AVG(sample_size) AS avg_n FROM studies GROUP BY status;", ordered = FALSE,
       solution = "SELECT status, AVG(sample_size) AS avg_n FROM studies GROUP BY status;"),
  list(prompt = paste0("For each participant species, return the species, the number of participants ",
                       "as n, and the number of different nicknames among them as n_names."),
       ref = "SELECT species, COUNT(*) AS n, COUNT(DISTINCT nickname) AS n_names FROM participants GROUP BY species;", ordered = FALSE,
       solution = "SELECT species, COUNT(*) AS n, COUNT(DISTINCT nickname) AS n_names\nFROM participants\nGROUP BY species;"),
  list(prompt = paste0("Return the youngest and oldest known participant age, as columns ",
                       "youngest and oldest."),
       ref = "SELECT MIN(age) AS youngest, MAX(age) AS oldest FROM participants;", ordered = FALSE,
       solution = "SELECT MIN(age) AS youngest, MAX(age) AS oldest FROM participants;"),
  list(prompt = paste0("Return each researcher title and its count as n, but only titles held ",
                       "by more than one researcher. Use HAVING."),
       ref = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;",
       ordered = FALSE,
       solution = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;"),
  list(prompt = paste0("For each lab_id in the researchers table, return the lab_id and the ",
                       "average salary as avg_salary. (The unaffiliated researcher forms a ",
                       "NULL group of one. That is expected, and slightly poignant.)"),
       ref = "SELECT lab_id, AVG(salary) AS avg_salary FROM researchers GROUP BY lab_id;",
       ordered = FALSE,
       solution = "SELECT lab_id, AVG(salary) AS avg_salary FROM researchers GROUP BY lab_id;"),
  list(prompt = paste0("How many distinct labs have at least one researcher? Return a single ",
                       "column named n_labs."),
       ref = "SELECT COUNT(DISTINCT lab_id) AS n_labs FROM researchers;", ordered = FALSE,
       solution = "SELECT COUNT(DISTINCT lab_id) AS n_labs FROM researchers;")
)

GROUP_STEP_TITLES <- c(
  "Aggregate the whole table",
  "One row per group with GROUP BY",
  "COUNT(*) vs COUNT(column)",
  "Filter groups with HAVING",
  "Group by several columns",
  "SUM, COUNT(DISTINCT), GROUP_CONCAT"
)

source_group_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$group_step <- reactive(walk$step)
  outputOptions(output, "group_step", suspendWhenHidden = FALSE)

  observeEvent(input$group_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$group_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$group_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$group_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$group_continue5, walk$step <- max(walk$step, 6))

  output$group_toc <- renderUI(
    walkthrough_toc(walk$step, GROUP_STEP_TITLES, "group-step"))

  wire_demo_editors("gsed", 6, input, output, session)
  wire_quest_editors(GROUP_QUEST, "grpq", input, output, session)

  output$group_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("group_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "GROUP BY — one row per group",
            eyebrow = "Summarizing",
            p(style = "margin-bottom:6px;",
              "So far every query returned rows that already existed somewhere in a ",
              "table. Aggregation is different in kind: it ",
              tags$b("collapses many rows into a summary"),
              " (a count, a total, an average), and with ", kbd("GROUP BY"),
              ", one summary per group. The rows that come back are rows nobody ",
              "ever typed in."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("GROUP BY title"), " + ", kbd("COUNT(*)"),
              " is ", kbd("group_by(title) |> summarise(n = n())"),
              ", or ", kbd("aggregate()"), ", or ", kbd("table()"),
              " if you are in a hurry. If you have used dplyr's ", kbd("summarise()"),
              ", the mental model transfers intact.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Aggregate the whole table",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Aggregate functions (", tags$code("COUNT"), ", ", tags$code("SUM"),
                ", ", tags$code("AVG"), ", ", tags$code("MIN"), ", ", tags$code("MAX"),
                ") reduce a set of rows to a single value. With no ",
                tags$code("GROUP BY"), " the whole table is one group, so you get ",
                "one row back."),
              plain = tagList(tags$b("In human words. "),
                "Ask one summary question of the entire table, how many or what's ",
                "the average, and you get a one-row answer. That answer is still a ",
                "table, a very short one."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT COUNT(*) FROM researchers"), " returns 12; ",
                kbd("SELECT SUM(salary) FROM researchers"), " returns the payroll, 897000; ",
                kbd("SELECT AVG(salary) FROM researchers"),
                " returns the mean salary of the ten who disclosed one, a distinction ",
                "step 3 makes precise.")
            ),
            sql_code("SELECT COUNT(*) AS n,\n       SUM(salary) AS payroll,\n       AVG(salary) AS avg_salary,\n       MAX(salary) AS top_salary\nFROM researchers;"),
            sql_editor("gsed1", starter = "SELECT COUNT(*) AS n,\n       SUM(salary) AS payroll,\n       AVG(salary) AS avg_salary,\n       MAX(salary) AS top_salary\nFROM researchers;"),
            callout_warm(tags$b("Name your summaries. "),
              "An aggregate's default column name is the expression itself (",
              kbd("COUNT(*)"), "), which is legal and unlovely. Give it an alias with ",
              kbd("AS"), " so the result reads cleanly, and so the grader on this ",
              "tab, which checks column names, can find it."),
            continue_button("group_continue1"),
            id_prefix = "group-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.group_step >= 2",
          step_container(2, "One row per group with GROUP BY",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("GROUP BY"), " partitions rows into groups sharing the ",
                "grouping key; each aggregate is then computed per group, yielding ",
                "one output row per group. Non-aggregated columns in ",
                tags$code("SELECT"), " must appear in ", tags$code("GROUP BY"),
                ". Rows whose key is ", tags$code("NULL"), " are grouped together."),
              plain = tagList(tags$b("In human words. "),
                "Sort the rows into piles by some column, then summarize each ",
                "pile. “How many researchers ", tags$em("per title"), "?” is one ",
                "row per title."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT title, COUNT(*) FROM researchers GROUP BY title"),
                " returns four rows, one per job title, with its head count. Seven ",
                "PIs and one RA. That tells you something about how the institute is run.")
            ),
            sql_code("SELECT title, COUNT(*) AS n\nFROM researchers\nGROUP BY title;"),
            sql_editor("gsed2", starter = "SELECT title, COUNT(*) AS n\nFROM researchers\nGROUP BY title;"),
            help_details("The “must be in GROUP BY” rule",
              tagList("If you ", kbd("SELECT title, full_name, COUNT(*)"),
                " but only ", kbd("GROUP BY title"), ", which of the seven PIs' ",
                kbd("full_name"), "s should the PI row show? There is no good ",
                "answer, so most databases refuse. (SQLite, unusually, will pick one ",
                "for you and say nothing. Don't rely on it.) Either group by the ",
                "column too or wrap it in an aggregate.")),
            continue_button("group_continue2"),
            id_prefix = "group-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.group_step >= 3",
          step_container(3, "COUNT(*) vs COUNT(column)",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("COUNT(*)"), " counts rows; ", tags$code("COUNT(col)"),
                " counts rows where ", tags$code("col"), " is non-NULL. Likewise ",
                tags$code("AVG"), "/", tags$code("SUM"),
                " ignore NULLs in their argument."),
              plain = tagList(tags$b("In human words. "),
                kbd("COUNT(*)"), " is “how many rows.” ", kbd("COUNT(salary)"),
                " is “how many rows have a salary on file.” NULLs are not counted, ",
                "averaged, or summed; the aggregate behaves as if they were not there."),
              lab = tagList(tags$b("Lab database. "),
                "12 researchers, but ", kbd("COUNT(salary)"), " is 10 because two ",
                "salaries are NULL. ", kbd("AVG(salary)"),
                " averages only those ten, and does not mention that it did so.")
            ),
            sql_code("SELECT COUNT(*) AS rows_total,\n       COUNT(salary) AS with_salary\nFROM researchers;"),
            sql_editor("gsed3", starter = "SELECT COUNT(*) AS rows_total,\n       COUNT(salary) AS with_salary\nFROM researchers;"),
            callout_warm(tags$b("A subtle averaging trap. "),
              kbd("AVG(salary)"), " divides by the 10 disclosed salaries, not 12. ",
              "That is usually what you want, and occasionally it is exactly wrong: ",
              "if the undisclosed salaries are, say, the two smallest, your average ",
              "is flattering the institute. Averages skip NULLs without comment, so it is ",
              "worth running ", kbd("COUNT(*)"), " next to ", kbd("COUNT(col)"),
              " once, so you know what you are dividing by."),
            continue_button("group_continue3"),
            id_prefix = "group-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.group_step >= 4",
          step_container(4, "Filter groups with HAVING",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("WHERE"), " filters rows before grouping; ", tags$code("HAVING"),
                " filters groups after aggregation. Conditions on an aggregate ",
                "(", tags$code("COUNT(*) > 1"), ") must live in ", tags$code("HAVING"),
                ", since the count does not exist until the group is formed."),
              plain = tagList(tags$b("In human words. "),
                kbd("WHERE"), " decides which rows go into the piles; ", kbd("HAVING"),
                " decides which piles survive. If the test is about a group total, ",
                "it belongs in ", kbd("HAVING"), ". If it is about an individual row, ",
                "it belongs in ", kbd("WHERE"), ". The distinction falls out of the ",
                "execution order below: the count does not exist until after grouping, ",
                "so only ", kbd("HAVING"), " can see it."),
              lab = tagList(tags$b("Lab database. "),
                kbd("GROUP BY title HAVING COUNT(*) > 1"),
                " keeps only the job titles held by more than one researcher, which ",
                "drops the institute's solitary RA.")
            ),
            sql_code("SELECT title, COUNT(*) AS n\nFROM researchers\nGROUP BY title\nHAVING COUNT(*) > 1;"),
            sql_editor("gsed4", starter = "SELECT title, COUNT(*) AS n\nFROM researchers\nGROUP BY title\nHAVING COUNT(*) > 1;"),
            help_details("WHERE and HAVING in the same query",
              tagList("They are not alternatives, and a query often wants both. ",
                kbd("SELECT status, COUNT(*) AS n FROM studies WHERE year >= 2018 GROUP BY status HAVING COUNT(*) >= 2"),
                " first discards the studies from before 2018, then counts what is left ",
                "per status, then keeps the statuses with at least two. (Completed has ",
                "four and ongoing three; the abandoned cricket study, from 2014, never ",
                "made it past the ", kbd("WHERE"), ".)")),
            help_details("HAVING n > 1?",
              tagList("SQLite and MySQL let ", kbd("HAVING"), " refer to the ",
                kbd("SELECT"), " alias, so ", kbd("HAVING n > 1"), " runs here. ",
                "PostgreSQL and SQL Server do not, for the reason the ribbon gives: ",
                kbd("HAVING"), " runs before ", kbd("SELECT"), " names anything. ",
                "Writing the aggregate out again, ", kbd("HAVING COUNT(*) > 1"),
                ", works everywhere, and it is what this tutorial's solutions do.")),
            execution_order_ribbon(
              active = c("WHERE", "GROUP BY", "HAVING"),
              note = "The order explains the split: WHERE runs before GROUP BY (so it cannot see counts), HAVING runs after (so it can). If you ever forget which is which, come back to this ribbon rather than guessing."),
            continue_button("group_continue4"),
            id_prefix = "group-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.group_step >= 5",
          step_container(5, "Group by several columns",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Grouping by multiple columns forms one group per distinct ",
                tags$em("combination"), " of those columns; the aggregates are ",
                "computed within each combination."),
              plain = tagList(tags$b("In human words. "),
                "Sort into piles by two things at once, e.g. status ", tags$em("and"),
                " year, and summarize each pile. You get a row for every combination ",
                "that actually occurs, and none for the ones that don't."),
              lab = tagList(tags$b("Lab database. "),
                kbd("GROUP BY status, year"), " gives a count for each ",
                "status-and-year pairing present in the data. There is exactly one ",
                "abandoned study, from 2014, so that pairing gets a row of its own.")
            ),
            sql_code("SELECT status, year, COUNT(*) AS n\nFROM studies\nGROUP BY status, year;"),
            sql_editor("gsed5", starter = "SELECT status, year, COUNT(*) AS n\nFROM studies\nGROUP BY status, year;"),
            continue_button("group_continue5"),
            id_prefix = "group-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.group_step >= 6",
          step_container(6, "SUM, COUNT(DISTINCT), GROUP_CONCAT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("SUM(x)"), " totals the non-NULL values of ", tags$code("x"),
                ". Placing ", tags$code("DISTINCT"), " inside an aggregate, as in ",
                tags$code("COUNT(DISTINCT x)"), ", removes duplicate values before ",
                "aggregating. ", tags$code("GROUP_CONCAT(x, sep)"),
                " concatenates the group's non-NULL values into one string."),
              plain = tagList(tags$b("In human words. "),
                kbd("COUNT(*)"), " says how many rows are in the pile; ",
                kbd("COUNT(DISTINCT nickname)"), " says how many ", tags$em("different"),
                " nicknames are in it. ", kbd("GROUP_CONCAT"), " is for when you want ",
                "to see the pile's contents and not only its size: it writes the values ",
                "out in one cell, separated by commas."),
              lab = tagList(tags$b("Lab database. "),
                "The institute has 18 human participants sharing 11 nicknames, which is ",
                "what eight Daves will do to a sample. The three seagulls and three ",
                "crickets, to their credit, have three names apiece.")
            ),
            sql_code("SELECT species,\n       COUNT(*)                 AS n,\n       COUNT(DISTINCT nickname) AS n_names,\n       GROUP_CONCAT(DISTINCT nickname) AS who\nFROM participants\nGROUP BY species;"),
            sql_editor("gsed6", starter = "SELECT species,\n       COUNT(*)                 AS n,\n       COUNT(DISTINCT nickname) AS n_names,\n       GROUP_CONCAT(DISTINCT nickname) AS who\nFROM participants\nGROUP BY species;", height = "150px"),
            callout_warm(tags$b("SUM of nothing is NULL, not zero. "),
              "If a group has no non-NULL values to add up, ", kbd("SUM"),
              " returns ", kbd("NULL"), ". (", kbd("COUNT"), " of nothing, by contrast, ",
              "is 0.) This matters the first time you subtract one total from another ",
              "and the answer comes back blank. The Functions tab has the usual fix, ",
              kbd("COALESCE(SUM(x), 0)"), "."),
            help_details("GROUP_CONCAT elsewhere, and its ordering",
              tagList("The name is SQLite's and MySQL's. PostgreSQL and SQL Server call it ",
                kbd("STRING_AGG(x, ', ')"), " (recent SQLite accepts that spelling too), ",
                "and Oracle calls it ", kbd("LISTAGG"), ". In all of them the order of the ",
                "values inside the string is not promised unless you ask for one, so the ",
                "output is for reading; don't build a comparison on it.")),
            continue_button("group_done", "I've got GROUP BY \u2193"),
            id_prefix = "group-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.group_step >= 6",
          quest_section(
            "Ten queries to make summarizing automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("COUNT"), ", ", kbd("SUM"), ", ", kbd("AVG"), ", ",
              kbd("MIN"), "/", kbd("MAX"), ", ", kbd("COUNT(DISTINCT …)"), ", ",
              kbd("GROUP BY"), ", and ", kbd("HAVING"), ". ",
              "Use the exact output column names each prompt asks for; the grader ",
              "reads the header. Tables: ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), "."),
            quest_editors_ui(GROUP_QUEST, "grpq")
          ))
        )
      )
    )
  })
}
