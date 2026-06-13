# =============================================================================
# tab_windows.R — "Window functions" walkthrough (Tab 8)
#
# Five gated steps (aggregate OVER () without collapsing; PARTITION BY; ranking
# ROW_NUMBER/RANK/DENSE_RANK; order-aware running totals; LAG/LEAD) then a
# 10-question quest. The order-aware items use the measurements table, whose
# repeated trials per enrollment are made for this.
# =============================================================================

WIN_QUEST <- list(
  list(prompt = paste0("Return full_name, salary, and the overall average salary (same on every ",
                       "row) as overall_avg. Use AVG(...) OVER ()."),
       ref = "SELECT full_name, salary, AVG(salary) OVER () AS overall_avg FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, AVG(salary) OVER () AS overall_avg\nFROM researchers;"),
  list(prompt = paste0("Return full_name, lab_id, and each researcher's lab-average salary as ",
                       "lab_avg. Use PARTITION BY lab_id."),
       ref = "SELECT full_name, lab_id, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, lab_id, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg\nFROM researchers;"),
  list(prompt = paste0("Among researchers with a known salary, return full_name, salary, and ",
                       "their salary rank (highest = 1) as salary_rank. Use RANK()."),
       ref = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank FROM researchers WHERE salary IS NOT NULL;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
  list(prompt = paste0("Among researchers with a known salary, return full_name, lab_id, salary, ",
                       "and their rank within their lab as lab_rank (highest in the lab = 1)."),
       ref = "SELECT full_name, lab_id, salary, RANK() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS lab_rank FROM researchers WHERE salary IS NOT NULL;",
       ordered = FALSE,
       solution = "SELECT full_name, lab_id, salary,\n       RANK() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS lab_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and a row number ",
                       "within its enrollment ordered by trial, as rn. Use ROW_NUMBER()."),
       ref = "SELECT enrollment_id, trial, value, ROW_NUMBER() OVER (PARTITION BY enrollment_id ORDER BY trial) AS rn FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       ROW_NUMBER() OVER (PARTITION BY enrollment_id ORDER BY trial) AS rn\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and the running ",
                       "total of value within its enrollment ordered by trial, as running_total."),
       ref = "SELECT enrollment_id, trial, value, SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and the previous ",
                       "trial's value within the enrollment as prev_value. Use LAG()."),
       ref = "SELECT enrollment_id, trial, value, LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and the next ",
                       "trial's value within the enrollment as next_value. Use LEAD()."),
       ref = "SELECT enrollment_id, trial, value, LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, and value minus the ",
                       "previous trial's value as delta. (The first trial's delta is NULL.)"),
       ref = "SELECT enrollment_id, trial, value - LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS delta FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial,\n       value - LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS delta\nFROM measurements;"),
  list(prompt = paste0("Return title, sample_size, and the dense rank of each study by sample_size ",
                       "(largest = 1) as size_rank. Use DENSE_RANK()."),
       ref = "SELECT title, sample_size, DENSE_RANK() OVER (ORDER BY sample_size DESC) AS size_rank FROM studies;",
       ordered = FALSE,
       solution = "SELECT title, sample_size, DENSE_RANK() OVER (ORDER BY sample_size DESC) AS size_rank\nFROM studies;")
)

WIN_STEP_TITLES <- c(
  "A window that doesn't collapse rows",
  "Per-group windows: PARTITION BY",
  "Ranking: ROW_NUMBER, RANK, DENSE_RANK",
  "Order-aware windows: running totals",
  "Looking at neighbors: LAG and LEAD"
)

source_windows_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$win_step <- reactive(walk$step)
  outputOptions(output, "win_step", suspendWhenHidden = FALSE)

  observeEvent(input$win_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$win_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$win_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$win_continue4, walk$step <- max(walk$step, 5))

  output$win_toc <- renderUI(
    walkthrough_toc(walk$step, WIN_STEP_TITLES, "win-step"))

  wire_demo_editors("nsed", 5, input, output, session)
  wire_quest_editors(WIN_QUEST, "winq", input, output, session)

  output$windows_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("win_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Window functions — calculations across rows, without collapsing them",
            eyebrow = "Per-row calculations over a group",
            p(style = "margin-bottom:6px;",
              kbd("GROUP BY"), " collapses a group to one summary row. A ",
              tags$b("window function"), " computes a group-wise value but ",
              tags$b("keeps every row"), " — so you can show each researcher ",
              tags$em("and"), " their lab's average on the same line, or rank rows, ",
              "or compute running totals."),
            p(style = "margin-bottom:0;",
              "For an R user: this is ", kbd("ave()"), ", or ", kbd("group_by()"),
              " + ", kbd("mutate()"), " (not ", kbd("summarise()"),
              ") — a per-row value informed by the group.")
          ),

          # ===== STEP 1 =====
          step_container(1, "A window that doesn't collapse rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Appending ", tags$code("OVER (…)"), " to an aggregate turns it into ",
                "a window function: it is computed over a set of rows (the window) ",
                "but produces a value for ", tags$em("every"),
                " row rather than one per group. ", tags$code("OVER ()"),
                " with an empty window spans the whole result."),
              plain = tagList(tags$b("In human words. "),
                "Same aggregate, but instead of shrinking the table it adds a column. ",
                kbd("AVG(salary) OVER ()"), " stamps the overall average onto every ",
                "row so you can compare."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT full_name, salary, AVG(salary) OVER () AS overall_avg"),
                " shows each researcher beside the institute-wide average.")
            ),
            sql_code("SELECT full_name, salary,\n       AVG(salary) OVER () AS overall_avg\nFROM researchers;"),
            sql_editor("nsed1", starter = "SELECT full_name, salary,\n       AVG(salary) OVER () AS overall_avg\nFROM researchers;"),
            callout_warm(tags$b("Window vs GROUP BY. "),
              "A plain ", kbd("AVG(salary)"), " returns one row. ",
              kbd("AVG(salary) OVER ()"), " returns all rows with the average ",
              "attached. Reach for a window when you need the detail ",
              tags$em("and"), " the summary together."),
            continue_button("win_continue1"),
            id_prefix = "win-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.win_step >= 2",
          step_container(2, "Per-group windows: PARTITION BY",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("PARTITION BY"), " divides rows into partitions; the window ",
                "function restarts for each, so the value reflects that row's ",
                "partition only — the windowed analog of ", tags$code("GROUP BY"), "."),
              plain = tagList(tags$b("In human words. "),
                "Compute the average (or rank, or total) ", tags$em("within each group"),
                ", but still keep every row. “Each researcher's salary next to ",
                tags$em("their lab's"), " average.”"),
              lab = tagList(tags$b("Lab database. "),
                kbd("AVG(salary) OVER (PARTITION BY lab_id)"),
                " gives every researcher their own lab's average salary.")
            ),
            sql_code("SELECT full_name, lab_id, salary,\n       AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg\nFROM researchers;"),
            sql_editor("nsed2", starter = "SELECT full_name, lab_id, salary,\n       AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg\nFROM researchers;"),
            continue_button("win_continue2"),
            id_prefix = "win-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.win_step >= 3",
          step_container(3, "Ranking: ROW_NUMBER, RANK, DENSE_RANK",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Ranking functions need an ", tags$code("ORDER BY"),
                " inside ", tags$code("OVER"), ". ", tags$code("ROW_NUMBER"),
                " is a strict 1..n; ", tags$code("RANK"),
                " gives ties the same rank and then skips; ", tags$code("DENSE_RANK"),
                " gives ties the same rank without skipping."),
              plain = tagList(tags$b("In human words. "),
                kbd("ROW_NUMBER"), " = numbered list. ", kbd("RANK"),
                " = competition ranking (two firsts, then third). ", kbd("DENSE_RANK"),
                " = two firsts, then second."),
              lab = tagList(tags$b("Lab database. "),
                kbd("RANK() OVER (ORDER BY salary DESC)"),
                " ranks researchers by pay; add ", kbd("PARTITION BY lab_id"),
                " to rank within each lab.")
            ),
            sql_code("SELECT full_name, salary,\n       RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
            sql_editor("nsed3", starter = "SELECT full_name, salary,\n       RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
            help_details("Which ranking function?",
              tagList("Need exactly one row per position (a tiebreak winner)? ",
                kbd("ROW_NUMBER"), ". Reporting standings where ties share a place? ",
                kbd("RANK"), " or ", kbd("DENSE_RANK"),
                " depending on whether you skip the next number.")),
            continue_button("win_continue3"),
            id_prefix = "win-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.win_step >= 4",
          step_container(4, "Order-aware windows: running totals",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "With an ", tags$code("ORDER BY"), " inside ", tags$code("OVER"),
                ", an aggregate accumulates over the rows up to and including the ",
                "current one (the default frame), producing running totals, ",
                "moving sums, and the like."),
              plain = tagList(tags$b("In human words. "),
                "Order the window, and ", kbd("SUM"),
                " becomes a running total — the sum so far, row by row."),
              lab = tagList(tags$b("Lab database. "),
                "Each enrollment has repeated trials. ",
                kbd("SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial)"),
                " gives the cumulative value across trials within each enrollment.")
            ),
            sql_code("SELECT enrollment_id, trial, value,\n       SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total\nFROM measurements;"),
            sql_editor("nsed4", starter = "SELECT enrollment_id, trial, value,\n       SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total\nFROM measurements;"),
            continue_button("win_continue4"),
            id_prefix = "win-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.win_step >= 5",
          step_container(5, "Looking at neighbors: LAG and LEAD",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("LAG(col)"), " and ", tags$code("LEAD(col)"),
                " return the value of ", tags$code("col"),
                " from the previous / next row within the ordered window, or ",
                tags$code("NULL"), " at the boundary — ideal for row-to-row deltas."),
              plain = tagList(tags$b("In human words. "),
                "Peek at the row before (", kbd("LAG"), ") or after (", kbd("LEAD"),
                "). Subtract to get the change from one trial to the next."),
              lab = tagList(tags$b("Lab database. "),
                kbd("value - LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial)"),
                " is the trial-to-trial change; the first trial has no predecessor, ",
                "so its delta is NULL.")
            ),
            sql_code("SELECT enrollment_id, trial, value,\n       LAG(value)  OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
            sql_editor("nsed5", starter = "SELECT enrollment_id, trial, value,\n       LAG(value)  OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
            callout_warm(tags$b("Windows run last. "),
              "Window functions are computed after ", kbd("WHERE"), ", ", kbd("GROUP BY"),
              ", and ", kbd("HAVING"), " — almost the end of the pipeline — which is ",
              "why you can't filter on a window result in ", kbd("WHERE"),
              " (wrap the query in a subquery or CTE to do that)."),
            continue_button("win_done", "I've got window functions ↓"),
            id_prefix = "win-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.win_step >= 5",
          quest_section(
            "Ten queries to make windows automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("OVER ()"), ", ", kbd("PARTITION BY"), ", the ",
              "ranking functions, running totals, and ", kbd("LAG"), "/", kbd("LEAD"),
              ". Use the exact output column names each prompt asks for. Tables: ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("measurements"), "."),
            quest_editors_ui(WIN_QUEST, "winq")
          ))
        )
      )
    )
  })
}
