# =============================================================================
# tab_windows.R — "Window functions" walkthrough (Tab 8)
#
# Seven gated steps (aggregate OVER () without collapsing; PARTITION BY; ranking
# ROW_NUMBER/RANK/DENSE_RANK; order-aware running totals; LAG/LEAD; frames with
# ROWS BETWEEN; filtering on a window via a CTE, NTILE, FIRST_VALUE) then a
# 10-question quest. The order-aware items use the measurements table, whose
# repeated trials per enrollment are made for this.
# =============================================================================

WIN_QUEST <- list(
  list(prompt = paste0("Return full_name, salary, and the overall average salary (the same value on every ",
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
  list(prompt = paste0("Return the full_name, lab_id, and salary of the best-paid researcher in each ",
                       "lab, among those with a known salary. (Dr. Roams's NULL lab_id counts as a ",
                       "lab of one.) Number the rows within each lab with ROW_NUMBER() in a CTE, ",
                       "then keep the 1s."),
       ref = "WITH ranked AS (SELECT full_name, lab_id, salary, ROW_NUMBER() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS rn FROM researchers WHERE salary IS NOT NULL) SELECT full_name, lab_id, salary FROM ranked WHERE rn = 1;",
       ordered = FALSE,
       solution = "WITH ranked AS (\n  SELECT full_name, lab_id, salary,\n         ROW_NUMBER() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS rn\n  FROM researchers\n  WHERE salary IS NOT NULL\n)\nSELECT full_name, lab_id, salary\nFROM ranked\nWHERE rn = 1;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and the running ",
                       "total of value within its enrollment ordered by trial, as running_total."),
       ref = "SELECT enrollment_id, trial, value, SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS running_total\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, the previous ",
                       "trial's value within the enrollment as prev_value, and the next trial's as ",
                       "next_value. Use LAG() and LEAD()."),
       ref = "SELECT enrollment_id, trial, value, LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value, LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       LAG(value)  OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, value, and the average of ",
                       "this trial and the two before it within the enrollment, as moving_avg. ",
                       "Use a ROWS BETWEEN frame."),
       ref = "SELECT enrollment_id, trial, value, AVG(value) OVER (PARTITION BY enrollment_id ORDER BY trial ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg FROM measurements;",
       ordered = FALSE,
       solution = "SELECT enrollment_id, trial, value,\n       AVG(value) OVER (PARTITION BY enrollment_id ORDER BY trial\n                        ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg\nFROM measurements;"),
  list(prompt = paste0("For each measurement, return enrollment_id, trial, and value minus the ",
                       "previous trial's value as delta. (The first trial's delta is NULL; it has ",
                       "nothing to be compared with.)"),
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
  "Looking at neighbors: LAG and LEAD",
  "Frames: ROWS BETWEEN",
  "Filtering on a window; NTILE, FIRST_VALUE"
)

source_windows_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$win_step <- reactive(walk$step)
  outputOptions(output, "win_step", suspendWhenHidden = FALSE)

  observeEvent(input$win_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$win_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$win_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$win_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$win_continue5, walk$step <- max(walk$step, 6))
  observeEvent(input$win_continue6, walk$step <- max(walk$step, 7))

  output$win_toc <- renderUI(
    walkthrough_toc(walk$step, WIN_STEP_TITLES, "win-step"))

  wire_demo_editors("nsed", 8, input, output, session)
  wire_quest_editors(WIN_QUEST, "winq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzwin", 1, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(WIN_DQUEST, "dzwinq", input, output, session, con_fun = district_con_ro)

  output$windows_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("win_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Window functions — calculations across rows, without collapsing them",
            eyebrow = "Per-row calculations over a group",
            p(style = "margin-bottom:6px;",
              "Recall that ", kbd("GROUP BY"), " collapses a group to one summary ",
              "row. Useful, but you lose the individual rows in the process. A ",
              tags$b("window function"), " computes a group-wise value and ",
              tags$b("keeps every row"), ", so you can show each researcher ",
              tags$em("and"), " their lab's average on the same line, or rank rows, ",
              "or compute a running total. The name is unhelpful; there is no window, only an aggregate that has ",
              "agreed not to shrink the table."),
            p(style = "margin-bottom:0;",
              "For an R user: this is ", kbd("ave()"), ", or ", kbd("group_by()"),
              " + ", kbd("mutate()"), " (rather than ", kbd("summarise()"),
              "), a per-row value informed by the group. If the mutate/summarise ",
              "distinction is second nature to you, this tab will go quickly.")
          ),

          # ===== STEP 1 =====
          step_container(1, "A window that doesn't collapse rows",
            explanation_triad(
              formal = tagList(
                "Appending ", tags$code("OVER (…)"), " to an aggregate turns it into ",
                "a window function: it is computed over a set of rows (the window) ",
                "but produces a value for ", tags$em("every"),
                " row rather than one per group. ", tags$code("OVER ()"),
                " with an empty window spans the whole result."),
              plain = tagList(
                "Same aggregate, but instead of shrinking the table it adds a column. ",
                kbd("AVG(salary) OVER ()"), " stamps the overall average onto every ",
                "row so each person can be compared to it without a second query."),
              lab = tagList(
                kbd("SELECT full_name, salary, AVG(salary) OVER () AS overall_avg"),
                " shows each of the twelve researchers beside the institute-wide ",
                "average, which is a slightly awkward thing to see next to Wesley ",
                "Tern's 31000.")
            ),
            sql_code("SELECT full_name, salary,\n       AVG(salary) OVER () AS overall_avg\nFROM researchers;"),
            sql_editor("nsed1", starter = "SELECT full_name, salary,\n       AVG(salary) OVER () AS overall_avg\nFROM researchers;"),
            callout_warm(tags$b("Window vs GROUP BY. "),
              "A plain ", kbd("AVG(salary)"), " returns one row. ",
              kbd("AVG(salary) OVER ()"), " returns all twelve rows with the average ",
              "attached to each. Reach for a window when you need the detail ",
              tags$em("and"), " the summary together; reach for GROUP BY when the ",
              "summary is all you want."),
            continue_button("win_continue1"),
            id_prefix = "win-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.win_step >= 2",
          step_container(2, "Per-group windows: PARTITION BY",
            explanation_triad(
              formal = tagList(
                tags$code("PARTITION BY"), " divides rows into partitions; the window ",
                "function restarts for each, so the value reflects that row's ",
                "partition only. It is the windowed analog of ", tags$code("GROUP BY"), "."),
              plain = tagList(
                "Compute the average (or rank, or total) ", tags$em("within each group"),
                ", but still keep every row. “Each researcher's salary next to ",
                tags$em("their own lab's"), " average.” It is GROUP BY's grouping, ",
                "without the collapse."),
              lab = tagList(
                kbd("AVG(salary) OVER (PARTITION BY lab_id)"),
                " gives every researcher their own lab's average salary. Dr. Roams, ",
                "having no lab, is a partition of one, and her lab average is her salary.")
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
              formal = tagList(
                "Ranking functions need an ", tags$code("ORDER BY"),
                " inside ", tags$code("OVER"), ". ", tags$code("ROW_NUMBER"),
                " is a strict 1..n; ", tags$code("RANK"),
                " gives ties the same rank and then skips; ", tags$code("DENSE_RANK"),
                " gives ties the same rank without skipping."),
              plain = tagList(
                kbd("ROW_NUMBER"), " is a numbered list, ties broken arbitrarily. ",
                kbd("RANK"), " is competition ranking: two firsts, then a third. ",
                kbd("DENSE_RANK"), " is two firsts, then a second. Three functions ",
                "for one idea because people disagree about what happens after a tie."),
              lab = tagList(
                kbd("RANK() OVER (ORDER BY salary DESC)"),
                " ranks researchers by pay; add ", kbd("PARTITION BY lab_id"),
                " to rank within each lab. There are no salary ties at the institute, ",
                "so all three functions agree here; the studies table has ties in ",
                "sample_size, which is why the last quest question uses it.")
            ),
            sql_code("SELECT full_name, salary,\n       RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
            sql_editor("nsed3", starter = "SELECT full_name, salary,\n       RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
            help_details("Which ranking function?",
              tagList("Need exactly one row per position (a tiebreak winner, whoever ",
                "it happens to be)? ", kbd("ROW_NUMBER"), ". Reporting standings where ",
                "ties share a place? ", kbd("RANK"), " or ", kbd("DENSE_RANK"),
                ", depending on whether the number after a tie should skip. If in ",
                "doubt, ", kbd("RANK"), " is what most people mean by “rank.”")),
            continue_button("win_continue3"),
            id_prefix = "win-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.win_step >= 4",
          step_container(4, "Order-aware windows: running totals",
            explanation_triad(
              formal = tagList(
                "With an ", tags$code("ORDER BY"), " inside ", tags$code("OVER"),
                ", an aggregate accumulates over the rows up to and including the ",
                "current one (the default frame, which step 6 states precisely), ",
                "producing running totals."),
              plain = tagList(
                "Order the window, and ", kbd("SUM"),
                " becomes a running total: the sum so far, row by row. Same function, ",
                "different frame. In practical terms, adding ORDER BY inside OVER ",
                "changes the question from “what is the total?” to “what was the ",
                "total as of this row?”"),
              lab = tagList(
                "Each enrollment has repeated trials, up to five. ",
                kbd("SUM(value) OVER (PARTITION BY enrollment_id ORDER BY trial)"),
                " gives the cumulative value across trials within each enrollment, ",
                "restarting at every new enrollee.")
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
              formal = tagList(
                tags$code("LAG(col)"), " and ", tags$code("LEAD(col)"),
                " return the value of ", tags$code("col"),
                " from the previous / next row within the ordered window, or ",
                tags$code("NULL"), " at the boundary. They are the standard tool ",
                "for row-to-row differences."),
              plain = tagList(
                "Peek at the row before (", kbd("LAG"), ") or after (", kbd("LEAD"),
                "). Subtract, and you have the change from one trial to the next, ",
                "which is otherwise surprisingly annoying to compute in SQL."),
              lab = tagList(
                kbd("value - LAG(value) OVER (PARTITION BY enrollment_id ORDER BY trial)"),
                " is the trial-to-trial change. The first trial has no predecessor, ",
                "so its delta is NULL, which is the correct answer to “how much did ",
                "it change from nothing?”")
            ),
            sql_code("SELECT enrollment_id, trial, value,\n       LAG(value)  OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
            sql_editor("nsed5", starter = "SELECT enrollment_id, trial, value,\n       LAG(value)  OVER (PARTITION BY enrollment_id ORDER BY trial) AS prev_value,\n       LEAD(value) OVER (PARTITION BY enrollment_id ORDER BY trial) AS next_value\nFROM measurements;"),
            help_details("LAG(value, 2), and LAG(value, 1, 0)",
              tagList("Both functions take two optional arguments: how many rows to reach ",
                "back (or forward), and what to return when there is no such row. ",
                kbd("LAG(value, 2)"), " is the trial before last; ", kbd("LAG(value, 1, 0)"),
                " returns 0 at the first trial where it would otherwise return NULL. The ",
                "caution from the Functions tab about stand-in values applies: a first ",
                "trial's change from “0” is a large number that means nothing.")),
            continue_button("win_continue5"),
            id_prefix = "win-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.win_step >= 6",
          step_container(6, "Frames: ROWS BETWEEN",
            explanation_triad(
              formal = tagList(
                "Within a partition, the ", tags$em("frame"), " is the subset of rows an ",
                "aggregate window function is computed over for the current row. It is ",
                "specified as ", tags$code("ROWS BETWEEN <start> AND <end>"),
                ", with bounds ", tags$code("UNBOUNDED PRECEDING"), ", ",
                tags$code("n PRECEDING"), ", ", tags$code("CURRENT ROW"), ", ",
                tags$code("n FOLLOWING"), ", and ", tags$code("UNBOUNDED FOLLOWING"),
                ". When ", tags$code("ORDER BY"), " is present and no frame is given, the ",
                "default is ", tags$code("RANGE BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"),
                ", which extends to every row tied with the current one."),
              plain = tagList(
                "The partition says which rows are in the same group as this one. The ",
                "frame says which of those rows to use right now: all of them so far (a ",
                "running total), the last three (a moving average), or this one and its ",
                "two neighbors. Step 4's running total was a frame all along; you were ",
                "using the default without being told."),
              lab = tagList(
                "Enrollment 2's five trials read 7.95, 5.49, 5.79, 7.27, 7.46. A frame of ",
                kbd("2 PRECEDING AND CURRENT ROW"), " averages each trial with the two ",
                "before it: 6.41 at trial 3, 6.18 at trial 4. Trials 1 and 2 have fewer ",
                "than two predecessors, so their averages are over one and two values.")
            ),
            sql_code("SELECT enrollment_id, trial, value,\n       AVG(value) OVER (PARTITION BY enrollment_id ORDER BY trial\n                        ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg\nFROM measurements;"),
            sql_editor("nsed6", starter = "SELECT enrollment_id, trial, value,\n       AVG(value) OVER (PARTITION BY enrollment_id ORDER BY trial\n                        ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg\nFROM measurements;", height = "140px"),
            callout_warm(tags$b("A cautionary note on the default frame. "),
              "The default is a ", kbd("RANGE"), " frame, and ", kbd("RANGE"),
              " counts rows that tie on the ", kbd("ORDER BY"), " as arriving together. ",
              "Two studies are from 2022, each with 50 participants. A running total of ",
              kbd("sample_size"), " ordered by ", kbd("year"), " therefore reads 489 for ",
              "both of them, where you probably expected 439 and then 489. Step 4 was ",
              "safe because no enrollment has two measurements with the same trial ",
              "number. When the sort key can tie, write ",
              kbd("ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW"),
              " out in full. The editor below has both versions side by side."),
            sql_editor("nsed7", starter = "SELECT title, year, sample_size,\n       SUM(sample_size) OVER (ORDER BY year) AS default_frame,\n       SUM(sample_size) OVER (ORDER BY year\n                              ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS rows_frame\nFROM studies;", height = "150px"),
            help_details("A centered window, and the other frame units",
              tagList(kbd("ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING"), " takes a row and one ",
                "neighbor on each side, the usual smoothing window. Besides ", kbd("ROWS"),
                " (count physical rows) and ", kbd("RANGE"), " (count by value of the sort ",
                "key), recent SQLite and PostgreSQL offer ", kbd("GROUPS"),
                " (count sets of tied rows). ", kbd("ROWS"), " is the one to reach for ",
                "unless you have a specific reason; it is the only one of the three whose ",
                "behavior can be predicted without knowing whether the data has ties.")),
            continue_button("win_continue6"),
            id_prefix = "win-step"
          )),

          # ===== STEP 7 =====
          conditionalPanel("output.win_step >= 7",
          step_container(7, "Filtering on a window; NTILE, FIRST_VALUE",
            explanation_triad(
              formal = tagList(
                "Window functions are evaluated after ", tags$code("WHERE"), ", ",
                tags$code("GROUP BY"), ", and ", tags$code("HAVING"),
                ", so their results cannot be referenced in those clauses. To filter on ",
                "one, compute it in a CTE or derived table and filter in the enclosing ",
                "query. Further window functions include ", tags$code("NTILE(n)"),
                ", which assigns rows to ", tags$code("n"), " buckets of near-equal size, and ",
                tags$code("FIRST_VALUE(x)"), ", which returns ", tags$code("x"),
                " from the first row of the frame."),
              plain = tagList(
                "“The best-paid person in each lab” is a ranking followed by a filter, ",
                "and the filter cannot go in the same query as the ranking, because ",
                kbd("WHERE"), " has finished before any rank exists. So: rank in a CTE, ",
                "then select from the CTE where the rank is 1. This two-step shape, ",
                "top-N per group, is probably the most common reason anyone writes a ",
                "window function."),
              lab = tagList(
                "Seven rows, one per lab and one for the lab-less Dr. Roams, who tops a ",
                "field of one. ", kbd("NTILE(4)"), " splits the ten disclosed salaries ",
                "into quartiles of 3, 3, 2, and 2, and puts both grad students, to ",
                "nobody's surprise, in the fourth.")
            ),
            sql_code("WITH ranked AS (\n  SELECT full_name, lab_id, salary,\n         ROW_NUMBER() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS rn\n  FROM researchers\n  WHERE salary IS NOT NULL\n)\nSELECT full_name, lab_id, salary\nFROM ranked\nWHERE rn = 1;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then change ", kbd("rn = 1"), " to ",
              kbd("rn <= 2"), " for the top two per lab:"),
            sql_editor("nsed8", starter = "WITH ranked AS (\n  SELECT full_name, lab_id, salary,\n         ROW_NUMBER() OVER (PARTITION BY lab_id ORDER BY salary DESC) AS rn\n  FROM researchers\n  WHERE salary IS NOT NULL\n)\nSELECT full_name, lab_id, salary\nFROM ranked\nWHERE rn = 1;", height = "200px"),
            execution_order_ribbon(
              active = c("WINDOW"),
              note = "Windows are computed after WHERE, GROUP BY, and HAVING, and before DISTINCT, ORDER BY, and LIMIT. Hence the CTE: by the time a rank exists, this query's WHERE is over, and only an enclosing query's WHERE can see it. Try it without the CTE and SQLite says “misuse of aliased window function rn.”"),
            help_details("NTILE, FIRST_VALUE, and the LAST_VALUE trap",
              tagList(
                p(style = "margin-bottom:6px;", kbd("NTILE(4) OVER (ORDER BY salary DESC)"),
                  " labels each row 1 to 4 by quartile. ",
                  kbd("FIRST_VALUE(salary) OVER (PARTITION BY lab_id ORDER BY salary DESC)"),
                  " puts the lab's top salary on every row, so ",
                  kbd("salary - FIRST_VALUE(salary) OVER (…)"),
                  " is each person's gap to the top of their lab (Wesley Tern's is −87000)."),
                p(style = "margin-bottom:0;", kbd("LAST_VALUE"), " looks like the mirror image and is not. ",
                  "Under the default frame, which ends at the current row, the “last” ",
                  "row of the frame is the current row, so ", kbd("LAST_VALUE(x)"),
                  " returns each row's own ", kbd("x"), ". To get the partition's true ",
                  "last value, extend the frame: ",
                  kbd("ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING"),
                  ", or sort the other way and use ", kbd("FIRST_VALUE"), "."))),
            help_details("Naming a window: the WINDOW clause",
              tagList("When several functions share one ", kbd("OVER (…)"),
                ", define it once: ",
                kbd("SELECT RANK() OVER w, NTILE(4) OVER w FROM researchers WINDOW w AS (ORDER BY salary DESC)"),
                ". The ", kbd("WINDOW"), " clause goes after ", kbd("HAVING"),
                " and before ", kbd("ORDER BY"), ". SQLite, PostgreSQL, and MySQL ",
                "support it; SQL Server only since its 2022 release.")),
            continue_button("win_done", "I've got window functions \u2193"),
            id_prefix = "win-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.win_step >= 7",
          quest_section(
            "Ten queries to make windows automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("OVER ()"), ", ", kbd("PARTITION BY"), ", the ",
              "ranking functions, running totals, ", kbd("LAG"), "/", kbd("LEAD"),
              ", a ", kbd("ROWS BETWEEN"), " frame, and one CTE that filters on a rank. Use the exact output column names each prompt asks for. Tables: ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("measurements"), "."),
            quest_editors_ui(WIN_QUEST, "winq")
          ),
          dsec_windows())
        )
      )
    )
  })
}
