# =============================================================================
# tab_select.R — "SELECT ... FROM" walkthrough
#
# First full concept-steps + live-editor-quest tab; establishes the pattern the
# other query tabs follow. Five gated steps (projecting columns, SELECT *,
# aliasing, DISTINCT, expressions), a sticky progress sidebar, then a "Now you
# try" section of 10 live-editor questions graded by result-set comparison.
#
# Expected answers are computed at runtime by running a reference query against
# the same read-only database, so the key can never drift from the data.
# =============================================================================

# Reference queries for the 10 quest questions (the grader runs these to build
# the expected result set, then compares the learner's result to it).
SELECT_QUEST <- list(
  list(prompt = "Return just the name of every lab. (One column: lab_name.)",
       ref = "SELECT lab_name FROM labs;",
       ordered = FALSE,
       solution = "SELECT lab_name FROM labs;"),
  list(prompt = "Return each researcher's full name and title, in that order.",
       ref = "SELECT full_name, title FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, title FROM researchers;"),
  list(prompt = "Return every column of every study using the shortcut for \u201call columns.\u201d",
       ref = "SELECT * FROM studies;",
       ordered = FALSE,
       solution = "SELECT * FROM studies;"),
  list(prompt = "Return each lab's name, but label the output column name instead of lab_name.",
       ref = "SELECT lab_name AS name FROM labs;",
       ordered = FALSE,
       solution = "SELECT lab_name AS name FROM labs;"),
  list(prompt = "List the distinct researcher titles (each title once).",
       ref = "SELECT DISTINCT title FROM researchers;",
       ordered = FALSE,
       solution = "SELECT DISTINCT title FROM researchers;"),
  list(prompt = "List the distinct participant species.",
       ref = "SELECT DISTINCT species FROM participants;",
       ordered = FALSE,
       solution = "SELECT DISTINCT species FROM participants;"),
  list(prompt = "List the distinct study statuses (completed / ongoing / abandoned).",
       ref = "SELECT DISTINCT status FROM studies;",
       ordered = FALSE,
       solution = "SELECT DISTINCT status FROM studies;"),
  list(prompt = paste0("Return each researcher's full_name and salary, plus a computed ",
                       "column annual_pay equal to salary times 12."),
       ref = "SELECT full_name, salary, salary*12 AS annual_pay FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, salary * 12 AS annual_pay FROM researchers;"),
  list(prompt = paste0("Return each study's title, aliased as study_title, ",
                       "and its sample_size aliased as n."),
       ref = "SELECT title AS study_title, sample_size AS n FROM studies;",
       ordered = FALSE, check_names = TRUE,
       solution = "SELECT title AS study_title, sample_size AS n FROM studies;"),
  list(prompt = paste0("Which labs actually run studies? List the distinct lab_id ",
                       "values that appear in the studies table."),
       ref = "SELECT DISTINCT lab_id FROM studies;",
       ordered = FALSE,
       solution = "SELECT DISTINCT lab_id FROM studies;")
)

SELECT_STEP_TITLES <- c(
  "SELECT columns FROM a table",
  "SELECT * (all columns)",
  "Rename with AS",
  "DISTINCT: drop duplicates",
  "Compute new columns"
)

source_select_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$select_step <- reactive(walk$step)
  outputOptions(output, "select_step", suspendWhenHidden = FALSE)

  observeEvent(input$select_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$select_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$select_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$select_continue4, walk$step <- max(walk$step, 5))

  # Sidebar TOC
  output$select_toc <- renderUI({
    cur <- walk$step; nT <- length(SELECT_STEP_TITLES)
    pct <- round((cur - 1) / (nT - 1) * 100)
    items <- lapply(seq_len(nT), function(i) {
      state <- if (i < cur) "completed" else if (i == cur) "current" else "locked"
      check <- if (state == "completed") tags$span(class = "toc-check", HTML("&#10003;")) else NULL
      href  <- if (state == "locked") NULL else sprintf("#select-step-%d", i)
      tags$a(href = href, class = paste("toc-item", state),
             tags$span(class = "toc-num", i),
             tags$span(class = "toc-title", SELECT_STEP_TITLES[i]), check)
    })
    div(class = "sidebar-toc",
        div(class = "sidebar-toc-title", "Walkthrough"),
        div(class = "toc-progress-wrap", div(class = "toc-progress-bar", style = sprintf("width:%d%%;", pct))),
        div(class = "toc-progress-label", sprintf("Step %d of %d \u00b7 %d%%", cur, nT, pct)),
        items)
  })

  # Wire the in-step "try it" editors (Run only).
  wire_demo_editors("seled", 5, input, output, session)

  # Wire the 10 quest editors. Each computes its expected set by running its
  # reference query against the read-only DB.
  for (i in seq_along(SELECT_QUEST)) {
    local({
      idx <- i
      q   <- SELECT_QUEST[[idx]]
      con0 <- institute_con_ro(); expected <- dbGetQuery(con0, q$ref); dbDisconnect(con0)
      sql_editor_server(
        id = paste0("selq", idx), input = input, output = output, session = session,
        con_fun = institute_con_ro,
        expected = expected, ordered = isTRUE(q$ordered),
        solution = q$solution)
    })
  }

  output$select_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        # ---- Sidebar ----
        div(class = "col-lg-3 sidebar-col", uiOutput("select_toc")),

        # ---- Main column ----
        div(class = "col-lg-9",

          scenario_card(
            "SELECT \u2014 pulling columns out of a table",
            eyebrow = "The foundation of every query",
            p(style = "margin-bottom:6px;",
              "Every SQL query you will ever write starts by choosing ",
              tags$b("which columns"), " from ", tags$b("which table"), ". That's ",
              kbd("SELECT"), " and ", kbd("FROM"), ". Everything else \u2014 filtering, ",
              "grouping, joining \u2014 is refinement layered on top of these two words."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("SELECT col FROM tbl"),
              " is ", kbd("tbl[, \"col\"]"), ", and the result is itself a table ",
              "you could feed into another query.")
          ),

          # ===== STEP 1 =====
          step_container(1, "SELECT columns FROM a table",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A ", tags$code("SELECT"), " statement names a ",
                tags$em("projection"), " \u2014 the columns to keep \u2014 and a ",
                tags$code("FROM"), " clause naming the source relation. The result ",
                "is a new relation with the same rows but only the chosen columns."),
              plain = tagList(tags$b("In human words. "),
                "Say which columns you want and which table they live in. ",
                "You get back a table with just those columns, every row included."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT lab_name FROM labs"), " returns the six lab names. ",
                "Swap in ", kbd("full_name"), " and ", kbd("researchers"),
                " to get the people instead.")
            ),
            sql_code("SELECT lab_name\nFROM labs;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then try changing ",
              kbd("lab_name"), " to ", kbd("founded_year"), ", or list two columns ",
              "separated by a comma:"),
            sql_editor("seled1", starter = "SELECT lab_name\nFROM labs;"),
            callout_warm(tags$b("Comma-separated columns. "),
              "Want more than one column? Separate names with commas: ",
              kbd("SELECT lab_name, building FROM labs"), ". The output columns ",
              "appear in the order you list them."),
            continue_button("select_continue1"),
            id_prefix = "select-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.select_step >= 2",
          step_container(2, "SELECT * \u2014 every column",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "The wildcard ", tags$code("*"), " projects all columns of the ",
                "source relation, in their defined order. Convenient for ",
                "exploration; discouraged in durable queries because the result ",
                "shape silently changes if the table's columns change."),
              plain = tagList(tags$b("In human words. "),
                kbd("*"), " means \u201cgive me everything.\u201d Great for a quick ",
                "look at a table; risky in saved queries because you stop ",
                "controlling exactly what comes back."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT * FROM studies"), " shows all seven columns of all ",
                "ten studies \u2014 the fastest way to see what a table holds.")
            ),
            sql_code("SELECT *\nFROM studies;"),
            sql_editor("seled2", starter = "SELECT *\nFROM studies;"),
            help_details("Why is SELECT * discouraged in real queries?",
              tagList("If someone later adds a column to ", kbd("studies"),
                ", your ", kbd("SELECT *"), " quietly starts returning it too \u2014 ",
                "which can break code downstream that expected a fixed set of ",
                "columns. Naming columns explicitly keeps a query's output stable.")),
            continue_button("select_continue2"),
            id_prefix = "select-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.select_step >= 3",
          step_container(3, "Rename output columns with AS",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "An ", tags$code("AS"), " clause assigns an ", tags$em("alias"),
                " to a projected column (or, later, a table). The alias names the ",
                "output column; the underlying data is unchanged."),
              plain = tagList(tags$b("In human words. "),
                "Relabel a column in the result. Handy when a column name is ",
                "cryptic, or when you compute something new and need to name it."),
              lab = tagList(tags$b("Lab database. "),
                kbd("SELECT lab_name AS name FROM labs"),
                " returns the same six names, but the column header now reads ",
                kbd("name"), ".")
            ),
            sql_code("SELECT lab_name AS name\nFROM labs;"),
            sql_editor("seled3", starter = "SELECT lab_name AS name\nFROM labs;"),
            callout_warm(tags$b("The AS keyword is optional\u2026 but write it anyway. "),
              kbd("SELECT lab_name name"), " also works, but spelling out ",
              kbd("AS"), " makes intent obvious and prevents a missing comma from ",
              "silently turning two columns into one aliased column."),
            continue_button("select_continue3"),
            id_prefix = "select-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.select_step >= 4",
          step_container(4, "DISTINCT \u2014 collapse duplicate rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("DISTINCT"), " removes duplicate rows from the result, ",
                "keeping one of each unique combination of the selected columns. ",
                "With several columns it dedupes on the whole ", tags$em("combination"),
                ", not each column separately."),
              plain = tagList(tags$b("In human words. "),
                "\u201cShow me each value once.\u201d Perfect for answering ",
                "\u201cwhat are the possible categories here?\u201d"),
              lab = tagList(tags$b("Lab database. "),
                "The ", kbd("researchers"), " table has twelve rows but only four ",
                "job titles. ", kbd("SELECT DISTINCT title FROM researchers"),
                " returns just PI, Grad Student, Postdoc, RA.")
            ),
            sql_code("SELECT DISTINCT title\nFROM researchers;"),
            sql_editor("seled4", starter = "SELECT DISTINCT title\nFROM researchers;"),
            help_details("DISTINCT across two columns",
              tagList(kbd("SELECT DISTINCT title, lab_id FROM researchers"),
                " returns each unique ", tags$em("pairing"), " of title and lab \u2014 ",
                "so the same title can appear several times, once per lab it occurs in.")),
            continue_button("select_continue4"),
            id_prefix = "select-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.select_step >= 5",
          step_container(5, "Compute new columns in SELECT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "The select list may contain ", tags$em("expressions"),
                " \u2014 arithmetic, string operations, function calls, literals \u2014 ",
                "evaluated per row. These computed columns usually get an ",
                tags$code("AS"), " alias."),
              plain = tagList(tags$b("In human words. "),
                "You're not limited to columns that already exist; you can do ",
                "math on them and return the result as a new column."),
              lab = tagList(tags$b("Lab database. "),
                kbd("salary * 12 AS annual_pay"), " turns monthly salary into a ",
                "yearly figure. Note any ", kbd("NULL"), " salary yields a ",
                kbd("NULL"), " result \u2014 a preview of the WHERE tab.")
            ),
            sql_code("SELECT full_name,\n       salary,\n       salary * 12 AS annual_pay\nFROM researchers;"),
            sql_editor("seled5", starter = "SELECT full_name,\n       salary,\n       salary * 12 AS annual_pay\nFROM researchers;"),
            execution_order_ribbon(
              active = c("FROM", "SELECT"),
              note = "Notice: FROM happens first (find the table), and your computed column in SELECT happens much later. That ordering matters more on the next tabs."),
            continue_button("select_done", "I've got SELECT \u2193"),
            id_prefix = "select-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.select_step >= 5",
          quest_section(
            "Ten queries to make SELECT automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These only need ", kbd("SELECT"), ", ", kbd("FROM"), ", ",
              kbd("AS"), ", and ", kbd("DISTINCT"), " \u2014 no filtering or ",
              "joining yet. The tables you'll need: ", kbd("labs"), ", ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), "."),
            lapply(seq_along(SELECT_QUEST), function(i) {
              q <- SELECT_QUEST[[i]]
              sql_editor(paste0("selq", i), prompt = q$prompt,
                         starter = "", graded = TRUE, number = i)
            })
          ))
        )
      )
    )
  })
}
