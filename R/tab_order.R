# =============================================================================
# tab_order.R — "ORDER BY ... LIMIT" walkthrough (Tab 4)
#
# Five gated steps (sort asc/desc; multi-column sort; LIMIT; OFFSET / top-N;
# where NULLs sort) then a 10-question quest. These questions are graded with
# ordered = TRUE: row order is part of the answer, so the grader compares the
# sequence, not just the set. References are minimal canonical queries so a
# learner's identical query reproduces the same deterministic order.
# =============================================================================

ORDER_QUEST <- list(
  list(prompt = "Return lab_name and founded_year for all labs, oldest first (founded_year ascending).",
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year;"),
  list(prompt = paste0("Return full_name and salary of researchers with a known salary, ",
                       "highest salary first."),
       ref = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC;",
       ordered = TRUE,
       solution = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC;"),
  list(prompt = "Return the nickname of every participant, sorted alphabetically (A to Z).",
       ref = "SELECT nickname FROM participants ORDER BY nickname;", ordered = TRUE,
       solution = "SELECT nickname FROM participants ORDER BY nickname;"),
  list(prompt = "Return title and year of all studies, newest year first.",
       ref = "SELECT title, year FROM studies ORDER BY year DESC;", ordered = TRUE,
       solution = "SELECT title, year FROM studies ORDER BY year DESC;"),
  list(prompt = paste0("Return title, status, and year of all studies, sorted by status ",
                       "(A to Z), then by year within each status."),
       ref = "SELECT title, status, year FROM studies ORDER BY status, year;", ordered = TRUE,
       solution = "SELECT title, status, year FROM studies ORDER BY status, year;"),
  list(prompt = "Return the lab_name and founded_year of the three oldest labs.",
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 3;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 3;"),
  list(prompt = "Return the full_name and salary of the five highest-paid researchers.",
       ref = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC LIMIT 5;",
       ordered = TRUE,
       solution = "SELECT full_name, salary FROM researchers WHERE salary IS NOT NULL ORDER BY salary DESC LIMIT 5;"),
  list(prompt = "Return the title and sample_size of the single largest study by sample_size.",
       ref = "SELECT title, sample_size FROM studies ORDER BY sample_size DESC LIMIT 1;", ordered = TRUE,
       solution = "SELECT title, sample_size FROM studies ORDER BY sample_size DESC LIMIT 1;"),
  list(prompt = paste0("Return nickname and age of the three youngest participants whose age ",
                       "is known (ignore unknown ages)."),
       ref = "SELECT nickname, age FROM participants WHERE age IS NOT NULL ORDER BY age LIMIT 3;",
       ordered = TRUE,
       solution = "SELECT nickname, age FROM participants WHERE age IS NOT NULL ORDER BY age LIMIT 3;"),
  list(prompt = paste0("Return the lab_name and founded_year of the second- and third-oldest labs ",
                       "(skip the oldest). Use LIMIT and OFFSET."),
       ref = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 2 OFFSET 1;", ordered = TRUE,
       solution = "SELECT lab_name, founded_year FROM labs ORDER BY founded_year LIMIT 2 OFFSET 1;")
)

ORDER_STEP_TITLES <- c(
  "Sort with ORDER BY",
  "Sort by several columns",
  "Take the top few with LIMIT",
  "Skip rows with OFFSET",
  "Where NULLs sort"
)

source_order_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$order_step <- reactive(walk$step)
  outputOptions(output, "order_step", suspendWhenHidden = FALSE)

  observeEvent(input$order_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$order_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$order_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$order_continue4, walk$step <- max(walk$step, 5))

  output$order_toc <- renderUI(
    walkthrough_toc(walk$step, ORDER_STEP_TITLES, "order-step"))

  wire_demo_editors("osed", 5, input, output, session)
  wire_quest_editors(ORDER_QUEST, "ordq", input, output, session)

  output$order_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("order_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "ORDER BY and LIMIT — sorting and taking the top N",
            eyebrow = "Putting rows in order",
            p(style = "margin-bottom:6px;",
              "A query's rows come back in ", tags$b("no guaranteed order"),
              " unless you ask for one. ", kbd("ORDER BY"), " sorts them; ",
              kbd("LIMIT"), " keeps only the first few — together they answer every ",
              "“top 5,” “most recent,” “largest” question."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("ORDER BY x DESC"), " is ",
              kbd("arrange(desc(x))"), ", and ", kbd("LIMIT 5"), " is ", kbd("head(5)"), ".")
          ),

          # ===== STEP 1 =====
          step_container(1, "Sort with ORDER BY",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "An ", tags$code("ORDER BY"), " clause sorts the result by one or ",
                "more sort keys, each ascending (", tags$code("ASC"), ", the ",
                "default) or descending (", tags$code("DESC"), "). It is logically ",
                "the last step before ", tags$code("LIMIT"), "."),
              plain = tagList(tags$b("In human words. "),
                "Pick a column to sort by. Add ", kbd("DESC"), " for largest-first ",
                "or newest-first; leave it off for smallest-first (A→Z, low→high)."),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year"), " lists labs oldest to newest; ",
                kbd("ORDER BY salary DESC"), " puts the best-paid researchers on top.")
            ),
            sql_code("SELECT full_name, salary\nFROM researchers\nORDER BY salary DESC;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then flip to ", kbd("ASC"),
              " or sort by ", kbd("start_year"), " instead:"),
            sql_editor("osed1", starter = "SELECT full_name, salary\nFROM researchers\nORDER BY salary DESC;"),
            callout_warm(tags$b("No ORDER BY, no promise. "),
              "Without ", kbd("ORDER BY"), " the database may return rows in any ",
              "order, and that order can change. If order matters to your answer, ",
              "say so explicitly."),
            continue_button("order_continue1"),
            id_prefix = "order-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.order_step >= 2",
          step_container(2, "Sort by several columns",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "Listing multiple sort keys sorts by the first, breaking ties with ",
                "the second, and so on. Each key carries its own ", tags$code("ASC"),
                "/", tags$code("DESC"), "."),
              plain = tagList(tags$b("In human words. "),
                "Sort by A; when two rows tie on A, sort those by B. Like sorting a ",
                "spreadsheet by one column, then a second as a tiebreaker."),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY status, year"), " groups studies by status, and ",
                "within each status lists them oldest to newest.")
            ),
            sql_code("SELECT title, status, year\nFROM studies\nORDER BY status, year;"),
            sql_editor("osed2", starter = "SELECT title, status, year\nFROM studies\nORDER BY status, year;"),
            help_details("Mixing ASC and DESC",
              tagList("Each key is independent: ", kbd("ORDER BY status, year DESC"),
                " sorts status A→Z but, within a status, newest year first.")),
            continue_button("order_continue2"),
            id_prefix = "order-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.order_step >= 3",
          step_container(3, "Take the top few with LIMIT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("LIMIT n"), " returns at most the first ", tags$code("n"),
                " rows of the (ordered) result. Paired with ", tags$code("ORDER BY"),
                " it expresses top-N queries."),
              plain = tagList(tags$b("In human words. "),
                "Sort first, then keep only the first few. ", kbd("ORDER BY x DESC LIMIT 5"),
                " is exactly “the top 5 by x.”"),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year LIMIT 3"), " gives the three oldest labs; ",
                kbd("ORDER BY sample_size DESC LIMIT 1"), " gives the single biggest study.")
            ),
            sql_code("SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 3;"),
            sql_editor("osed3", starter = "SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 3;"),
            callout_warm(tags$b("LIMIT without ORDER BY is a coin flip. "),
              kbd("LIMIT 5"), " on its own gives you ", tags$em("some"),
              " five rows, not a meaningful “top 5.” Always sort first."),
            continue_button("order_continue3"),
            id_prefix = "order-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.order_step >= 4",
          step_container(4, "Skip rows with OFFSET",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("OFFSET k"), " skips the first ", tags$code("k"),
                " rows before ", tags$code("LIMIT"), " takes over — the basis of ",
                "pagination (page 2 = ", tags$code("LIMIT 10 OFFSET 10"), ")."),
              plain = tagList(tags$b("In human words. "),
                "Jump past the first few rows, then take some. Useful for “the ",
                "runner-up,” “the second page,” “skip the winner.”"),
              lab = tagList(tags$b("Lab database. "),
                kbd("ORDER BY founded_year LIMIT 2 OFFSET 1"),
                " skips the oldest lab and returns the next two.")
            ),
            sql_code("SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 2 OFFSET 1;"),
            sql_editor("osed4", starter = "SELECT lab_name, founded_year\nFROM labs\nORDER BY founded_year\nLIMIT 2 OFFSET 1;"),
            continue_button("order_continue4"),
            id_prefix = "order-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.order_step >= 5",
          step_container(5, "Where NULLs sort",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "In SQLite, ", tags$code("NULL"), " sorts as the lowest value: ",
                "first under ", tags$code("ASC"), ", last under ", tags$code("DESC"),
                ". So a “smallest first” sort surfaces unknowns at the top."),
              plain = tagList(tags$b("In human words. "),
                "Unknown values clump at one end. If you sort ages ascending, the ",
                "unknown ages come first — usually not what you want for “youngest.”"),
              lab = tagList(tags$b("Lab database. "),
                "Some participants have a ", kbd("NULL"), " age. ",
                kbd("ORDER BY age"), " lists those unknowns first; filter them out ",
                "with ", kbd("WHERE age IS NOT NULL"), " when you mean “youngest known.”")
            ),
            sql_code("SELECT nickname, age\nFROM participants\nWHERE age IS NOT NULL\nORDER BY age\nLIMIT 3;"),
            sql_editor("osed5", starter = "SELECT nickname, age\nFROM participants\nORDER BY age;"),
            callout_warm(tags$b("“Top N” and NULLs. "),
              "A ", kbd("DESC"), " top-N is usually safe (NULLs sink to the bottom), ",
              "but an ", kbd("ASC"), " bottom-N will pull NULLs in unless you exclude ",
              "them first."),
            continue_button("order_done", "I've got ORDER BY ↓"),
            id_prefix = "order-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.order_step >= 5",
          quest_section(
            "Ten queries to make sorting automatic",
            intro = tagList(
              p(style = "line-height:1.6; margin-bottom:4px;",
                "These use ", kbd("ORDER BY"), ", ", kbd("LIMIT"), ", and ",
                kbd("OFFSET"), ". Tables: ", kbd("labs"), ", ", kbd("researchers"),
                ", ", kbd("studies"), ", ", kbd("participants"), "."),
              p(style = "line-height:1.6; margin-bottom:0; color:#7A4F00; font-size:13px;",
                tags$b("Order counts here:"), " these are graded on the exact row ",
                "order, so get the sort direction right.")),
            quest_editors_ui(ORDER_QUEST, "ordq")
          ))
        )
      )
    )
  })
}
