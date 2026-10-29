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
  list(prompt = "Return every column of every study, using the one-character shortcut for “all columns.”",
       ref = "SELECT * FROM studies;",
       ordered = FALSE,
       solution = "SELECT * FROM studies;"),
  list(prompt = "Return each lab's name, but label the output column name rather than lab_name.",
       ref = "SELECT lab_name AS name FROM labs;",
       ordered = FALSE,
       solution = "SELECT lab_name AS name FROM labs;"),
  list(prompt = "List the distinct researcher titles (each title once, however many people hold it).",
       ref = "SELECT DISTINCT title FROM researchers;",
       ordered = FALSE,
       solution = "SELECT DISTINCT title FROM researchers;"),
  list(prompt = "List the distinct participant species. There are fewer than you'd hope.",
       ref = "SELECT DISTINCT species FROM participants;",
       ordered = FALSE,
       solution = "SELECT DISTINCT species FROM participants;"),
  list(prompt = "List the distinct study statuses (completed / ongoing / abandoned).",
       ref = "SELECT DISTINCT status FROM studies;",
       ordered = FALSE,
       solution = "SELECT DISTINCT status FROM studies;"),
  list(prompt = paste0("The institute is granting a 3% raise. Return each researcher's full_name ",
                       "and salary, plus a computed column after_raise equal to salary times 1.03."),
       ref = "SELECT full_name, salary, salary*1.03 AS after_raise FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, salary * 1.03 AS after_raise FROM researchers;"),
  list(prompt = paste0("Return each study's title, aliased as study_title, ",
                       "and its sample_size aliased as n."),
       ref = "SELECT title AS study_title, sample_size AS n FROM studies;",
       ordered = FALSE, check_names = TRUE,
       solution = "SELECT title AS study_title, sample_size AS n FROM studies;"),
  list(prompt = paste0("Which labs actually run studies, as opposed to merely existing? List the ",
                       "distinct lab_id values that appear in the studies table."),
       ref = "SELECT DISTINCT lab_id FROM studies;",
       ordered = FALSE,
       solution = "SELECT DISTINCT lab_id FROM studies;")
)

SELECT_STEP_TITLES <- c(
  "SELECT columns FROM a table",
  "SELECT *, and what tables exist",
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
        div(class = "toc-progress-label", sprintf("Step %d of %d · %d%%", cur, nT, pct)),
        items)
  })

  # Wire the in-step "try it" editors (Run only).
  wire_demo_editors("seled", 7, input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzsel", 2, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(SELECT_DQUEST, "dzselq", input, output, session, con_fun = district_con_ro)

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
            "SELECT — pulling columns out of a table",
            eyebrow = "What every query needs",
            p(style = "margin-bottom:6px;",
              "What does a query need, at minimum? Two things: ",
              tags$b("which columns"), " and ", tags$b("which table"), ". That is ",
              kbd("SELECT"), " and ", kbd("FROM"), ", and every query that reads a ",
              "table has both. Everything else in this tutorial is refinement of those two words."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("SELECT col FROM tbl"),
              " is ", kbd("tbl[, \"col\"]"), ". The result is itself a table, which ",
              "matters more than it sounds: it means you can feed one query's answer ",
              "into another, which is most of what the later tabs are about.")
          ),

          # ===== STEP 1 =====
          step_container(1, "SELECT columns FROM a table",
            explanation_triad(
              formal = tagList(
                "A ", tags$code("SELECT"), " statement names a ",
                tags$em("projection"), " (the columns to keep) and a ",
                tags$code("FROM"), " clause naming the source relation. The result ",
                "is a new relation with the same rows and only the chosen columns."),
              plain = tagList(
                "Say which columns you want and which table they live in. ",
                "You get back a table with just those columns and every row. ",
                "In practical terms, this is the least a database will ever ask of you."),
              lab = tagList(
                kbd("SELECT lab_name FROM labs"), " returns the six lab names, ",
                "from the Avian Intimidation Lab to the Bioacoustics & Thermometry Lab. ",
                "Swap in ", kbd("full_name"), " and ", kbd("researchers"),
                " to get the twelve people who work at them.")
            ),
            sql_code("SELECT lab_name\nFROM labs;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then try changing ",
              kbd("lab_name"), " to ", kbd("founded_year"), ", or list two columns ",
              "separated by a comma:"),
            sql_editor("seled1", starter = "SELECT lab_name\nFROM labs;"),
            callout_warm(tags$b("Comma-separated columns. "),
              "Want more than one column? Separate the names with commas: ",
              kbd("SELECT lab_name, building FROM labs"), ". The output columns ",
              "come back in the order you list them, whatever order the table ",
              "stores them in. The last column takes no comma after it; a comma ",
              "left dangling before ", kbd("FROM"), " is the most common syntax ",
              "error there is."),
            help_details("Can a SELECT have no FROM?",
              tagList("It can. ", kbd("SELECT 2 + 2;"), " returns one row with one ",
                "column, no table involved, which makes SQLite a serviceable if ",
                "overqualified calculator. It is handy for trying out a function before ",
                "pointing it at real data: ", kbd("SELECT UPPER('dave');"),
                ". (Oracle, until its 2023 release, insisted on ", kbd("FROM dual"),
                " even here.)")),
            continue_button("select_continue1"),
            id_prefix = "select-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.select_step >= 2",
          step_container(2, "SELECT *, and finding what tables and columns exist",
            explanation_triad(
              formal = tagList(
                "The wildcard ", tags$code("*"), " projects all columns of the ",
                "source relation, in their defined order. Convenient for ",
                "exploration; discouraged in durable queries because the result ",
                "shape silently changes if the table's columns change."),
              plain = tagList(
                kbd("*"), " means “give me everything.” Use it for a first look at ",
                "an unfamiliar table, not in a query you intend to keep; the ",
                "collapsible below says why."),
              lab = tagList(
                kbd("SELECT * FROM studies"), " shows all seven columns of all ",
                "ten studies, including the one titled “Abandoned: telepathy in lab ",
                "crickets,” which is the fastest way to learn what kind of ",
                "institute this is.")
            ),
            sql_code("SELECT *\nFROM studies;"),
            sql_editor("seled2", starter = "SELECT *\nFROM studies;"),
            help_details("Why is SELECT * discouraged in real queries?",
              tagList("If someone later adds a column to ", kbd("studies"),
                ", your ", kbd("SELECT *"), " starts returning it too, and ",
                "code downstream that expected a fixed set of columns may break ",
                "somewhere far from the query that caused it. Naming columns explicitly ",
                "keeps a query's output stable. It also documents what you actually ",
                "wanted, which you will appreciate when you reread it.")),
            h5(class = "mt-4", "What tables and columns are there? Ask the database"),
            p(style = "line-height:1.6;",
              kbd("SELECT *"), " shows you a table you already know about. To find the tables in the ",
              "first place, or to check a column's exact name, query the database's own ",
              tags$b("catalog"), ": every database keeps a list of what it contains, and you read it ",
              "with an ordinary ", kbd("SELECT"), ". In SQLite the list of tables is ",
              kbd("sqlite_master"), ":"),
            sql_code("SELECT type, name\nFROM sqlite_master;"),
            sql_editor("seled6", starter = "SELECT type, name\nFROM sqlite_master;"),
            p(style = "line-height:1.6;",
              "You get ten rows: the six tables, plus four ", tags$em("indexes"), " (lookup aids the ",
              "database built to speed up searches; the Tables tab covers them). The ", kbd("type"),
              " column tells them apart. For the columns of one table, name the table inside ",
              kbd("pragma_table_info('...')"), " in the ", kbd("FROM"), " clause, quotes included:"),
            sql_code("SELECT name, type, pk\nFROM pragma_table_info('studies');"),
            sql_editor("seled7", starter = "SELECT name, type, pk\nFROM pragma_table_info('studies');"),
            p(style = "line-height:1.6;",
              "One row per column: its ", kbd("name"), ", the ", kbd("type"), " of value it holds ",
              "(", kbd("INTEGER"), ", ", kbd("TEXT"), ", ...), and ", kbd("pk"), ", which is 1 for the ",
              "primary key and 0 otherwise. Change ", kbd("'studies'"), " to any table name. Both ",
              "queries work in every editor in this tutorial, so when you forget a column name, ",
              "you can look it up where you are instead of scrolling back to the Start tab."),
            help_details("The same lookups in SQL Server, PostgreSQL, and MySQL",
              tagList("The catalog's name is the main thing that differs between databases. Most ",
                "follow the SQL standard's ", kbd("INFORMATION_SCHEMA"), " views:",
                tags$pre(style = "background:#f6fafb; padding:8px 10px; border-radius:6px; margin:8px 0; font-size:12.5px;",
                  "SELECT TABLE_SCHEMA, TABLE_NAME\nFROM INFORMATION_SCHEMA.TABLES;\n\nSELECT COLUMN_NAME, DATA_TYPE, IS_NULLABLE\nFROM INFORMATION_SCHEMA.COLUMNS\nWHERE TABLE_NAME = 'studies';"),
                "SQL Server also has a one-line shortcut, ", kbd("EXEC sp_help 'studies';"), ". On a job ",
                "with an unfamiliar warehouse, these are the first queries to run.")),
            continue_button("select_continue2"),
            id_prefix = "select-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.select_step >= 3",
          step_container(3, "Rename output columns with AS",
            explanation_triad(
              formal = tagList(
                "An ", tags$code("AS"), " clause assigns an ", tags$em("alias"),
                " to a projected column (or, later, a table). The alias names the ",
                "output column; the underlying data is unchanged."),
              plain = tagList(
                "Relabel a column in the result. Useful when a column name is ",
                "cryptic, and necessary when you compute something new and it ",
                "needs a name that isn't the formula itself."),
              lab = tagList(
                kbd("SELECT lab_name AS name FROM labs"),
                " returns the same six names; only the column header changes, to ",
                kbd("name"), ". The labs themselves remain unrenamed, and probably ",
                "relieved.")
            ),
            sql_code("SELECT lab_name AS name\nFROM labs;"),
            sql_editor("seled3", starter = "SELECT lab_name AS name\nFROM labs;"),
            callout_warm(tags$b("The AS keyword is optional. Write it anyway. "),
              kbd("SELECT lab_name name"), " also works, and I'd counsel against it. ",
              "Spelling out ", kbd("AS"), " makes your intent obvious, and it ",
              "prevents a forgotten comma from turning two columns into ",
              "one column with a strange alias, an error that produces no error."),
            help_details("Aliases with spaces or capitals",
              tagList("An alias is an identifier, so if you want a space in it, it goes in ",
                tags$b("double"), " quotes: ", kbd("SELECT lab_name AS \"Lab name\" FROM labs"),
                ". Single quotes are for text values. In practice most people stick to ",
                kbd("snake_case"), " aliases and never need to quote them; the graded ",
                "questions in this tutorial all do.")),
            continue_button("select_continue3"),
            id_prefix = "select-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.select_step >= 4",
          step_container(4, "DISTINCT — collapse duplicate rows",
            explanation_triad(
              formal = tagList(
                tags$code("DISTINCT"), " removes duplicate rows from the result, ",
                "keeping one of each unique combination of the selected columns. ",
                "With several columns it dedupes on the whole ", tags$em("combination"),
                ", not each column separately."),
              plain = tagList(
                "“Show me each value once.” This is how you answer “what are the ",
                "possible categories here?” without reading the whole column and ",
                "counting on your fingers."),
              lab = tagList(
                "The ", kbd("researchers"), " table has twelve rows but only four ",
                "job titles. ", kbd("SELECT DISTINCT title FROM researchers"),
                " returns just PI, Grad Student, Postdoc, RA. (Try it on ",
                kbd("participants.nickname"), " to see how much work DISTINCT does ",
                "at an institute with eight Daves.)")
            ),
            sql_code("SELECT DISTINCT title\nFROM researchers;"),
            sql_editor("seled4", starter = "SELECT DISTINCT title\nFROM researchers;"),
            help_details("DISTINCT across two columns",
              tagList(kbd("SELECT DISTINCT title, lab_id FROM researchers"),
                " returns each unique ", tags$em("pairing"), " of title and lab. ",
                "The same title can therefore appear several times, once per lab ",
                "it occurs in. DISTINCT is asking “is this whole row new?”, not ",
                "“is this value new?”")),
            execution_order_ribbon(
              active = c("SELECT", "DISTINCT"),
              note = "DISTINCT runs immediately after SELECT, on the columns SELECT produced, which is why it compares whole output rows: by then, the output rows are all there is."),
            continue_button("select_continue4"),
            id_prefix = "select-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.select_step >= 5",
          step_container(5, "Compute new columns in SELECT",
            explanation_triad(
              formal = tagList(
                "The select list may contain ", tags$em("expressions"),
                " (arithmetic, string operations, function calls, literals) ",
                "evaluated per row. These computed columns usually get an ",
                tags$code("AS"), " alias."),
              plain = tagList(
                "You are not limited to columns that already exist. Do arithmetic ",
                "on the ones that do and return the answer as a new column, named ",
                "with ", kbd("AS"), " so the header isn't a formula."),
              lab = tagList(
                kbd("salary * 1.03 AS after_raise"), " shows what a 3% raise would ",
                "cost the institute, person by person. A ", kbd("NULL"),
                " salary times anything is still ", kbd("NULL"), ": two researchers ",
                "declined to disclose, and arithmetic cannot make them. More on ",
                "that on the WHERE tab, and the Functions tab has the rest of what a ",
                "select list can compute: text, rounding, dates, and ", kbd("CASE"), ".")
            ),
            sql_code("SELECT full_name,\n       salary,\n       salary * 1.03 AS after_raise\nFROM researchers;"),
            sql_editor("seled5", starter = "SELECT full_name,\n       salary,\n       salary * 1.03 AS after_raise\nFROM researchers;"),
            execution_order_ribbon(
              active = c("FROM", "SELECT"),
              note = "FROM happens first (find the table); the computed column in SELECT happens much later. In standard SQL that gap means WHERE, which has already run, cannot see a name you invent here. SQLite is lenient on this point and most databases are not; the WHERE tab has the details."),
            continue_button("select_done", "I've got SELECT ↓"),
            id_prefix = "select-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.select_step >= 5",
          quest_section(
            "Ten queries to make SELECT automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These need only ", kbd("SELECT"), ", ", kbd("FROM"), ", ",
              kbd("AS"), ", and ", kbd("DISTINCT"), "; no filtering or joining ",
              "yet. Tables you'll need: ", kbd("labs"), ", ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), "."),
            lapply(seq_along(SELECT_QUEST), function(i) {
              q <- SELECT_QUEST[[i]]
              sql_editor(paste0("selq", i), prompt = q$prompt,
                         starter = "", graded = TRUE, number = i)
            })
          ),
          dsec_select())
        )
      )
    )
  })
}
