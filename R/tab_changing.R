# =============================================================================
# tab_changing.R — "Changing data" CRUD walkthrough (Tab 9)
#
# Unlike every other tab, this one writes. Each session gets a PRIVATE in-memory
# copy of the database (via institute_con_scratch()), so INSERT/UPDATE/DELETE
# run freely and a Reset button re-seeds it. Graded items are checked by post-
# state verification: "Run" applies your statement; "Check" runs a verify query
# against the live copy and compares it to the expected post-state.
#
# The 10 tasks are deliberately independent (each targets distinct rows/tables)
# so accumulated edits don't break later checks — verified in build notes.
# =============================================================================

CRUD_QUEST <- list(
  list(prompt = paste0("Insert a new lab: lab_id 7, lab_name 'Quantum Napping Lab', ",
                       "building 'Wexler Hall', founded_year 2024."),
       verify = "SELECT lab_name, building, founded_year FROM labs WHERE lab_id = 7;",
       solution = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (7, 'Quantum Napping Lab', 'Wexler Hall', 2024);"),
  list(prompt = "Add a participant: participant_id 25, nickname 'Dave', species 'human', age 40.",
       verify = "SELECT nickname, species, age FROM participants WHERE participant_id = 25;",
       solution = "INSERT INTO participants (participant_id, nickname, species, age)\nVALUES (25, 'Dave', 'human', 40);"),
  list(prompt = paste0("Add a visiting researcher with unknowns: researcher_id 13, full_name ",
                       "'Dr. Nil Void', lab_id NULL, title 'PI', salary NULL, start_year 2025."),
       verify = "SELECT full_name, lab_id, salary FROM researchers WHERE researcher_id = 13;",
       solution = "INSERT INTO researchers (researcher_id, full_name, lab_id, title, salary, start_year)\nVALUES (13, 'Dr. Nil Void', NULL, 'PI', NULL, 2025);"),
  list(prompt = "Study 10 was wrongly shelved. Update the study with study_id 10 to status 'completed'.",
       verify = "SELECT status FROM studies WHERE study_id = 10;",
       solution = "UPDATE studies SET status = 'completed' WHERE study_id = 10;"),
  list(prompt = "Give researcher_id 2 (Wesley Tern) a raise: set their salary to 36000.",
       verify = "SELECT salary FROM researchers WHERE researcher_id = 2;",
       solution = "UPDATE researchers SET salary = 36000 WHERE researcher_id = 2;"),
  list(prompt = "Every ongoing study just wrapped up. Set every study with status 'ongoing' to 'completed'.",
       verify = "SELECT COUNT(*) AS n FROM studies WHERE status = 'ongoing';",
       solution = "UPDATE studies SET status = 'completed' WHERE status = 'ongoing';"),
  list(prompt = "Clean up dropouts: delete every enrollment where dropped_out = 1.",
       verify = "SELECT COUNT(*) AS n FROM enrollments WHERE dropped_out = 1;",
       solution = "DELETE FROM enrollments WHERE dropped_out = 1;"),
  list(prompt = "Pip withdrew from the institute. Delete the participant with participant_id 24.",
       verify = "SELECT COUNT(*) AS n FROM participants WHERE participant_id = 24;",
       solution = "DELETE FROM participants WHERE participant_id = 24;"),
  list(prompt = paste0("Register a new study: study_id 11, title 'Do crickets prefer jazz?', ",
                       "lab_id 6, lead_researcher_id 9, year 2025, status 'completed', sample_size 12."),
       verify = "SELECT title, lab_id FROM studies WHERE study_id = 11;",
       solution = "INSERT INTO studies (study_id, title, lab_id, lead_researcher_id, year, status, sample_size)\nVALUES (11, 'Do crickets prefer jazz?', 6, 9, 2025, 'completed', 12);"),
  list(prompt = "Study 4 recruited ten more subjects. Increase its sample_size by 10 (study_id 4).",
       verify = "SELECT sample_size FROM studies WHERE study_id = 4;",
       solution = "UPDATE studies SET sample_size = sample_size + 10 WHERE study_id = 4;")
)

CRUD_STEP_TITLES <- c(
  "Your own private copy",
  "INSERT: add rows",
  "UPDATE: change rows",
  "DELETE: remove rows",
  "CREATE TABLE and a safety recap"
)

source_changing_server <- function(input, output, session) {

  # ---- Persistent per-session scratch connection --------------------------
  scratch <- reactiveValues(con = NULL)
  ensure_con <- function() {
    if (is.null(scratch$con) || !dbIsValid(scratch$con)) {
      scratch$con <- institute_con_scratch()
    }
    scratch$con
  }
  con_holder <- function() ensure_con()

  observeEvent(input$crud_reset, {
    if (!is.null(scratch$con) && dbIsValid(scratch$con)) dbDisconnect(scratch$con)
    scratch$con <- institute_con_scratch()
    output$crud_reset_msg <- renderUI(
      div(style = "color:#0F6E56; font-size:13px; margin-top:8px;",
          "✓ Fresh copy restored — every table is back to its original rows."))
  })

  session$onSessionEnded(function() {
    c <- isolate(scratch$con)
    if (!is.null(c) && dbIsValid(c)) dbDisconnect(c)
  })

  # ---- Step gating --------------------------------------------------------
  walk <- reactiveValues(step = 1)
  output$crud_step <- reactive(walk$step)
  outputOptions(output, "crud_step", suspendWhenHidden = FALSE)
  observeEvent(input$crud_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$crud_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$crud_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$crud_continue4, walk$step <- max(walk$step, 5))

  output$crud_toc <- renderUI(
    walkthrough_toc(walk$step, CRUD_STEP_TITLES, "crud-step"))

  # ---- Wire the in-step demo editors against the scratch copy -------------
  for (.i in 1:5) {
    local({
      eid <- paste0("csed", .i)
      sql_editor_server_crud(eid, input, output, session, con_holder = con_holder)
    })
  }

  # ---- Wire the 10 graded tasks (post-state verification) -----------------
  for (i in seq_along(CRUD_QUEST)) {
    local({
      idx <- i
      q   <- CRUD_QUEST[[idx]]
      con_tmp <- institute_con_scratch()
      dbExecute(con_tmp, q$solution)
      expected <- dbGetQuery(con_tmp, q$verify)
      dbDisconnect(con_tmp)
      sql_editor_server_crud(
        id = paste0("crudq", idx), input = input, output = output, session = session,
        con_holder = con_holder, verify = q$verify,
        expected = expected, solution = q$solution)
    })
  }

  reset_button <- function() {
    div(class = "p-3 my-3 d-flex",
        style = "background:#E1F1FB; border-radius:10px; align-items:center; gap:14px; flex-wrap:wrap;",
        div(style = "flex:1; min-width:220px;",
            div(style = "font-weight:700; color:#003D6B;", "This is your private copy."),
            div(style = "font-size:13px; color:#244; line-height:1.5;",
                "Nothing you do here touches the real database or anyone else's ",
                "session. Experiment freely — and reset whenever you want a clean slate.")),
        div(actionButton("crud_reset", "↺ Reset my copy", class = "btn-outline-secondary btn-sm"),
            uiOutput("crud_reset_msg")))
  }

  output$changing_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("crud_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "Changing data — INSERT, UPDATE, DELETE",
            eyebrow = "Writing, safely",
            p(style = "margin-bottom:6px;",
              "Every other tab only ", tags$b("reads"), ". Here you'll ", tags$b("write"),
              " — add rows, change them, remove them. Because mistakes are part of ",
              "learning, you get a ", tags$b("private, disposable copy"),
              " of the institute's database to do it on."),
            p(style = "margin-bottom:0;",
              "For an R user: these are the writes you'd normally do by reassigning ",
              "a data frame (", kbd("df <- rbind(df, row)"), ", ", kbd("df$x[i] <- v"),
              ", ", kbd("df <- df[-i, ]"), ") — but persisted in the database.")
          ),

          reset_button(),

          # ===== STEP 1 =====
          step_container(1, "Your own private copy",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "The query tabs connect read-only, so no statement can alter the ",
                "shipped data. This tab instead seeds a fresh in-memory database ",
                "from the same schema and rows, scoped to your session — a ",
                "disposable sandbox with full write access."),
              plain = tagList(tags$b("In human words. "),
                "You're editing a throwaway clone, not the master. If you mangle it, ",
                "press ", tags$b("Reset"), " and it's pristine again. No one else ",
                "sees your changes."),
              lab = tagList(tags$b("Lab database. "),
                "Same six tables, same rows you've been querying — now editable. ",
                "Run a harmless ", kbd("SELECT"), " to confirm the data's all here, ",
                "then start changing it.")
            ),
            sql_code("-- Your copy starts identical to the real thing:\nSELECT COUNT(*) AS labs FROM labs;"),
            sql_editor("csed1", starter = "SELECT COUNT(*) AS labs FROM labs;"),
            callout_warm(tags$b("Read-only elsewhere, writable here. "),
              "On the query tabs, an ", kbd("INSERT"), " or ", kbd("DELETE"),
              " is politely refused. On this tab it runs — against your copy only."),
            continue_button("crud_continue1"),
            id_prefix = "crud-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.crud_step >= 2",
          step_container(2, "INSERT: add rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("INSERT INTO table (columns…) VALUES (values…)"),
                " appends a row. Naming the columns explicitly makes the statement ",
                "robust to schema changes and lets you omit columns that have ",
                "defaults or accept ", tags$code("NULL"), "."),
              plain = tagList(tags$b("In human words. "),
                "List the columns, then the matching values in the same order. Use ",
                kbd("NULL"), " for an unknown, quotes for text, bare numbers for ",
                "numbers."),
              lab = tagList(tags$b("Lab database. "),
                kbd("INSERT INTO labs (lab_id, lab_name, building, founded_year) VALUES (8, 'Test Lab', 'Wexler Hall', 2025)"),
                " adds a lab. Then ", kbd("SELECT * FROM labs"), " to see it.")
            ),
            sql_code("INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Wexler Hall', 2025);"),
            sql_editor("csed2", starter = "INSERT INTO labs (lab_id, lab_name, building, founded_year)\nVALUES (8, 'Test Lab', 'Wexler Hall', 2025);"),
            help_details("Leaving a column out",
              tagList("Omit a nullable column and it becomes ", kbd("NULL"),
                "; omit one with a default (like ", kbd("enrollments.dropped_out"),
                ") and it takes the default. Columns marked ", kbd("NOT NULL"),
                " with no default must be supplied.")),
            continue_button("crud_continue2"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.crud_step >= 3",
          step_container(3, "UPDATE: change rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("UPDATE table SET col = expr WHERE …"),
                " modifies the rows matching the predicate. The ", tags$code("WHERE"),
                " is what scopes it; its expression may reference the column's ",
                "current value."),
              plain = tagList(tags$b("In human words. "),
                "Say what to change and, crucially, ", tags$b("which rows"),
                ". You can compute from the old value, e.g. ",
                kbd("SET sample_size = sample_size + 10"), "."),
              lab = tagList(tags$b("Lab database. "),
                kbd("UPDATE researchers SET salary = 36000 WHERE researcher_id = 2"),
                " gives one person a raise; without the ", kbd("WHERE"),
                " it would overwrite ", tags$em("everyone's"), " salary.")
            ),
            sql_code("UPDATE studies\nSET sample_size = sample_size + 10\nWHERE study_id = 4;"),
            sql_editor("csed3", starter = "UPDATE studies\nSET sample_size = sample_size + 10\nWHERE study_id = 4;"),
            callout_warm(tags$b("The most expensive missing word in SQL. "),
              "An ", kbd("UPDATE"), " with no ", kbd("WHERE"),
              " changes every row in the table. Always write the ", kbd("WHERE"),
              " first, then the ", kbd("SET"), "."),
            continue_button("crud_continue3"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.crud_step >= 4",
          step_container(4, "DELETE: remove rows",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("DELETE FROM table WHERE …"),
                " removes the matching rows. As with ", tags$code("UPDATE"),
                ", the ", tags$code("WHERE"), " defines the scope; omitting it ",
                "empties the table."),
              plain = tagList(tags$b("In human words. "),
                "Pick the rows to drop with ", kbd("WHERE"),
                ", and they're gone. No ", kbd("WHERE"), ", no survivors."),
              lab = tagList(tags$b("Lab database. "),
                kbd("DELETE FROM enrollments WHERE dropped_out = 1"),
                " purges the dropouts; ", kbd("DELETE FROM participants WHERE participant_id = 24"),
                " removes just Pip.")
            ),
            sql_code("DELETE FROM enrollments\nWHERE dropped_out = 1;"),
            sql_editor("csed4", starter = "DELETE FROM enrollments\nWHERE dropped_out = 1;"),
            callout_warm(tags$b("Preview before you delete. "),
              "Run the same ", kbd("WHERE"), " as a ", kbd("SELECT"),
              " first — ", kbd("SELECT * FROM enrollments WHERE dropped_out = 1"),
              " — to see exactly which rows will vanish. Then swap ", kbd("SELECT *"),
              " for ", kbd("DELETE"), "."),
            continue_button("crud_continue4"),
            id_prefix = "crud-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.crud_step >= 5",
          step_container(5, "CREATE TABLE and a safety recap",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("CREATE TABLE name (col TYPE constraints, …)"),
                " defines a new relation: column names, types (", tags$code("INTEGER"),
                ", ", tags$code("TEXT"), ", ", tags$code("REAL"), "), and constraints ",
                "(", tags$code("PRIMARY KEY"), ", ", tags$code("NOT NULL"), ")."),
              plain = tagList(tags$b("In human words. "),
                "You can make your own tables, not just edit existing ones. Declare ",
                "the columns and their types, then ", kbd("INSERT"), " into it like ",
                "any other table."),
              lab = tagList(tags$b("Lab database. "),
                kbd("CREATE TABLE notes (id INTEGER PRIMARY KEY, note TEXT)"),
                " makes a scratch table; add rows, query it, and Reset wipes it ",
                "along with everything else.")
            ),
            sql_code("CREATE TABLE notes (\n  id   INTEGER PRIMARY KEY,\n  note TEXT NOT NULL\n);\n-- then: INSERT INTO notes (id, note) VALUES (1, 'hello');"),
            sql_editor("csed5", starter = "CREATE TABLE notes (\n  id   INTEGER PRIMARY KEY,\n  note TEXT NOT NULL\n);", height = "150px"),
            callout_warm(tags$b("The golden rule, one more time. "),
              tags$b("Always"), " scope an ", kbd("UPDATE"), " or ", kbd("DELETE"),
              " with ", kbd("WHERE"), " unless you truly mean every row. On a real ",
              "database there's no Reset button."),
            continue_button("crud_done", "I've got CRUD ↓"),
            id_prefix = "crud-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.crud_step >= 5",
          quest_section(
            "Ten changes to make writing automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "Write each statement, press ", tags$b("Run"), " to apply it to your ",
              "copy, then ", tags$b("Check my answer"), " — it inspects your copy's ",
              "new state and compares it to the target. Tasks are independent, but ",
              "if things get tangled, hit ", tags$b("Reset my copy"), " above."),
            quest_editors_ui(CRUD_QUEST, "crudq")
          ))
        )
      )
    )
  })
}
