# =============================================================================
# tab_joins.R — "JOIN" walkthrough (Tab 6)
#
# The big-hurdle tab, given extra scaffolding per the research: a row-by-row
# trace (Venn diagrams hide row multiplication), the LEFT-JOIN anti-join
# pattern, the LEFT->INNER filter trap, and one-to-many row multiplication.
# Five gated steps then a 10-question quest.
# =============================================================================

JOINS_QUEST <- list(
  list(prompt = "Return each study's title and its lab's lab_name. (INNER JOIN studies to labs.)",
       ref = "SELECT s.title, l.lab_name FROM studies s JOIN labs l ON s.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
  list(prompt = "Return each researcher's full_name and their lab's lab_name (INNER JOIN).",
       ref = "SELECT r.full_name, l.lab_name FROM researchers r JOIN labs l ON r.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT r.full_name, l.lab_name\nFROM researchers r\nJOIN labs l ON r.lab_id = l.lab_id;"),
  list(prompt = "Return each study's title and the full_name of its lead researcher.",
       ref = "SELECT s.title, r.full_name FROM studies s JOIN researchers r ON s.lead_researcher_id = r.researcher_id;",
       ordered = FALSE,
       solution = "SELECT s.title, r.full_name\nFROM studies s\nJOIN researchers r ON s.lead_researcher_id = r.researcher_id;"),
  list(prompt = paste0("Return every researcher's full_name and their lab's lab_name, including ",
                       "researchers with no lab. Use a LEFT JOIN; the unaffiliated one shows a ",
                       "NULL lab_name."),
       ref = "SELECT r.full_name, l.lab_name FROM researchers r LEFT JOIN labs l ON r.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT r.full_name, l.lab_name\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id;"),
  list(prompt = paste0("Return each study's title alongside the nickname of each enrolled ",
                       "participant. Join studies -> enrollments -> participants."),
       ref = "SELECT s.title, p.nickname FROM studies s JOIN enrollments e ON s.study_id = e.study_id JOIN participants p ON e.participant_id = p.participant_id;",
       ordered = FALSE,
       solution = "SELECT s.title, p.nickname\nFROM studies s\nJOIN enrollments e ON s.study_id = e.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
  list(prompt = "Return the title of every study that has no enrollments. (LEFT JOIN, then look for NULLs.)",
       ref = "SELECT s.title FROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id WHERE e.enrollment_id IS NULL;",
       ordered = FALSE,
       solution = "SELECT s.title\nFROM studies s\nLEFT JOIN enrollments e ON s.study_id = e.study_id\nWHERE e.enrollment_id IS NULL;"),
  list(prompt = "Return the nickname of every participant who is enrolled in no study.",
       ref = "SELECT p.nickname FROM participants p LEFT JOIN enrollments e ON p.participant_id = e.participant_id WHERE e.enrollment_id IS NULL;",
       ordered = FALSE,
       solution = "SELECT p.nickname\nFROM participants p\nLEFT JOIN enrollments e ON p.participant_id = e.participant_id\nWHERE e.enrollment_id IS NULL;"),
  list(prompt = "For every enrollment, return its enrollment_id, the study title, and the participant nickname.",
       ref = "SELECT e.enrollment_id, s.title, p.nickname FROM enrollments e JOIN studies s ON e.study_id = s.study_id JOIN participants p ON e.participant_id = p.participant_id;",
       ordered = FALSE,
       solution = "SELECT e.enrollment_id, s.title, p.nickname\nFROM enrollments e\nJOIN studies s ON e.study_id = s.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
  list(prompt = paste0("Return all pairs of researchers in the same lab, as columns ",
                       "researcher_a and researcher_b. List each pair once (a's id < b's id). ",
                       "This is a self-join."),
       ref = "SELECT a.full_name AS researcher_a, b.full_name AS researcher_b FROM researchers a JOIN researchers b ON a.lab_id = b.lab_id AND a.researcher_id < b.researcher_id;",
       ordered = FALSE,
       solution = "SELECT a.full_name AS researcher_a, b.full_name AS researcher_b\nFROM researchers a\nJOIN researchers b ON a.lab_id = b.lab_id AND a.researcher_id < b.researcher_id;"),
  list(prompt = paste0("For each lab, return its lab_name and the number of studies it runs as ",
                       "n_studies. Use a LEFT JOIN so every lab is included."),
       ref = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies FROM labs l LEFT JOIN studies s ON l.lab_id = s.lab_id GROUP BY l.lab_id, l.lab_name;",
       ordered = FALSE,
       solution = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies\nFROM labs l\nLEFT JOIN studies s ON l.lab_id = s.lab_id\nGROUP BY l.lab_id, l.lab_name;")
)

JOINS_STEP_TITLES <- c(
  "Why join: keys and INNER JOIN",
  "Qualify columns; table aliases",
  "LEFT JOIN and finding what's missing",
  "The LEFT-to-INNER filter trap",
  "Row multiplication; many tables"
)

source_joins_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$joins_step <- reactive(walk$step)
  outputOptions(output, "joins_step", suspendWhenHidden = FALSE)

  observeEvent(input$joins_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$joins_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$joins_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$joins_continue4, walk$step <- max(walk$step, 5))

  output$joins_toc <- renderUI(
    walkthrough_toc(walk$step, JOINS_STEP_TITLES, "joins-step"))

  wire_demo_editors("jsed", 5, input, output, session)
  wire_quest_editors(JOINS_QUEST, "joiq", input, output, session)

  output$joins_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("joins_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "JOINs — combining tables by their keys",
            eyebrow = "The real power of a database",
            p(style = "margin-bottom:6px;",
              "The institute's data is spread across linked tables so nothing is ",
              "duplicated: a study stores a ", kbd("lab_id"), ", not the whole lab. ",
              "A ", kbd("JOIN"), " follows those keys to stitch the rows back together."),
            p(style = "margin-bottom:0;",
              "For an R user: a ", kbd("JOIN"), " is ", kbd("merge()"),
              " / ", kbd("dplyr::*_join()"), " — match rows from two tables on a ",
              "shared key column.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Why join: keys and INNER JOIN",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "An ", tags$code("INNER JOIN"), " pairs each row of one relation ",
                "with rows of another that satisfy the ", tags$code("ON"),
                " predicate (typically foreign key = primary key), emitting the ",
                "matched combinations. Unmatched rows are excluded."),
              plain = tagList(tags$b("In human words. "),
                "Tell SQL which column in table A points at which column in table B. ",
                "It returns the rows that match up. Rows with no match are left out."),
              lab = tagList(tags$b("Lab database. "),
                kbd("studies.lab_id → labs.lab_id"), " is the wiring. ",
                kbd("studies JOIN labs ON studies.lab_id = labs.lab_id"),
                " gives each study its lab's details.")
            ),
            sql_code("SELECT studies.title, labs.lab_name\nFROM studies\nJOIN labs ON studies.lab_id = labs.lab_id;"),
            p(style = "margin:8px 0 4px 0;", "Run it. Every study lines up with its ",
              "lab. Try adding ", kbd("labs.building"), " to the SELECT:"),
            sql_editor("jsed1", starter = "SELECT studies.title, labs.lab_name\nFROM studies\nJOIN labs ON studies.lab_id = labs.lab_id;"),
            callout_warm(tags$b("JOIN and INNER JOIN are the same thing. "),
              "Plain ", kbd("JOIN"), " means ", kbd("INNER JOIN"),
              " — only matched rows. The word INNER is optional."),
            continue_button("joins_continue1"),
            id_prefix = "joins-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.joins_step >= 2",
          step_container(2, "Qualify columns; table aliases",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "When tables share a column name (both have ", tags$code("lab_id"),
                "), references must be qualified as ", tags$code("table.column"),
                ". A table alias (", tags$code("studies s"),
                ") shortens those qualifications and is required for self-joins."),
              plain = tagList(tags$b("In human words. "),
                "Give each table a short nickname and prefix shared column names ",
                "with it, so SQL knows which ", kbd("lab_id"), " you mean."),
              lab = tagList(tags$b("Lab database. "),
                kbd("FROM studies s JOIN labs l ON s.lab_id = l.lab_id"),
                " — now write ", kbd("s.title"), " and ", kbd("l.lab_name"), ".")
            ),
            sql_code("SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
            sql_editor("jsed2", starter = "SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
            help_details("“ambiguous column name” errors",
              tagList("If you write just ", kbd("lab_id"),
                " when both tables have one, SQLite stops with ",
                tags$em("ambiguous column name"), ". The fix is always to qualify it: ",
                kbd("s.lab_id"), ".")),
            continue_button("joins_continue2"),
            id_prefix = "joins-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.joins_step >= 3",
          step_container(3, "LEFT JOIN and finding what's missing",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A ", tags$code("LEFT JOIN"), " keeps every row of the left relation; ",
                "where the right has no match, its columns are ", tags$code("NULL"),
                ". Filtering ", tags$code("WHERE right_key IS NULL"),
                " yields the anti-join: left rows with no partner."),
              plain = tagList(tags$b("In human words. "),
                "“Keep everyone on the left, matched or not.” The unmatched ones get ",
                "NULLs on the right — and that's exactly how you ", tags$em("find"),
                " the unmatched ones."),
              lab = tagList(tags$b("Lab database. "),
                "Dr. Wanda Roams has a ", kbd("NULL"), " lab_id. An ", kbd("INNER JOIN"),
                " drops her; a ", kbd("LEFT JOIN"), " keeps her with a NULL lab_name. ",
                "Study 10 has no enrollments — find it the same way.")
            ),
            sql_code("SELECT r.full_name, l.lab_name\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id;"),
            sql_editor("jsed3", starter = "SELECT r.full_name, l.lab_name\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id;"),
            callout_warm(tags$b("The anti-join pattern. "),
              "To find “rows with no match,” LEFT JOIN, then ",
              kbd("WHERE <right key> IS NULL"), ". It's how you ask “which studies ",
              "have nobody enrolled?” or “which participants signed up for nothing?”"),
            continue_button("joins_continue3"),
            id_prefix = "joins-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.joins_step >= 4",
          step_container(4, "The LEFT-to-INNER filter trap",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A predicate on the right table placed in ", tags$code("WHERE"),
                " is applied ", tags$em("after"), " the join, rejecting the ",
                "NULL-filled rows and silently demoting a ", tags$code("LEFT JOIN"),
                " to an ", tags$code("INNER JOIN"), ". To keep them, put the ",
                "condition in the ", tags$code("ON"), " clause instead."),
              plain = tagList(tags$b("In human words. "),
                "If you LEFT JOIN to keep unmatched rows, then filter the right ",
                "table in WHERE, you throw those rows right back out — because ",
                "NULL fails almost every test."),
              lab = tagList(tags$b("Lab database. "),
                kbd("LEFT JOIN ... WHERE l.building = 'Wexler Hall'"),
                " quietly discards the unaffiliated researcher (her lab is NULL), so ",
                "you're back to an inner join without realizing it.")
            ),
            sql_code("-- Looks like a LEFT JOIN, behaves like an INNER one:\nSELECT r.full_name, l.building\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id\nWHERE l.building = 'Wexler Hall';"),
            sql_editor("jsed4", starter = "SELECT r.full_name, l.building\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id\nWHERE l.building = 'Wexler Hall';"),
            help_details("Keep the unmatched rows: filter in ON",
              tagList("Move the condition into the join: ",
                kbd("LEFT JOIN labs l ON r.lab_id = l.lab_id AND l.building = 'Wexler Hall'"),
                ". Now non-matching labs become NULL instead of vanishing, and the ",
                "left rows survive.")),
            continue_button("joins_continue4"),
            id_prefix = "joins-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.joins_step >= 5",
          step_container(5, "Row multiplication; many tables",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "In a one-to-many join, each left row is emitted once per matching ",
                "right row, so result cardinality is the number of matched pairs, ",
                "not the number of left rows. Joins chain: add another ",
                tags$code("JOIN"), " to reach a third table."),
              plain = tagList(tags$b("In human words. "),
                "Join a study to its many enrollments and the study's row repeats — ",
                "once per enrollee. That's not a bug; it's the join showing every ",
                "pairing. A Venn diagram hides this; tracing rows shows it."),
              lab = tagList(tags$b("Lab database. "),
                kbd("studies JOIN enrollments JOIN participants"),
                " yields one row per enrolled participant — the seagull study's ",
                "title repeats for each bird in it.")
            ),
            sql_code("SELECT s.title, p.nickname\nFROM studies s\nJOIN enrollments e ON s.study_id = e.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
            sql_editor("jsed5", starter = "SELECT s.title, p.nickname\nFROM studies s\nJOIN enrollments e ON s.study_id = e.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
            callout_warm(tags$b("Counting after a join. "),
              "Because rows multiply, ", kbd("COUNT(*)"), " after a one-to-many join ",
              "counts pairings, not studies. To count participants per study, ",
              kbd("GROUP BY"), " the study and count — you'll do this in the quest."),
            execution_order_ribbon(
              active = c("FROM"),
              note = "Joins happen in FROM — the very first step — building the combined row set that WHERE, GROUP BY, and SELECT then work on."),
            continue_button("joins_done", "I've got JOINs ↓"),
            id_prefix = "joins-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.joins_step >= 5",
          quest_section(
            "Ten queries to make joining automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("JOIN"), " and ", kbd("LEFT JOIN"),
              ", table aliases, the anti-join pattern, a self-join, and a join + ",
              kbd("GROUP BY"), ". Keys: ", kbd("studies.lab_id→labs"), ", ",
              kbd("researchers.lab_id→labs"), ", ",
              kbd("enrollments.study_id→studies"), ", ",
              kbd("enrollments.participant_id→participants"), "."),
            quest_editors_ui(JOINS_QUEST, "joiq")
          ))
        )
      )
    )
  })
}
