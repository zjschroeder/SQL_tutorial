# =============================================================================
# tab_joins.R — "JOIN" walkthrough (Tab 6)
#
# The big-hurdle tab, given extra scaffolding per the research: a row-by-row
# trace (Venn diagrams hide row multiplication), the LEFT-JOIN anti-join
# pattern, the LEFT->INNER filter trap, and one-to-many row multiplication.
# Seven gated steps then a 10-question quest. Steps 6 (RIGHT / FULL OUTER /
# CROSS) and 7 (self-joins) complete the set of join types; the self-join is
# tested in the quest, so it needs a step of its own.
#
# RIGHT and FULL OUTER JOIN need SQLite 3.39 (2022) or later. They appear only
# in ungraded demo editors, never in a quest reference, so an older build
# degrades to a friendly error in one editor and nothing else.
# =============================================================================

JOINS_QUEST <- list(
  list(prompt = "Return each study's title and its lab's lab_name. (INNER JOIN studies to labs.)",
       ref = "SELECT s.title, l.lab_name FROM studies s JOIN labs l ON s.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
  list(prompt = "Return each researcher's full_name and their lab's lab_name (INNER JOIN). Notice who is missing.",
       ref = "SELECT r.full_name, l.lab_name FROM researchers r JOIN labs l ON r.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT r.full_name, l.lab_name\nFROM researchers r\nJOIN labs l ON r.lab_id = l.lab_id;"),
  list(prompt = "Return each study's title and the full_name of its lead researcher.",
       ref = "SELECT s.title, r.full_name FROM studies s JOIN researchers r ON s.lead_researcher_id = r.researcher_id;",
       ordered = FALSE,
       solution = "SELECT s.title, r.full_name\nFROM studies s\nJOIN researchers r ON s.lead_researcher_id = r.researcher_id;"),
  list(prompt = paste0("Return every researcher's full_name and their lab's lab_name, including ",
                       "the researcher with no lab. Use a LEFT JOIN; the unaffiliated one shows a ",
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
  list(prompt = "Return the nickname of every participant who is enrolled in no study at all.",
       ref = "SELECT p.nickname FROM participants p LEFT JOIN enrollments e ON p.participant_id = e.participant_id WHERE e.enrollment_id IS NULL;",
       ordered = FALSE,
       solution = "SELECT p.nickname\nFROM participants p\nLEFT JOIN enrollments e ON p.participant_id = e.participant_id\nWHERE e.enrollment_id IS NULL;"),
  list(prompt = "For every enrollment, return its enrollment_id, the study title, and the participant nickname.",
       ref = "SELECT e.enrollment_id, s.title, p.nickname FROM enrollments e JOIN studies s ON e.study_id = s.study_id JOIN participants p ON e.participant_id = p.participant_id;",
       ordered = FALSE,
       solution = "SELECT e.enrollment_id, s.title, p.nickname\nFROM enrollments e\nJOIN studies s ON e.study_id = s.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
  list(prompt = paste0("Return all pairs of researchers in the same lab, as columns ",
                       "researcher_a and researcher_b. List each pair once (a's id < b's id). ",
                       "This is a self-join: the researchers table, meeting itself."),
       ref = "SELECT a.full_name AS researcher_a, b.full_name AS researcher_b FROM researchers a JOIN researchers b ON a.lab_id = b.lab_id AND a.researcher_id < b.researcher_id;",
       ordered = FALSE,
       solution = "SELECT a.full_name AS researcher_a, b.full_name AS researcher_b\nFROM researchers a\nJOIN researchers b ON a.lab_id = b.lab_id AND a.researcher_id < b.researcher_id;"),
  list(prompt = paste0("For each lab, return its lab_name and the number of studies it runs as ",
                       "n_studies. Use a LEFT JOIN so every lab is included, even one running nothing."),
       ref = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies FROM labs l LEFT JOIN studies s ON l.lab_id = s.lab_id GROUP BY l.lab_id, l.lab_name;",
       ordered = FALSE,
       solution = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies\nFROM labs l\nLEFT JOIN studies s ON l.lab_id = s.lab_id\nGROUP BY l.lab_id, l.lab_name;")
)

JOINS_STEP_TITLES <- c(
  "Why join: keys and INNER JOIN",
  "Qualify columns; table aliases",
  "LEFT JOIN and finding what's missing",
  "The LEFT-to-INNER filter trap",
  "Row multiplication; many tables",
  "RIGHT, FULL OUTER, and CROSS JOIN",
  "Self-joins: a table meets itself"
)

source_joins_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$joins_step <- reactive(walk$step)
  outputOptions(output, "joins_step", suspendWhenHidden = FALSE)

  observeEvent(input$joins_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$joins_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$joins_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$joins_continue4, walk$step <- max(walk$step, 5))
  observeEvent(input$joins_continue5, walk$step <- max(walk$step, 6))
  observeEvent(input$joins_continue6, walk$step <- max(walk$step, 7))

  output$joins_toc <- renderUI(
    walkthrough_toc(walk$step, JOINS_STEP_TITLES, "joins-step"))

  wire_demo_editors("jsed", 8, input, output, session)
  wire_quest_editors(JOINS_QUEST, "joiq", input, output, session)
  # The "In a school district" section (R/district_sections.R)
  wire_demo_editors("dzjoi", 1, input, output, session, con_fun = district_con_ro)
  wire_quest_editors(JOINS_DQUEST, "dzjoiq", input, output, session, con_fun = district_con_ro)

  output$joins_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("joins_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "JOINs — combining tables by their keys",
            eyebrow = "The reason a database is not a pile of spreadsheets",
            p(style = "margin-bottom:6px;",
              "Recall from the first tab that the institute's data is spread across ",
              "linked tables so that nothing is written twice: a study stores a ",
              kbd("lab_id"), ", not the whole lab. That is tidy right up until you ",
              "want to see a study's title next to its lab's name, at which point you ",
              "need to follow the key back. A ", kbd("JOIN"), " is that act of ",
              "following. This is the tab people find hardest, so it goes slowest, and ",
              "the collapsibles are longer than usual."),
            p(style = "margin-bottom:0;",
              "For an R user: a ", kbd("JOIN"), " is ", kbd("merge()"),
              " or ", kbd("dplyr::*_join()"), ": match rows from two tables on a ",
              "shared key column. If you have used ", kbd("left_join()"),
              " you already know the difference between steps 1 and 3; the syntax is ",
              "the only new part.")
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
                "You get back the rows that match up, side by side. Rows with no ",
                "partner on the other side are left out, which is sometimes ",
                "what you want and sometimes the beginning of a long afternoon."),
              lab = tagList(tags$b("Lab database. "),
                kbd("studies.lab_id → labs.lab_id"), " is the wiring. ",
                kbd("studies JOIN labs ON studies.lab_id = labs.lab_id"),
                " gives each of the ten studies its lab's details, so “Do seagulls ",
                "back off when stared at?” finally sits next to “Avian Intimidation Lab.”")
            ),
            sql_code("SELECT studies.title, labs.lab_name\nFROM studies\nJOIN labs ON studies.lab_id = labs.lab_id;"),
            p(style = "margin:8px 0 4px 0;", "Run it. Every study lines up with its ",
              "lab. Then try adding ", kbd("labs.building"), " to the SELECT:"),
            sql_editor("jsed1", starter = "SELECT studies.title, labs.lab_name\nFROM studies\nJOIN labs ON studies.lab_id = labs.lab_id;"),
            callout_warm(tags$b("JOIN and INNER JOIN are the same thing. "),
              "Plain ", kbd("JOIN"), " means ", kbd("INNER JOIN"),
              ": only matched rows. The word INNER is optional, and most people ",
              "leave it off, which is fine as long as you remember that “join” ",
              "without qualification means “drop the unmatched.”"),
            help_details("The older spelling: FROM studies, labs WHERE …",
              tagList("Before ", kbd("JOIN … ON"), " existed, people listed the tables ",
                "with a comma and put the matching rule in ", kbd("WHERE"), ": ",
                kbd("FROM studies, labs WHERE studies.lab_id = labs.lab_id"),
                ". It still runs and you will meet it in old code. Its weakness is that ",
                "nothing forces you to write the ", kbd("WHERE"), ". Leave it off and ",
                "every study is paired with every lab, 60 rows where you wanted 10, and ",
                "no error. ", kbd("JOIN … ON"), " keeps the rule next to the join it ",
                "belongs to, which is the whole argument for it.")),
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
                "with it, so SQL knows which ", kbd("lab_id"), " you mean. One-letter ",
                "aliases (", kbd("s"), ", ", kbd("l"), ") are the convention. Nobody ",
                "will admire a longer one."),
              lab = tagList(tags$b("Lab database. "),
                kbd("FROM studies s JOIN labs l ON s.lab_id = l.lab_id"),
                ", and from then on you write ", kbd("s.title"), " and ", kbd("l.lab_name"), ".")
            ),
            sql_code("SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
            sql_editor("jsed2", starter = "SELECT s.title, l.lab_name\nFROM studies s\nJOIN labs l ON s.lab_id = l.lab_id;"),
            help_details("“ambiguous column name” errors",
              tagList("If you write just ", kbd("lab_id"),
                " when both tables have one, SQLite stops with ",
                tags$em("ambiguous column name"), ". Of all the errors on this tab, ",
                "this is the friendliest: it tells you exactly what is wrong, and the ",
                "fix is always to qualify it: ", kbd("s.lab_id"), ".")),
            help_details("USING, and why to leave NATURAL JOIN alone",
              tagList(
                p(style = "margin-bottom:6px;", "When the key has the same name on both sides, ",
                  kbd("JOIN labs l USING (lab_id)"), " is shorthand for ",
                  kbd("ON s.lab_id = l.lab_id"), ", and it has the courtesy to return one ",
                  kbd("lab_id"), " column in place of two."),
                p(style = "margin-bottom:0;", kbd("NATURAL JOIN"), " goes a step further and joins on ",
                  tags$em("every"), " column the two tables have in common, without being ",
                  "told which. ", kbd("studies NATURAL JOIN labs"), " happens to work. ",
                  kbd("studies NATURAL JOIN researchers"), " returns zero rows, because both ",
                  "tables have a ", kbd("lab_id"), " and both have a ", kbd("title"),
                  ", and no study's title is “PI.” A join whose meaning changes when ",
                  "someone adds a column is one I'd counsel against."))),
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
                "NULLs on the right, and those NULLs are exactly how you ",
                tags$em("find"), " the unmatched ones: LEFT JOIN, then look for the NULLs."),
              lab = tagList(tags$b("Lab database. "),
                "Dr. Wanda Roams has a ", kbd("NULL"), " lab_id (she is, per the name, ",
                "between labs). An ", kbd("INNER JOIN"),
                " drops her; a ", kbd("LEFT JOIN"), " keeps her, lab_name NULL. ",
                "Study 10, the cricket telepathy one, has no enrollments. Same trick finds it.")
            ),
            sql_code("SELECT r.full_name, l.lab_name\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id;"),
            sql_editor("jsed3", starter = "SELECT r.full_name, l.lab_name\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id;"),
            callout_warm(tags$b("The anti-join pattern. "),
              "To find “rows with no match,” LEFT JOIN, then ",
              kbd("WHERE <right key> IS NULL"), ". This is how you ask “which studies ",
              "have nobody enrolled?” or “which participants signed up for nothing?” ",
              "Commit it to memory; “what is missing?” comes up in every dataset, and ",
              "there is no MISSING keyword."),
            help_details("Tidying the NULLs a LEFT JOIN leaves behind",
              tagList("In a report, Dr. Roams's blank lab reads better as a label. ",
                kbd("COALESCE(l.lab_name, 'no lab')"), ", from the Functions tab, does it. ",
                "Do the substituting in ", kbd("SELECT"), " only. The anti-join still has ",
                "to test the real column in ", kbd("WHERE"), ", since a NULL you have ",
                "papered over can no longer be found.")),
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
                "You LEFT JOIN to keep the unmatched rows, then filter the right ",
                "table in WHERE, and you have thrown those rows straight back out, ",
                "because NULL fails almost every test (recall the WHERE tab). ",
                "The query still runs. It answers a different question than the ",
                "one you asked."),
              lab = tagList(tags$b("Lab database. "),
                kbd("LEFT JOIN ... WHERE l.building = 'Wexler Hall'"),
                " discards Dr. Roams (her building is NULL, and NULL is not ",
                "Wexler Hall), so you are back to an inner join without having typed one.")
            ),
            sql_code("-- Looks like a LEFT JOIN, behaves like an INNER one:\nSELECT r.full_name, l.building\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id\nWHERE l.building = 'Wexler Hall';"),
            sql_editor("jsed4", starter = "SELECT r.full_name, l.building\nFROM researchers r\nLEFT JOIN labs l ON r.lab_id = l.lab_id\nWHERE l.building = 'Wexler Hall';"),
            help_details("Keep the unmatched rows: filter in ON",
              tagList("Move the condition into the join: ",
                kbd("LEFT JOIN labs l ON r.lab_id = l.lab_id AND l.building = 'Wexler Hall'"),
                ". Now a non-matching lab becomes NULL rather than vanishing, and the ",
                "left rows survive. A condition in ON decides which rows pair up. A ",
                "condition in WHERE runs afterwards, on the joined rows, so a NULL-filled ",
                "row fails it.")),
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
                "Join a study to its many enrollments and the study's row repeats, ",
                "once per enrollee. The repetition is correct: the join returns one ",
                "row per pairing. A Venn diagram, the way joins are usually drawn, ",
                "hides this entirely, which is why this tutorial has not shown you one."),
              lab = tagList(tags$b("Lab database. "),
                kbd("studies JOIN enrollments JOIN participants"),
                " yields one row per enrolled participant, 89 in all, so the seagull ",
                "study's title repeats once for each bird (and Dave) in it.")
            ),
            sql_code("SELECT s.title, p.nickname\nFROM studies s\nJOIN enrollments e ON s.study_id = e.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
            sql_editor("jsed5", starter = "SELECT s.title, p.nickname\nFROM studies s\nJOIN enrollments e ON s.study_id = e.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
            callout_warm(tags$b("Counting after a join. "),
              "Because rows multiply, ", kbd("COUNT(*)"), " after a one-to-many join ",
              "counts pairings, not studies. To count participants per study, ",
              kbd("GROUP BY"), " the study and count; the last quest question asks ",
              "for exactly this. If a count ever comes back suspiciously large, a ",
              "join that multiplied rows is the first suspect."),
            execution_order_ribbon(
              active = c("FROM"),
              note = "Joins happen in FROM, the very first step, building the combined row set that WHERE, GROUP BY, and SELECT then work on. Everything downstream sees the joined table, never the originals."),
            continue_button("joins_continue5"),
            id_prefix = "joins-step"
          )),

          # ===== STEP 6 =====
          conditionalPanel("output.joins_step >= 6",
          step_container(6, "RIGHT, FULL OUTER, and CROSS JOIN",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A ", tags$code("RIGHT JOIN"), " keeps every row of the right relation, ",
                "NULL-filling the left: a ", tags$code("LEFT JOIN"), " with its operands ",
                "exchanged. A ", tags$code("FULL OUTER JOIN"), " keeps the unmatched rows ",
                "of both. A ", tags$code("CROSS JOIN"), " has no ", tags$code("ON"),
                " clause and returns the Cartesian product: every row of one relation ",
                "paired with every row of the other."),
              plain = tagList(tags$b("In human words. "),
                kbd("LEFT"), " and ", kbd("RIGHT"), " differ only in which table you ",
                "happened to write first, so most people pick ", kbd("LEFT"),
                " and reorder the tables when they need the other one. ", kbd("FULL OUTER"),
                " is for reconciling two lists: everything in either, matched up where ",
                "possible. ", kbd("CROSS JOIN"), " is every combination, which is rarely ",
                "what you want and occasionally exactly what you want."),
              lab = tagList(tags$b("Lab database. "),
                "Every lab at the institute has at least one researcher, so here ",
                kbd("researchers FULL OUTER JOIN labs"), " returns what the ",
                kbd("LEFT JOIN"), " did: 12 rows, Dr. Roams and her NULLs included. The ",
                "extra rows appear only when the right side has orphans too. (Task 1 on ",
                "the Changing data tab founds a lab with nobody in it. Run the same ",
                "join there afterwards and the new lab gets a row of its own.)")
            ),
            sql_code("SELECT r.full_name, l.lab_name\nFROM researchers r\nFULL OUTER JOIN labs l ON r.lab_id = l.lab_id;"),
            sql_editor("jsed6", starter = "SELECT r.full_name, l.lab_name\nFROM researchers r\nFULL OUTER JOIN labs l ON r.lab_id = l.lab_id;"),
            p(style = "margin:14px 0 4px 0;", "And the cross join. Six labs, paired every ",
              "possible way, with ", kbd("a.lab_id < b.lab_id"), " keeping one copy of each ",
              "pair and no lab paired with itself:"),
            sql_code("SELECT a.lab_name AS lab_a, b.lab_name AS lab_b\nFROM labs a\nCROSS JOIN labs b\nWHERE a.lab_id < b.lab_id;"),
            sql_editor("jsed7", starter = "SELECT a.lab_name AS lab_a, b.lab_name AS lab_b\nFROM labs a\nCROSS JOIN labs b\nWHERE a.lab_id < b.lab_id;"),
            callout_warm(tags$b("Cross joins multiply, literally. "),
              "Those are the 15 possible two-lab collaborations, none of which has ",
              "happened. Remove the ", kbd("WHERE"), " and there are 36 rows (6 × 6). ",
              "Cross ", kbd("enrollments"), " with ", kbd("measurements"),
              " and there are 25,721 (89 × 289), from two tables you could print on a few ",
              "pages. On real tables an accidental cross join is how a query comes to run ",
              "all night. The deliberate use is generating a full grid, e.g., every lab × ",
              "every year, so that combinations with no data still get a row."),
            help_details("If RIGHT or FULL OUTER JOIN gives an error",
              tagList("SQLite added both in version 3.39, in 2022; before that it had only ",
                kbd("INNER"), ", ", kbd("LEFT"), ", and ", kbd("CROSS"),
                ". PostgreSQL, SQL Server, and Oracle have always had all of them. MySQL ",
                "still has no ", kbd("FULL OUTER JOIN"), ", and the workaround there is a ",
                kbd("LEFT JOIN"), " and a ", kbd("RIGHT JOIN"), " glued together with ",
                kbd("UNION"), ", which is the subject of the next tab.")),
            continue_button("joins_continue6"),
            id_prefix = "joins-step"
          )),

          # ===== STEP 7 =====
          conditionalPanel("output.joins_step >= 7",
          step_container(7, "Self-joins: a table meets itself",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "A self-join joins a relation to itself. The two occurrences must be ",
                "given distinct aliases, after which they behave as two independent ",
                "tables. The join predicate need not be an equality: ",
                tags$code("<"), ", ", tags$code("<>"), ", and ranges are all permitted ",
                "(a non-equi join)."),
              plain = tagList(tags$b("In human words. "),
                "To compare rows of a table with other rows of the same table, list the ",
                "table twice under two names and join the copies. There is no ",
                kbd("SELF JOIN"), " keyword; it is an ordinary ", kbd("JOIN"),
                " in which both sides happen to be the same table."),
              lab = tagList(tags$b("Lab database. "),
                "Who shares a lab with whom? Join ", kbd("researchers a"), " to ",
                kbd("researchers b"), " on ", kbd("lab_id"), " and you get 23 rows: every ",
                "pair twice (Albatross–Tern, then Tern–Albatross) and each of the eleven ",
                "researchers who has a lab paired with themselves. Adding ",
                kbd("a.researcher_id < b.researcher_id"), " leaves the six real pairs.")
            ),
            sql_code("SELECT a.full_name AS researcher_a, b.full_name AS researcher_b\nFROM researchers a\nJOIN researchers b\n  ON a.lab_id = b.lab_id\n AND a.researcher_id < b.researcher_id;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then delete the ", kbd("AND"),
              " line to see the 23:"),
            sql_editor("jsed8", starter = "SELECT a.full_name AS researcher_a, b.full_name AS researcher_b\nFROM researchers a\nJOIN researchers b\n  ON a.lab_id = b.lab_id\n AND a.researcher_id < b.researcher_id;", height = "140px"),
            callout_warm(tags$b("Dr. Roams is in neither version. "),
              "Her ", kbd("lab_id"), " is NULL, and ", kbd("NULL = NULL"),
              " is unknown, so she does not even match herself. The same rule that ",
              "governed ", kbd("WHERE"), " governs ", kbd("ON"), ". The classic self-join, ",
              "for what it is worth, is an employee table whose ", kbd("manager_id"),
              " points back at its own ", kbd("employee_id"), "; the institute has no ",
              "such column, its reporting lines being what they are."),
            continue_button("joins_done", "I've got JOINs \u2193"),
            id_prefix = "joins-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.joins_step >= 7",
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
          ),
          dsec_joins())
        )
      )
    )
  })
}
