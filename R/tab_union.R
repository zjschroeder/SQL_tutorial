# =============================================================================
# tab_union.R — "UNION and the set operators" walkthrough
#
# Sits between JOINs and Subqueries, which is where sqlitetutorial.net, Neon's
# PostgreSQL tutorial, W3Schools, and CS50 SQL all put it. Five gated steps
# (UNION ALL; UNION; INTERSECT; EXCEPT; the rules, with the execution-order
# ribbon) then a 10-question quest.
#
# The running example is the institute's one replication: study 1 (seagull
# staring) and study 7 (its replication) share three participants, so all four
# operators give visibly different answers on the same two queries.
# =============================================================================

UNION_QUEST <- list(
  list(prompt = paste0("Return one column, name, holding every researcher's full_name and every ",
                       "participant's nickname: 36 rows, duplicates and all."),
       ref = "SELECT full_name AS name FROM researchers UNION ALL SELECT nickname FROM participants;",
       ordered = FALSE,
       solution = "SELECT full_name AS name FROM researchers\nUNION ALL\nSELECT nickname FROM participants;"),
  list(prompt = paste0("Return each lab_id that appears in the researchers table, the studies table, ",
                       "or both, once each. (Dr. Roams's NULL is a value like any other here, and ",
                       "belongs in the answer.)"),
       ref = "SELECT lab_id FROM researchers UNION SELECT lab_id FROM studies;", ordered = FALSE,
       solution = "SELECT lab_id FROM researchers\nUNION\nSELECT lab_id FROM studies;"),
  list(prompt = paste0("Return the participant_id of everyone enrolled in study 1, study 7, or both, ",
                       "each id once."),
       ref = "SELECT participant_id FROM enrollments WHERE study_id = 1 UNION SELECT participant_id FROM enrollments WHERE study_id = 7;",
       ordered = FALSE,
       solution = "SELECT participant_id FROM enrollments WHERE study_id = 1\nUNION\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
  list(prompt = "Return the participant_id of everyone enrolled in both study 1 and study 7.",
       ref = "SELECT participant_id FROM enrollments WHERE study_id = 1 INTERSECT SELECT participant_id FROM enrollments WHERE study_id = 7;",
       ordered = FALSE,
       solution = "SELECT participant_id FROM enrollments WHERE study_id = 1\nINTERSECT\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
  list(prompt = "Return the participant_id of everyone enrolled in study 1 but not in study 7.",
       ref = "SELECT participant_id FROM enrollments WHERE study_id = 1 EXCEPT SELECT participant_id FROM enrollments WHERE study_id = 7;",
       ordered = FALSE,
       solution = "SELECT participant_id FROM enrollments WHERE study_id = 1\nEXCEPT\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
  list(prompt = "Now the other direction: the participant_id of everyone in study 7 but not in study 1.",
       ref = "SELECT participant_id FROM enrollments WHERE study_id = 7 EXCEPT SELECT participant_id FROM enrollments WHERE study_id = 1;",
       ordered = FALSE,
       solution = "SELECT participant_id FROM enrollments WHERE study_id = 7\nEXCEPT\nSELECT participant_id FROM enrollments WHERE study_id = 1;"),
  list(prompt = paste0("Return the researcher_id of every researcher who leads no study. Use EXCEPT: ",
                       "all the researcher ids, less the ones that appear as a lead_researcher_id."),
       ref = "SELECT researcher_id FROM researchers EXCEPT SELECT lead_researcher_id FROM studies;",
       ordered = FALSE,
       solution = "SELECT researcher_id FROM researchers\nEXCEPT\nSELECT lead_researcher_id FROM studies;"),
  list(prompt = paste0("Return two columns, name and role: every researcher's full_name with the role ",
                       "'researcher', and every participant's nickname with the role 'participant'. ",
                       "Keep all 36 rows."),
       ref = "SELECT full_name AS name, 'researcher' AS role FROM researchers UNION ALL SELECT nickname, 'participant' FROM participants;",
       ordered = FALSE,
       solution = "SELECT full_name AS name, 'researcher' AS role FROM researchers\nUNION ALL\nSELECT nickname, 'participant' FROM participants;"),
  list(prompt = paste0("Return one column, year, listing each year in which both a researcher started ",
                       "and a study was run."),
       ref = "SELECT start_year AS year FROM researchers INTERSECT SELECT year FROM studies;",
       ordered = FALSE,
       solution = "SELECT start_year AS year FROM researchers\nINTERSECT\nSELECT year FROM studies;"),
  list(prompt = paste0("Return one column, year, listing every year in which anything began at the ",
                       "institute (a lab founded, a researcher started, a study run), each year ",
                       "once, earliest first. This one is graded on order."),
       ref = "SELECT founded_year AS year FROM labs UNION SELECT start_year FROM researchers UNION SELECT year FROM studies ORDER BY year;",
       ordered = TRUE,
       solution = "SELECT founded_year AS year FROM labs\nUNION\nSELECT start_year FROM researchers\nUNION\nSELECT year FROM studies\nORDER BY year;")
)

UNION_STEP_TITLES <- c(
  "Stack two results: UNION ALL",
  "Stack and de-duplicate: UNION",
  "Rows in both: INTERSECT",
  "Rows in one but not the other: EXCEPT",
  "The rules, and where sets run"
)

source_union_server <- function(input, output, session) {

  walk <- reactiveValues(step = 1)
  output$union_step <- reactive(walk$step)
  outputOptions(output, "union_step", suspendWhenHidden = FALSE)

  observeEvent(input$union_continue1, walk$step <- max(walk$step, 2))
  observeEvent(input$union_continue2, walk$step <- max(walk$step, 3))
  observeEvent(input$union_continue3, walk$step <- max(walk$step, 4))
  observeEvent(input$union_continue4, walk$step <- max(walk$step, 5))

  output$union_toc <- renderUI(
    walkthrough_toc(walk$step, UNION_STEP_TITLES, "union-step"))

  wire_demo_editors("used", 5, input, output, session)
  wire_quest_editors(UNION_QUEST, "uniq", input, output, session)

  output$union_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
      div(class = "row",
        div(class = "col-lg-3 sidebar-col", uiOutput("union_toc")),
        div(class = "col-lg-9",

          scenario_card(
            "UNION, INTERSECT, EXCEPT — combining whole results",
            eyebrow = "Set operators",
            p(style = "margin-bottom:6px;",
              "A ", kbd("JOIN"), " puts tables side by side: the result gets wider. What ",
              "if you want one result stacked on top of another, so that it gets longer? ",
              "Or only the rows two results have in common, or the rows one has and the ",
              "other lacks? These are the ", tags$b("set operators"), ". Each sits between ",
              "two complete ", kbd("SELECT"), " statements and combines their outputs, ",
              "row against row."),
            p(style = "margin-bottom:0;",
              "For an R user: ", kbd("UNION ALL"), " is ", kbd("rbind()"), " (or ",
              kbd("dplyr::bind_rows()"), "), and ", kbd("UNION"), ", ", kbd("INTERSECT"),
              ", and ", kbd("EXCEPT"), " are ", kbd("union()"), ", ", kbd("intersect()"),
              ", and ", kbd("setdiff()"), " applied to data frames. The names even ",
              "line up, which in SQL is never to be taken for granted.")
          ),

          # ===== STEP 1 =====
          step_container(1, "Stack two results: UNION ALL",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("A UNION ALL B"), " returns every row of ", tags$code("A"),
                " followed by every row of ", tags$code("B"), ", duplicates retained. ",
                "Both queries must return the same number of columns; columns are ",
                "matched by position, and the result takes its column names from the ",
                "first query."),
              plain = tagList(tags$b("In human words. "),
                "Write one query, write ", kbd("UNION ALL"), ", write another. The second ",
                "result is appended under the first. The two queries can read entirely ",
                "different tables, so long as the columns line up: first with first, ",
                "second with second."),
              lab = tagList(tags$b("Lab database. "),
                "Everyone the institute has on file: 12 researchers' names stacked on 24 ",
                "participants' nicknames makes 36 rows. A literal such as ",
                kbd("'researcher'"), " in each half records which table a row came ",
                "from, which you will otherwise have no way of telling afterwards.")
            ),
            sql_code("SELECT full_name AS name, 'researcher' AS role FROM researchers\nUNION ALL\nSELECT nickname, 'participant' FROM participants;"),
            sql_editor("used1", starter = "SELECT full_name AS name, 'researcher' AS role FROM researchers\nUNION ALL\nSELECT nickname, 'participant' FROM participants;"),
            callout_warm(tags$b("Columns are matched by position and by nothing else. "),
              "SQL does not check that the first column of each half means the same ",
              "thing. ", kbd("SELECT full_name, title … UNION ALL SELECT species, nickname …"),
              " runs without complaint and files three seagulls under “name.” The second ",
              "query's own column names and aliases are ignored entirely, so an alias ",
              "there documents your intent and does nothing else."),
            continue_button("union_continue1"),
            id_prefix = "union-step"
          ),

          # ===== STEP 2 =====
          conditionalPanel("output.union_step >= 2",
          step_container(2, "Stack and de-duplicate: UNION",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("A UNION B"), " returns the distinct rows appearing in ",
                tags$code("A"), ", in ", tags$code("B"), ", or in both. It is ",
                tags$code("UNION ALL"), " followed by duplicate elimination over the ",
                "whole row, including duplicates that were already present within ",
                tags$code("A"), " alone."),
              plain = tagList(tags$b("In human words. "),
                "Same stacking, then every repeated row is collapsed to one, the way ",
                kbd("DISTINCT"), " would. Note which word is the short one: plain ",
                kbd("UNION"), " does the extra work, and you have to ask for ",
                kbd("ALL"), " to be spared it."),
              lab = tagList(tags$b("Lab database. "),
                "Study 1 stared at seagulls; study 7 is its replication. Study 1 enrolled ",
                "10 participants and study 7 enrolled 8, so ", kbd("UNION ALL"),
                " of their ids has 18 rows. ", kbd("UNION"), " has 15, because three ",
                "participants signed up for both.")
            ),
            sql_code("SELECT participant_id FROM enrollments WHERE study_id = 1\nUNION\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
            p(style = "margin:8px 0 4px 0;", "Run it, then add ", kbd("ALL"),
              " and watch the row count go from 15 to 18:"),
            sql_editor("used2", starter = "SELECT participant_id FROM enrollments WHERE study_id = 1\nUNION\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
            callout_warm(tags$b("Default to UNION ALL. "),
              "De-duplicating means comparing every row with every other, which costs ",
              "time on a large result, and it can silently delete rows you wanted: stack ",
              "two months of identical-looking transactions with ", kbd("UNION"),
              " and some of them vanish. Use ", kbd("UNION ALL"), " unless removing ",
              "duplicates is the point of the query, as it is above."),
            continue_button("union_continue2"),
            id_prefix = "union-step"
          )),

          # ===== STEP 3 =====
          conditionalPanel("output.union_step >= 3",
          step_container(3, "Rows in both: INTERSECT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("A INTERSECT B"), " returns the distinct rows that appear in ",
                "both ", tags$code("A"), " and ", tags$code("B"),
                ". Rows are compared in their entirety: two rows match only if every ",
                "column matches."),
              plain = tagList(tags$b("In human words. "),
                "The overlap of two results. It answers “who is on both lists?” without a ",
                "join, provided both queries produce the same columns to compare."),
              lab = tagList(tags$b("Lab database. "),
                "Three participants are in both the seagull study and its replication: ",
                "Steven Seagull, The Admiral, and one Dave (participant 4), who, the ",
                "enrollments table records, dropped out of the original and came back ",
                "for the replication anyway.")
            ),
            sql_code("SELECT p.participant_id, p.nickname\nFROM enrollments e JOIN participants p ON e.participant_id = p.participant_id\nWHERE e.study_id = 1\nINTERSECT\nSELECT p.participant_id, p.nickname\nFROM enrollments e JOIN participants p ON e.participant_id = p.participant_id\nWHERE e.study_id = 7;"),
            sql_editor("used3", starter = "SELECT p.participant_id, p.nickname\nFROM enrollments e JOIN participants p ON e.participant_id = p.participant_id\nWHERE e.study_id = 1\nINTERSECT\nSELECT p.participant_id, p.nickname\nFROM enrollments e JOIN participants p ON e.participant_id = p.participant_id\nWHERE e.study_id = 7;", height = "190px"),
            callout_warm(tags$b("Set operators compare values and know nothing of identity. "),
              "Delete ", kbd("p.participant_id,"), " from both halves, so that only the ",
              "nickname is compared, and the answer is still three rows. It is also now ",
              "unreliable: any Dave in study 1 matches any Dave in study 7, whether or ",
              "not they are the same person. At an institute with eight Daves, a ",
              "nickname is not an identifier. Carry the key through any set operation ",
              "whose rows are supposed to mean particular individuals."),
            continue_button("union_continue3"),
            id_prefix = "union-step"
          )),

          # ===== STEP 4 =====
          conditionalPanel("output.union_step >= 4",
          step_container(4, "Rows in one but not the other: EXCEPT",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                tags$code("A EXCEPT B"), " returns the distinct rows of ", tags$code("A"),
                " that do not appear in ", tags$code("B"), ". Unlike ",
                tags$code("UNION"), " and ", tags$code("INTERSECT"),
                ", it is not symmetric: ", tags$code("B EXCEPT A"),
                " is a different result."),
              plain = tagList(tags$b("In human words. "),
                "Subtraction. Start with the first result and strike out anything that ",
                "also appears in the second. Which query goes on top therefore matters, ",
                "in the way that 10 − 3 and 3 − 10 are different sums."),
              lab = tagList(tags$b("Lab database. "),
                "Seven participants were in the original seagull study and skipped the ",
                "replication; five joined the replication fresh. Seven, five, and the ",
                "three who did both add up to the fifteen from step 2, which is the kind ",
                "of check worth running whenever you split a list this way.")
            ),
            sql_code("SELECT participant_id FROM enrollments WHERE study_id = 1\nEXCEPT\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
            p(style = "margin:8px 0 4px 0;", "Run it (seven rows), then swap the 1 and the 7 (five):"),
            sql_editor("used4", starter = "SELECT participant_id FROM enrollments WHERE study_id = 1\nEXCEPT\nSELECT participant_id FROM enrollments WHERE study_id = 7;"),
            help_details("EXCEPT, the anti-join, and NOT EXISTS",
              tagList("This is the third way this tutorial has asked “what is missing?” The ",
                "JOINs tab did it with ", kbd("LEFT JOIN … WHERE … IS NULL"),
                ", and the next tab does it with ", kbd("NOT EXISTS"), ". ", kbd("EXCEPT"),
                " is the most readable of the three when you only need the key back. The ",
                "other two are better when you want further columns from the surviving ",
                "rows, since ", kbd("EXCEPT"), " can return only what both halves selected.")),
            help_details("The Dave problem, again",
              tagList("Run the ", kbd("EXCEPT"), " on nicknames alone and it returns four ",
                "names (Cor, Greer, Jonathan, Tam) where there should be seven people. The ",
                "three Daves who were only in study 1 have been struck out by a different ",
                "Dave in study 7. The query did what it was told. Oracle, incidentally, ",
                "spells this operator ", kbd("MINUS"), ".")),
            continue_button("union_continue4"),
            id_prefix = "union-step"
          )),

          # ===== STEP 5 =====
          conditionalPanel("output.union_step >= 5",
          step_container(5, "The rules, and where sets run",
            explanation_triad(
              formal = tagList(tags$b("Academic. "),
                "In a compound query, each operand is a complete ", tags$code("SELECT"),
                " with its own ", tags$code("WHERE"), ", ", tags$code("GROUP BY"),
                ", and ", tags$code("HAVING"), ". A single ", tags$code("ORDER BY"),
                " and ", tags$code("LIMIT"), " may follow the last operand, and they ",
                "apply to the combined result. For the purpose of detecting duplicates, ",
                "NULLs are considered equal to one another."),
              plain = tagList(tags$b("In human words. "),
                "Each half is finished before the two are combined, and the sorting ",
                "happens once, at the very end, on the combined rows. ", kbd("ORDER BY"),
                " therefore goes after the last query only, and it uses the first ",
                "query's column names, those being the only names the result has."),
              lab = tagList(tags$b("Lab database. "),
                "Every year in which anything began at the institute: lab foundings, ",
                "researcher start dates, and studies, three tables unioned into one ",
                "column of 21 distinct years, from 1979 (Bioacoustics & Thermometry) to ",
                "2023 (the gull replication), sorted once at the end.")
            ),
            sql_code("SELECT founded_year AS year FROM labs\nUNION\nSELECT start_year FROM researchers\nUNION\nSELECT year FROM studies\nORDER BY year;"),
            sql_editor("used5", starter = "SELECT founded_year AS year FROM labs\nUNION\nSELECT start_year FROM researchers\nUNION\nSELECT year FROM studies\nORDER BY year;", height = "150px"),
            execution_order_ribbon(
              active = c("SET", "ORDER BY", "LIMIT"),
              note = "Everything to the left of the lit chips happens separately inside each SELECT. The set operator then combines the finished results, and ORDER BY and LIMIT act on the combination."),
            callout_warm(tags$b("NULL equals NULL here, and only here. "),
              "On the WHERE tab, ", kbd("NULL = NULL"), " was unknown and matched nothing. ",
              "The set operators, like ", kbd("DISTINCT"), " and ", kbd("GROUP BY"),
              ", take the other view and treat two NULLs as duplicates of each other, ",
              "so ", kbd("SELECT lab_id FROM researchers UNION SELECT lab_id FROM studies"),
              " returns Dr. Roams's NULL exactly once. SQL is not consistent about this. ",
              "The rule of thumb: comparisons (", kbd("="), ", ", kbd("IN"), ", ",
              kbd("ON"), ") never match NULLs, and operations that group or ",
              "de-duplicate rows do."),
            help_details("Mixing operators, and a SQLite quirk",
              tagList("You can chain as many as you like, as the example does. When the ",
                "chain mixes operators, the standard says ", kbd("INTERSECT"),
                " is applied before ", kbd("UNION"), " and ", kbd("EXCEPT"),
                ", much as multiplication goes before addition. SQLite ignores that and ",
                "works strictly left to right, and it does not accept parentheses around ",
                "the halves either (", kbd("(SELECT …) UNION (SELECT …)"),
                " is a syntax error here and fine in PostgreSQL). If a mixed chain has to ",
                "mean the same thing in both, compute the inner combination in a CTE ",
                "(next tab) and combine that.")),
            help_details("Availability",
              tagList(kbd("UNION"), " and ", kbd("UNION ALL"), " are everywhere. ",
                kbd("INTERSECT"), " and ", kbd("EXCEPT"), " are in SQLite, PostgreSQL, and ",
                "SQL Server, and reached MySQL only in version 8.0.31 (2022). Oracle has ",
                "always had both, with ", kbd("EXCEPT"), " spelled ", kbd("MINUS"), ".")),
            continue_button("union_done", "I've got set operators ↓"),
            id_prefix = "union-step"
          )),

          # ===== QUEST =====
          conditionalPanel("output.union_step >= 5",
          quest_section(
            "Ten queries to make set operators automatic",
            intro = p(style = "line-height:1.6; margin-bottom:0;",
              "These use ", kbd("UNION ALL"), ", ", kbd("UNION"), ", ", kbd("INTERSECT"),
              ", and ", kbd("EXCEPT"), ". The output column names come from the first ",
              "query in each, so that is where the alias goes. Tables: ", kbd("labs"), ", ",
              kbd("researchers"), ", ", kbd("studies"), ", ", kbd("participants"), ", ",
              kbd("enrollments"), "."),
            quest_editors_ui(UNION_QUEST, "uniq")
          ))
        )
      )
    )
  })
}
