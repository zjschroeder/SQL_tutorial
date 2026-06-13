# =============================================================================
# tab_capstone.R — "Put it together" capstone (Tab 10)
#
# No new syntax: 20 mixed challenges drawing on every prior tab (graded by
# result-set comparison against the read-only DB), a free SELECT sandbox with
# the schema on hand, and a short indexing / performance aside with a runnable
# EXPLAIN QUERY PLAN demo.
# =============================================================================

CAPSTONE_QUEST <- list(
  # --- Warm-up: one idea each ---
  list(prompt = "Return the full_name of every researcher who is a PI.",
       ref = "SELECT full_name FROM researchers WHERE title = 'PI';", ordered = FALSE,
       solution = "SELECT full_name FROM researchers WHERE title = 'PI';"),
  list(prompt = "Return the lab_name of every lab with no building on file (building is NULL).",
       ref = "SELECT lab_name FROM labs WHERE building IS NULL;", ordered = FALSE,
       solution = "SELECT lab_name FROM labs WHERE building IS NULL;"),
  list(prompt = "Return the title and year of the three most recent studies, newest first.",
       ref = "SELECT title, year FROM studies ORDER BY year DESC LIMIT 3;", ordered = TRUE,
       solution = "SELECT title, year FROM studies ORDER BY year DESC LIMIT 3;"),
  list(prompt = "For each study status, return the status and the number of studies as n.",
       ref = "SELECT status, COUNT(*) AS n FROM studies GROUP BY status;", ordered = FALSE,
       solution = "SELECT status, COUNT(*) AS n FROM studies GROUP BY status;"),
  list(prompt = "Return each researcher title held by more than one person, with its count as n.",
       ref = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;",
       ordered = FALSE,
       solution = "SELECT title, COUNT(*) AS n FROM researchers GROUP BY title HAVING COUNT(*) > 1;"),
  list(prompt = "Return the distinct participant species.",
       ref = "SELECT DISTINCT species FROM participants;", ordered = FALSE,
       solution = "SELECT DISTINCT species FROM participants;"),
  list(prompt = "Return the nickname of every participant whose nickname starts with D.",
       ref = "SELECT nickname FROM participants WHERE nickname LIKE 'D%';", ordered = FALSE,
       solution = "SELECT nickname FROM participants WHERE nickname LIKE 'D%';"),
  # --- Combining tables ---
  list(prompt = "Return each study's title alongside its lab's lab_name.",
       ref = "SELECT s.title, l.lab_name FROM studies s JOIN labs l ON s.lab_id = l.lab_id;",
       ordered = FALSE,
       solution = "SELECT s.title, l.lab_name\nFROM studies s JOIN labs l ON s.lab_id = l.lab_id;"),
  list(prompt = "Return the title of every study that has no enrollments.",
       ref = "SELECT s.title FROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id WHERE e.enrollment_id IS NULL;",
       ordered = FALSE,
       solution = "SELECT s.title\nFROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id\nWHERE e.enrollment_id IS NULL;"),
  list(prompt = paste0("For every study, return its title and the number of enrollments as ",
                       "n_enrollments (include studies with zero — use a LEFT JOIN)."),
       ref = "SELECT s.title, COUNT(e.enrollment_id) AS n_enrollments FROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id GROUP BY s.study_id, s.title;",
       ordered = FALSE,
       solution = "SELECT s.title, COUNT(e.enrollment_id) AS n_enrollments\nFROM studies s LEFT JOIN enrollments e ON s.study_id = e.study_id\nGROUP BY s.study_id, s.title;"),
  list(prompt = "Return the title and the participant nickname for every enrollment (3-table join).",
       ref = "SELECT s.title, p.nickname FROM enrollments e JOIN studies s ON e.study_id = s.study_id JOIN participants p ON e.participant_id = p.participant_id;",
       ordered = FALSE,
       solution = "SELECT s.title, p.nickname\nFROM enrollments e\nJOIN studies s ON e.study_id = s.study_id\nJOIN participants p ON e.participant_id = p.participant_id;"),
  list(prompt = paste0("Return each lab_name and its number of studies as n_studies, for labs ",
                       "running more than one study."),
       ref = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies FROM labs l JOIN studies s ON l.lab_id = s.lab_id GROUP BY l.lab_id, l.lab_name HAVING COUNT(s.study_id) > 1;",
       ordered = FALSE,
       solution = "SELECT l.lab_name, COUNT(s.study_id) AS n_studies\nFROM labs l JOIN studies s ON l.lab_id = s.lab_id\nGROUP BY l.lab_id, l.lab_name\nHAVING COUNT(s.study_id) > 1;"),
  # --- Subqueries, CTEs, windows ---
  list(prompt = "Return the full_name of every researcher earning above the overall average salary.",
       ref = "SELECT full_name FROM researchers WHERE salary > (SELECT AVG(salary) FROM researchers);",
       ordered = FALSE,
       solution = "SELECT full_name\nFROM researchers\nWHERE salary > (SELECT AVG(salary) FROM researchers);"),
  list(prompt = "Return the full_name of researchers whose lab is in 'Wexler Hall' (use a subquery).",
       ref = "SELECT full_name FROM researchers WHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');",
       ordered = FALSE,
       solution = "SELECT full_name\nFROM researchers\nWHERE lab_id IN (SELECT lab_id FROM labs WHERE building = 'Wexler Hall');"),
  list(prompt = "Return the lab_name of every lab that has at least one study (use EXISTS).",
       ref = "SELECT lab_name FROM labs l WHERE EXISTS (SELECT 1 FROM studies s WHERE s.lab_id = l.lab_id);",
       ordered = FALSE,
       solution = "SELECT lab_name\nFROM labs l\nWHERE EXISTS (SELECT 1 FROM studies s WHERE s.lab_id = l.lab_id);"),
  list(prompt = paste0("Using a CTE (WITH), count researchers per lab, then return each lab_name ",
                       "with its researcher count as n."),
       ref = "WITH rc AS (SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id) SELECT l.lab_name, rc.n FROM labs l JOIN rc ON l.lab_id = rc.lab_id;",
       ordered = FALSE,
       solution = "WITH rc AS (\n  SELECT lab_id, COUNT(*) AS n FROM researchers GROUP BY lab_id\n)\nSELECT l.lab_name, rc.n\nFROM labs l JOIN rc ON l.lab_id = rc.lab_id;"),
  list(prompt = "Return the average study sample_size across all studies, as avg_n.",
       ref = "SELECT AVG(sample_size) AS avg_n FROM studies;", ordered = FALSE,
       solution = "SELECT AVG(sample_size) AS avg_n FROM studies;"),
  list(prompt = paste0("Among researchers with a known salary, return full_name, salary, and ",
                       "their salary rank (highest = 1) as salary_rank."),
       ref = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank FROM researchers WHERE salary IS NOT NULL;",
       ordered = FALSE,
       solution = "SELECT full_name, salary, RANK() OVER (ORDER BY salary DESC) AS salary_rank\nFROM researchers\nWHERE salary IS NOT NULL;"),
  list(prompt = "Return each researcher's full_name and their lab-average salary as lab_avg.",
       ref = "SELECT full_name, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg FROM researchers;",
       ordered = FALSE,
       solution = "SELECT full_name, AVG(salary) OVER (PARTITION BY lab_id) AS lab_avg\nFROM researchers;"),
  list(prompt = paste0("For enrollment_id 1, return trial, value, and the running total of value ",
                       "ordered by trial, as running_total."),
       ref = "SELECT trial, value, SUM(value) OVER (ORDER BY trial) AS running_total FROM measurements WHERE enrollment_id = 1;",
       ordered = FALSE,
       solution = "SELECT trial, value, SUM(value) OVER (ORDER BY trial) AS running_total\nFROM measurements\nWHERE enrollment_id = 1;")
)

source_capstone_server <- function(input, output, session) {

  wire_quest_editors(CAPSTONE_QUEST, "capq", input, output, session)

  # Free sandbox + the EXPLAIN demo (both read-only).
  sql_editor_server("sandbox", input, output, session, con_fun = institute_con_ro)
  sql_editor_server("explain_demo", input, output, session, con_fun = institute_con_ro)

  output$capstone_tab_ui <- renderUI({
    div(class = "container-fluid", style = "max-width: 1100px; padding-top: 16px;",

      scenario_card(
        "Put it together",
        eyebrow = "The capstone",
        p(style = "margin-bottom:6px;",
          "No new syntax here — just ", tags$b("twenty challenges"),
          " that mix everything: filtering, sorting, grouping, joining, ",
          "subqueries, and windows. Then a ", tags$b("free sandbox"),
          " to explore the institute however you like, and a short note on why ",
          "some queries are fast and others slow."),
        p(style = "margin-bottom:0;",
          "Stuck on one? ", tags$b("Show a solution"),
          " reveals a correct query — but try first; this is where it all clicks.")
      ),

      quest_section(
        "Twenty mixed challenges",
        intro = p(style = "line-height:1.6; margin-bottom:0;",
          "Work them in any order. Each is graded against the live database. The ",
          "full schema is in the sandbox below if you need a reminder of column names."),
        quest_editors_ui(CAPSTONE_QUEST, "capq")
      ),

      # ---- Free sandbox ----
      div(class = "mt-5 mb-3",
        h3("Free sandbox", style = "margin-bottom:6px;"),
        p(style = "line-height:1.6; max-width:820px;",
          "Ask the institute anything. This editor runs any read-only query against ",
          "the full database — no grading, no goal, just exploration."),
        sql_editor("sandbox",
                   starter = "SELECT *\nFROM studies\nORDER BY year DESC;",
                   height = "150px"),
        div(class = "mt-4",
          div(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-bottom:10px;",
              "Schema reference"),
          schema_explorer())
      ),

      # ---- Indexing / performance aside ----
      div(class = "mt-5 mb-4 p-4",
        style = "background:#f6fafb; border:1px solid #e0e6ea; border-radius:12px;",
        div(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#0072B2; margin-bottom:8px;",
            "One last idea: why some queries are slow"),
        h3("Indexes, in ninety seconds", style = "margin-top:0; margin-bottom:10px;"),
        p(style = "line-height:1.65; max-width:840px;",
          "To find every study in lab 3 without help, the database reads ",
          tags$b("every row"), " and checks each — a “full table scan.” An ",
          tags$b("index"), " is a pre-sorted lookup structure on a column, exactly ",
          "like the index at the back of a book: instead of skimming all 300 pages ",
          "for “seagull,” you jump to the entry. ", kbd("CREATE INDEX idx_name ON studies(lab_id)"),
          " builds one."),
        p(style = "line-height:1.65; max-width:840px;",
          "Indexes pay off on columns you ", tags$b("filter"), " (", kbd("WHERE"),
          "), ", tags$b("join"), " on, or ", tags$b("sort"), " by — especially ",
          "foreign keys. They aren't free: each one takes space and makes ",
          kbd("INSERT"), "/", kbd("UPDATE"), "/", kbd("DELETE"),
          " a little slower (the index must be maintained), so you index the ",
          "columns that searches actually use, not every column."),
        p(style = "line-height:1.65; max-width:840px; margin-bottom:6px;",
          "This database already ships with indexes on the busy foreign keys (",
          kbd("studies.lab_id"), ", ", kbd("researchers.lab_id"), ", ",
          kbd("enrollments.study_id"), ", ", kbd("measurements.enrollment_id"),
          "). You can ", tags$b("see the planner's choice"), " with ",
          kbd("EXPLAIN QUERY PLAN"), " in front of any query — run the example, ",
          "then try one with a non-indexed column like ", kbd("studies.year"),
          " and compare “USING INDEX” versus “SCAN.”"),
        sql_editor("explain_demo",
                   starter = "EXPLAIN QUERY PLAN\nSELECT * FROM studies WHERE lab_id = 3;",
                   height = "110px"),
        callout_warm(tags$b("Don't optimize what isn't slow. "),
          "On six tiny tables like these, every query is instant and indexes won't ",
          "change much. Indexing matters at scale — millions of rows — where a scan ",
          "is the difference between milliseconds and minutes. Reach for it when a ",
          "real query is slow, guided by ", kbd("EXPLAIN QUERY PLAN"), ".")
      ),

      div(style = "text-align:center; color:#6b7c85; font-size:14px; padding:8px 0 28px 0;",
          "That's the whole language in working order. ",
          tags$b("You can now read and write real SQL."))
    )
  })
}
