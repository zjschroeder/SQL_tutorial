# =============================================================================
# district_helpers.R — shared pieces for the school-district material
#
# The app teaches on two databases. The institute (data/institute.sqlite) is
# small and absurd, which makes each new clause easy to see. The district
# (data/district.sqlite) is Prairie Lakes Public Schools, a FICTIONAL district
# whose tables follow the shape of a real student information system: the
# material where the clauses meet the problems a K-12 data analyst actually
# has. It appears in two places:
#
#   1. An "In a school district" section at the end of each query tab, after
#      the institute quest, applying that tab's clause to district data.
#   2. Seven district-only tabs under the "School district" menu, for topics
#      that only make sense with SIS data (snapshots, attendance metrics,
#      cohorts, reconciliation, data quality).
#
# Conventions the tests rely on (tests/verify_content.R):
#   - SQL shown for the district goes in dsql_code(), not sql_code().
#   - Every editor that runs against the district has an id starting "dz".
#   - District quests are lists whose names end in _DQUEST; district writing
#     tasks end in _DTASKS.
#
# Everything is on the as-of date 2026-03-31 ("today" in the data). Queries
# never use date('now'), which would make every answer drift with the calendar.
# =============================================================================

DISTRICT_AS_OF <- "2026-03-31"
DISTRICT_NAME  <- "Prairie Lakes Public Schools"

# ---- Visual pieces ----------------------------------------------------------
D_PAL <- list(bg = "#EAF6F1", fg = "#0B5A45", acc = "#009E73",
              ts_bg = "#F7EEF3", ts_fg = "#6B2C50", ts_acc = "#CC79A7")

# SQL shown for the district database. Looks like sql_code() with a green rule
# and a small label, so the reader knows which database it runs on.
dsql_code <- function(code) {
  div(class = "my-2",
      div(style = sprintf("font-size:10.5px; font-weight:700; letter-spacing:0.07em; text-transform:uppercase; color:%s; margin:0 0 -4px 2px;", D_PAL$acc),
          "District database"),
      div(class = "p-3 my-2",
          style = sprintf("background:#0f2430; color:#e8f0f3; border-radius:8px; border-left:5px solid %s; font-family:'JetBrains Mono',monospace; font-size:13.5px; white-space:pre-wrap; line-height:1.55; overflow-x:auto;", D_PAL$acc),
          code))
}

# The same idea written for Microsoft SQL Server (T-SQL), which is what most
# district SIS reporting databases run. Display only: it does not run here.
# `preview` names syntax the note uses before its tab teaches it, e.g.
# preview = c(CASE = "Functions"); it renders as a small "Preview" badge so
# the learner knows the unfamiliar part is coming, not something they missed.
tsql_note <- function(code, ..., preview = NULL) {
  div(class = "p-3 my-3",
      style = sprintf("background:%s; color:%s; border-radius:8px; border-left:5px solid %s; line-height:1.55;",
                      D_PAL$ts_bg, D_PAL$ts_fg, D_PAL$ts_acc),
      div(style = sprintf("font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:%s; margin-bottom:6px;", D_PAL$ts_fg),
          "The same thing in T-SQL (SQL Server)"),
      if (length(preview))
        div(class = "mb-2", style = "font-size:12.5px;",
            span(style = sprintf("display:inline-block; background:#fff; color:%s; border:1px solid %s; border-radius:10px; padding:1px 8px; font-weight:700; font-size:11px; margin-right:6px;",
                                 D_PAL$ts_fg, D_PAL$ts_acc), "Preview"),
            "Uses ", tagList(lapply(seq_along(preview), function(i) tagList(
              if (i > 1) ", ", kbd(names(preview)[i]), " (taught in ", tags$b(preview[[i]]), ")"))),
            ". Read it for the idea; the syntax comes later."),
      div(class = "p-2 mb-2",
          style = "background:#2a1a24; color:#f3e6ee; border-radius:6px; font-family:'JetBrains Mono',monospace; font-size:13px; white-space:pre-wrap; line-height:1.5; overflow-x:auto;",
          code),
      div(style = "font-size:13.5px;", ...))
}

# "How this comes up in an interview" box.
interview_note <- function(...) {
  div(class = "p-3 my-3",
      style = "background:#fff; border:1px dashed #009E73; border-radius:8px; line-height:1.55;",
      div(style = sprintf("font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:%s; margin-bottom:6px;", D_PAL$acc),
          "How this comes up in an interview"),
      ...)
}

# The "In a school district" section that closes each query tab. `title` names
# the K-12 problem; `...` holds the explanation, demos, T-SQL note, and quest.
district_section <- function(title, ..., id = NULL) {
  div(id = id, class = "mt-5 mb-3",
      style = sprintf("background:%s; border-radius:12px; border-top:6px solid %s; padding:24px 24px 16px 24px; scroll-margin-top:80px;",
                      D_PAL$bg, D_PAL$acc),
      div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.05em; font-size:12px; text-transform:uppercase;", D_PAL$acc),
          "In a school district"),
      h3(title, style = "margin:6px 0 12px 0;"),
      ...)
}

# A graded set of district queries (the district counterpart of quest_section).
district_quest <- function(quest, prefix, title = "District quest", intro = NULL) {
  if (!startsWith(prefix, "dz")) stop("district editor prefixes must start with 'dz'")
  div(class = "mt-4",
      style = "background:#fff; border-radius:10px; border:1px solid #cfe9df; padding:18px 18px 8px 18px;",
      div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.05em; font-size:11px; text-transform:uppercase;", D_PAL$acc),
          "Now you try, on the district"),
      h4(title, style = "margin:4px 0 8px 0;"),
      if (!is.null(intro)) div(style = "line-height:1.6; margin-bottom:8px;", intro),
      p(style = sprintf("color:%s; font-size:13px; font-style:italic; margin-top:4px;", D_PAL$fg),
        "These editors run on the district database. ", tags$b("Check my answer"),
        " compares your result with the target, as on the institute questions. ",
        "“Today” in the data is ", DISTRICT_AS_OF, ", so write that date ",
        "where a question says “today” or “as of now.”"),
      quest_editors_ui(quest, prefix))
}

# Ungraded district editor (Run only). Ids must start with "dz".
dsql_editor <- function(id, starter, height = "120px") {
  if (!startsWith(id, "dz")) stop("district editor ids must start with 'dz'")
  sql_editor(id, starter = starter, height = height)
}

# ---- Schema -----------------------------------------------------------------
DISTRICT_SCHEMA <- list(
  "Who and where" = list(
    schools = list(
      blurb = "Nine schools: four elementary (K-5), two middle (6-8), two high (9-12), and Horizon, an area learning center (an alternative high school).",
      cols = c("school_id (PK)", "school_name", "school_type", "grade_low", "grade_high",
               "state_school_number", "opened_year"),
      rel = "One school has many enrollments, calendar days, incidents, sections."),
    students = list(
      blurb = "One row per person. student_id is the district's number; state_id is the state's, stored as text so its leading zeros survive. A few state_ids are NULL.",
      cols = c("student_id (PK)", "state_id", "last_name", "first_name", "birth_date", "gender",
               "hispanic_latino", "race_american_indian", "race_asian", "race_black",
               "race_pacific_islander", "race_white", "home_language", "entered_9th_year"),
      rel = "One student has many enrollments, attendance rows, program spells, transcript rows."),
    enrollments = list(
      blurb = "One row per enrollment spell: a student, at a school, in a grade, from a start date to an end date. end_date NULL means still enrolled. service_type P is the primary enrollment; S is a secondary (part-time) one.",
      cols = c("enrollment_id (PK)", "student_id (FK→students)", "school_id (FK→schools)",
               "school_year", "grade", "start_date", "end_date", "start_status", "end_status", "service_type"),
      rel = "Many per student per year: transfers, grade changes, and secondary enrollments each add a row."),
    calendar_days = list(
      blurb = "Every weekday of each school year at each school, with instructional = 1 on school days. Membership and attendance are counted on these days only.",
      cols = c("school_id (PK, FK→schools)", "cal_date (PK)", "school_year", "instructional", "day_note"),
      rel = "Join to enrollments by school and date range to list the days a student was enrolled.")
  ),
  "Attendance and programs" = list(
    attendance = list(
      blurb = "Current year only (2025-26, through 2026-03-31). Exceptions only: a row exists when a student was absent or tardy. No row means present.",
      cols = c("attendance_id (PK)", "student_id (FK)", "school_id (FK)", "att_date", "status", "excuse"),
      rel = "status: A absent all day, H half day, T tardy. excuse: E, U, X (exempt: counts as present), S (suspended)."),
    attendance_year_summary = list(
      blurb = "Prior years (2021-22 to 2024-25), frozen at year end: days of membership and days absent per student per school.",
      cols = c("school_year (PK)", "student_id (PK)", "school_id (PK)", "days_membership", "days_absent"),
      rel = "One row per student per school per year."),
    programs = list(
      blurb = "Program participation spells with start and end dates: EL (English learner), SPED (special education), 504, FRL (free/reduced-price meals), McKV (homeless, McKinney-Vento), GT (gifted).",
      cols = c("program_id (PK)", "student_id (FK)", "program", "start_date", "end_date"),
      rel = "A student's status depends on the date you ask about.")
  ),
  "Behavior" = list(
    incidents = list(
      blurb = "One row per behavior event (2024-25 and 2025-26).",
      cols = c("incident_id (PK)", "school_id (FK)", "incident_date", "incident_type", "location"),
      rel = "One incident involves one or more students."),
    incident_students = list(
      blurb = "Who was involved, and as what: Offender or Victim.",
      cols = c("incident_id (PK, FK)", "student_id (PK, FK)", "role"),
      rel = "Bridge between incidents and students."),
    resolutions = list(
      blurb = "Consequences: one row per action taken for one student for one incident. A student can receive more than one.",
      cols = c("resolution_id (PK)", "incident_id (FK)", "student_id (FK)", "resolution_code (FK)",
               "start_date", "days_removed"),
      rel = "OSS, ISS, EXP, DET, CONF; see resolution_codes.")
  ),
  "Courses and outcomes" = list(
    courses = list(
      blurb = "The course catalog, with fictional state course codes.",
      cols = c("course_id (PK)", "course_name", "subject", "state_course_code", "credits_per_term", "course_level"),
      rel = "One course has many sections and transcript rows."),
    sections = list(
      blurb = "2025-26 high school class sections: a course, a term, a period, a teacher.",
      cols = c("section_id (PK)", "course_id (FK)", "school_id (FK)", "school_year", "term", "period", "teacher_id (FK→staff)"),
      rel = "One section has many roster rows."),
    roster = list(
      blurb = "Which student is in which section, from when to when.",
      cols = c("roster_id (PK)", "section_id (FK)", "student_id (FK)", "start_date", "end_date"),
      rel = "Bridge between sections and students."),
    staff = list(
      blurb = "Principals, counselors, and high school teachers.",
      cols = c("staff_id (PK)", "last_name", "first_name", "school_id (FK)", "role", "subject_area"),
      rel = "Teachers are linked to sections."),
    transcripts = list(
      blurb = "Final marks and credits by term. school_id is NULL for credit earned elsewhere (transfer credit, and credit converted from the district's previous SIS).",
      cols = c("transcript_id (PK)", "student_id (FK)", "school_year", "term", "school_id", "course_id (FK)",
               "grade_level", "mark", "credits_attempted", "credits_earned"),
      rel = "Sum credits_earned per student to track progress to graduation (21.5 credits)."),
    test_results = list(
      blurb = "Spring state tests, 2023 to 2025. Students who did not test have a row with a test_status and NULL score.",
      cols = c("test_id (PK)", "student_id (FK)", "school_year", "test_date", "subject", "grade_tested",
               "school_id", "test_status", "scale_score", "achievement_level"),
      rel = "Achievement levels: Does Not Meet, Partially Meets, Meets, Exceeds.")
  ),
  "Reporting and access" = list(
    oct1_snapshot = list(
      blurb = "Frozen copy of who was enrolled on October 1 each year, as it was submitted. The live enrollments table has been corrected since; this has not.",
      cols = c("school_year (PK)", "snapshot_date", "student_id (PK)", "school_id", "grade", "el", "sped", "frl", "homeless"),
      rel = "One row per student per year."),
    state_oct1_return = list(
      blurb = "The state's file of the October 1, 2025 records it accepted. Keyed by state_id and state_school_number, not the district's numbers.",
      cols = c("school_year", "snapshot_date", "state_id", "state_school_number", "grade", "accepted"),
      rel = "Reconcile against the district's own count."),
    user_access = list(
      blurb = "Who may see which school's data. scope 'District' sees everything.",
      cols = c("username (PK)", "scope", "school_id", "role"),
      rel = "Join to a report to apply row-level security.")
  ),
  "Lookup tables" = list(
    grade_levels = list(
      blurb = "The valid grade codes, with a number to sort by.",
      cols = c("grade (PK)", "grade_order", "grade_band"),
      rel = "Join on grade; enrollments has no foreign key to it, on purpose."),
    status_codes = list(
      blurb = "Enrollment start and end codes. documented_transfer = 1 marks the exits that remove a student from a graduation cohort.",
      cols = c("code (PK)", "code_type (PK)", "description", "leaves_district", "documented_transfer"),
      rel = "Composite key: INTR is both a start code and an end code."),
    attendance_codes = list(
      blurb = "What each status and excuse letter means.",
      cols = c("code_type (PK)", "code (PK)", "description"),
      rel = "Composite key, like status_codes."),
    resolution_codes = list(
      blurb = "Behavior consequences and whether they remove the student from class.",
      cols = c("code (PK)", "description", "removal_type"),
      rel = "Referenced by resolutions.")
  )
)

district_schema_explorer <- function(groups = names(DISTRICT_SCHEMA)) {
  tagList(lapply(groups, function(g) tagList(
    h5(g, style = sprintf("margin:14px 0 8px 0; color:%s;", D_PAL$fg)),
    schema_explorer(DISTRICT_SCHEMA[[g]]))))
}

# Plain-language map of the tables, for the District data tab.
district_relationship_map <- function() {
  div(class = "p-3 my-3",
      style = "background:#f6fafb; border:1px solid #e0e6ea; border-radius:10px;",
      div(style = "font-size:11px; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; color:#6b7c85; margin-bottom:8px;",
          "How the district's tables connect"),
      div(style = "font-family:'JetBrains Mono',monospace; font-size:13px; color:#243b44; line-height:1.9;",
          HTML(paste(
            "students &nbsp;1───∞&nbsp; enrollments &nbsp;∞───1&nbsp; schools",
            "schools &nbsp;1───∞&nbsp; calendar_days &nbsp;&nbsp;(joined to enrollments by date range)",
            "students &nbsp;1───∞&nbsp; attendance, programs, transcripts, test_results",
            "incidents &nbsp;∞───∞&nbsp; students &nbsp;&nbsp;(via incident_students; consequences in resolutions)",
            "sections &nbsp;∞───∞&nbsp; students &nbsp;&nbsp;(via roster); &nbsp;sections ∞───1 courses, staff",
            "state_oct1_return ↔ students &nbsp;&nbsp;(on state_id, not student_id)",
            sep = "<br>"))))
}

# ---- District-only tabs: one builder for all of them --------------------------
# Every district tab has the same shape as the query tabs: a header card, gated
# steps with a sticky table of contents, then a graded quest. This builds the
# server function for one. `key` must be a valid JavaScript identifier (it
# names the step output used by conditionalPanel). Demo editors inside the steps
# must be named dsql_editor("dz<key>1"), dsql_editor("dz<key>2"), ... in order,
# `n_demo` of them; the quest's editors are "dz<key>q1", "dz<key>q2", ...
district_tab_server <- function(key, step_titles, step_bodies, header, quest,
                                quest_title, quest_intro = NULL, n_demo = 0,
                                after_quest = NULL) {
  # `step_bodies` and `header` are functions returning UI, so nothing is built
  # when the file is sourced: Shiny auto-loads R/ before app.R defines the
  # palette and helpers the UI uses.
  function(input, output, session) {
    walk <- reactiveValues(step = 1)
    sid <- paste0(key, "_step")
    output[[sid]] <- reactive(walk$step)
    outputOptions(output, sid, suspendWhenHidden = FALSE)
    for (i in seq_along(step_titles)[-1]) local({
      j <- i
      observeEvent(input[[sprintf("%s_continue%d", key, j - 1)]], walk$step <- max(walk$step, j))
    })
    output[[paste0(key, "_toc")]] <- renderUI(
      walkthrough_toc(walk$step, step_titles, paste0(key, "-step")))
    if (n_demo > 0) wire_demo_editors(paste0("dz", key), n_demo, input, output, session,
                                      con_fun = district_con_ro)
    wire_quest_editors(quest, paste0("dz", key, "q"), input, output, session,
                       con_fun = district_con_ro)
    nS <- length(step_titles)
    output[[paste0(key, "_tab_ui")]] <- renderUI({
      bodies <- step_bodies()
      stopifnot(length(bodies) == nS)
      steps_ui <- lapply(seq_len(nS), function(i) {
        btn <- if (i < nS) continue_button(sprintf("%s_continue%d", key, i))
               else continue_button(paste0(key, "_done"), "On to the quest ↓")
        blk <- step_container(i, step_titles[i], bodies[[i]], btn,
                              id_prefix = paste0(key, "-step"))
        if (i == 1) blk else conditionalPanel(sprintf("output.%s >= %d", sid, i), blk)
      })
      div(class = "container-fluid", style = "max-width: 1380px; padding-top: 16px;",
        div(class = "row",
          div(class = "col-lg-3 sidebar-col", uiOutput(paste0(key, "_toc"))),
          div(class = "col-lg-9",
            header(),
            steps_ui,
            conditionalPanel(sprintf("output.%s >= %d", sid, nS),
              district_quest(quest, paste0("dz", key, "q"), quest_title, quest_intro),
              after_quest)
          )))
    })
  }
}

# Header card for a district tab (the scenario_card in district colors).
district_header <- function(title, eyebrow, ...) {
  div(class = "p-3 mb-3",
      style = sprintf("background:%s; color:%s; border-radius:10px; border-top:6px solid %s;",
                      D_PAL$bg, D_PAL$fg, D_PAL$acc),
      div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; font-size:12px; margin-bottom:6px;", D_PAL$acc),
          eyebrow),
      h2(title, style = "margin-top:0;"), ...)
}

# The explanation triad with its third card about the district rather than the
# lab: Academic / In human words / In the district.
district_triad <- function(formal, plain, district) {
  card <- function(label, body, bg, fg, acc) {
    div(class = "col-md-4",
        div(class = "p-3 h-100",
            style = sprintf("background:%s; color:%s; border-radius:8px; border-left:5px solid %s;", bg, fg, acc),
            div(style = sprintf("font-size:11px; font-weight:700; letter-spacing:0.06em; color:%s; opacity:0.85; text-transform:uppercase;", fg),
                label),
            div(style = "margin-top:8px; line-height:1.55;", body)))
  }
  div(class = "row g-3 my-3",
      card("Academic",        formal,   PAL$card_def_bg, PAL$card_def_fg, PAL$card_def_acc),
      card("In human words",  plain,    PAL$card_ex_bg,  PAL$card_ex_fg,  PAL$card_ex_acc),
      card("In the district", district, D_PAL$bg,        D_PAL$fg,        D_PAL$acc))
}
