# SQL, step by step ----------------------------------------------------------
# A multi-tab Shiny app that builds SQL fluency from the ground up for an
# audience that knows R/data frames but not databases. Companion to the PSY 302
# t_tests_and_anova app; it reuses that app's visual system (bslib "minty",
# Okabe-Ito palette, step/card/TOC idiom) so the two feel like one product.
#
# Each query tab is a short sequence of gated concept steps (Continue to
# unlock), each adding one clause to a query the learner watches grow, and ends
# with a "Now you try" section of 10 live-editor questions graded by comparing
# the learner's result set to the expected one. All SQL runs in the browser via
# DBI + RSQLite compiled to WebAssembly (webR / Shinylive) — no server.
#
# Running database: data/institute.sqlite, "The Institute for Highly
# Questionable Research" (build/rebuild with data/build_db.R).
#
# Run locally:  shiny::runApp(".")
# ----------------------------------------------------------------------------

library(shiny)
library(bslib)

# Okabe-Ito palette + semantic aliases (carried from the stats app) -----------
OI <- list(
  black = "#000000", orange = "#E69F00", sky = "#56B4E9", green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00", purple = "#CC79A7"
)
PAL <- list(
  step_bg = "#F0F8FB", step_brd = OI$blue,
  ok_bg = "#E1F5EE", ok_fg = "#0F6E56",
  warn_bg = "#FFF1D6", warn_fg = "#7A4F00",
  info_bg = "#E1F1FB", info_fg = "#003D6B",
  pop_dark = OI$blue,
  card_def_bg = "#E1F1FB", card_def_fg = "#003D6B", card_def_acc = OI$blue,
  card_ex_bg  = "#E1F5EE", card_ex_fg  = "#0F6E56", card_ex_acc  = OI$green,
  card_tldr_bg = "#FFF1D6", card_tldr_fg = "#7A4F00", card_tldr_acc = OI$orange
)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Carried-over UI helpers (same style/idiom as the stats app) ------------
step_container <- function(step_num, title, ..., id_prefix = "step") {
  div(
    id = sprintf("%s-%d", id_prefix, step_num),
    class = "mt-4 mb-3 p-4 step-block",
    style = sprintf(
      "background:%s; border-left:6px solid %s; border-radius:10px; scroll-margin-top:80px;",
      PAL$step_bg, PAL$step_brd),
    div(style = sprintf(
          "color:%s; font-weight:600; letter-spacing:0.05em; font-size:12px; text-transform:uppercase;",
          PAL$step_brd),
        sprintf("Step %d", step_num)),
    h3(title, style = "margin:4px 0 14px 0;"),
    ...
  )
}

# Three-form colour-coded explanation block. For SQL the third card is the
# "Lab database" card (the concept on our real tables), parallel to the stats
# app's "Case study" card.
explanation_triad <- function(formal, plain, lab) {
  card <- function(label, body, bg, fg, acc) {
    div(class = "col-md-4",
        div(class = "p-3 h-100",
            style = sprintf("background:%s; color:%s; border-radius:8px; border-left:5px solid %s;",
                            bg, fg, acc),
            div(style = sprintf(
                  "font-size:11px; font-weight:700; letter-spacing:0.06em; color:%s; opacity:0.85; text-transform:uppercase;",
                  fg),
                label),
            div(style = "margin-top:8px; line-height:1.55;", body)
        ))
  }
  div(class = "row g-3 my-3",
      card("Academic",       formal, PAL$card_def_bg,  PAL$card_def_fg,  PAL$card_def_acc),
      card("In human words", plain,  PAL$card_ex_bg,   PAL$card_ex_fg,   PAL$card_ex_acc),
      card("Lab database",   lab,    PAL$card_tldr_bg, PAL$card_tldr_fg, PAL$card_tldr_acc)
  )
}

callout_warm <- function(...) {
  div(class = "p-3 my-3",
      style = sprintf("background:%s; color:%s; border-radius:8px; line-height:1.55;",
                      PAL$warn_bg, PAL$warn_fg), ...)
}

continue_button <- function(id, label = "Continue \u2193") {
  div(class = "mt-3", style = "text-align:right;",
      actionButton(id, label, class = "btn-primary"))
}

help_details <- function(summary_text, body) {
  tags$details(class = "mt-2",
    tags$summary(style = sprintf("cursor:pointer; font-weight:600; color:%s;", PAL$pop_dark),
                 summary_text),
    div(class = "p-2 mt-1",
        style = sprintf("background:#fff; border-left:3px solid %s; border-radius:4px; line-height:1.55;",
                        PAL$pop_dark), body))
}

# A SQL code block (read-only display), monospace on the dark editor ground.
sql_code <- function(code) {
  div(class = "p-3 my-2",
      style = "background:#0f2430; color:#e8f0f3; border-radius:8px; font-family:'JetBrains Mono',monospace; font-size:13.5px; white-space:pre-wrap; line-height:1.55; overflow-x:auto;",
      code)
}

# Inline monospace token for clause/keyword names in prose.
kbd <- function(x) tags$code(style = "background:#eef4f6; color:#0072B2; padding:1px 6px; border-radius:4px; font-family:'JetBrains Mono',monospace; font-size:0.92em;", x)

# Scenario header card (parallel to the stats app's scenario_card)
scenario_card <- function(title, ..., eyebrow = NULL) {
  div(class = "p-3 mb-3",
      style = sprintf("background:%s; color:%s; border-radius:10px; border-top:6px solid %s;",
                      PAL$info_bg, PAL$info_fg, PAL$pop_dark),
      if (!is.null(eyebrow))
        div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.06em; text-transform:uppercase; font-size:12px; margin-bottom:6px;",
                            PAL$pop_dark), eyebrow),
      h2(title, style = "margin-top:0;"), ...)
}

# "Now you try" quest wrapper (amber), holding the 10 live-editor questions.
quest_section <- function(title, intro, ...) {
  div(class = "mt-5 mb-3",
      style = sprintf("background:#f6efe2; border-radius:12px; border-top:6px solid %s; padding:24px 24px 16px 24px;", OI$orange),
      div(style = sprintf("color:%s; font-weight:700; letter-spacing:0.05em; font-size:12px; text-transform:uppercase;", OI$orange),
          "Now you try"),
      h3(title, style = "margin:6px 0 12px 0;"),
      intro,
      p(style = "color:#7A4F00; font-size:13px; font-style:italic; margin-top:8px;",
        "Write each query, press ", tags$b("Run"), " to see your result, then ",
        tags$b("Check my answer"), " to grade it against the target. Stuck? ",
        tags$b("Show a solution"), " reveals one correct query; there are usually ",
        "several, and yours may be one of them even if it looks different."),
      ...
  )
}

# Publish shared helpers into globalenv ---------------------------------------
# Shiny sources app.R into its own frame, but auto-sources R/*.R into globalenv.
# The per-tab modules in R/ therefore cannot see the helpers defined above
# unless we explicitly copy them across. (Functions retain their original
# closures, so PAL/OI that they internally reference still resolve.) Same idiom
# as the PSY 302 app.
local({
  shared_helpers <- c(
    "OI", "PAL", "%||%",
    "step_container", "explanation_triad",
    "callout_warm", "continue_button", "help_details",
    "sql_code", "kbd", "scenario_card", "quest_section"
  )
  for (.h in shared_helpers) {
    if (exists(.h, inherits = TRUE))
      assign(.h, get(.h), envir = globalenv())
  }
})

# Source the shared SQL machinery and the tab files. Shiny 1.5.0+ auto-loads
# files in R/, but we source explicitly so order is deterministic, the app
# works under older Shiny, and new tabs are picked up without editing this list
# (same idiom as the PSY 302 app). sql_helpers.R must load before the tab files
# because the tabs use its helpers, so it goes first.
local({
  helpers <- file.path("R", "sql_helpers.R")
  if (file.exists(helpers)) source(helpers, local = FALSE)
  tabs <- setdiff(list.files("R", pattern = "\\.R$", full.names = TRUE), helpers)
  for (.f in sort(tabs)) source(.f, local = FALSE)
})

# ============================================================================
# UI
# ============================================================================
ui <- page_navbar(
  title = "SQL, step by step",
  id = "main_nav",
  theme = bs_theme(
    version = 5, bootswatch = "minty",
    base_font    = font_google("Inter", local = FALSE),
    heading_font = font_google("Source Serif Pro", local = FALSE),
    code_font    = font_google("JetBrains Mono", local = FALSE)
  ),
  fillable = FALSE,
  header = tags$head(
    tags$style(HTML("
      html { scroll-behavior: smooth; }
      h2, h3, h4, h5 { font-weight:600; color:#1a3a4a; }
      .step-block { animation: stepFadeIn 0.45s ease-out; }
      @keyframes stepFadeIn { from {opacity:0; transform:translateY(10px);} to {opacity:1; transform:translateY(0);} }
      .sidebar-col { position: sticky; top: 80px; align-self:flex-start; max-height: calc(100vh - 100px); overflow-y:auto; }
      @media (max-width: 991.98px) { .sidebar-col { position:static; max-height:none; overflow-y:visible; } }
      .sidebar-toc { background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:12px 8px; box-shadow:0 1px 3px rgba(0,0,0,0.04); }
      .sidebar-toc-title { font-size:11px; font-weight:700; letter-spacing:0.08em; text-transform:uppercase; color:#6b7c85; padding:4px 10px 8px 10px; }
      .toc-progress-wrap { height:6px; background:#ecf2f4; border-radius:3px; margin:0 10px 12px 10px; overflow:hidden; }
      .toc-progress-bar { height:100%; background:linear-gradient(90deg,#78c2ad,#56B4E9); transition:width 0.35s ease-out; }
      .toc-progress-label { font-size:11px; color:#6b7c85; padding:0 10px 6px 10px; }
      .toc-item { display:flex; align-items:center; gap:10px; padding:8px 10px; border-radius:6px; margin:2px 0; text-decoration:none; color:#1a3a4a; font-size:13.5px; line-height:1.3; transition:background 0.15s ease, transform 0.15s ease; }
      .toc-item:hover { background:#f3f8fa; transform:translateX(2px); text-decoration:none; }
      .toc-item.completed { color:#0f6e56; }
      .toc-item.current { background:#e1f1fb; font-weight:700; color:#003d6b; border-left:3px solid #0072B2; padding-left:7px; }
      .toc-item.locked { color:#b0bcc2; pointer-events:none; cursor:default; }
      .toc-num { display:inline-flex; align-items:center; justify-content:center; width:22px; height:22px; border-radius:50%; background:#ecf2f4; color:#6b7c85; font-size:11px; font-weight:700; flex-shrink:0; }
      .toc-item.completed .toc-num { background:#78c2ad; color:#fff; }
      .toc-item.current .toc-num { background:#0072B2; color:#fff; }
      .toc-item.locked .toc-num { background:#f5f7f8; color:#c5cfd3; }
      .toc-check { margin-left:auto; color:#78c2ad; font-weight:700; font-size:13px; }
      .sql-textarea:focus { outline:2px solid #56B4E9; outline-offset:1px; }
      .tab-grid-card { display:block; background:#fff; border:1px solid #e0e6ea; border-radius:10px; padding:16px; text-decoration:none; color:#1a3a4a; transition:transform 0.15s ease, box-shadow 0.15s ease; height:100%; }
      .tab-grid-card:hover { transform:translateY(-2px); box-shadow:0 4px 14px rgba(0,0,0,0.08); text-decoration:none; color:#1a3a4a; }
    "))
  ),

  nav_panel(title = "Start here",      uiOutput("start_tab_ui")),
  nav_panel(title = "SELECT",          uiOutput("select_tab_ui")),
  nav_panel(title = "WHERE",           uiOutput("where_tab_ui")),
  nav_panel(title = "ORDER BY",        uiOutput("order_tab_ui")),
  nav_panel(title = "GROUP BY",        uiOutput("group_tab_ui")),
  nav_panel(title = "JOINs",           uiOutput("joins_tab_ui")),
  nav_panel(title = "Subqueries",      uiOutput("subqueries_tab_ui")),
  nav_panel(title = "Windows",         uiOutput("windows_tab_ui")),
  nav_panel(title = "Changing data",   uiOutput("changing_tab_ui")),
  nav_panel(title = "Put it together", uiOutput("capstone_tab_ui"))
)

# ============================================================================
# Server
# ============================================================================
server <- function(input, output, session) {
  if (exists("source_start_server"))      source_start_server(input, output, session)
  if (exists("source_select_server"))     source_select_server(input, output, session)
  if (exists("source_where_server"))      source_where_server(input, output, session)
  if (exists("source_order_server"))      source_order_server(input, output, session)
  if (exists("source_group_server"))      source_group_server(input, output, session)
  if (exists("source_joins_server"))      source_joins_server(input, output, session)
  if (exists("source_subqueries_server")) source_subqueries_server(input, output, session)
  if (exists("source_windows_server"))    source_windows_server(input, output, session)
  if (exists("source_changing_server"))   source_changing_server(input, output, session)
  if (exists("source_capstone_server"))   source_capstone_server(input, output, session)
}

shinyApp(ui, server)
