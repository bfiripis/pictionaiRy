# ui.R  –  Pictionary AI Generator
#
# note: All UI module functions (sidebar_word_gen_ui, dashboard_ui, etc.) are defined
# in global.R so they are visible to both ui.R and server.R.
# This file is ONLY for page_fluid composition

source("global.R")


# UI composition ----------------------------------------------------------

ui <- page_fluid(
  
  theme = app_theme(),
  title = "pictionaiRy",
  
  app_html_head(),
  app_header(),
  
  layout_sidebar(
    sidebar = sidebar_panel_ui(),
    root_ui()
  )
)