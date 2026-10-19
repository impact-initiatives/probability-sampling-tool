page_navbar(
  title = "Probability sampling tool",
  id = "main_nav",
  lang = "en",
  theme = app_theme(),
  navbar_options = navbar_options(bg = "#12355B", theme = "dark"),
  fillable = FALSE,
  header = app_header(),
  guide_ui("guide"),
  frame_tab(),
  sampling_tab()
)
