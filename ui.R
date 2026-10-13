library(markdown)
library(bslib)

# Kish, L. (1965). Survey Sampling. New York: Wiley.
kish_1965 <- tagList("Kish, ", tags$em("Survey Sampling"), " (1965)")
# Cochran, W. G. (1977). Sampling Techniques, 3rd ed. New York: Wiley.
cochran_1977 <- tagList("Cochran, ", tags$em("Sampling Techniques"), " (1977)")

# numericInput() whose label carries a help icon with a tooltip. The input is
# named by the label text only, not by the icon.
numeric_input_with_tip <- function(inputId, label, tip, source = NULL, ...) {
	name_id <- paste0(inputId, "-name")
	htmltools::tagQuery(
		numericInput(
			inputId,
			tagList(
				tags$span(id = name_id, label),
				tooltip(
					tags$span(
						class = "info-tip",
						tabindex = "0",
						role = "img",
						`aria-label` = paste("About", label),
						icon("circle-info", `aria-hidden` = "true")
					),
					tip,
					if (!is.null(source)) div(class = "info-tip-source", source)
				)
			),
			...
		)
	)$find("input")$addAttrs("aria-labelledby" = name_id)$allTags()
}

page_navbar(
	title = "Probability sampling tool",
	lang = "en",
	theme = bs_theme(
		version = 5,
		# WCAG AA: white text on primary/danger/success is above 4.5:1
		primary = "#1A56A0",
		secondary = "#5C6670",
		success = "#146C43",
		danger = "#B42318",
		"body-bg" = "#F5F7FA",
		# form control borders need 3:1 against the background (WCAG 1.4.11)
		"input-border-color" = "#8A939C",
		"card-cap-bg" = "#EEF3FA",
		"card-cap-color" = "#12355B",
		base_font = font_collection(
			"system-ui",
			"-apple-system",
			"Segoe UI",
			"Roboto",
			"Helvetica Neue",
			"Arial",
			"sans-serif"
		),
		# the Bootstrap 3 base size, to keep the setup card on one screen
		"font-size-base" = "0.875rem",
		# the Bootstrap 3 paragraph spacing, which keeps the Introduction compact
		"paragraph-margin-bottom" = "0.7rem",
		"h1-font-size" = "2rem",
		"h2-font-size" = "1.4rem"
	),
	navbar_options = navbar_options(bg = "#12355B", theme = "dark"),
	fillable = FALSE,
	header = tagList(
		shinyjs::useShinyjs(),
		# Spinner on the outputs being recalculated and a pulse while the app is busy
		useBusyIndicators(),
		# Cache-bust: without a version query string, browsers keep serving a
		# previously-cached app.css after it changes on disk.
		tags$link(
			rel = "stylesheet",
			type = "text/css",
			href = paste0("app.css?v=", as.integer(file.mtime("www/app.css")))
		)
	),
	nav_panel(
		"Introduction",
		fluidRow(
			h1("Brief overview of the tool"),
			class = "text-center"
		),
		fluidRow(
			column(
				5,
				offset = 1,
				h2("Mode of sampling"),
				p(
					strong("Enter sample size:"),
					"You can input the target sample size"
				),
				p(
					strong("Sample size based on population:"),
					"The sample size will be based on confidence level, error margin and proportion"
				),

				h2("Type of sampling"),

				p(
					strong("Random sampling:"),
					"Simple Random sampling, a random sample directly from the sampling frame which consists of every unit in the population of interest, thus ensuring equal probability of each unit to be selected."
				),
				p(
					strong("Simple random - allocation:"),
					"Use when the sampling frame lists geographic units (e.g. cities, wards) with their population sizes but no household-level frame is available. Surveys are allocated across units with probability proportional to population size, so larger units receive more surveys and a unit can be selected several times. The output gives the number of surveys to collect in each selected unit."
				),
				p(
					strong("Cluster sampling:"),
					"Clusters, or Primary Sampling Units (PSUs), are first randomly selected, before a set number of units (e.g. households) at each cluster is randomly selected."
				),
				p(
					strong("Stratified:"),
					"If ticked, a variable characterizing the stratification of the sampling will be ask to stratify the sample."
				),

				h2("Parameters"),
				p(strong("Confidence level:"), "The desired confidence level."),
				p(strong("Error Margin:"), "The desired error margin."),
				p(
					strong("Proportion:"),
					"The expected proportion in the sample."
				),
				p(strong("Buffer:"), "The desired percentage buffer."),
				p(
					strong("Cluster size:"),
					"The minimum number of interviews to conduct by cluster."
				),
				p(
					strong("ICC:"),
					"Intracluster  correlation : average value estimated in previous assessments = 0.06."
				)
			),
			column(
				5,
				h2("Loading files"),
				p(
					"The CSV is your sampling frame: one row per sampling unit, covering the entire population of interest (not a pre-drawn subset). For Simple random, each row is a unit and the frame size is the population. For Cluster sampling and Simple random - allocation, each row is a cluster or area, and the Population column gives the number of sub-units (e.g. households) it contains."
				),
				p(
					"The app takes only files .csv; each row of the csv need to be the primary sampling unit.",
					strong(
						"The headers of the dataset must NOT contain special characters."
					),
					strong("Uploaded files are limited to 30 MB."),
					("You can use some example data by ticking 'example data'")
				),
				p(
					"When the files is loaded, depending of the type of sampling loaded before, some parameters will have to be specify:"
				),
				p(
					strong("Cluster:"),
					"The variable including the name of each cluster (PSU). The sampling frame is expected to have one row per cluster, so this column must contain a unique value per row (no duplicates)."
				),
				p(
					strong("Stratification:"),
					"The variable in the data defining the stratification."
				),
				p(
					strong("Population:"),
					"The variable in the data defining the population number by PSU.",
					strong("Must be > 0 and > to cluster size")
				),
				p(strong(
					"When all this parameters are defined please press'Apply'."
				)),

				h2("Computing target"),
				p(
					"Under the target section will be compute the target based on the confidence level and margin error defined previously."
				),
				p("Click Apply to compute the target"),

				h2("Sampling"),
				p(
					"When target appear in the tab, go to the 'Sampling tab' and click 'Sample!'. You can then download the list of units sampled and the sampling summary."
				)
			)
		)
	),
	nav_panel(
		"Sampling frame",
		layout_columns(
			col_widths = c(6, 6),
			card(
				card_header(h2("Sampling setup", class = "h6 mb-0")),
				card_body(
					fillable = FALSE,
					fluidRow(
						column(
							6,
							selectInput(
								"topup",
								"Mode of sampling",
								c(
									"Enter sample size",
									"Sample size based on population"
								),
								"Sample size based on population"
							)
						),
						column(
							6,
							selectInput(
								"samp_type",
								"Type of sampling",
								c(
									"Simple random",
									"Simple random - allocation",
									"Cluster sampling"
								)
							)
						)
					),
					fluidRow(
						column(
							4,
							selectInput(
								"stratified",
								"Stratified ?",
								c("Not stratified", "Stratified")
							)
						),
						conditionalPanel(
							# the buffer only applies to a sample size computed from the population
							condition = "input.topup != 'Enter sample size'",
							column(
								4,
								numeric_input_with_tip(
									'buf',
									"Buffer",
									tip = "Extra share added to the target to cover non-response or unusable interviews: target \u00d7 (1 + buffer). Base it on the non-response rate you expect.",
									value = 0.05,
									min = 0,
									step = 0.01
								)
							)
						),
						column(
							4,
							numeric_input_with_tip(
								'seed_value',
								"Seed",
								tip = "Starting point of the random draw. The same seed with the same settings gives the same sample, so a draw can be reproduced. Change it to get a different sample.",
								value = 1,
								min = -2147483647,
								max = 2147483647,
								step = 1
							)
						)
					),
					# min/max mirror validate_parameters(); HTML cannot express a strict bound,
					# so the closest value on the step grid is used.
					fluidRow(
						conditionalPanel(
							condition = "input.topup != 'Enter sample size'",
							column(
								4,
								numeric_input_with_tip(
									'conf_level',
									"Confidence level",
									tip = "How often the interval around your estimate would contain the true value if the survey were repeated (0.95 = 95% of the time). Higher confidence needs a larger sample.",
									source = cochran_1977,
									value = 0.95,
									min = 0.005,
									max = 0.995,
									step = 0.005
								)
							)
						),
						conditionalPanel(
							condition = "input.topup != 'Enter sample size'",
							column(
								4,
								numeric_input_with_tip(
									'e_marg',
									"Error Margin",
									tip = "Largest gap you accept between the estimate and the true value, at the chosen confidence level (0.05 = \u00b15 percentage points). A smaller margin needs a larger sample.",
									source = cochran_1977,
									value = 0.05,
									min = 0.005,
									max = 0.995,
									step = 0.005
								)
							)
						),
						conditionalPanel(
							condition = "input.topup != 'Enter sample size'",
							column(
								4,
								numeric_input_with_tip(
									'pror',
									"Proportion",
									tip = "Share of the population expected to have the characteristic you measure. With no estimate, keep 0.5: it gives the largest, most cautious sample size.",
									source = cochran_1977,
									value = 0.5,
									min = 0.05,
									max = 0.95,
									step = 0.05
								)
							)
						),
						conditionalPanel(
							condition = "input.topup == 'Enter sample size'",
							column(
								4,
								numericInput(
									'target',
									'Enter target sample size',
									NULL,
									min = 1,
									step = 1
								)
							)
						)
					),
					radioButtons(
						"data_source",
						tags$span(class = "param-subhead", "Data source"),
						choices = c(
							"Upload CSV" = "upload",
							"Use test data" = "test"
						),
						inline = TRUE
					),
					fluidRow(
						column(
							7,
							conditionalPanel(
								condition = "input.data_source == 'upload'",
								# aria-label on the inputs: a hidden label would still take up space
								htmltools::tagQuery(
									fileInput(
										'popdata',
										NULL,
										accept = c(
											'text/csv',
											'text/comma-separated-values,text/plain'
										),
										placeholder = "CSV, max 30 MB"
									)
								)$find("input")$addAttrs(
									"aria-label" = "CSV file"
								)$allTags()
							),
							conditionalPanel(
								condition = "input.data_source == 'test'",
								div(
									class = "example-link",
									downloadLink(
										"download_example",
										tagList(
											icon("download"),
											"Download example CSV"
										)
									)
								)
							)
						),
						column(5, uiOutput("data_status"))
					),
					# Simple random without stratification needs no column from the data
					conditionalPanel(
						condition = "input.samp_type != 'Simple random' || input.stratified == 'Stratified'",
						div(class = "param-subhead", "Column mapping"),
						fluidRow(
							conditionalPanel(
								condition = "input.samp_type == 'Cluster sampling'",
								column(
									4,
									selectInput(
										'col_psu',
										'Input cluster',
										choices = NULL
									)
								)
							),
							conditionalPanel(
								condition = "input.stratified == 'Stratified'",
								column(
									4,
									selectInput(
										'strata',
										'Input strata',
										choices = NULL,
										multiple = F
									)
								)
							),
							conditionalPanel(
								condition = "input.samp_type == 'Cluster sampling'|input.samp_type =='Simple random - allocation'",
								column(
									4,
									selectInput(
										'colpop',
										'Input population',
										choices = NULL
									)
								)
							)
						)
					),
					conditionalPanel(
						condition = "input.samp_type == 'Cluster sampling'",
						div(class = "param-subhead", "Cluster design"),
						fluidRow(
							column(
								6,
								numeric_input_with_tip(
									'cls',
									"Cluster size",
									tip = "Number of interviews planned in each selected cluster (at least 2). Larger clusters lower the fieldwork cost but raise the design effect.",
									source = kish_1965,
									value = 5,
									min = 2,
									step = 1
								)
							),
							column(
								6,
								numeric_input_with_tip(
									'ICC',
									"ICC",
									tip = "Intracluster correlation: how alike the units of a cluster are (0 = not at all, 1 = identical). It inflates the sample through the design effect, 1 + (cluster size \u2212 1) \u00d7 ICC. Use a value from a similar past survey.",
									source = kish_1965,
									value = 0.06,
									min = 0,
									max = 1,
									step = 0.01
								)
							)
						)
					),
					div(
						class = "text-center mt-2",
						input_task_button(
							"f_apply",
							"Apply",
							label_busy = "Applying...",
							style = "width: 200px;"
						)
					)
				)
			),
			layout_column_wrap(
				width = 1,
				heights_equal = "row",
				fill = FALSE,
				card(
					card_header(h2("Target sampling", class = "h6 mb-0")),
					card_body(
						fillable = FALSE,
						uiOutput("apply_error"),
						DT::dataTableOutput("target_frame")
					)
				),
				card(
					card_header(h2("Sampling frame", class = "h6 mb-0")),
					card_body(
						fillable = FALSE,
						DT::dataTableOutput("sampling_frame")
					)
				)
			)
		)
	),
	nav_panel(
		"Sampling",
		layout_columns(
			col_widths = c(3, 9),
			layout_column_wrap(
				width = 1,
				heights_equal = "row",
				fill = FALSE,
				card(
					card_header(h2("Actions", class = "h6 mb-0")),
					shinyjs::disabled(
						actionButton(
							"desButton",
							"Sample",
							class = "btn-primary",
							width = "100%"
						)
					)
				),
				card(
					card_header(h2("Export", class = "h6 mb-0")),
					uiOutput("export_buttons")
				)
			),
			layout_column_wrap(
				width = 1,
				heights_equal = "row",
				fill = FALSE,
				card(
					card_header(h2("Sample summary", class = "h6 mb-0")),
					card_body(
						fillable = FALSE,
						uiOutput("sample_error"),
						DT::dataTableOutput("test_1")
					)
				),
				card(
					card_header(h2("Sample", class = "h6 mb-0")),
					card_body(
						fillable = FALSE,
						DT::dataTableOutput("sampling_output")
					)
				)
			)
		)
	)
)
