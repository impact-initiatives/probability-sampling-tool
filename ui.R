library(markdown)

# Section header used across the app, styled once via www/app.css instead of
# a repeated inline style on every h4().
section_title <- function(text) h4(text, class = "section-title")

navbarPage(
	"Probability sampling tool",
	shinyjs::useShinyjs(),
	tags$head(
		tags$link(
			rel = "stylesheet",
			type = "text/css",
			href = "bootstrap.css"
		),
		# Cache-bust: without a version query string, browsers keep serving a
		# previously-cached app.css after it changes on disk.
		tags$link(
			rel = "stylesheet",
			type = "text/css",
			href = paste0("app.css?v=", as.integer(file.mtime("www/app.css")))
		)
	),
	tabPanel(
		"Introduction",
		fluidRow(
			h1("Brief overview of the tool"),
			align = "center"
		),
		fluidRow(
			column(1, br()),
			column(
				4,
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
			column(1, br()),
			column(
				4,
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
	tabPanel(
		"Sampling frame",
		fluidRow(
			div(
				class = "col-sm-5 frame-card",
				section_title("Sampling setup"),
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
					column(
						4,
						numericInput(
							'buf',
							'Buffer',
							0.05,
							min = 0.00,
							max = 0.50,
							step = 0.01
						)
					),
					column(
						4,
						numericInput(
							'seed_value',
							'Seed',
							value = 1,
							min = 1,
							max = 1e6,
							step = 1
						)
					)
				),
				fluidRow(
					conditionalPanel(
						condition = "input.topup != 'Enter sample size'",
						column(
							4,
							numericInput(
								'conf_level',
								'Confidence level',
								0.95,
								min = 0.70,
								max = 0.99,
								step = 0.005
							)
						)
					),
					conditionalPanel(
						condition = "input.topup != 'Enter sample size'",
						column(
							4,
							numericInput(
								'e_marg',
								'Error Margin',
								0.05,
								min = 0.01,
								max = 0.25,
								step = 0.005
							)
						)
					),
					conditionalPanel(
						condition = "input.topup != 'Enter sample size'",
						column(
							4,
							numericInput(
								'pror',
								'Proportion',
								0.5,
								min = 0.01,
								max = 1,
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
								0,
								min = 0,
								max = 10000,
								step = 1
							)
						)
					)
				),
				div(class = "param-subhead", "Data source"),
				radioButtons(
					"data_source",
					NULL,
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
							fileInput(
								'popdata',
								NULL,
								accept = c(
									'text/csv',
									'text/comma-separated-values,text/plain'
								),
								placeholder = "CSV, max 30 MB"
							)
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
				),
				conditionalPanel(
					condition = "input.samp_type == 'Cluster sampling'",
					div(class = "param-subhead", "Cluster design"),
					fluidRow(
						column(
							6,
							numericInput(
								'cls',
								'Cluster size',
								5,
								min = 2,
								step = 1
							)
						),
						column(
							6,
							numericInput(
								'ICC',
								'ICC',
								0.06,
								min = 0.01,
								max = 0.5,
								step = 0.01
							)
						)
					)
				),
				fluidRow(
					actionButton(
						"f_apply",
						"Apply",
						width = "200px",
						style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
					),
					align = "center"
				)
			),
			div(
				class = "col-sm-7",
				div(
					class = "frame-card",
					section_title("Target sampling"),
					uiOutput("apply_error"),
					DT::dataTableOutput("target_frame")
				),
				div(
					class = "frame-card",
					section_title("Sampling frame"),
					DT::dataTableOutput("sampling_frame")
				)
			)
		)
	),
	tabPanel(
		"Sampling",
		fluidRow(
			div(
				class = "col-sm-2 frame-card",
				section_title("Actions"),
				shinyjs::disabled(
					actionButton(
						"desButton",
						"Sample",
						style = "width: 100%; margin-bottom: 20px; color: #fff; background-color: #337ab7; border-color: #2e6da4"
					)
				),
				section_title("Export"),
				shinyjs::disabled(
					downloadButton(
						"downloadBtn2",
						"Sample summary",
						class = "btn-success",
						style = "width: 100%; margin-bottom: 10px;"
					)
				),
				shinyjs::disabled(
					downloadButton(
						"downloadBtn",
						"Sample",
						class = "btn-success",
						style = "width: 100%;"
					)
				)
			),
			div(
				class = "col-sm-10",
				div(
					class = "frame-card",
					section_title("Sample summary"),
					DT::dataTableOutput("test_1")
				),
				div(
					class = "frame-card",
					section_title("Sample"),
					DT::dataTableOutput("sampling_output")
				)
			)
		)
	)
)
