#intro panel
# This is the code for the intro panel




graph1_input <- selectInput(
  inputId = "graph_input",
  label = "Select An AQI Category",
  choices = c("coal_co2_sum", "gas_co2_sum", "oil_co2_sum"))


Slider_range <- reactive(sliderInput("range", "Range:",
            min = 1950, max = 2010,
            value = c(1950,2010)))

second_int_sidebar_content <- sidebarPanel(
  graph1_input,
  Slider_range
)

second_int_main_content <- mainPanel(
  plotlyOutput("plot")
)

second_int_panel <- tabPanel(
  "Graph",
  titlePanel("year vs. CO2 emissions globally"),
  sidebarLayout(
    second_int_sidebar_content,
    second_int_main_content
  )
)

ui <- navbarPage(
  "CO2 emissions",
  second_int_panel,
)
