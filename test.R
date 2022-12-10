#test
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggmap)
library(markdown)

world_data_1 <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
View(world_data_1)
world_data_1[is.na(world_data_1)] <- 0

coal_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    coal_co2_sum = sum(coal_co2)
  )%>%
  select(year, coal_co2_sum)

oil_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    oil_co2_sum = sum(oil_co2)
  )%>%
  select(year, oil_co2_sum)

gas_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    gas_co2_sum = sum(gas_co2)
  )%>%
  select(year, gas_co2_sum)

World_data_all <- merge(oil_sum_co2, gas_sum_co2, by= "year")
World_data_use <- merge(World_data_all, coal_sum_co2, by = "year")






server <- function(input, output) {

  input_min <- input$range[1]
  input_max <- input$range[2]
  input_co2 <- input$graph_input
  
  
World_data <- World_data_use[World_data_use$year >= reactive(input$range[1]) & World_data_use$year <= reactive(input$range[2]),]


  
output$plot <- renderPlotly({
  plot <- ggplotly(ggplot(data = World_data %>%
                            aes(x = year, y = reactive(input$graph_input))) +
                     geom_bar(stat="identity", fill="steelblue") +
                     labs(
                       x = "year",
                       y = "Annual CO2 emmissions in million tonnes",
                       title = "emissions from gas gloabally vs year",
                       
                     )
  )
  theme_minimal()
})

}

#ui
graph1_input <- selectInput(
  inputId = "graph_input",
  label = "Select An AQI Category",
  choices = c(coal_co2_sum, gas_co2_sum, oil_co2_sum))


Slider_range <- sliderInput("range", "Range:",
                            min = 1950, max = 2010,
                            value = c(1950,2010))

first_int_sidebar_content <- sidebarPanel(
  graph1_input,
  Slider_range
)

first_int_main_content <- mainPanel(
  plotlyOutput("plot")
)

first_int_panel <- tabPanel(
  "Graph",
  titlePanel("year vs. CO2 emissions globally"),
  sidebarLayout(
    first_int_sidebar_content,
    first_int_main_content
  )
)
ui <- navbarPage(
  "Life vs. Air",
  first_int_panel
)
shinyApp(ui,server)
