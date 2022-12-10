#app_server.R
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggmap)
library(markdown)
#make our grph and what we want on the graph

world_data_1 <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
View(world_data_1)
world_data_1[is.na(world_data_1)] <- 0
#three dfs year vs cummalative Co2, Oil, Gas

coal_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    coal_co2_sum = sum(coal_co2)
  )%>%
  select(year, coal_co2_sum)
#View(coal_sum_co2)

coal_highest <- coal_sum_co2 %>%
  filter(coal_co2_sum == max(coal_co2_sum)) %>%
  pull(coal_co2_sum)


oil_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    oil_co2_sum = sum(oil_co2)
  )%>%
  select(year, oil_co2_sum)
#View(oil_sum_co2)
oil_highest <- oil_sum_co2 %>%
  filter(oil_co2_sum == max(oil_co2_sum)) %>%
  pull(oil_co2_sum)

gas_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    gas_co2_sum = sum(gas_co2)
  )%>%
  select(year, gas_co2_sum)
#View(gas_sum_co2)
gas_highest <- gas_sum_co2 %>%
  filter(gas_co2_sum == max(gas_co2_sum)) %>%
  pull(gas_co2_sum)

World_data_all <- merge(oil_sum_co2, gas_sum_co2, by= "year")
World_data_use <- merge(World_data_all, coal_sum_co2, by = "year")
#View(World_data_use)
#bar chart coal
coal_bar <- ggplotly(ggplot(data= coal_sum_co2, aes(x=year, y=coal_co2_sum)) +
                       geom_bar(stat="identity", fill="steelblue")+
                       labs(
                         x = "year",
                         y = "Annual CO2 emmissions in million tonnes",
                         title = "Co2 emissions from coal gloabally vs year"
                       ))

theme_minimal()
coal_bar

#bar chart gas
gas_bar <- ggplotly(ggplot(data= gas_sum_co2, aes(x=year, y=gas_co2_sum)) +
                      geom_bar(stat="identity", fill="steelblue")+
                      labs(
                        x = "year",
                        y = "Annual CO2 emmissions in million tonnes",
                        title = "Co2 emissions from gas gloabally vs year"
                      ))
gas_bar

oil_bar <- ggplotly(ggplot(data= oil_sum_co2, aes(x=year, y=oil_co2_sum)) +
                      geom_bar(stat="identity", fill="steelblue")+
                      labs(
                        x = "year",
                        y = "Annual CO2 emmissions in million tonnes",
                        title = "emissions from gas gloabally vs year"
                      ))
oil_bar
#server
server <- function(input, output) {
  
  output$plot <- renderPlotly({
    plot <- if (input$graph_input == "Coal") {
      return(coal_bar)
    } else if (input$graph_input == "Gas") {
      return(gas_bar)
    } else if (input$graph_input == "Oil") {
      return(oil_bar)
    }
  })
  
}

#ui

graph1_input <- selectInput(
  inputId = "graph_input",
  label = "Select a emission type",
  choices = c("Coal", "Gas", "Oil"),
  selected = "Gas")


first_panel <- tabPanel(
  "Summary",
  titlePanel("Co2 emissions vs year"),
  mainPanel(
    intro1,
    intro2,
    intro3,
    intro4,
    intro5,
    intro6,
    intro7,
    intro8,
    intro9,
    intro10
  )
)
intro_content <- print("For this data set I chose to examine different types of CO2 emissions globally.
          to do this I created three seperate data frames summed up by year to examine
          the total sum of CO2 emissions for gas, oil, and coal respectively. From this
          we can compare the three impacts below are the max values for each dataframe")

intro1 <- print("For this data set I chose to examine different types of CO2 emissions globally.
          to do this I created three seperate data frames summed up by year to examine
          the total sum of CO2 emissions for gas, oil, and coal respectively. From this
          we can compare the three impacts below are the max values for each dataframe.")
intro2 <- print("Max value for CO2 emissions from coal = ")
intro3 <- print(coal_highest)
intro4 <- print("in million tonnes")
intro5 <- print("Max value for CO2 emissions from gas = ")
intro6 <- print(coal_highest)
intro7 <- print("in million tonnes")
intro8 <- print("Max value for CO2 emissions from oil = ")
intro9 <- print(coal_highest)
intro10<- print("in million tonnes")

second_int_sidebar_content <- sidebarPanel(
  graph1_input
)

second_int_main_content <- mainPanel(
  plotlyOutput("plot"),
  print("This chart compares different annual cummalative sums per year
        of Oil, Gas, and Coal")
)

second_int_panel <- tabPanel(
  "Emission Categories",
  titlePanel("year vs. Gas, Oil, Coal emissions"),
  sidebarLayout(
    second_int_sidebar_content,
    second_int_main_content
  )
)

ui <- navbarPage(
  "year vs Emission type",
  first_panel,
  second_int_panel,
)
shinyApp(ui=ui,server=server)
