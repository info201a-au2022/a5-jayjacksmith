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


oil_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    oil_co2_sum = sum(oil_co2)
  )%>%
  select(year, oil_co2_sum)
#View(oil_sum_co2)

gas_sum_co2 <- world_data_1 %>%
  group_by(year) %>%
  summarise(
    gas_co2_sum = sum(gas_co2)
  )%>%
  select(year, gas_co2_sum)
#View(gas_sum_co2)

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
  
  
  World_data <- World_data_use[World_data_use$year >= input$range[1] & World_data_use$year <= input$range[2],]
  
  output$plot <- renderPlotly({
    plot <- ggplotly(ggplot(data = World_data %>%
        aes(x = year, y = input$graph_input)) +
        geom_bar(stat="identity", fill="steelblue") +
        labs(
          x = "year",
          y = "Annual CO2 emmissions in million tonnes",
          title = "emissions from gas gloabally vs year",
          
        )
    )
    theme_minimal()
    print(plot)
  })
  
}



