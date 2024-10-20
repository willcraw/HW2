
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warnings = FALSE)
library(tidyr)
library(dplyr)
library(tidyverse)
library(shiny)
```
# London Housing
*In this project, we will be comparing housing prices among boroughs in London. This data set includes price, house type, square feet, borough, city, number of beds and bath. We will be filtering out all rows with house_type equal to bungalow, studio, mews, and duplex because of a lack of data.*
  
  ```{r}
#our data set
housing = read_csv("C:/Users/wjcra/Downloads/LondonHousing/London_houses.csv") %>% filter(house_type %in% c("House", "Flat / Apartment", "New development", "Penthouse")) %>% drop_na()

```

```{r}
#shiny app
reset_selection <- function(x, brush) {
  brushedPoints(x, brush, allRows=TRUE)$selected_
}
# scatter helper function
scatter = function(x) {
  ggplot(x) + geom_point(mapping=aes(x=sqft, y=price, color=factor(house_type), size=selected, alpha=selected)) + scale_x_log10() + scale_color_manual(values = c("red", "darkblue", "darkgreen", "brown")) + scale_size(limits = c(0,1), range = c(.5, 2), guide="none") + scale_alpha(limits = c(0,1), range = c(.1, 1), guide = "none")
  
}
#ui component
ui = fluidPage(
  titlePanel("Hello! Welcome to the London Housing interactive!"),
  paste("Select desired locations:"),
  selectInput("location", "Location", unique(housing$location), multiple = TRUE),
  paste("The below scatter plot contains all data points. The larger points on this plot correspond to selected locations."),
  plotOutput("scatter", brush = "plot_brush"),
  paste("Data table with only brushed points:"),
  dataTableOutput("table")
)
#server component
server = function(input, output) {
  selected = reactiveVal(rep(TRUE, nrow(housing)))
  observeEvent(
    input$plot_brush,
    selected(reset_selection(housing, input$plot_brush))
  )
  housing_subset = reactive({
    housing %>% mutate(selected = 1 * (location %in% input$location))
  })
  output$scatter = renderPlot({scatter(housing_subset())})
  output$table = renderDataTable({filter(housing_subset(), selected())})
}

shinyApp(ui, server)
```






