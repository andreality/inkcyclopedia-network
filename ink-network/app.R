#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(igraph)
library(networkD3)

dists <- readRDS("colour_distance_matrix.rds")
cols <- readRDS("node_colours.rds")
name_map <- read_csv("ink_name_map.csv")
name_choices <- name_map$name
names(name_choices) <- name_map$ink_name

get_neighbours <- function(node.name, links.df, dist = 1, k = 1) {
  if(dist == 1) {
    return(node.name)
  }
  if(k == dist | dist == 2) {
    return(unique(links.df$target_name[links.df$source_name %in% node.name]))
  } else {
    k <- k + 1
    get_neighbours(nbs, links.df, dist, k)
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("inkcyclopedia network"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("col", "Select Color", choices = name_choices),
         selectInput("sep",
                      "Degrees of separation",
                      choices = c(1, 2)),
         numericInput("thresh",
                      "Threshold for link",
                      min = 0.01,
                      max = 0.15,
                      value = 0.06,
                      step = 0.01),
         numericInput("node_size", 
                      "Node Size", 
                      min = 1, 
                      max = 20, 
                      value = 5, 
                      step = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         forceNetworkOutput("net")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
 output$net <- renderForceNetwork({
    dists <- ifelse(dists < input$thresh, dists, 0)
    g <- graph.adjacency(dists, weighted = TRUE)
    nodes <- V(g)$name
    linksdf <- get.data.frame(g) %>% 
      rename(source_name = from, target_name = to, value = weight)
    nodesdf <- data.frame(name = V(g)$name) %>% 
      mutate(id = 1:n() - 1, group = name, color = cols, size = input$node_size) 
    
    nbs <- get_neighbours(req(input$col), linksdf, input$sep)
    sub_links <- linksdf %>% 
      filter(target_name %in% nbs)
    sub_nodes <- nodesdf %>% 
      filter(name %in% c(sub_links$source_name, sub_links$target_name)) %>% 
      mutate(id = 1:n() - 1, 
             group = name)
    sub_links$source <- sub_nodes$id[match(sub_links$source_name, sub_nodes$name)]
    sub_links$target <- sub_nodes$id[match(sub_links$target_name, sub_nodes$name)]
    
    col_str <- paste(sub_nodes$color, collapse = '\","')
    name_str <- paste(sub_nodes$name, collapse = '\","')
    
    colour_scale <- paste0("d3.scaleOrdinal().domain([\"", 
                           name_str, "\"]).range([\"",
                           col_str, "\"]);")
    MyClickScript <- 
      '      d3.select(this).select("circle").transition()
    .duration(750)
    .attr("r", 35)'
    forceNetwork(Links = sub_links,
                 Nodes = sub_nodes,
                 Source = "source",
                 Target = "target",
                 Value = "value",
                 NodeID = "name",
                 opacity = 0.8,
                 Group = "group",
                 colourScale = colour_scale,
                 zoom = TRUE,
                 fontSize = 20,
                 clickAction = MyClickScript,
                 fontFamily = "sans-serif",
                 Nodesize = "size")
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

