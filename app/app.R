library(shiny)
library(ggplot2)
library(ca)
library(FactoMineR)
library(factoextra)


pca_result <- prcomp(iris[,1:4], center = TRUE, scale. = TRUE)
pca_df <- data.frame(Species = iris$Species, pca_result$x)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("NCCU_DS2023_hw4_111971023"),
  tabsetPanel(
    tabPanel("PCA",
             sidebarLayout(
               sidebarPanel(
                 selectInput("x_axis", "X軸座標：", choices = colnames(pca_df)[-1], selected = "PC1"),
                 selectInput("y_axis", "Y軸座標：", choices = colnames(pca_df)[-1], selected = "PC2"),
                 
               ),
               mainPanel(
                 plotOutput("pcaPlot")
               )      
             )
    ),
    tabPanel("CA",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("invisible", "顯示", 
                              c("all" = "none",
                                "col" = "row",
                                "row" = "col")),
                 radioButtons("label", "Label", 
                              c("all" = "all",
                                "col" = "col",
                                "row" = "row"))
               ),
               mainPanel(
                 plotOutput("caPlot")
               )      
             )
    ),
    tabPanel("Data Summary",
             mainPanel(
               verbatimTextOutput("summary")
             )
    )
  )
  # App title ----
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$pcaPlot <- renderPlot({
    ggplot(pca_df, aes_string(x = input$x_axis, y = input$y_axis, color = "Species")) + 
      geom_point() +
      labs(x = input$x_axis, y = input$y_axis, title = input$x_axis) +
      theme_minimal() +
      stat_ellipse(aes_string(x = input$x_axis, y = input$y_axis), linetype = 2, size = 0.5, level = 0.95)
  })
  
  output$caPlot <- renderPlot({
    ca_result <- ca(iris[, -5])
    coords <- as.data.frame(ca_result$ind$coord)
    fviz_ca_biplot(ca_result, arrows = c(FALSE, TRUE), invisible=input$invisible, label = input$label)
  })
  
  output$summary <- renderPrint({
    summary(iris) 
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
