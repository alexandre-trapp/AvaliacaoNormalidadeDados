library(shiny)

ui <- fluidPage(

    titlePanel("Teste de normalidade"),
    
    fluidRow(
      
        column(6,
            
            helpText("Será analisada a primeira coluna do arquivo"),
            fileInput("arquivo", "Escolha or arquivo: ", multiple = F, accept = c("text/csv"))
               ),
        
        column(6,
        
           actionButton("Processar","Processar")
        )
    ),
    
    fluidRow(
        
        column(4,
               
            plotOutput("Grafhist")
        ),
        column(4,
            
            plotOutput("Grafqqplot")   
        ),
        column(4,
               
            h1(textOutput("test")) 
        )
    ),
)

server <- function(input, output) {


    observeEvent(input$Processar, {
        
        file1 = input$arquivo
        data = read.csv(file1$datapath, header = T)
        
        output$Grafhist = renderPlot({ hist(data[,1], main="Histograma") })
        output$Grafqqplot = renderPlot({ qqnorm(data[,1]) 
            
                                qqline(data)
                            })
        
        tst = shapiro.test(data[,1])[2]
        tst = paste0("Valor de P: ", tst)
        output$test = renderText({tst})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)