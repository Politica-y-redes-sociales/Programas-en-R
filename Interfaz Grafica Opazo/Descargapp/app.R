#install.packages("shiny")

library(shiny)
library(RMySQL)


# Define la aplicacion de interfaz grafica
ui <- fluidPage(

    
    # Sidebar with a slider input for number of bins 
    sidebarLayout( 
        position = "right",
        sidebarPanel( 
                img(src = "imagen.png", heigth = 280, width = 280),
                hr(),
                textInput("usuario","Usuario"),
                textInput("contraseña","Contraseña"),
                submitButton("Ingresar"),
                hr(),
                fileInput(inputId = "carga",buttonLabel = "Subir" , label = "Subir Archio Query" ,width = 200, placeholder = "Archivo")
                ),
        
        mainPanel(
          h3("Panel de insumos"),
          hr(),
          fluidRow(
            column(5,)
          ),
          hr(),
        )
    )
)
   
# Define server logic required to draw a histogram
server <- function(input, output) 
    {
       
    }
    
# Run the application 
shinyApp(ui = ui,  server = server)
