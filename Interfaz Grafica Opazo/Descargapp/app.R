#install.packages("shiny")
library(shiny)
library(RSQLite)
library(sqldf)

carpeta = "/Users/alfonsoopazom/Desktop/Observatorio/ProgramasenR/Interfaz Grafica Opazo"

# Define la aplicacion de interfaz grafica
ui <- fluidPage(
    # Sidebar with a slider input for number of bins 
    sidebarLayout( 
        position = "right",
        sidebarPanel( 
                div(img(src = "Logo observatorio.png", heigth = 100, width = 120,align ="center"),
                    style="text-align: center;"),
                hr(),
                textInput("usuario","Usuario"),
                textInput("contraseña","Contraseña"),
                submitButton("Ingresar"),
                hr(),
                fileInput(inputId ="carga", 
                          buttonLabel = "Subir" , 
                          label = "Subir Base de Datos" ,
                          width = 200, 
                          placeholder = "Archivo",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv"))
                ),
        
        mainPanel(
          h3("Panel de insumos"),
          hr(),
          h4("Datos del archivo"),
          fluidRow(
            column(5,
                   tableOutput('tabla'))
          ),
          hr(),
          fluidRow(
            column(6,
                   checkboxGroupInput("check", label = h4("Programas elegidos"), 
                                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), selected = NULL)),
            ),

          fluidRow(
            column(5,
                   tableOutput("vista"))
          ),
          downloadLink('descarga', 'Download source')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input,output) 
{
  output$tabla <- renderTable({
    Entrada <- input$carga
    if(is.null(Entrada)){
      return(NULL)
    }else{
      read.csv.sql(Entrada$datapath,sql = "Select name 'Nombre Archivo', type Tipo from Entrada")
    }
  })
  
  #Se lee el Archivo 
  archivo_csv <- reactive({
    datos_csv <- input$carga
    if(is.null(datos_csv)){
    }else{
      datos <- read.csv(datos_csv$datapath)
    }
  })
  
  output$vista <-renderTable({
    df <- archivo_csv()
    if(is.null(df)){
      return(NULL)
    }else{
      as.data.frame(df)
      try(influenciadores <- sqldf("SELECT retweet_screen_name 'Usuario_Retweeado',count(retweet_screen_name) Cantidad_Retweet 
                      FROM df 
                      WHERE is_retweet 
                      GROUP BY retweet_screen_name 
                      ORDER BY COUNT(retweet_screen_name) DESC
                      LIMIT 5"), silent =TRUE)
      }
  })
  
  
}
# Run the application 
shinyApp(ui = ui,  server = server)
