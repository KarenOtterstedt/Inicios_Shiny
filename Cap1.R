# MASTERING SHINY


# CAPITULO 1 

# install.packages("shiny")
# library(shiny)

# Defino la interfaz del usuario (sería una página que diga hola mundo)
# ui <- fluidPage(
#   "Hola mundo"
# )


# Defino el comportamiento de la app, en este caso la función está
# vacía así que no hace nada
# servidor <- function(input, output, sesion) {
# }

# Ejecuto shinnyApp para construir la aplicación, teniendo en cuenta 
# la interfaz del usuario y el servidor
# shinyApp(ui, servidor)


# SHORTCUT PARA HACER UNA SHINY GENERAL
# shinyapp y después SHIFT + TAB

# PARA CORRER LA SHINY:
# CTRL + SHIFT + ENTER

# Las shinys bloquean la consola, no se puede hacer nada hasta que 
# paremos la shiny



# Para ir cambiando la shiny no hace falta frenar todo y volverlo 
# a correr (por ser RStudio), se puede directamente actualizar
# la página 


# Ahora agrego un poco más de cosas:

ui <- fluidPage( # fluidPage para estructurar la página visualmente
  
  # selectInput para interactuar con la app
  selectInput("dataset", 
              # Caja para seleccionar de nombre Dataset
              label = "Dataset", 
              
              # Pongo de dónde selecciona las opciones de la caja
              choices = ls("package:datasets")),
  
  # Opciones de output, le dicen a shiny dónde poner los output
  # renderizados
  verbatimTextOutput("summary"), # Código
  tableOutput("table") # Tablas
)
#     Hasta acá se ven los inputs, osea no se ve nada casi



# Defino en server lo que va a aparecer en el output
# 
# Programación reactiva: contarle a shiny cómo calcular algo (no
# es decirle que lo haga, es como si fuera darle una receta a alguien)
# server <- function(input, output, session) {
#   # Decimos que el output va a llamarse summary y le ponemos
#   # qué es lo que va a ser adentro de renderPrint
#   
#   # Hay muchos render{Type}
#   # Le pusimos de nombre summary como el argumento del 
#   # verbatimTextOutput
#   output$summary <- renderPrint(
#     {
#       dataset <- get(input$dataset, 
#                      "package:datasets")
#       summary(dataset)
#     }
#   )
#   
#   # Le pusimos de nombre table como el argumento del 
#   # tableOutput
#   output$table <- renderTable({
#     dataset <- get(input$dataset,
#                    "package:datasets")
#     dataset
#   })
# }
# 
# shinyApp(ui, server)


# COSAS GENERALES:
# *) CREO que dentro de render{Type} van sentencias de R que solemos
# usar
# *) A medida que cambiemos input$dataset etc, se va a actualizar 
# con el valor actual (las salidas se actualizan automáticamente 
# cuando cambian los inputs)



#       AHORA REDUCIMOS LAS COSAS DUPLICADAS
#   (usando expresiones reactivas)

# Una expresión reactiva sólo ejecuta la primera vez que se llama
# y luego almacena su resultado en caché hasta que lo necesite
# actualizar

server <- function(input, output, session){
  # Creamos una expresión reactiva:
  dataset <- reactive({
    get(input$dataset,
        "package:datasets")
  })
  
  output$summary <- renderPrint({
    
    # CREO que pone dataset() con los paréntesis para que sea 
    # reactivo
    summary(dataset())
  })
  
  output$table <- renderTable({
    # De nuevo con los paréntesis, creo que eso hace que sea 
    # reactivo
    dataset()
  })
}

shinyApp(ui, server)


# CHEATSHEET SHINY SIMPLE: 
# https://rstudio.github.io/cheatsheets/shiny.pdf






# VER EJERCICIOS








