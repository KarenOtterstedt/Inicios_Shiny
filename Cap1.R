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


# Defino en server lo que va a aparecer en el output
# Programación reactiva: contarle a shiny cómo calcular algo (no
# es decirle que lo haga, es como si fuera darle una receta a alguien)
server <- function(input, output, session) {
  output$summary <- renderPrint(
    {
      
    }
  )
}

shinyApp(ui, server)







