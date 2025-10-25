# IMPORTANTE PONER LAS LIBRERÍAS DE CADA FUNCION A LA HORA DE USARLAS



library(shiny)
# Cuando escribo shinyapp + SHIFT + TAB aparece fluidPage
# pero tiene personalización más limitada así que conviene usar
# bslib::page_fluid  (parece que también se recomienda para github, etc )
# ui <- fluidPage(
# )


#                CREANDO LA INTERFAZ DEL USER                     #

# bslib: para que se adapte al tamaño de la pantalla
MiInterfaz <- bslib::page_fluid(
  
  # Título principal de la app en la parte superior
  shiny::titlePanel("Mi Primera Shiny App - Repaso"),
  
  # layout_sidebar: barra lateral para poner controles o filtros
  bslib::layout_sidebar(
    
    sidebar = bslib::sidebar(),
    
    bslib::card(DT)
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
