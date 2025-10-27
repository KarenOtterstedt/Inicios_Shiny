# IMPORTANTE PONER LAS LIBRERÍAS DE CADA FUNCION A LA HORA DE USARLAS

# Nombro cosas_clase, en lugar de app, porque lo hago para practicar
# SI FUERA ALGO QUE SE TIENE QUE PUBLICAR HABRÍA QUE PONERLE DE 
# NOMBRE APP!!!!!



# FORMA GENERAL:
# interfaz <- bslib::.....
# (se definen configuraciones y nombres de las cosas)
# 
# servidor <- function(input, output, session){
# (funciones, gráficos, tablas, cosas que se calculan, etc)
# 
# USANDO LOS NOMBRES QUE USAMOS EN INTERFAZ PARA PODER UNIRLO
# output$nombre_interfaz <- paquete::renderpaquete({
# 
# SIEMPRE PONER LLAVER PARA QUE SHINY ENTIENDA QUE ESO ES
# LO QUE TIENE QUE CORRER TODO JUNTO
# })
# }









library(shiny)
# Cuando escribo shinyapp + SHIFT + TAB aparece fluidPage
# pero tiene personalización más limitada así que conviene usar
# bslib::page_fluid  (parece que también se recomienda para github, etc )
# ui <- fluidPage(
# )


#                     CARGANDO LOS DATOS                           #
queen <- readr::read_delim("queen.txt",
                           delim = "\t")

# Componentes principales:
cp <- FactoMineR::PCA(
  # Elegimos las variables numéricas del dataset
  X = dplyr::select(queen, 
                    dplyr::where(is.numeric)),
  
  # Elegimos que nos muestre 11 componentes principales
  ncp = 11
)



#                CREANDO LA INTERFAZ DEL USER                     #

# bslib: para que se adapte al tamaño de la pantalla
MiInterfaz <- bslib::page_fluid(
  
  # Título principal de la app en la parte superior
  shiny::titlePanel("Mi Primera Shiny App - Repaso"),
  
  # layout_sidebar: barra lateral para poner controles o filtros
  bslib::layout_sidebar(
    
    # En este caso la barra lateral está vacía
    sidebar = bslib::sidebar(),
    
    # card: para evitar superposición de las tablas
    # Tabla interactiva, se pone dentro de card para no superponerse
    bslib::card(DT::DTOutput("tablas_cargas"),
                height = "620px"), # Fijando el tamaño
    # tablas_cargas es el ID que se conecta con el server
    
    # Otra tabla interactiva (tiene mejor diseño visual)
    reactable::reactableOutput("tabla_pjevar"),
    # De nuevo, tabla_pjevar es el ID que lo va a 
    # conectar con el server
    
    # Gráficos interactivos de COMPONENTES PRINCIPALES
    plotly::plotlyOutput("plot_scree"),
    plotly::plotlyOutput("plot_indiv")
  )
)

# HASTA ACA SE DEFINIÓ:
# - tabla_cargas
# - tabla_pjevar
# - plot_scree
# - plot_indiv
# (hay que unir todos esos nombres con lo que van a tener adentro)





#                       CREANDO SERVIDOR                          #
# Le dice a R lo qué tiene que hacer, interfaz se conecta con esto,
# pero tiene los nombres para unirlos y las configuraciones
# más que nada

# Conviene hacer la mayoría de las cosas antes, así el servidor 
# las usa directamente como las necesita, en lugar de correr todo
MiServidor <- function(input, output) {
  # La versión más simple sólo tiene:
  # - input: opciones elegidas por los usuarios a través de la interfaz
  # - output: elementos de salida (valores numéricos, tablas, gráficos, etc)
  
  
# GUARDANDO EN OUTPUT:
  # tabla_cargas:
  output$tabla_cargas <- DT::renderDT({ # HAY MUCHOS RENDERS
    # renderDT tiene tablas más modernas que renderDataTable
    
    # Buscando las coordenadas de las variables en cp
    cp$var$coord |> 
      
      # Pidiendo que la página tome sólo 11:
      DT::datatable(options = list(pageLength = 11)) |> 
      DT::formatRound(columns = 1:11,
                      digits = 3)
  })
  
  
  # tabla_pjevar:
  output$tabla_pjevar <- reactable::renderReactable({
    cp$eig |> 
      reactable::reactable(
        pagination = F, # Para que no ponga en distintas páginas
        defaultColDef = reactable::colDef(
          
          format = reactable::colFormat(digits = 2)
        )
      )
  })
 
  
  # plot_scree:
  output$plot_scree <- plotly::renderPlotly({
    
    gg_scree <- tibble::tibble(CP = 1:nrow(cp$eig),
                               
                # eig[,2] para elegir porcentaje de variancia
                # Columnas:
                # 1: autovalor
                # 2: porcentaje de variancia
                # 3: porcentaje de variancia acumulada
                               PVE = cp$eig[,2]) |> 
      
      # Gráfico:
      ggplot2::ggplot() +
      ggplot2::aes(x = CP,
                   
                   # Porcentaje de variancia
                   y = PVE) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3,
                          color = "pink3") +
      ggplot2::scale_x_continuous(breaks = 1:nrow(cp$eig)) +
      ggplot2::scale_y_continuous(name = "% Variancia Explicada") +
      ggplot2::ggtitle("Scree Plot") +
      ggplot2::theme_bw()
    # Hasta acá se guarda el gráfico en gg_scree
    
    # Haciéndolo interactivo:
    plotly::ggplotly(gg_scree)
  })
  
  
  # plot_indiv:
  output$plot_indiv <- plotly::renderPlotly({
    
    individuos <- cp$ind$coord |> 
      dplyr::bind_cols(queen) |> 
      dplyr::mutate(name = stringr::str_wrap(name, 25)) |> 
      
      # Gráfico:
      ggplot2::ggplot() +
      ggplot2::aes(x = Dim.1,
                   y = Dim.2,
                   color = album,
                   label = name) +
      ggplot2::geom_hline(yintercept = 0,
                        linewidth = 0.1) +
      ggplot2::geom_vline(xintercept = 0,
                          linewidth = 0.1) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::geom_text(size = 2,
                         show.legend = F) +
      ggplot2::ggtitle("Gráfico de los individuos en las CP seleccionadas") +
      ggplot2::theme_bw()
      
    plotly::ggplotly(individuos)
  })
  
}



#                     LANZANDO LA APLICACIÓN                       #

# Para ver la app fuera de RStudio hay que elegir que la corra 
# en external
shiny::shinyApp(MiInterfaz, MiServidor)






              #           AGREGANDO WIDGETS           #
# A cada widget que se crea hay que asignarle un ID para hacer
# referencia a los valores que cada uno toma en determinado momento

# El código se debe incluir dentro del sidebarPanel de la INTERFAZ

# Copiando de nuevo los datos:
queen <- readr::read_delim("queen.txt",
                           delim = "\t")

cp <- FactoMineR::PCA(
  X = dplyr::select(queen,
                    dplyr::where(is.numeric)),
  ncp = 11,
  graph = F
)


#                CREANDO LA INTERFAZ DEL USER                     #

ui <- bslib::page_fluid(
  shiny::titlePanel("Mi Primera Shiny App"),
  bslib::layout_sidebar(
    
    # SIDEBAR:
    sidebar = bslib::sidebar(
      
      # Agregando un listado de albums
      shiny::checkboxGroupInput(
        
        # ID del widget (para el server)
        inputId = "album",
        
        # Título que va a mostrar la app
        label = "Álbums a incluir",
        
        # Opciones de álbunes disponibles
        choices = sort(unique(queen$album)),
        
        # Aparecen los 4 primeros seleccionados
        selected = sort(unique(queen$album)) [1:3]
      ),
      
      # Agregando componentes eje X
      shiny::numericInput(
        
        # ID del widget
        inputId = "ejex",
        
        # Título a mostrar en la app
        label = "CP Eje X",
        
        # Valor seleccionado inicialmente
        value = 1,
        
        # Mínimo valor posible
        min = 1,
        
        # Máximo valor posible
        max = 11
      ),
      
      # Agregando componentes eje y
      shiny::numericInput(
        
        # ID del widget
        inputId = "ejey",
        
        # Título a mostrar en la app
        label = "CP Eje Y",
        
        # Valor seleccionado inciialmente
        value = 2,
        
        # Mínimo valor posible
        min = 1,
        
        # Maximo valor posible
        max = 11
      )
    ),
    
    # Para evitar superposición de tablas:
    bslib::card(DT::DTOutput("tabla_cargas"),
                height = "620px"),
    reactable::reactableOutput("tabla_pjevar"),
    plotly::plotlyOutput("plot_scree"),
    plotly::plotlyOutput("plot_indiv")
  )
)



#                       CREANDO SERVIDOR                          #

server <- function(input, output) {
  
  # - tabla_cargas:
  output$tabla_cargas <- DT::renderDT({
    cp$var$coord |>
      
      # Para que tome todo en una sóla página (son 11)
      DT::datatable(options = list(pageLength = 11)) |> 
      DT::formatRound(columns = 1:11,
                      digits = 4)
  })
  
  # - tabla_pjevar:
  output$tabla_pjevar <- reactable::renderReactable({
    cp$eig |> 
      reactable::reactable(
        # Para que no se formen páginas
        pagination = F,
        
        # Cómo se ven las columnas
        defaultColDef = reactable::colDef(
          # Para que tenga 4 dígitos
          format = reactable::colFormat(digits = 4)
        )
      )
  })
  
  output$plot_scree <- plotly::renderPlotly({
    
    gg_scree <- tibble::tibble(CP = 1:nrow(cp$eig),
                               PVE = cp$eig[,2]) |> 
      
      ggplot2::ggplot() +
      ggplot2::aes(x = CP,
                   y = PVE) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3,
                          color = "pink3") +
      ggplot2::scale_x_continuous(breaks = 1:nrow(cp$eig)) +
      ggplot2::scale_y_continuous(name = "% Variancia Explicada") +
      ggplot2::ggtitle("Scree Plot") +
      ggplot2::theme_bw()
    
    # Para que corra el gráfico:
    plotly::ggplotly(gg_scree)
  })
  
  # - plot_indiv:
  output$plot_indiv <- plotly::renderPlotly({
    
    individuos <- cp$ind$coord |> 
      dplyr::bind_cols(queen) |> 
      dplyr::mutate(name = stringr::str_wrap(name, 25)) |> 
      
      # Grafico
      ggplot2::ggplot() +
      ggplot2::aes(x = Dim.1,
                   y = Dim.2,
                   color = album,
                   label = name) +
      ggplot2::geom_hline(yintercept = 0,
                          linewidth = 0.1) +
      ggplot2::geom_vline(xintercept = 0,
                          linewidth = 0.1) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::geom_text(size = 2, 
                         show.legend = F) +
      ggplot2::ggtitle("Gráfico de individuos en las CP seleccionadas") +
      ggplot2::theme_bw()
    
    # Para que corra el gráfico:
    plotly::ggplotly(individuos)
  })
  
}

shinyApp(ui, server)



          #           AGREGANDO REACTIVIDAD           #
          # Ver capítulo "Mastering reactivity" de Mastering Shiny


# Widgets: comunican al server el valor seleccionado por el usuario a 
#          través de la lista INPUT


queen <-  readr::read_delim("queen.txt",
                            delim = "\t")


# RESUMEN:
# Creo que para la reactividad lo que hay que cambiar es la forma
# que tiene el dataset más que nada, la interfaz y servidor creo
# que quedarían iguales

#                CREANDO LA INTERFAZ DEL USER                     #

ui <- bslib::page_fluid(
  
  shiny::titlePanel("Mi Primera Shiny App"),
  
  bslib::layout_sidebar(
    
    # SIDEBAR
    sidebar = bslib::sidebar(
      
      # Listado de Albums:
      shiny::checkboxGroupInput(
        # ID del widget
        inputId = "album",
        # Título que muestra
        label = "Álbums a incluir",
        # Opciones disponibles
        choices = sort(unique(queen$album)),
        # Opciones seleccionadas por default
        selected = sort(unique(queen$album)) [1:3]
      ),
      
      # Componente Eje X:
      shiny::numericInput(
        # ID del widget
        inputId = "ejex",
        # Título que muestra en la app
        label = "CP Eje X",
        # Valor seleccionado inicialmente
        value = 1,
        # Mínimo y máximo
        min = 1,
        max = 11
      ),
      
      # Componente Eje Y
      shiny::numericInput(
        inputId = "ejey",
        label = "CP Eje Y",
        value = 2,
        min = 1,
        max = 11
      )
    ), # ACA TERMINA EL SIDEBAR
    
    # bslib::card() para evitar superposición de las tablas
    bslib::card(DT::DTOutput("tabla_cargas"),
                height = "620px"),
    reactable::reactableOutput("tabla_pjevar"),
    plotly::plotlyOutput("plot_scree"),
    plotly::plotlyOutput("plot_indiv")
  )
)

# EN EL SERVIDOR PONEMOS LOS DATASETS REACTIVOS, GRÁFICOS, TABLAS, ETC
server <- function(input, output, session) {
  
  # Primero: los datasets reactivos
  
  
  # Se usa INPUT ID para acceder al valor definido por un usuario 
  # LA BASE FILTRADA DEBE SER UN ELEMENTO REACTIVO (OBJETO DE R CUYO
  # VALOR NO SEA FIJO)
  base_filtrada <- shiny::reactive({
    
    # Para que seleccione album que elijamos en particular
    dplyr::filter(queen, 
                  
                  # Forma general de elegir el album en particular 
                  album %in% input$album)
  })
  
  # AHORA SE PONE SHINY::REACTIVE PARA QUE SEA REACTIVO
  cp <- shiny::reactive({FactoMineR::PCA(
    
    # COMO LA BASE FILTRADA ES REACTIVA, TODO LO QUE LA USE TAMBIÉN
    # LO VA A SER (depende de la base de datos que a su vez depende
    # de los discos elegidos)
    
    
    # Para usar elementos reactivos se agrega un paréntesis al final
    X = dplyr::select(base_filtrada(),
                      dplyr::where(is.numeric)),
    ncp = 11,
    graph = F
  )
  })

  
  # AHORA SE COPIA LO QUE HABÍA ANTES, PERO ALGO MODIFICADO
  # PARA QUE SE CORRESPONDA CON LA REACTIVIDAD
  
  # tabla_cargas 
  output$tabla_cargas <- DT::renderDT({
    
    # EL DATASET CP SE PONE CON PARÉNTESIS
    cp()$var$coord |> 
      DT::datatable(options = list(pageLength = 11)) |> 
      DT::formatRound(columns = 1:11, digits = 4)
  })
  
  
  # tabla_pjevar
  output$tabla_pjevar <- reactable::renderReactable({
    # DE NUEVO CON PARÉNTESIS AL FINAL
    cp()$eig |> 
      reactable::reactable(
        pagination = FALSE,
        defaultColDef = reactable::colDef(
          format = reactable::colFormat(digits = 4)
        )
      )
  })

  
  # plot_scree
  output$plot_scree <- plotly::renderPlotly({
    
    # ACÁ DE NUEVO CP() EN LUGAR DE SÓLO CP
    gg_scree <- tibble::tibble(CP = 1:nrow(cp()$eig),
                               PVE = cp()$eig[,2]) |> 
      ggplot2::ggplot() +
      ggplot2::aes(x = CP, y = PVE) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3,
                          color = "pink3") +
      # ACA TAMBIEN CP()
      ggplot2::scale_x_continuous(breaks = 1:nrow(cp()$eig)) +
      ggplot2::scale_y_continuous(name = "% Variancia Explicada") +
      ggplot2::ggtitle("Scree Plot") +
      ggplot2::theme_bw()
    
    plotly::ggplotly(gg_scree)
    
  })
  
  
  # plot_indiv
  output$plot_indiv <- plotly::renderPlotly({
    
    # De nuevo cp() en lugar de sólo cp
    individuos <- cp()$ind$coord |> 
      
      # BASE FILTRADA TAMBIÉN CON PARÉNTESIS PORQUE ES REACTIVA
      dplyr::bind_cols(base_filtrada()) |> 
      dplyr::mutate(name = stringr::str_wrap(name, 25)) |> 
      
      # Agregamos esta parte antes de graficar
      dplyr::select(album,
                    name, 
                    x = paste0("Dim.", input$ejex), 
                    y = paste0("Dim.", input$ejey)) |> 
      
      # Gráfico
      ggplot2::ggplot() +
      ggplot2::aes(x = x, y = y, color = album, label = name) +
      ggplot2::geom_hline(yintercept = 0, 
                          linewidth = 0.1) +
      ggplot2::geom_vline(xintercept = 0,
                          linewidth = 0.1) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::geom_text(size = 2,
                         show.legend = FALSE) +
      ggplot2::ggtitle("Gráfico de los individuos en las CP seleccionadas") +
      ggplot2::labs(x = paste0("CP", input$ejex),
                    y = paste0("CP", input$ejey)) +
      ggplot2::theme_bw()
    
    plotly::ggplotly(individuos)
    
  })
}

shiny::shinyApp(ui, server)


# INCLUIR EN EL SERVER:
# - OUTPUTS
# - OBJETOS REACTIVOS (u objetos que dependan de reactivos)

# RECOMENDACIONES: ARMAR ESQUEMA
# - Inputs (creo que serían cosas que se pueden seleccionar)
# - Outputs (lo que se termina viendo)



            #             TRABAJO EN EQUIPO           #

library(shiny)

# PASOS QUE PIENSO QUE HAY QUE HACER:
#   1. Cargar los datos
#   2. Hacerlo reactivo (álbums y variables)
#   3. Interfaz
#             Widgets para (shinyWidgets::pickerInput)
#               - Seleccionar álbums 
#               - Seleccionar variables
#             
#             Histograma
#               - Siempre con 10 barras
#               - El título debe mostrar los álbumes seleccionados



# 1. Carga de datos
queen <- readr::read_delim("queen.txt",
                           delim = "\t")

# INTERFAZ
ui <- bslib::page_fluid(
  
  # Título Mi Segunda Shiny App
  shiny::titlePanel("Mi Segunda Shiny App"),
  
  # Agrego la sidebar:
  bslib::layout_sidebar(
    
    sidebar = bslib::sidebar(
      
      # Para los álbumes
      shinyWidgets::pickerInput(
        inputId = "album",
        label = "Álbum(s)",
        choices = unique(queen$album),
        
        # Parece que para que se pueda usar más de un album a la vez
        multiple = T,
        
        # Para que empiece con todos seleccionados, como está en
        # la imagen de referencia
        selected = unique(queen$album), 
        options = "livesearch"
      ),
      
      # Para las variables
      shinyWidgets::pickerInput(
        inputId = "variable",
        
        # Nombre que aparece en el widget
        label = "Variable",
        
        # ** PONER NAMES PARA QUE CONSIDERE LOS NOMBRES DE COLUMNAS
        # De 4 a 14 porque esas serían las opciones útiles
        choices = names(queen)[4:14],
        
        # Para que seleccione automáticamente popularity
        selected = names(queen) [4],
        options = "livesearch"
      )
    ),
    
    
    # Gráfico UNICO OUTPUT PORQUE SOLO QUEREMOS UN GRÁFICO
    plotly::plotlyOutput("histograma")
  )
)


# USAR:
# SHINYWIDGETS::PICKERINPUT(
# OPTIONS = LIVESEARCH)


# SERVIDOR
server <- function(input, output) {
  
  # Un datos filtrado para album
  filtrado <- shiny::reactive({
    queen |> 
    # Para que seleccione las variables que le pedimos
    dplyr::filter(# El %in% para que seleccione los que están
                  album %in% input$album)
    
    
    
  })
  
  
  # VER!!!!!
  seleccionada <-  shiny::reactive({
    filtrado() [[input$variable]]
    })
  
 
  # HISTOGRAMA
  output$histograma <- plotly::renderPlotly({
    
    # Titulo
    titulo <- paste0("Álbumes considerados:",
                     # Para que los divida con / collapse
                     paste0(input$album,
                            collapse = "/"))
    
    histograma <- ggplot2::ggplot() +
      ggplot2::geom_histogram(
        ggplot2::aes(x = seleccionada()), 
        bins = 10,
        fill = "steelblue3",
        color = "black",
        size = 0.15
      ) +
      
      # Para los ejes
      ggplot2::labs (
        x = input$variable,
        y = "Frecuencia") +
      
      ggplot2::ggtitle(stringr::str_wrap(titulo, 100)) +
  
      ggplot2::theme_bw()
    
    plotly::ggplotly(histograma)
  })
}

shiny::shinyApp(ui, server)






