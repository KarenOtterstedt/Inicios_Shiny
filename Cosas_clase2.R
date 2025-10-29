# COSAS CLASE 2


# La idea es tener un monitoreo de los procesos que se analizan

# También existen tableau, powerbi,


library(shiny)

ui <- bslib::page_sidebar(
  # Titulo
  title = "Mi Tablero",
  
  # Los que siguen pueden contener cualquier cantidad de elementos
  sidebar = "Sidebar",
  "CONTENIDO DEL AREA PRINCIPAL"
  
  # PONER 
  # - INPUTS EN EL SIDE BAR
  # - OUTPUTS EN EL ÁREA PRINCIPAL (gráficos, etc)
)

# Usamos 
# bslib::card() para tablas
# bslib::sidebar() -> acá se ponen los contenidos del sidebar



# Por esta clase no nos vamos a enfocar mucho en el server
server <- function(input, output) {
  
}

shiny::shinyApp(ui, server)



#                                                                 #
#       CREANDO UN DASHBOARD QUE ANALIZA LOS PARTOS EN ROSARIO    #
#                                                                 #

datos <- readr::read_csv("partos_rosario.csv")

# TIPS:
# CREAR UN ARCHIVO NUEVO, CON PARTES DEL CÓDIGO FIJAS PARA VER 
# SI FUNCIONA BIEN (Y DESPUÉS COPIARLO Y AGREGARLE LA REACTIVIDAD
# AL DASHBOARD/SHINY)

ui <- bslib::page_sidebar(
  fillable = F, # Más adelante
  
  # Título
  title = "Partos en Rosario",
  
  # Sidebar
  sidebar = bslib::sidebar(
    
    # Título del sidebar
    title = "Panel de Control",
    
    # Efector
    shinyWidgets::pickerInput( # PICKERINPUT QUEDA MEJOR
      inputId = "efector",
      label = "Seleccionar Efector",
      # Opciones sean efector, únicos (que no considere repetidos
      # y que los ordene)
      choices = datos$efector |> unique() |> sort(),
      # Por default está seleccionado HRSP
      selected = "HRSP" # Hospital Roque Saens Peña
    ),
    
    
    # Año
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      # Selecciona los años, únicos, y que se ordenen
      # en forma decreciente
      choices = datos$año |> unique() |> sort(decreasing = T),
      # Por default se selecciona el mayor año
      selected = max(datos$año)
    )
    
  ), # ACÁ TERMINA EL SIDEBAR
  
  
  # - Gráfico del sexo de los bebés
  bslib::card(
    # Está bueno usar card para poder usar full_screen = T
    # (se puede apretar un botoncito para que agrande la imagen)
    full_screen = T,
    # Para definir el título (card_header)
    # Si ya le ponemos título con card_header NO PONER TÍTULO EN
    # EL GRÁFICO EN SÍ (el título ya está en la tarjeta)
    bslib::card_header("Partos según sexo del bebé"),
    # Output del gráfico
    echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
  ),
  
  
  # - Gráfico de la edad de la madre y el tipo de parto
  bslib::card(
    full_screen = T,
    bslib::card_header("Partos según edad de la madre y tipo de parto"),
    echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
  ),
  
  
  # - Tabla con datos
  bslib::card(
    full_screen = T,
    bslib::card_header("Datos"),
    reactable::reactableOutput(outputId = "tabla")
  )
  
)

server <- function(input, output) {
  
  # Datos filtrados (el usuario filtra como quiere)
  # OBJETO REACTIVO
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      # Se actualiza datos filtrados cuando los inputs cambian
      # (ya sea efector o año)
      dplyr::filter(
        # Tiene dos iguales para elegir un sólo elemento
        # Si se pudiera elegir más de uno, se podría usar %in% en
        # lugar de ==
        efector == input$efector,
        año == input$año
      )
  })
  
  
  # RECORDAR QUE OUTPUT TIENE ASOCIADO UNA FUNCION RENDER
  
  # - Gráfico sexo de los bebés
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    # Para correr la aplicación el browser corta donde está puesto
    #browser() # RECORDAR BORRAR DESPUÉS
    datos_filtrados() |> # Reactivo
      # Más manipulación de datos
      dplyr::count(sexo_bb) |> 
      # ECHARTS4R PARA APRENDER!!!!!
      echarts4r::e_chart(x = sexo_bb) |>  # Se define objeto echarts
      echarts4r::e_pie( # Se define un gráfico de torta
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list( # VER LISTAS ANIDADAS
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  
  # - Gráfico madre según tipo de parto
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      
      # Contar rango de edad y tipo de parto
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = T)
  })
  
  
  # - Tabla
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)




#                                                         #
#                         LAYOUTS                         #
#                                                         #

# Sería para:
# - Poner outputs en columnas
# - Agregar páginas al dashboard



#     Múltiples columnas -> bslib::layout_columns()
# (si no se especifica cuánto ocupa cada elemento, se divide
# en partes iguales, se ubican los elementos EN UNA MISMA FILA)

# SE PUEDEN PONER VALORES NEGATIVOS PARA DEJAR ESPACIOS
# (sirven para centrar las cosas, etc)
# col_widths: espera un vector NUMÉRICO (número de columnas sobre 
# un total de 12), si es más de 12, los ELEMENTOS PASAN A LA SIGUIENTE
# FILA 
# row_heights: si queremos personalizar la altura de las filas


datos <- readr::read_csv("partos_rosario.csv")

ui <- bslib::page_sidebar(
  fillable = F, # Más sobre esto en la sección "Scrolling vs Filling"
  
  title = "Partos en Rosario",
  sidebar = bslib::sidebar(
    title = "Panel de Control",
    
    shinyWidgets::pickerInput(
      inputId = "efector",
      label = "Seleccionar Efector",
      choices = datos$efector |> unique() |> sort(),
      selected = "HRSP"
    ),
    
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      choices = datos$año |> unique() |> sort(decreasing = TRUE),
      selected = max(datos$año)
    )
    
  ),
  
  # ACA SE PONE (ADENTRO DEL SIDEBAR)
  bslib::layout_columns(
    # Aclara que ponga las cosas en columnas (en una fila)
    bslib::card( # Las cards  se ponen en columnas
      full_screen = T,
      bslib::card_header("Partos según sexo del bebé"),
      echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
    ),
    
    bslib::card(
      full_screen = T,
      bslib::card_header("Partos según edad de la madre y tipo de parto"),
      echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
    )
  ),
  
  
  # EJEMPLO DE LAYOUT_COLUMNS
  #  bslib::layout_columns(
  #  Cada 12 va a una fila nueva (las columas)
#   col_widths = c(4, 8,
#                 # Algo interno, pero se puede hasta 12
#                 # (entonces en 12 baja de fila)
#                    12),
#                    
#   # Alturas relativas
#   row_heights = c(2, 1), # Probar con c(4, 3) para ver diferencias!
#
## Se podrían aclarar cosas dentro de cada tarjeta o card
## (creo que medio se sobreescriben)   
#   bslib::card(
#     full_screen = TRUE,
#     bslib::card_header("Partos según sexo del bebé"),
#     echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
#   ),
#   
#   bslib::card(
#     full_screen = TRUE,
#     bslib::card_header("Partos según edad de la madre y tipo de parto"),
#     echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
#   ),
#   
#   bslib::card(
#     full_screen = TRUE,
#     bslib::card_header("Datos"),
#     reactable::reactableOutput(outputId = "tabla")
#   )
#   
# )
  
  
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Datos"),
    reactable::reactableOutput(outputId = "tabla")
  )
  
)

server <- function(input, output) {
  
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      dplyr::filter(
        efector == input$efector,
        año == input$año
      )
  })
  
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    datos_filtrados() |>
      dplyr::count(sexo_bb) |> 
      echarts4r::e_chart(x = sexo_bb) |> 
      echarts4r::e_pie(
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list(
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = TRUE)
  })
  
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)





#     Múltiples páginas -> bslib:: (en vez de sidebar)
#     page_sidebar()
#     page_navbar() (navigation bar)
#     nav_panel()  (para definir cada panel)
#     nav_spacer() (controlan alineación y 
#     nav_item()    elementos del navbat
# PANEL SERÍA COMO MÚLTIPLES PÁGINAS

# IMPORTANTE TENER EN CUENTA QUE SI SE USA SIDEBAR EN PAGE_NAVBAR()
# SE COLOCA EL MISMO SIDEBAR EN CADA PAGINA!!!! (ver)

datos <- readr::read_csv("partos_rosario.csv")

ui <- bslib::page_navbar(
  # Se pone casi todo igual, sólo que lo ponemos dentro de page_navbar
  fillable = FALSE, # Más sobre esto en la sección "Scrolling vs Filling"
  
  title = "Partos en Rosario",
  # Puede seguir usando sidebar
  sidebar = bslib::sidebar(
    title = "Panel de Control",
    
    shinyWidgets::pickerInput(
      inputId = "efector",
      label = "Seleccionar Efector",
      choices = datos$efector |> unique() |> sort(),
      selected = "HRSP"
    ),
    
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      choices = datos$año |> unique() |> sort(decreasing = TRUE),
      selected = max(datos$año)
    )
    
  ),
  
  bslib::nav_panel(
    # Dentro de panel puede haber muchas cosas 
    # (en este caso hay dos elementos)
    
    # Panel tablero 
    title = "Tablero", # Nombre que aparece en la lista de páginas
    bslib::card(
      full_screen = TRUE,
      # Nombre de la tarjeta o cuadrito
      bslib::card_header("Partos según sexo del bebé"),
      # Gráfico en si
      echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según edad de la madre y tipo de parto"),
      echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
    )
  ),
  
  bslib::nav_panel(
    title = "Datos",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Datos"),
      reactable::reactableOutput(outputId = "tabla")
    )
  ),
  
  # Para que haga un espacio
  bslib::nav_spacer(),
  
  
  bslib::nav_item(shiny::a("Rosario3", # EL a ES PARA GENERAR UN NUEVO
                           # VINCULO
                           
                           # LINK DE REFERENCIA
                           href = "https://rosario3.com",
                           
                           # ESE ARGUMENTO PARA QUE SE ABRA EN UNA
                           # NUEVA PESTAÑA <3 (odiamos que se
                           # redireccione la misma)
                           target = "_blank"))
  
)

server <- function(input, output) {
  
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      dplyr::filter(
        efector == input$efector,
        año == input$año
      )
  })
  
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    datos_filtrados() |>
      dplyr::count(sexo_bb) |> 
      echarts4r::e_chart(x = sexo_bb) |> 
      echarts4r::e_pie(
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list(
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = TRUE)
  })
  
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)



# SCROLLING VS FILLING
# page_sidebar() y page_navbar() usan FILLING LAYOUTS 
# (los distintos outputs se agrandan o achican para ajustarse 
# a la ventana del navegador)
# Para que no pase eso, se usa min_height o max_height en cards
# (tamaños no deben excederse de ciertos límites)

# Si no queremos que los elementos se agranden o achiquen en una 
# determinada página: fillable = F 
# users pueden desplazarse (scroll) si el espacio vertical que
# ocupan los outputs es mayor a la altura de la ventana 

# Si no se pusiera fillable = F, no se podría escrollear, 
# quedarían las cards compactas (se acomodan por defecto a la 
# altura del navegador)






#                                        #
#                COMPONENTES             #
#                                        #




# Tarjetas o Cards: contenedor rectangular


# Cajas de valores o Value Boxes
# Tipo especial de TARJETA, resalta valores con títulos e íconos
datos <- readr::read_csv("partos_rosario.csv")


# GENERALMENTE SE USA BSICONS
ui <- bslib::page_navbar(
  fillable = FALSE, # Más sobre esto en la sección "Scrolling vs Filling"
  
  title = "Partos en Rosario",
  sidebar = bslib::sidebar(
    title = "Panel de Control",
    
    shinyWidgets::pickerInput(
      inputId = "efector",
      label = "Seleccionar Efector",
      choices = datos$efector |> unique() |> sort(),
      selected = "HRSP"
    ),
    
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      choices = datos$año |> unique() |> sort(decreasing = TRUE),
      selected = max(datos$año)
    )
    
  ),
  
  bslib::nav_panel(
    title = "Tablero",
    
    # AHORA SE PONE UNA FILA (SE ARMA CON LAYOUT_COLUMNS)
    bslib::layout_columns(
      fill = FALSE,
      # Si armáramos el value_box de forma reactiva no estaría bueno
      # Queremos que se actualiza sólo el valor
      bslib::value_box(
        title = "# de Partos",
        # Osea esta parte que sigue sería la que se actualiza
        # TEXT_OUTPUT SERÍA PARA PONER EL ID: n_partos
        value = shiny::textOutput("n_partos"),
        # ACA SE AGREGA
        # (se copia el nombre del ícono)
        showcase = bsicons::bs_icon("person-arms-up")
      ),
      bslib::value_box(
        title = "Edad Gestacional Promedio",
        value = shiny::textOutput("promedio_edad_gestacional"),
        # SE AGREGA
        showcase = bsicons::bs_icon("calendar4-week")
      ),
      bslib::value_box(
        title = "Peso Promedio del Bebé",
        value = shiny::textOutput("promedio_peso_bebe"),
        # SE AGREGA
        showcase = bsicons::bs_icon("handbag")
      )
    ),
    
    # Hay varias cards porque serían varios elementos, y queremos
    # que cada uno tenga una línea nueva
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según sexo del bebé"),
      echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según edad de la madre y tipo de parto"),
      echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
    )
  ),
  
  bslib::nav_panel(
    title = "Datos",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Datos"),
      reactable::reactableOutput(outputId = "tabla")
    )
  ),
  
  bslib::nav_spacer(),
  
  bslib::nav_item(shiny::a("Rosario3",
                           href = "https://rosario3.com",
                           target = "_blank"))
  
)

server <- function(input, output) {
  
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      dplyr::filter(
        efector == input$efector,
        año == input$año
      )
  })
  
  
  # Para hacer un texto
  output$n_partos <- shiny::renderText({
    # Aclara el separador de miles, y después el de decimales
    # PUEDE QUEDAR FEO SI NO FORMATEAMOS LOS NÚMEROS
    scales::label_number(big.mark = ".", 
                         decimal.mark = ",")(nrow(datos_filtrados()))
  }) 
  
  output$promedio_edad_gestacional <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(edad_gestacional_valor) |> 
      # Elimina los valores faltantes, si es que hay
      mean(na.rm = TRUE)
      # INVESTIGAR LABEL_NUMBER
    scales::label_number(suffix = " semanas", 
                         decimal.mark = ",",
                         accuracy = 0.01)(promedio)
  })
  
  # CONVIENE PONER LO QUE ES Y DESPUÉS A QUÉ CORRESPONDE
  # (promedio_despuésdeque)
  output$promedio_peso_bebe <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(peso) |> 
      mean(na.rm = TRUE)
    # Aclara que es en gramos, etc
    scales::label_number(suffix = "g",
                         big.mark = ".", 
                         decimal.mark = ",")(promedio)
    
  })
  
  
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    datos_filtrados() |>
      dplyr::count(sexo_bb) |> 
      echarts4r::e_chart(x = sexo_bb) |> 
      echarts4r::e_pie(
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list(
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = TRUE)
  })
  
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)




# Personalizar Apariencia o Theming
# bslib::bs_theme(bootswatch)
# PARA INVESTIGAR:
# https://rstudio.github.io/bslib/articles/theming/index.html

datos <- readr::read_csv("partos_rosario.csv")

ui <- bslib::page_navbar(
  fillable = FALSE, 
  
  # ACA CAMBIA EL TEMA!!! (SÓLO AGREGA ESTAS DOS SENTENCIAS)
  theme = bslib::bs_theme(
    bootswatch = "sketchy"
  ),
  
  title = "Partos en Rosario",
  sidebar = bslib::sidebar(
    title = "Panel de Control",
    
    shinyWidgets::pickerInput(
      inputId = "efector",
      label = "Seleccionar Efector",
      choices = datos$efector |> unique() |> sort(),
      selected = "HRSP"
    ),
    
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      choices = datos$año |> unique() |> sort(decreasing = TRUE),
      selected = max(datos$año)
    )
    
  ),
  
  bslib::nav_panel(
    title = "Tablero",
    
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "# de Partos",
        value = shiny::textOutput("n_partos"),
        showcase = bsicons::bs_icon("person-arms-up")
      ),
      bslib::value_box(
        title = "Edad Gestacional Promedio",
        value = shiny::textOutput("promedio_edad_gestacional"),
        showcase = bsicons::bs_icon("calendar4-week")
      ),
      bslib::value_box(
        title = "Peso Promedio del Bebé",
        value = shiny::textOutput("promedio_peso_bebe"),
        showcase = bsicons::bs_icon("handbag")
      )
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según sexo del bebé"),
      echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según edad de la madre y tipo de parto"),
      echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
    )
  ),
  
  bslib::nav_panel(
    title = "Datos",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Datos"),
      reactable::reactableOutput(outputId = "tabla")
    )
  ),
  
  bslib::nav_spacer(),
  
  bslib::nav_item(shiny::a("Rosario3", href = "https://rosario3.com", target = "_blank"))
  
)

server <- function(input, output) {
  
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      dplyr::filter(
        efector == input$efector,
        año == input$año
      )
  })
  
  output$n_partos <- shiny::renderText({
    scales::label_number(big.mark = ".", decimal.mark = ",")(nrow(datos_filtrados()))
  })
  
  output$promedio_edad_gestacional <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(edad_gestacional_valor) |> 
      mean(na.rm = TRUE)
    
    scales::label_number(suffix = " semanas", decimal.mark = ",", accuracy = 0.01)(promedio)
  })
  
  output$promedio_peso_bebe <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(peso) |> 
      mean(na.rm = TRUE)
    scales::label_number(suffix = "g", big.mark = ".", decimal.mark = ",")(promedio)
    
  })
  
  
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    datos_filtrados() |>
      dplyr::count(sexo_bb) |> 
      echarts4r::e_chart(x = sexo_bb) |> 
      echarts4r::e_pie(
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list(
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = TRUE)
  })
  
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)


# Se pueden crear temas propios:
datos <- readr::read_csv("partos_rosario.csv")

ui <- bslib::page_navbar(
  fillable = FALSE, 
  # TEMA PERSONALIZADO
  theme = bslib::bs_theme(
    bg = "#000000",
    fg = "#FFFFFF",
    primary = "#9600FF",
    secondary = "#1900A0",
    success = "#38FF12",
    info = "#00F5FB",
    warning = "#FFF100",
    danger = "#FF00E3",
    # Cambiar la fuente, no me gusta :/
    base_font = "Marker Felt",
    heading_font = "Marker Felt",
    code_font = "Chalkduster"
  ),
  
  title = "Partos en Rosario",
  sidebar = bslib::sidebar(
    title = "Panel de Control",
    
    shinyWidgets::pickerInput(
      inputId = "efector",
      label = "Seleccionar Efector",
      choices = datos$efector |> unique() |> sort(),
      selected = "HRSP"
    ),
    
    shinyWidgets::pickerInput(
      inputId = "año",
      label = "Seleccionar Año",
      choices = datos$año |> unique() |> sort(decreasing = TRUE),
      selected = max(datos$año)
    )
    
  ),
  
  bslib::nav_panel(
    title = "Tablero",
    
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        title = "# de Partos",
        value = shiny::textOutput("n_partos"),
        showcase = bsicons::bs_icon("person-arms-up")
      ),
      bslib::value_box(
        title = "Edad Gestacional Promedio",
        value = shiny::textOutput("promedio_edad_gestacional"),
        showcase = bsicons::bs_icon("calendar4-week")
      ),
      bslib::value_box(
        title = "Peso Promedio del Bebé",
        value = shiny::textOutput("promedio_peso_bebe"),
        showcase = bsicons::bs_icon("handbag")
      )
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según sexo del bebé"),
      echarts4r::echarts4rOutput(outputId = "graf_sexo_bebe")
    ),
    
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Partos según edad de la madre y tipo de parto"),
      echarts4r::echarts4rOutput(outputId = "graf_edad_madre_tipo_parto")
    )
  ),
  
  bslib::nav_panel(
    title = "Datos",
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Datos"),
      reactable::reactableOutput(outputId = "tabla")
    )
  ),
  
  bslib::nav_spacer(),
  
  bslib::nav_item(shiny::a("Rosario3", href = "https://rosario3.com", target = "_blank"))
  
)

server <- function(input, output) {
  
  datos_filtrados <- shiny::eventReactive(c(input$efector, input$año), {
    datos |> 
      dplyr::filter(
        efector == input$efector,
        año == input$año
      )
  })
  
  output$n_partos <- shiny::renderText({
    scales::label_number(big.mark = ".", decimal.mark = ",")(nrow(datos_filtrados()))
  })
  
  output$promedio_edad_gestacional <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(edad_gestacional_valor) |> 
      mean(na.rm = TRUE)
    
    scales::label_number(suffix = " semanas", decimal.mark = ",", accuracy = 0.01)(promedio)
  })
  
  output$promedio_peso_bebe <- shiny::renderText({
    promedio <- datos_filtrados() |> 
      dplyr::pull(peso) |> 
      mean(na.rm = TRUE)
    scales::label_number(suffix = "g", big.mark = ".", decimal.mark = ",")(promedio)
    
  })
  
  
  output$graf_sexo_bebe <- echarts4r::renderEcharts4r({
    datos_filtrados() |>
      dplyr::count(sexo_bb) |> 
      echarts4r::e_chart(x = sexo_bb) |> 
      echarts4r::e_pie(
        serie = n,
        name = "Número de partos", # Cambiar el nombre de la serie
        itemStyle = list(
          borderColor = "black" # Agregar bordes al gráfico
        ),
        label = list(
          position = "inside", # Ubicar los labels dentro del gráfico
          formatter = "{b}\n\n{d}%", # Agregar porcentajes a los labels
          fontSize = 15 # Modificar el tamaño de fuente del label
        ),
        # Agregar "énfasis"
        emphasis = list(
          label = list(
            fontSize = 20,
            fontWeight = "bold"
          )
        )
      ) |> 
      echarts4r::e_legend(show = FALSE) |> # Remover guía
      echarts4r::e_tooltip() |> # Agregar tooltip
      echarts4r::e_color(c("lightgrey", "white")) # Definir colores para cada categoría
    
  })
  
  output$graf_edad_madre_tipo_parto <- echarts4r::renderEcharts4r({
    datos_procesados <- datos_filtrados() |> 
      dplyr::count(rango_edad, parto) |> 
      dplyr::group_by(parto)
    
    datos_procesados |>
      echarts4r::e_chart(x = rango_edad) |> 
      echarts4r::e_bar(
        serie = n,
        stack = "barras_apiladas" # Para nuestro ejemplo, este nombre no es importante
      ) |> 
      echarts4r::e_flip_coords() |> 
      echarts4r::e_tooltip(trigger = "axis") |> 
      echarts4r::e_grid(containLabel = TRUE)
  })
  
  output$tabla <- reactable::renderReactable({
    datos_filtrados() |> 
      reactable::reactable()
  })
}

shiny::shinyApp(ui, server)

#                    ESTILO DE LOS GRÁFICOS               #
#                                                         #

# Ver colores de gráficos y fondos
# EN GGPLOT2 HAY UN PAQUETE: THEMATIC PARA APLICAR AUTOMÁTICAMENTE
# A LOS GRÁFICOS 
# Sería thematic::thematic_shiny()
# VISUALIZACIONES DINÁMICAS!!!!




#



ui <- bslib::page(
  bslib::layout_columns(
    col_widths = c(4, 8),
    shinyWidgets::pickerInput(
      inputId = "ID",
      label = "Seleccionar paquete",
      choices = installed.packages()[,1],
      multiple = FALSE,
      options = list(`live-search` = TRUE)
    ),
    shiny::tagList(
      # Poner paquetes para que no tire error
      shiny::h4("Cita del paquete"),
      shiny::textOutput("salida")
    )
  )
)

server <- function(input, output) {
  output$salida <- shiny::renderText({format(utils::citation(input$ID), style = "text")})
}

shiny::shinyApp(ui, server)




















