library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(arrow)
library(dplyr)
library(tidyr)
library(sf)

# CARGA
mapa <- readRDS("data/mapa_ready.rds")
datos <- read_parquet("data/bd_resumen.parquet")
ui <- page_sidebar(
  title = "Monitor Laboral y de Cuidados",
  sidebar = sidebar(
    selectInput("anio", "Año:", c("Todos los años" = "Todos", sort(unique(datos$anio)))),
    selectInput("entidad", "Entidad:", c("Todos los estados" = "Todos", sort(unique(datos$NOMGEO)))),
    hr(),
    actionButton("reset", "Reiniciar Filtros", icon = icon("refresh"), class = "btn-primary")
  ),
  
  # FILA DE KPIs (Value Boxes)
  layout_columns(
    fill = FALSE,
    height = "100px",
    value_box(
      title = "Tasa Ocupación Hombres",
      value = uiOutput("tasa_h"),
      theme = "blue"
    ),
    value_box(
      title = "Tasa Ocupación Mujeres",
      value = uiOutput("tasa_m"),
      theme = "danger"
    ),
    value_box(
      title = "Brecha de Género",
      value = uiOutput("brecha"),
      theme = "purple"
    ),
    value_box(
      title = "Cuidado: Mujeres No Ocup.",
      value = uiOutput("horas_m_no"),
      theme = "warning"
    )
  ),

 # Fila 1: Gráfica de Estados (Izquierda) y Mapa (Derecha)
  layout_columns(
    col_widths = c(4, 8), # 4 unidades para la gráfica, 8 para el mapa
    
    # IZQUIERDA SUPERIOR: Gráfica de barras de estados
    card(
  full_screen = TRUE, 
  card_header("Desigualdad por Entidad (Brecha H-M)"), 
  # Envolvemos en un div con scroll
  div(
    style = "max-height: 450px; overflow-y: auto; overflow-x: hidden;",
    plotlyOutput("barras", height = "1000px") # Altura mayor para forzar el scroll
  )
),
    
    # DERECHA SUPERIOR: Mapa
    card(
      full_screen = TRUE,
      card_header("Mapa de Horas de cuidado"), 
      leafletOutput("mapa", height = 450)
    )
  ),
  
  # Fila 2: Gráfica de Evolución Temporal (Abajo, ocupando todo o compartido)
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE, 
      card_header("Evolución Temporal del Cuidado"), 
      plotlyOutput("serie")
    ),
    card(
      full_screen = TRUE,
      card_header("Tiempo de Cuidado por Grupo de Edad"),
      plotlyOutput("grafica_edad")
    )
  )
)
server <- function(input, output,session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = "Todos")
    updateSelectInput(session, "entidad", selected = "Todos")
  })

  # --- LÓGICA DE DATOS REACTIVOS (Igual a la anterior) ---
  datos_reactivos <- reactive({
    d <- datos
    if (input$entidad != "Todos") d <- d %>% filter(NOMGEO == input$entidad)
    
    d_anio <- d
    if (input$anio != "Todos") d_anio <- d_anio %>% filter(anio == input$anio)
    
    list(general = d, filtrado = d_anio)
  })
  

  # --- MAPA BASE ---
  output$mapa <- renderLeaflet({
    leaflet(mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
setView(lng = -102.5, lat = 23.8, zoom = 4)
  })

  # --- ACTUALIZACIÓN DINÁMICA DEL MAPA (Zoom y Colores) ---
  observe({
    # 1. Preparamos los datos: unimos hombres y mujeres en la misma fila por estado
    mapa_vals <- datos_reactivos()$filtrado %>%
      group_by(CVEGEO, NOMGEO, sexo_lab) %>%
      summarise(
        prom_horas = sum(cuidados_total) / sum(peso_cuidadores), 
        .groups = "drop"
      ) %>%
      # Convertimos a formato ancho para tener columnas separadas por sexo
      tidyr::pivot_wider(names_from = sexo_lab, values_from = prom_horas) %>%
      # Calculamos un promedio general para el color del mapa (opcional)
      mutate(prom_general = (replace_na(Hombre, 0) + replace_na(Mujer, 0)) / 2)
    
    mapa_data <- mapa %>% left_join(mapa_vals, by = "CVEGEO")
    
    # La paleta de colores puede seguir basada en el promedio general o en el de mujeres
    pal <- colorNumeric("Purples", mapa_data$prom_general, na.color = "transparent")
    
    # 2. Proxy para actualizar el mapa
    proxy <- leafletProxy("mapa", data = mapa_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(prom_general), 
        color = "white", 
        weight = 1, 
        fillOpacity = 0.7,
        # AQUÍ ESTÁ EL CAMBIO: El label ahora muestra ambos sexos
        label = ~paste0(
          "<strong>", NOMGEO.x, "</strong><br/>",
          "Mujeres: ", round(Mujer, 1), " hrs<br/>",
          "Hombres: ", round(Hombre, 1), " hrs"
        ) %>% lapply(htmltools::HTML), # Importante para que reconozca el <br/>
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          direction = "auto"
        )
      )
    
    # 3. LÓGICA DE ZOOM
    if (input$entidad == "Todos") {
      proxy %>% setView(lng = -102.5, lat = 23.8, zoom = 4) 
    } else {
     geom_estado <- mapa %>% filter(NOMGEO == input$entidad)
      if(nrow(geom_estado) > 0) {
        bbox <- st_bbox(geom_estado)
        proxy %>% fitBounds(
          bbox[["xmin"]], bbox[["ymin"]], 
          bbox[["xmax"]], bbox[["ymax"]]
        )
      }
    }
  })

  # 2. SERIE DE TIEMPO (Evolución por Sexo)
  # Esta gráfica ignora el filtro de "Año" para poder mostrar la línea
  output$serie <- renderPlotly({
  df_serie <- datos_reactivos()$general %>% 
    group_by(anio, sexo_lab) %>%
    summarise(
      # Promedio solo sobre quienes reportaron horas > 0
      Ocupado = sum(cuidados_ocu) / sum(peso_cuid_ocu),
      `No Ocupado` = sum(cuidados_no_ocu) / sum(peso_cuid_no_ocu),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      cols = c(Ocupado, `No Ocupado`), 
      names_to = "Estado", 
      values_to = "Horas"
    )

  p <- ggplot(df_serie, aes(x = anio, y = Horas, color = sexo_lab, linetype = Estado, group = interaction(sexo_lab, Estado))) +
    geom_line() + geom_point() +
    scale_color_manual(values = c("Hombre" = "#2c7bb6", "Mujer" = "#d7191c")) +
    theme_minimal() +
    labs(title = "Horas Promedio de la Población Cuidadora a la semana", y = "Horas por semana")
  
  ggplotly(p)
})
  # 3. BARRAS (Cuidados por Sexo)
  # 3. PIRÁMIDE (Ocupados vs. No Ocupados)
  # 3. BARRAS AGRUPADAS (Distribución 100% por Sexo)
 
 output$barras <- renderPlotly({
  # 1. Procesamiento de datos para calcular la brecha
  df_brecha <- datos_reactivos()$filtrado %>%
    group_by(NOMGEO, sexo_lab) %>%
    summarise(
      horas = sum(cuidados_total) / sum(peso_cuidadores),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(names_from = sexo_lab, values_from = horas) %>%
    mutate(
      brecha = Mujer - Hombre,
      NOMGEO = case_when(
        NOMGEO == "Coahuila de Zaragoza" ~ "Coahuila",
        NOMGEO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        NOMGEO == "Michoacán de Ocampo" ~ "Michoacán",
       TRUE ~ NOMGEO),
      # Creamos el texto para el hover de Plotly
      texto_hover = paste0(
        "<b>", NOMGEO, "</b><br>",
        "Mujeres: ", round(Mujer, 1), " hrs<br>",
        "Hombres: ", round(Hombre, 1), " hrs<br>",
        "Brecha: ", round(brecha, 1), " hrs"
      )
    ) %>%
    # Ordenamos: el estado con mayor desigualdad queda arriba
    arrange(brecha) %>% 
    mutate(NOMGEO = factor(NOMGEO, levels = NOMGEO)) 

   
   if (input$entidad != "Todos") {
    # Si hay un estado seleccionado, ese va primero, el resto después
    df_brecha <- df_brecha %>%
      mutate(es_seleccionado = ifelse(NOMGEO == input$entidad, 1, 0)) %>%
      arrange(es_seleccionado, brecha) # El 1 (seleccionado) quedará al final del factor (arriba en el eje Y)
  } else {
    # Si no hay filtro, orden normal por brecha
    df_brecha <- df_brecha %>% arrange(brecha)
  }
  
  # Aplicamos el orden al factor
  df_brecha <- df_brecha %>% 
    mutate(NOMGEO = factor(NOMGEO, levels = NOMGEO))
  # 2. Construcción de la gráfica divergente
  p <- ggplot(df_brecha) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
    
    # Barra de fondo
    geom_segment(aes(y = NOMGEO, yend = NOMGEO, x = -Hombre, xend = Mujer), 
                 color = "#e0e0e0", size = 2) +
    
    # Puntos
    geom_point(aes(x = -Hombre, y = NOMGEO, text = texto_hover), 
               color = "#2c7bb6", size = 3) +
    geom_point(aes(x = Mujer, y = NOMGEO, text = texto_hover), 
               color = "#d7191c", size = 3) +
    
    # Nombre del estado (centrado)
    geom_text(aes(x = 0, y = NOMGEO, label = NOMGEO), 
              vjust = -1, size = 3, color = "black", fontface = "bold") +
    
    # --- CAMBIOS AQUÍ: Valores hacia afuera ---
    # Hombres: Empujado a la izquierda (nudge_x negativo)
    geom_text(aes(x = -Hombre, y = NOMGEO, label = round(Hombre, 1)), 
              nudge_x = -7,  
              hjust = 1,      
              size = 3.5, 
              color = "#2c7bb6") +
    
    # Mujeres: Más a la derecha (nudge_x = 2.5)
    geom_text(aes(x = Mujer, y = NOMGEO, label = round(Mujer, 1)), 
              nudge_x = 7,   
              hjust = 0,      
              size = 3.5, 
              color = "#d7191c")+
    
    theme_minimal() +
    # Expandimos un poco los límites de X para que los números no se corten
    expand_limits(x = c(max(df_brecha$Hombre, na.rm = TRUE) * -1.2, 
                        max(df_brecha$Mujer, na.rm = TRUE) * 1.2)) +
    labs(
      title = "Brecha de Cuidados: Horas Semanales",
      x = "Horas (Hombres ← | → Mujeres)",
      y = NULL
    ) +
    theme(
      axis.text.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 20, r = 30, b = 20, l = 30)
    )
alt_dinamica <- if(input$entidad != "Todos") 300 else 1000

  ggplotly(p, tooltip = "text", height = alt_dinamica) %>% 
    config(displayModeBar = FALSE)
 })
  output$grafica_edad <- renderPlotly({
    # 1. Procesamiento de datos
    df_edad <- datos_reactivos()$filtrado %>%
      group_by(grupo_edad, sexo_lab) %>%
      summarise(
        horas = sum(cuidados_total) / sum(peso_cuidadores),
        .groups = "drop"
      ) %>% 
      filter(grupo_edad != "No sabe / No contesta") %>%
      mutate(label_text = paste0(round(horas, 1), " hrs"))


browser()
    p <- ggplot(df_edad, aes(x = grupo_edad, y = horas, fill = sexo_lab, text = label_text)) +
      # position_dodge hace que las barras estén una al lado de la otra
      geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
      
      # Añadimos las etiquetas de texto sobre las barras
      geom_text(
        aes(label = round(horas, 1)),
        position = position_dodge(width = 0.9),
        vjust = -2, # Las empuja hacia arriba (afuera de la barra)
        size = 3, 
      ) +
      
      scale_fill_manual(values = c("Hombre" = "#2c7bb6", "Mujer" = "#d7191c")) +
      theme_minimal() +
      # Expandimos el eje Y un poco para que las etiquetas superiores no se corten
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = "Tiempo promedio semanal de cuidado",
        x = "Grupo de Edad",
        y = "Horas Promedio",
        fill = "Sexo"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank() # Limpia el ruido visual
      )

    # Convertimos a Plotly
    # Usamos tooltip = "text" para que el hover sea limpio
    ggplotly(p, tooltip = "text") %>% 
      config(displayModeBar = FALSE)
  })
  indicadores <- reactive({
  df <- datos_reactivos()$filtrado
  
  # 1. TASAS DE OCUPACIÓN (Usamos el peso total de la población)
  # Denominador: peso_total_pob
  t_h <- (sum(df$ocupados_total[df$sexo == 1]) / sum(df$peso_total_pob[df$sexo == 1])) * 100
  t_m <- (sum(df$ocupados_total[df$sexo == 2]) / sum(df$peso_total_pob[df$sexo == 2])) * 100
  
  # 2. HORAS CUIDADO (Usamos solo el peso de quienes SI cuidan)
  # Denominador: peso_cuid_no_ocu
  # Esto evita que los "ceros" de la población no ocupada bajen el promedio
  h_m_no <- sum(df$cuidados_no_ocu[df$sexo == 2]) / sum(df$peso_cuid_no_ocu[df$sexo == 2])
  
  list(
    tasa_h = round(t_h, 1),
    tasa_m = round(t_m, 1),
    brecha = round(t_h - t_m, 1),
    horas_m_no = round(h_m_no, 1)
  )
})
  
  # Renderizar los valores en la UI
  output$tasa_h <- renderText({ paste0(indicadores()$tasa_h, "%") })
  output$tasa_m <- renderText({ paste0(indicadores()$tasa_m, "%") })
  output$brecha <- renderText({ paste0(indicadores()$brecha, " p.p.") }) # puntos porcentuales
  output$horas_m_no <- renderText({ paste0(indicadores()$horas_m_no, " hrs") })

}

shinyApp(ui, server)
