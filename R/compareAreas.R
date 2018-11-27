#' Compare spatial areas
#'
#' Call this function to open a Shiny Gadget.
#'
#' @export
compareAreas <- function() {

  df <- data.frame(lon = -121.9738, lat = 37.3407, Area = "Santa Clara", Unit = "") %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>%
    sf::st_transform(3488)

  create_area_box <- function(area, unit) {
    mArea <- measurements::conv_unit(area, unit, "m2")
    buff_dist <- (sqrt(mArea) / 2)
    df$unit <- unit

    df_buff <- sf::st_buffer(df, buff_dist)

    df_bbox <- df_buff %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_sf() %>%
      sf::st_transform(3488) %>%
      dplyr::mutate(area2 = round(sf::st_area(.)),
                    Area = area,
                    AreaLab = glue::glue("{area} square {unit}")) %>%
      dplyr::mutate(Unit = dplyr::case_when(
        grepl("m2", AreaLab) ~ "meters",
        grepl("km2", AreaLab) ~ "kilometers",
        grepl("ft2", AreaLab) ~ "feet",
        grepl("mi2", AreaLab) ~ "miles",
        grepl("acre", AreaLab) ~ "acres"))

    sf::st_join(df_bbox, df)
  }

  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("compareAreas", left = NULL),
    miniUI::miniContentPanel(
      shiny::fillRow(
        flex = c(2,1,5),
        shiny::fillCol(
          shiny::numericInput("area", "Area:", value = 0),
          shiny::selectInput("units", "Units:", c("Square Meters" = "m2",
                                                  "Square Kilometers" = "km2",
                                                  "Square Feet" = "ft2",
                                                  "Square Miles" = "mi2",
                                                  "Acres" = "acre"),
                             selectize = FALSE),
          shiny::br(),
          shiny::actionButton("btn", "Add Layer to Map",
                              width = "100%",
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          shiny::actionButton("clear", "Clear Layers",
                              width = "100%",
                              style = "color: #fff; background-color: #ce3c23; border-color: #2e6da4"),
          height = "70%"
        ),
        shiny::fillCol(
          shiny::br()
        ),
        shiny::fillCol(
          leaflet::leafletOutput("mv"),
          shiny::br(),
          height = "100%"
        )
      )
    )
  )

  server <- function(input, output, session) {

    values <- reactiveValues(
      areas_list = df,
      default = TRUE
    )

    observeEvent(input$btn, {
      box <- create_area_box(input$area, input$units)
      if (values$default) {
        values$areas_list <- box
        values$default <- FALSE
      } else {
        values$areas_list <- rbind(values$areas_list, box)
      }
    })

    observeEvent(input$clear, {
      values$areas_list <- df
      values$default <- TRUE
    })

    output$mv <- leaflet::renderLeaflet({
      layers <- values$areas_list
      if (!values$default) {
        layers <- values$areas_list %>%
          dplyr::arrange(desc(area2)) %>%
          dplyr::select(Label = AreaLab, Area = Area.x, Unit = Unit.x)
      }
      mapview::mapview(layers, zcol = "Area", alpha.regions = 0.25)@map
    })

    observeEvent(input$done, {
      stopApp(returnValue = invisible())
    })
  }
  shiny::runGadget(ui, server)
}
