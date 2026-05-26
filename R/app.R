#' Launch oglcnac Shiny App
#'
#' This function launches a Shiny App for uploading, processing,
#' and downloading UniProt data in CSV, TSV, or Excel format.
#' Users can upload data, preview it, and select specific columns for processing.
#' The processed data can be viewed and downloaded.
#'
#' @return None
#' @export
#' @examples
#' if (interactive()) {
#'   oglcnac::launch_app()
#' }
launch_app <- function() {
  required_packages <- c("shiny", "DT", "bslib", "readxl")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_packages) > 0) {
    cli::cli_abort("Install GUI packages before launching the app: {missing_packages}")
  }

  fluidPage <- shiny::fluidPage
  sidebarLayout <- shiny::sidebarLayout
  sidebarPanel <- shiny::sidebarPanel
  mainPanel <- shiny::mainPanel
  tabsetPanel <- shiny::tabsetPanel
  tabPanel <- shiny::tabPanel
  fileInput <- shiny::fileInput
  numericInput <- shiny::numericInput
  selectInput <- shiny::selectInput
  actionButton <- shiny::actionButton
  downloadButton <- shiny::downloadButton
  verbatimTextOutput <- shiny::verbatimTextOutput
  renderText <- shiny::renderText
  observeEvent <- shiny::observeEvent
  updateSelectInput <- shiny::updateSelectInput
  downloadHandler <- shiny::downloadHandler
  reactiveVal <- shiny::reactiveVal
  req <- shiny::req
  shinyApp <- shiny::shinyApp
  fluidRow <- shiny::fluidRow
  column <- shiny::column
  tags <- shiny::tags
  p <- shiny::p
  hr <- shiny::hr
  textInput <- shiny::textInput
  checkboxInput <- shiny::checkboxInput
  dialogViewer <- shiny::dialogViewer
  runGadget <- shiny::runGadget
  DTOutput <- DT::DTOutput
  renderDT <- DT::renderDT
  datatable <- DT::datatable
  bs_theme <- bslib::bs_theme
  read_excel <- readxl::read_excel

  # Define the UI
  ui <- fluidPage(
    theme = bs_theme(
      version = 5L,
      primary = "#112446",
      secondary = "#cccccc",
      preset = "bootstrap",
      font_scale = 0.9,
      "accordion-body-padding-y" = "3px", # Reduced padding
      "accordion-body-padding-x" = "3px" # Reduced padding
    ),

    # Styled title area with compact padding
    tags$div(
      class = "title-area",
      style = "background-color: #112446; padding: 10px; color: white; text-align: center; margin-bottom: 10px;", # Reduced padding
      tags$h2("OGlcNAc App")
    ),

    # Increased container width to reduce scrolling and condensed layout
    tags$div(
      class = "container-fluid",
      style = "max-width: 1300px;", # Increase the width slightly to fit more content
      sidebarLayout(
        sidebarPanel(
          width = 4, # Adjust sidebar width to leave more space for content

          p("Upload Atlas curator files in CSV, TSV, or Excel format. The app can validate Atlas columns,
             preserve dataset-I/dataset-II labels, enrich UniProt fields, and export clean CSV files."),
          p("Please select a file to begin the data processing workflow."),
          hr(),
          fileInput("file", "Upload your Excel, CSV, or TSV file", accept = c(".xlsx", ".csv", ".tsv")),
          hr(),


          # Dropdown menus arranged in pairs to save vertical space
          fluidRow(
            column(6, numericInput("n_rows", "Process first N rows", value = 20, min = 1)),
            column(6, selectInput(
              "atlas_dataset",
              "Atlas Dataset",
              choices = c(
                "Do not change" = "",
                "Unambiguous sites (dataset-I)" = "unambiguous",
                "Ambiguous sites (dataset-II)" = "ambiguous"
              )
            ))
          ),
          fluidRow(
            column(6, selectInput("accession_col", "Accession Column", choices = NULL)),
            column(6, selectInput("accession_source_col", "Source Column", choices = NULL))
          ),
          fluidRow(
            column(6, selectInput("entry_name_col", "Entry Name Column", choices = NULL)),
            column(6, selectInput("protein_name_col", "Protein Name Column", choices = NULL))
          ),
          fluidRow(
            column(6, selectInput("gene_name_col", "Gene Name Column", choices = NULL)),
            column(6, checkboxInput("use_cache", "Use UniProt cache", value = TRUE))
          ),
          textInput("cache_path", "Cache path", value = "~/.cache/oglcnac/uniprot-cache.rds"),

          # Add horizontal line to separate sections
          hr(),

          # Action buttons
          actionButton("validate", "Validate Atlas Data", class = "btn-secondary"),
          actionButton("process", "Process UniProt Data", class = "btn-primary"),
          downloadButton("download", "Download Processed CSV", class = "btn-success")
        ),
        mainPanel(
          width = 8, # Expand the main panel to fit more content
          tabsetPanel(
            tabPanel("Preview Data", DTOutput("preview")),
            tabPanel("Processed Data", DTOutput("result")),
            tabPanel("Validation", DTOutput("validation")),
            tabPanel("Status", verbatimTextOutput("status"))
          )
        )
      )
    )
  )

  # Define the server logic
  server <- function(input, output, session) {
    # Reactive value to store uploaded data
    uploaded_data <- reactiveVal()
    processed_data <- reactiveVal()
    validation_result <- reactiveVal()

    # Reactive value to store logs
    logs <- reactiveVal("")

    # Custom log function to capture output
    log_console <- function(...) {
      current_log <- logs()
      new_log <- paste0(current_log, paste(..., collapse = " "), "\n")
      logs(new_log)
    }

    # Display logs in real-time
    output$status <- renderText({
      logs()
    })

    output$validation <- renderDT({
      result <- validation_result()
      if (is.null(result)) {
        return(datatable(data.frame(message = "No validation has been run."), options = list(dom = "t")))
      }
      messages <- c(result$errors, result$warnings)
      if (!length(messages)) {
        table <- data.frame(type = "ok", message = "Validation passed.", stringsAsFactors = FALSE)
      } else {
        table <- data.frame(
          type = c(rep("error", length(result$errors)), rep("warning", length(result$warnings))),
          message = messages,
          stringsAsFactors = FALSE
        )
      }
      datatable(
        table,
        options = list(scrollX = TRUE)
      )
    })

    # Handle file upload and preview
    observeEvent(input$file, {
      req(input$file)
      ext <- tools::file_ext(input$file$name)

      # Load data based on file extension
      df <- switch(ext,
        csv = utils::read.csv(input$file$datapath),
        tsv = utils::read.delim(input$file$datapath),
        xlsx = read_excel(input$file$datapath),
        stop("Invalid file format")
      )

      uploaded_data(df)

      # Log file upload info
      log_console("File uploaded: ", input$file$name)

      # Update dropdown choices based on data columns
      updateSelectInput(session, "accession_col", choices = names(df), selected = "accession")
      updateSelectInput(session, "accession_source_col", choices = names(df), selected = "accession_source")
      updateSelectInput(session, "entry_name_col", choices = names(df), selected = "entry_name")
      updateSelectInput(session, "protein_name_col", choices = names(df), selected = "protein_name")
      updateSelectInput(session, "gene_name_col", choices = names(df), selected = "gene_name")

      # Preview the first 10 rows of the dataset without text wrapping and with horizontal scrolling
      output$preview <- renderDT({
        datatable(df, options = list(scrollX = TRUE, columnDefs = list(list(targets = "_all", className = "dt-nowrap"))))
      })

      # Log preview status
      log_console("Preview of the loaded data generated.")
    })

    observeEvent(input$validate, {
      req(uploaded_data())

      result <- validate_atlas_data(uploaded_data(), dataset = input$atlas_dataset)
      validation_result(result)
      if (result$valid) {
        log_console("Atlas validation passed.")
      } else {
        log_console("Atlas validation failed: ", paste(result$errors, collapse = " | "))
      }
      if (length(result$warnings)) {
        log_console("Validation warnings: ", paste(result$warnings, collapse = " | "))
      }
    })

    # Process data when the process button is clicked
    observeEvent(input$process, {
      req(uploaded_data())

      # Log processing start
      log_console("Processing data...")

      # Limit rows if the user specified N rows
      df <- uploaded_data()
      if (!is.null(input$n_rows) && input$n_rows > 0) {
        df <- utils::head(df, input$n_rows)
      }

      validation <- validate_atlas_data(df, dataset = input$atlas_dataset)
      validation_result(validation)
      if (!validation$valid) {
        log_console("Atlas validation failed before processing: ", paste(validation$errors, collapse = " | "))
        return(NULL)
      }

      cache_path <- if (isTRUE(input$use_cache)) path.expand(input$cache_path) else NULL
      processed_df <- process_tibble_uniprot_cached(df,
        cache_path = cache_path,
        accession_col = input$accession_col,
        accession_source_col = input$accession_source_col,
        entry_name_col = input$entry_name_col,
        protein_name_col = input$protein_name_col,
        gene_name_col = input$gene_name_col
      )
      processed_df <- prepare_atlas_data(processed_df, dataset = input$atlas_dataset)

      # Store the processed data
      processed_data(processed_df)

      # Display processed data without text wrapping and with horizontal scrolling
      output$result <- renderDT({
        datatable(processed_df, options = list(scrollX = TRUE, columnDefs = list(list(targets = "_all", className = "dt-nowrap"))))
      })

      # Log processing completion
      log_console("Data processed successfully!")
    })

    # Allow user to download processed data
    output$download <- downloadHandler(
      filename = function() {
        paste("processed_data.csv")
      },
      content = function(file) {
        df <- prepare_atlas_data(processed_data(), dataset = input$atlas_dataset)
        # Set `na = ''` to replace NA values with an empty string in the output file
        utils::write.csv(df, file, row.names = FALSE, na = "")

        # Log download event
        log_console("Processed data downloaded.")
      }
    )
  }

  # Launch the app as a shiny gadget in a dialog with a slightly larger size
  viewer <- dialogViewer("OGlcNAc App", width = 1200, height = 800)
  runGadget(shinyApp(ui, server), viewer = viewer)
}
