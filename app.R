library(shiny)
library(XML)
library(ggplot2)
library(dplyr)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Rack n' Roll - XML Editor"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose XML File", accept = c(".xml")),
      selectInput("location", "Select Location with 'No Tube':", choices = NULL),
      textInput("new_code", "Enter New Code:", value = ""),
      actionButton("update_code", "Update Code"),
      actionButton("save", "Save Changes"),
      downloadButton("download_file", "Download Edited XML"),
      tags$hr()
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store XML data and metadata
  xml_data <- reactiveVal(NULL)
  rack_id <- reactiveVal(NULL)
  date <- reactiveVal(NULL)
  
  # Observe file upload
  observeEvent(input$file, {
    req(input$file)
    
    # Read and parse XML file
    xml_file <- xmlTreeParse(input$file$datapath, useInternalNodes = TRUE)
    xml_root <- xmlRoot(xml_file)
    
    # Extract Rack_ID and Date
    rack_id_value <- xmlValue(xml_root[["Rack_ID"]])
    date_value <- xmlValue(xml_root[["Date"]])
    
    # Update reactive values for Rack_ID and Date
    rack_id(rack_id_value)
    date(date_value)
    
    # Extract tubes data
    tubes <- xmlToDataFrame(nodes = getNodeSet(xml_root, "//Tube"))
    
    # Update reactive value
    xml_data(tubes)
    
    # Update choices for selectInput
    updateSelectInput(session, "location", 
                      choices = tubes$Location[tubes$Code == "No Tube"])
  })
  
  # Render plot
  output$plot <- renderPlot({
    req(xml_data())
    
    # Prepare data for plot
    tubes <- xml_data()
    tubes$Status <- ifelse(tubes$Code == "No Tube", "No Tube", "Tube Present")
    
    ggplot(tubes, aes(x = substring(Location, 2), y = substring(Location, 1, 1), fill = Status)) +
      geom_tile(color = "white") +
      scale_fill_manual(values = c("Tube Present" = "seagreen", "No Tube" = "darkred")) +
      labs(title = "Rack Tube Status", x = "Column", y = "Row") +
      theme_minimal()
  })
  
  # Render table for debugging
  output$table <- renderTable({
    req(xml_data())
    xml_data()
  })
  
  # Observe update code button
  observeEvent(input$update_code, {
    req(input$location, input$new_code)
    
    # Update code in the data
    tubes <- xml_data()
    tubes$Code[tubes$Location == input$location] <- input$new_code
    xml_data(tubes)
    
    # Update choices for selectInput
    updateSelectInput(session, "location", 
                      choices = tubes$Location[tubes$Code == "No Tube"])
  })
  
  # Observe save button
  observeEvent(input$save, {
    req(xml_data(), rack_id(), date())
    
    # Convert back to XML and save
    tubes <- xml_data()
    doc <- newXMLDoc()
    root <- newXMLNode("ThermoRack", doc = doc)
    newXMLNode("Rack_ID", rack_id(), parent = root)
    newXMLNode("Date", date(), parent = root)
    tubes_node <- newXMLNode("Tubes", parent = root)
    
    for (i in 1:nrow(tubes)) {
      tube_node <- newXMLNode("Tube", parent = tubes_node)
      newXMLNode("Location", tubes$Location[i], parent = tube_node)
      newXMLNode("Code", tubes$Code[i], parent = tube_node)
    }
    
    saveXML(doc, file = "edited_file.xml")
    
    showModal(modalDialog(
      title = "Success",
      "The XML file has been saved as 'edited_file.xml'.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Provide file for download
  output$download_file <- downloadHandler(
    filename = function() {
      "edited_file.xml"
    },
    content = function(file) {
      req(xml_data(), rack_id(), date())
      
      # Convert data back to XML format
      tubes <- xml_data()
      doc <- newXMLDoc()
      root <- newXMLNode("ThermoRack", doc = doc)
      newXMLNode("Rack_ID", rack_id(), parent = root)
      newXMLNode("Date", date(), parent = root)
      tubes_node <- newXMLNode("Tubes", parent = root)
      
      for (i in 1:nrow(tubes)) {
        tube_node <- newXMLNode("Tube", parent = tubes_node)
        newXMLNode("Location", tubes$Location[i], parent = tube_node)
        newXMLNode("Code", tubes$Code[i], parent = tube_node)
      }
      
      # Write the XML content to the file
      saveXML(doc, file = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)