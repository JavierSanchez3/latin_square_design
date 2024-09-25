# Check and install packages if not installed
if (!require("shiny")) install.packages("shiny", dependencies = TRUE)
if (!require("magic")) install.packages("magic", dependencies = TRUE)
if (!require("writexl")) install.packages("writexl", dependencies = TRUE)
if (!require("shinyjs")) install.packages("shinyjs", dependencies = TRUE)

# Load necessary libraries
library(shiny)
library(magic)
library(writexl)
library(shinyjs)

# Define the UI
ui <- fluidPage(
    useShinyjs(),  # Initialize shinyjs
    titlePanel("Randomization Dashboard"),
    sidebarLayout(
        sidebarPanel(
            numericInput("n", "Number of blocks:", min = 1, value = 3),  # Default value 3
            numericInput("size", "Block size:", min = 1, value = 3),     # Default value 3
            numericInput("burnin", "Burn-in:", min = 0, value = 10),     # Default value 10
            actionButton("randomize", "Randomize", icon = icon("random"), 
                         style = "color: white; background-color: blue;"),
            downloadButton("downloadData", "Download Randomization File")
        ),
        mainPanel(
            tableOutput("table")
        )
    )
)

# Define the server
server <- function(input, output, session) {
    # Observer to enable/disable the "Randomize" button
    observe({
        # Enable the button only if all fields have values
        if (!is.null(input$n) && !is.null(input$size) && !is.null(input$burnin)) {
            shinyjs::enable("randomize")
        } else {
            shinyjs::disable("randomize")
        }
    })
    
    # Function to generate the Latin square and restructure the data
    generate_data <- reactive({
        req(input$randomize)  # Only execute if the button is pressed
        project_name <- rlatin(n = input$n, size = input$size, burnin = input$burnin)
        
        project_name_list <- list()
        for (i in 1:dim(project_name)[3]) {
            block <- as.data.frame(project_name[, , i])
            colnames(block) <- c("Visit 1", "Visit 2", "Visit 3")
            block$Block <- i
            project_name_list[[i]] <- block
        }
        
        final_df <- do.call(rbind, project_name_list)
        final_df$`Participant Number` <- 1:nrow(final_df)
        final_df <- final_df[, c("Participant Number", "Block", "Visit 1", "Visit 2", "Visit 3")]
        
        final_df$`Visit 1`[final_df$`Visit 1` == 1] <- "A"
        final_df$`Visit 1`[final_df$`Visit 1` == 2] <- "B"
        final_df$`Visit 1`[final_df$`Visit 1` == 3] <- "C"
        
        final_df$`Visit 2`[final_df$`Visit 2` == 1] <- "A"
        final_df$`Visit 2`[final_df$`Visit 2` == 2] <- "B"
        final_df$`Visit 2`[final_df$`Visit 2` == 3] <- "C"
        
        final_df$`Visit 3`[final_df$`Visit 3` == 1] <- "A"
        final_df$`Visit 3`[final_df$`Visit 3` == 2] <- "B"
        final_df$`Visit 3`[final_df$`Visit 3` == 3] <- "C"
        
        final_df
    })
    
    # Display the table in the UI
    output$table <- renderTable({
        req(input$randomize)  # Only show the table if the button is pressed
        generate_data()
    })
    
    # Download the Excel file
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("project_name_randomization_", Sys.Date(), ".xlsx", sep = "")
        },
        content = function(file) {
            write_xlsx(generate_data(), file)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)