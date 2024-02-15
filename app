library(shiny)
library(reactable)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(jpeg)
library(patchwork)
library(plotly)
library(htmltools)
library(reactablefmtr)

candidates <- read_csv("Dragonfly-Final - Sheet1.csv")
candidates <- candidates %>%
  rename(Email = `E-mail`)

# Function to create Pepe logo links
create_pepe_links <- function(value) {
  pepe_image_url <- "https://i.seadn.io/gae/nf3rwX17v3Jf4evpE9GKw-kRP7NxBli4lwCrx7WajkDR_Jy32SsoXYAX4sRojRFaP6YVnvOy01kELJrjbhbvnMJD7oLuuUgQ0O5I?auto=format&dpr=1&w=1000"  
  if (!is.na(value) && value != "") {
    paste0('<a href="', value, '" target="_blank"><img src="', pepe_image_url, '" alt="Pepe" style="width:40px;height:40px;"/></a>')
  } else {
    # Return red "X" if value is NA
    '<span style="color:red; font-size:28px; ">X</span>'
  }
}

# Function to create LinkedIn logo links
create_linkedin_links <- function(value) {
  linkedin_logo_url <- "https://upload.wikimedia.org/wikipedia/commons/c/ca/LinkedIn_logo_initials.png" 
  if (!is.na(value) && value != "") {
    paste0('<a href="', value, '" target="_blank"><img src="', linkedin_logo_url, '" alt="LinkedIn" style="width:40px;height:40px;"/></a>')
  } else {
    ""
  }
}

create_github_links <- function(value) {
  github_logo_url <- "https://upload.wikimedia.org/wikipedia/commons/9/91/Octicons-mark-github.svg" 
  if (!is.na(value) && value != "") {
    paste0('<a href="', value, '" target="_blank"><img src="', github_logo_url, '" alt="GitHub" style="width:40px;height:40px;"/></a>')
  } else {
    # Return red "X" if value is NA
    '<span style="color:red; font-size:28px; ">X</span>'
  }
}

# Function to create mailto links with an email icon
create_mailto_links <- function(email) {
  email_icon_url <- "https://upload.wikimedia.org/wikipedia/commons/e/ec/Circle-icons-mail.svg"  
  if (!is.na(email) && email != "") {
    paste0('<a href="mailto:', email, '"><img src="', email_icon_url, '" alt="Email" style="width:40px;height:40px;"/></a>')
  } else {
    # Return red "X" if value is NA
    '<span style="color:red; font-size:28px; ">X</span>'
  }
}



# Define UI
ui <- navbarPage(
  title = "Shadowy Super Coders and Friends",
  
  tabPanel("Candidates",  # First tab title
           fluidPage(
             # Custom CSS to position the logo, adjust the title, and center the table
             tags$head(
               tags$style(HTML("
                  #logo {
                    position: fixed;
                    top: 60px;
                    left: 10px;
                    height: 80px;  # Adjust height as needed
                  }
                  #main-title {
                    margin-left: 70px;  # Adjust this value to shift the title to the left
                  }
                  .centered-table-container {
                    display: flex;
                    justify-content: center;
                    width: 100%; /* Ensure the container takes full width */
                  }
                "))
             ),
             
             # Inserting the logo
             tags$img(src = "https://media.licdn.com/dms/image/C4E0BAQFIdpMDiuveEg/company-logo_200_200/0/1660568319199/dragonfly_capital_partners_logo?e=1715817600&v=beta&t=-lE9OYF2FLvxXEXv3wJxGgGpsnE-rvfbZvwveARFAiQ
", id = "logo"),
             
             tags$h1("Dragonfly Capital - Talent", id = "main-title"),
             
             br(),
             # Wrap reactableOutput in a div with the custom centering class
             div(class = "centered-table-container", reactableOutput("table"))
           )
  ),
  
  # Additional tabPanels can be added here
  tabPanel("ETH and SOL Devs (Github Data)",
           fluidPage(
             plotlyOutput("dev_activity"),
             br(),
             h2("Top 100 Github Contributors - Go-Ethereum and Solana (Rust)", style = "text-align: center;"),
             br(),
             div(class = "centered-table-container", reactableOutput("table2")),
             br(),
             p(em("Last update: February 14, 2024"), style = "text-align: right; font-size: smaller")
           )
  )
)


# Define Server Logic
server <- function(input, output) {
  
  output$table <- renderReactable({
    reactable(candidates, 
              fullWidth = FALSE,
              defaultColDef = colDef(vAlign = "center"),
              searchable = TRUE,
              highlight = TRUE,
              outlined = TRUE,
              bordered = TRUE,
              columns = list(
                Name = colDef(width = 170,
                  # Show species under character names
                  cell = function(value, index) {
                    Company <- candidates$Company[index]
                    Company <- if (!is.na(Company)) Company else "Unknown"
                    div(
                      div(style = "font-weight: 500", value),
                      div(style = "font-size: 1.25rem", Company)
                    )
                  }
                ),
                Company = colDef(show = FALSE, name = "Company", width = 150),
                `Skill/Role` = colDef(name = "Skill/Role", html = TRUE, width = 150),
                LinkedIn = colDef(name = "LinkedIn", html = TRUE, cell = create_linkedin_links, align = "center", width = 100),
                Github = colDef(name = "Github", html = TRUE, cell = create_github_links, align = "center", width = 100),
                Other = colDef(html = TRUE, cell = create_pepe_links, align = "center", width = 100),
                Email = colDef(name = "E-mail", html = TRUE, cell = create_mailto_links, align = "center", width = 100)
              ))
  })
  
  output$dev_activity <- renderPlotly({
    
    eth_sol_graph <- df_long %>%
      ggplot(aes(x = timestamp, y = commits, color = chain, group = chain)) +
      geom_line(linewidth = 2) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme_minimal()+
      ggtitle("Go-Ethereum & Solana Developer Activity")+
      xlab("Date")+
      ylab("Commits")+
      theme(plot.subtitle = element_text(face = "italic"),
            plot.title = element_text(face = "bold"),
            axis.text.x = element_text(angle = 60, size = 8),
            axis.text.y = element_text(size = 10),
            panel.grid.major = element_blank())+
      scale_color_manual(values = c("ETH" = "black", "SOL" = "darkgrey"))
    
    ggplotly(eth_sol_graph, tooltip = c("timestamp", "commits"))
    
  })
  
  output$table2 <- renderReactable({
    
    reactable(eth_sol_contributors,
              fullWidth = FALSE,
              defaultColDef = colDef(vAlign = "center"),
              searchable = TRUE,
              highlight = TRUE,
              outlined = TRUE,
              bordered = FALSE,
              columns = list(
                avatar_url = colDef(name = "", cell = embed_img(eth_sol_contributors, height = 40, width = 40), align = "left"),
                Username = colDef(width = 150, align = "left"),
                Contributions = colDef(width = 150, defaultSortOrder = "desc", align = "center"),
                Github = colDef(html = TRUE, cell = create_github_links, align = "center", width = 100),
                chain = colDef(name = "Chain")
              ))
  })
}

# Run the App
shinyApp(ui, server)

