#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(DT)

discorr <- function(df1
                    , df2 = NA
                    , idVar = "id"
                    , dataVars = "variable"
                    , metaVars = NA
                    , filterVar = NA
                    , outDir = getwd()
)
{        
    # build df including corrections
    correctionsDir <- file.path("corrections")
    
    loadData <- function() {
        files <- list.files(file.path(correctionsDir), full.names = TRUE)
        datalist <- lapply(files, 
                           read.csv, 
                           stringsAsFactors = FALSE,
                           colClasses = c("character"))
        df <- bind_rows(datalist)
        orderedDf <- df %>% arrange(id, desc(timestamp))
        data <- orderedDf[!duplicated(orderedDf$id),]
        data
    }
    
    updateDf <- function() {
        corrections <- loadData()
        names(corrections)[names(corrections) == "filter"] <- filterVar
        
        if(length(corrections) == 0) {
            df3 <- df1 %>%
            mutate(set = "corr")
        } else {
            df3 <- df1 %>%
                filter(!id %in% corrections$id) %>%
                mutate(set = "corr") %>%
                bind_rows(corrections %>% select(-timestamp))
        }
        
        if(is.na(df2)) {
            df <- rbind(df1 %>% mutate(set = "raw"), df3)
        } else {
            df <- rbind(df1 %>% mutate(set = "raw"), 
                        df2 %>% mutate(set = "comp"), df3)
        }
        
        df <- mutate(df, set = factor(set, c("raw", "corr", "comp")))
        
        df
    }
    
    df <- updateDf()

    filterChoices <- c("ALL", df %>% select(one_of(filterVar)) %>% unique)
    idChoices <- df %>% select(one_of(idVar)) %>% unique
    
    ui <- shinyUI(fluidPage(
        
        titlePanel("Discover and correct faulty data"),
        fluidRow(
            column(6, fluidRow(
                column(6,
                       selectInput("selectedVariable",
                                   "Variable",
                                   dataVars)),
                column(6,
                       selectInput("selectedFilter",
                                   paste0("Filter (", filterVar, ")"),
                                   filterChoices)
                )),
                htmlOutput("development"),
                plotOutput("graph"),
                dataTableOutput("dataTable")
            ),
            column(6, fluidRow(
                column(6,
                       selectInput("selectedId",
                                   paste0("Id (", idVar, ")"),
                                   idChoices)),
                column(6,
                       actionButton("build", "Build corrected dataset")
                )),
                fluidRow(
                    column(4, tableOutput("idMeta")),
                    column(8, tableOutput("idData"))
                ),
                wellPanel(
                    actionButton("save", "Save"),
                    actionButton("saveRefresh", "Save & Refresh")
                )
            )
        )
    ))
    
    server <- shinyServer(function(input, output, session) {
        
        updateDfCopy <- function() {
            data <- df
            names(data)[names(data) %in% filterVar] <- "filter"
            names(data)[names(data) %in% idVar] <- "id"
            data
        }
        
        dfCopy <- updateDfCopy()
        
        dfId <- reactive({
            data <- dfCopy %>% filter(id == input$selectedId)
            names(data)[names(data) == "filter"] <- filterVar
            data
        })
        
        output$development <- renderText({
            data <- dfCopy %>%
                gather(key, value, one_of(dataVars)) %>%
                mutate(value = as.numeric(value)) %>%
                spread(set, value) %>%
                filter(key == input$selectedVariable)
            
            if(input$selectedFilter != "ALL") {
                data <- data %>%
                    filter(filter == input$selectedFilter)
            }
            
            aggregate <- data %>%
                summarize_at(c("corr", "comp"), 
                             funs(sum(., na.rm = TRUE))) %>%
                mutate(abs = corr - comp,
                       rel = round(corr / comp * 100 - 100, 1))
            
            text <- paste0("Absolute: ", aggregate$abs, " | ",
                           "Relative: ", aggregate$rel, "%")
            
            HTML(paste0("<p style=\"text-align:center;font-weight:bold\">"
                        , text, "</p>"))
        })
        
        output$graph <- renderPlot({
            
            plotDf <- dfCopy %>%
                gather(key, value, one_of(dataVars)) %>%
                filter(set != "raw",
                       key == input$selectedVariable) %>% 
                mutate(value = as.numeric(value))
            
            if(input$selectedFilter != "ALL") {
                plotDf <- plotDf %>%
                    filter(filter == input$selectedFilter)
            }
            
            ggplot(plotDf,
                   aes(set, value)) + 
                geom_bar(stat = "identity")
        })
        
        output$dataTable <- renderDataTable({
            
            data <- dfCopy %>%
                gather(key, value, one_of(dataVars)) %>%
                mutate(value = as.numeric(value))
            
            if(input$selectedFilter != "ALL") {
                data <- data %>%
                    filter(filter == input$selectedFilter)
            }
            
            names(data)[names(data) == "filter"] <- filterVar
            
            dtDf <- data %>%
                select(-one_of(metaVars)) %>%
                spread(set, value) %>%
                filter(key == input$selectedVariable) %>%
                mutate(abs = corr - comp,
                       rel = round(corr / comp * 100 - 100, 1))

            datatable(dtDf)
        })
        
        output$idMeta <- renderTable({
            data <- dfId()

            idMf <- data %>%
                filter(set %in% c("raw", "comp")) %>%
                gather(key, value, one_of(metaVars)) %>%
                select(set, key, value) %>%
                mutate(key = factor(key, metaVars)) %>%
                spread(set, value)
            idMf
        })
        
        output$idData <- renderTable({
            data <- dfId()

            idDf <- data %>%
                gather(key, value, one_of(dataVars)) %>%
                mutate(value = as.numeric(value)) %>%
                select(set, key, value) %>%
                mutate(key = factor(key, dataVars)) %>%
                spread(set, value) %>%
                mutate(abs = corr - comp,
                       rel = round(corr / comp * 100 - 100, 1))
            idDf
        })
        
        observeEvent(input$selectedId, {
            
            for(i in 1:length(dataVars)) {
                
                dataId <- dfId() %>% filter(set == "corr")
                
                removeUI(
                    selector = sprintf('.shiny-input-container:has(#%s)', 
                                       dataVars[i])
                )
                insertUI(
                    selector = "#save",
                    where = "beforeBegin",
                    ui = numericInput(dataVars[i],
                                      dataVars[i],
                                      select(dataId, one_of(dataVars[i])))
                )
            }
        })
        
        # Save the data
        # https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
        # epochTime <- function() as.integer(Sys.time())
        humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
        
        correctedData <- reactive({
            meta <- dfId() %>% filter(set == "raw") %>%
                select(-one_of(dataVars)) %>%
                mutate(set = "corr")
            
            data <- sapply(dataVars, function(x) input[[x]])
            record <- c(meta, data, timestamp = humanTime()) %>% t()
            record
        })
        
        saveData <- function(data) {
            fileName <- sprintf("%s_%s.csv",
                                as.data.frame(data)$id,
                                humanTime())
            
            write.csv(x = data, file = file.path(correctionsDir, fileName),
                      row.names = FALSE, quote = TRUE)
        }
        
        observeEvent(input$save, {
            saveData(correctedData())
            df <<- updateDf()
            dfCopy <<- updateDfCopy()
        })
        
        observeEvent(input$saveRefresh, {
            saveData(correctedData())
            df <<- updateDf()
            dfCopy <<- updateDfCopy()
            session$reload()
        })
        
        observeEvent(input$saveRefresh, {
            data <- updateDf() %>% filter(set == "corr") %>% select(-set)
            
            fileName <- sprintf("FullSetWithCorrections_%s.csv",
                                humanTime())
            
            write.csv2(x = data, 
                       file = file.path(outDir, fileName),
                       row.names = FALSE, quote = TRUE)
        })
        
    })
    
    ## run app 
    runApp(list(ui = ui, server = server))
    
    return(invisible())
}

testDiscorr <- function() {
    
    df1 <- read.csv2("2016_RawData.csv", 
                     stringsAsFactors = FALSE, 
                     colClasses = c("character"))
    df2 <- read.csv2("2015_CorrectData.csv", 
                     stringsAsFactors = FALSE, 
                     colClasses = c("character"))
    
    discorr(df1
            , df2
            , idVar = "id"
            , dataVars = c("revenue", "cost", "salary")
            , metaVars = c("name", "industry", "size")
            , filterVar = "industry"
    )
}