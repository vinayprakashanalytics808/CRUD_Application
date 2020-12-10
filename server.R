library(RODBC)

shinyServer(function(input, output, session) {
  
  storage <- reactiveValues()
  storage$data <- data.frame()
  storage$data_type <- data.frame()
  storage$pk <- data.frame()
  storage$ik <- data.frame()
  
  
  database_name <- reactive(
    odbcDriverConnect(connection = paste0("Driver={SQL Server Native client 11.0};
                            server=localhost;database=",input$dbname,";trusted_connection=yes;"))
    
  )
  


  
  ## Primary key identification
  
  storage$pk <- reactive(
                          sqlQuery(database_name(), paste0("SELECT Col.Column_Name as 'Primary Key' ,'['+tab.CONSTRAINT_SCHEMA+ '].' + '['+tab.TABLE_NAME+ ']' 
                          as SCHEMA_TABLE from INFORMATION_SCHEMA.TABLE_CONSTRAINTS Tab,INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE Col
                          WHERE Col.Constraint_Name = Tab.Constraint_Name AND Col.Table_Name = Tab.Table_Name and 
                          '['+tab.CONSTRAINT_SCHEMA+ '].' + '['+tab.TABLE_NAME+ ']' = '",input$tname,"' AND Constraint_Type = 'PRIMARY KEY'"))
                        )


  
  ## Identity Key 
  
  
  storage$ik <- reactive(
                        sqlQuery(database_name(), paste0("select COLUMN_NAME as 'Identity', '['+TABLE_SCHEMA+'].' + '['+TABLE_NAME+']' as SCHEMA_TABLE from INFORMATION_SCHEMA.COLUMNS Col
                        where COLUMNPROPERTY(object_id(TABLE_SCHEMA+'.'+TABLE_NAME), COLUMN_NAME, 'IsIdentity') = 1 and 
                        '['+TABLE_SCHEMA+'].' + '['+TABLE_NAME+']' = '",input$tname,"'"))
  )
  
  
   observeEvent(input$submit,{
     # print(database_name())
     disable("tname")
     disable("dbname")
     disable("submit")
     enable("refresh")
     
     # print(storage$pk())
     # print("sagarrrrr")
     # 
     output$create_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
         actionButton("create", "Create/Add",width = 180)
       # }
     })
     
     output$update_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
       actionButton("update", "Update/Edit",width = 180)
       # }
     })
     
     output$delete_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
       actionButton("delete", "Delete/Remove",width = 180)
       # }
     })
     
     

## Primaty key     
     
     if(nrow(storage$pk()) > 0){
       if(nrow(storage$pk()) == 1){
         pk_color <- as.character(storage$pk()$`Primary Key`)
         noun <- c("is")
         PK <- c("Primary Key")
       } else {
         pk_color <- paste0(as.character(storage$pk()$`Primary Key`),collapse = ",")
         noun <- c("are")
         PK <- c("Primary Keys")
       }
       
       } else {
         pk_color <- c()
       }
    
       
     output$key <- renderText({
       if(nrow(storage$pk()) > 0){
       paste("> ",PK , noun,  pk_color)
       }
       else {
       paste0("> No Primary Key")
       }
     })
     
## Identity Key---------------------
     
     
     if(nrow(storage$ik()) > 0){
       ik_color <- as.character(storage$ik()$Identity)
     } else {
       ik_color <- c()
     }
      
      
     output$Ikey <- renderUI({
       if(nrow(storage$ik()) > 0){
         HTML(as.character(tags$label(style = "background-color:#FF7F50",paste0("> Identity key is ", ik_color))))
       }
       else {
         HTML(as.character(tags$label(style = "background-color:#FF7F50",paste0("> No Identity key"))))
       }
     })
     
## Main Table     
     
     output$input_table <- renderDataTable({
       storage$data <- sqlQuery(database_name(), paste0("select * from ",isolate(input$tname),""))
       print(nrow(storage$data))
       
       datatable(
         storage$data, escape = FALSE, rownames = FALSE, extensions = c('FixedColumns',"FixedHeader"),
         class = "compact nowrap hover row-border",
         options = list(paging = TRUE,scrollX = TRUE, fixedHeader=TRUE,autowitdth=FALSE, dom = "t", searching = FALSE,
                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                        pageLength = nrow(storage$data), scrollY = "400px")) 
       # %>% formatStyle(pk_color,
       #                    backgroundColor = styleEqual(c(1:nrow(storage$data)), c(rep("red", nrow(storage$data))))
       #                  ) 
     })
     
     # storage$data_type <- sqlQuery(database_name(), paste0("SELECT COLUMN_NAME, DATA_TYPE,'['+TABLE_SCHEMA +'].' + '['+TABLE_NAME+']' as SCHEMA_TABLE
     # FROM ",isolate(input$dbname),".INFORMATION_SCHEMA.COLUMNS
     # WHERE '['+TABLE_SCHEMA + '].' + '['+TABLE_NAME+ ']' = '",isolate(input$tname),"'"))
     
     storage$data_type <- sqlQuery(database_name(), paste0("SELECT COLUMN_NAME, DATA_TYPE as [SQL Date Type]
     FROM ",isolate(input$dbname),".INFORMATION_SCHEMA.COLUMNS
                                                           WHERE '['+TABLE_SCHEMA + '].' + '['+TABLE_NAME+ ']' = '",isolate(input$tname),"'"))
     
     
     
     # height_rhandsontable <- reactive(100 / nrow(storage$data_type))

     output$table_data_type <- renderRHandsontable({
       rhandsontable(storage$data_type, width = 380) %>%
         hot_cols(fixedColumnsLeft = 1)
     })
     
     
     })
   
   
   
   
   observeEvent(input$refresh,{
     disable("refresh")
     enable("tname")
     enable("dbname")
     enable("submit")
     
     output$input_table <- renderDataTable({
       storage$data <- 0
       
       datatable(
         storage$data, escape = FALSE, rownames = FALSE, extensions = c('FixedColumns',"FixedHeader"),
         class = "compact nowrap hover row-border",
         options = list(paging = TRUE,scrollX = TRUE, fixedHeader=TRUE,autowitdth=FALSE, dom = "t", searching = FALSE,
                        columnDefs = list(list(className = 'dt-center', targets = "_all")),
                        pageLength = nrow(storage$data), scrollY = "400px")) 
     })
     
     output$table_data_type <- renderRHandsontable({
       
       storage$data_type <- 0
       
       rhandsontable(storage$data_type, width = 380, height = height_rhandsontable) %>%
         hot_cols(fixedColumnsLeft = 1)
     })
     
     
     output$create_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
       NULL
       # }
     })

     output$update_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
       NULL
       # }
     })

     output$delete_button <- renderUI({
       # if(nrow(Sample_Superstore) > 0){
       NULL
       # }
     })
     
     output$key <- renderText({
       NULL
     })
     
     output$Ikey <- renderText({
       NULL
     })
     
   })
   
     observeEvent(input$create,{
       storage$data <- sqlQuery(database_name(), paste0("select * from ",isolate(input$tname),""))
       showModal(modalDialog(size = "l",title = "Insert",
                             footer = tagList(modalButton("Cancel"),actionButton("add", "Add"))))
     })

     
## Update functions-----------------------------------
     

     observeEvent(input$update,{
       showModal(where_modal())
     })
     
     where_modal <- function(){
       storage$data <- sqlQuery(database_name(), paste0("select * from ",isolate(input$tname),""))
       modalDialog(
         checkboxGroupInput("where", "Select variable for where clause", choices = colnames(storage$data),inline = TRUE),
         footer = list(actionButton("click", label="Click to continue"), modalButton("Close"))
       )
     }
     
     observe({
       my_min <- 1
       my_max <- length(colnames(storage$data))
       if(length(input$where) > my_max){
         updateCheckboxGroupInput(session, "where", selected= tail(input$where,my_max))
       }
       if(length(input$where) < my_min){
         updateCheckboxGroupInput(session, "where", selected= "a1")
       }
     })
     
     
     observeEvent(input$click,{
         storage$data <- sqlQuery(database_name(), paste0("select * from ",isolate(input$tname),""))
         
         update_column_names <- setdiff(as.character(colnames(storage$data)), c(input$where,as.character(storage$ik()$Identity)))
         update_column_names_bit <- setdiff(as.character(storage$data_type[["COLUMN_NAME"]])[storage$data_type[["SQL Date Type"]] == "bit"], input$where)
         update_column_names_where <- isolate(input$where)
         
         print("This is the identity key")
         print(storage$ik()$Identity)
         print("This is the identity key")
         
         list_of_values_to_update <- purrr::map(
           update_column_names,
           ~ fluidRow(
             column(
               width = 10, .,
               textInput(tolower(.), "",
                                  value = tolower("Please enter the values"))
             ))
         )
         
         list_of_values_to_update_bit <- purrr::map(
           update_column_names_bit,
           ~ fluidRow(
             column(
               width = 10, .,
               # selectInput(tolower(.), "",choices  = storage$data_type[[.]])
               selectInput(tolower(.), "",choices  = c(0,1))
             ))
         )
         
         list_of_values_where <- purrr::map(
           update_column_names_where,
           ~ fluidRow(
             column(
               width = 10, .,
               selectInput(tolower(.), "Where Clause",choices  = unique(storage$data[[.]]))
             ))
         )

         showModal(modalDialog(size = "l",title = "Update",
                               fluidPage(column(width = 4,list_of_values_to_update),column(width = 4,list_of_values_to_update_bit),column(width = 4, list_of_values_where)),
                               footer = tagList(modalButton("Back"),modalButton("Cancel"),actionButton("confirm", "Confirm"))))
         
         # observeEvent(input$confirm,{
         #   out <- c()
         #   for(i in 1:length(update_column_names)){
         #     out <- c(input[[(paste0("",tolower(update_column_names))[i])]])
         #     print(out)
         #     print((update_column_names))
         #   }
         # })
         
         observeEvent(input$confirm,{
           print(update_column_names)
           print("This is correct")
             for(i in 1:length(update_column_names)){
               out <- cbind(update_column_names[i],input[[(paste0("",tolower(update_column_names))[i])]])
               print(out)
             }
         })


   })
     
## Delete functions-----------------------------------
     
    
     observeEvent(input$delete,{
       showModal(where_modal_to_delete())
     })
     
     where_modal_to_delete <- function(){
       storage$data <- sqlQuery(database_name(), paste0("select * from ",isolate(input$tname),""))
       modalDialog(
         checkboxGroupInput("where", "Select variable for where clause", choices = colnames(storage$data),inline = TRUE),
         footer = list(actionButton("click_to_delete", label="Click to continue"), modalButton("Close"))
       )
     }
    
    
  
})