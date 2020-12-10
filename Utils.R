schema_table <- c("[HumanResources].[Shift]", "[HumanResources].[Employee]", "[HumanResources].[Department]","[HumanResources].[EmployeePayHistory]", "[HumanResources].[JobCandidate]")


# pkey <- function(dbname, tname){
# sqlQuery(dbname, paste0("SELECT Col.Column_Name as 'Primary Key' ,'['+tab.CONSTRAINT_SCHEMA+ '].' + '['+tab.TABLE_NAME+ ']' 
#      as SCHEMA_TABLE from INFORMATION_SCHEMA.TABLE_CONSTRAINTS Tab,INFORMATION_SCHEMA.CONSTRAINT_COLUMN_USAGE Col
#      WHERE Col.Constraint_Name = Tab.Constraint_Name AND Col.Table_Name = Tab.Table_Name  AND Constraint_Type = 'PRIMARY KEY'and 
#      '['+tab.CONSTRAINT_SCHEMA+ '].' + '['+tab.TABLE_NAME+ ']'"), paste0("=","'",tname,"'"))
# }
