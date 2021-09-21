library(magrittr)
library(DatabaseConnector)
library(SqlRender)
library(FeatureExtraction)
library(Andromeda)
library(CohortMethod)
library(rJava)

connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server = "testnode.arachnenetwork.com/synpuf_110k",
                                             user = "ohdsi",
                                             password = 'ohdsi',
                                             port = "5441",
                                             pathToDriver = 'c:/jdbcDrivers')


connection <- connect(connectionDetails)
resultsDatabaseSchema <- 'alex_alexeyuk_results'
cdmDatabaseSchema <- 'cdm_531'

generateSurvival <- function(connection,targetIds=1,outcomeIds=3,
                             cohortDatabaseSchema='alex_alexeyuk_results'){
  #sqlFileName <- "TimeToEvent.sql"
  #pathToSql <- system.file("sql", "sql_server", sqlFileName, package = packageName)
  sql <- readSql('TimeToEvent.sql')
  
  surv_outputs <- purrr::map_df(targetIds, function(targetId){
    
    purrr::map_df(outcomeIds, function(outcomeId){
      
      sql_tmp <- SqlRender::render(sql)
      sql_tmp <- SqlRender::translate(sql_tmp, targetDialect = "postgresql")
      
      km_raw <- DatabaseConnector::querySql(connection, sql_tmp, snakeCaseToCamelCase = T)
      
      ## edit
      if(nrow(km_raw) < 100 | length(km_raw$event[km_raw$event == 1]) < 1){return(NULL)}
      
      km_proc <- km_raw %>%
        dplyr::mutate(timeToEvent = as.integer(as.Date(eventDate) - as.Date(cohortStartDate)),
                      id = dplyr::row_number()) %>%
        dplyr::select(id, timeToEvent, event)
      
      surv_info <- survival::survfit(survival::Surv(timeToEvent, event) ~ 1, data = km_proc)
      
      #surv_info <- survminer::surv_summary(surv_info)
      
      data.frame(targetId = 1, outcomeId = 3, time = surv_info$time, surv = surv_info$surv, 
                 n.censor = surv_info$n.censor, n.event = surv_info$n.event, n.risk = surv_info$n.risk,
                 lower = surv_info$lower, upper = surv_info$upper)
      
      return(surv_info)
    })
  })
}
kw1 <- generateSurvival(connection = connection)
data <- read.csv('kw')


ggsurvplot(
  kw,
  data = data,
  size = 1,                 # change line size
  #palette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#E69F00", "#56B4E9","#E7B800", "#2E9FDF"),# custom color palettes
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = NULL,
  palette = c("#E7B800", "#2E9FDF"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()# Change ggplot2 theme
)

                 
write.csv(kw, file = 'kw')
