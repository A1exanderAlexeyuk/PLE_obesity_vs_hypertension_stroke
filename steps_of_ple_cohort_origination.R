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


conn <- connect(connectionDetails)
resultsDatabaseSchema <- 'alex_alexeyuk_results'
cdmDatabaseSchema <- 'cdm_531'

sql <- readSql("hypertension.sql")

sql <- render(sql,
              cdmDatabaseSchema = cdmDatabaseSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              target_cohort_table = 'hypertension',
              target_cohort_id = 1)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
connection <- connect(connectionDetails)
executeSql(connection, sql)

# let's check the table of interests
sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
             "FROM @resultsDatabaseSchema.hypertension",
             "GROUP BY cohort_definition_id")

sql <- render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
querySql(connection, sql)




sql <- readSql("obesity.sql")

sql <- render(sql,
              cdmDatabaseSchema = cdmDatabaseSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              comparator_cohort_table = 'obesity',
              target_cohort_id = 2)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
connection <- connect(connectionDetails)
executeSql(connection, sql)

# let's check the table of interests
sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
             "FROM @resultsDatabaseSchema.obesity",
             "GROUP BY cohort_definition_id")

sql <- render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
querySql(connection, sql)




sql <- readSql("death.sql")

sql <- render(sql,
              cdmDatabaseSchema = cdmDatabaseSchema,
              resultsDatabaseSchema = resultsDatabaseSchema,
              outcome_cohort_table = 'death',
              outcome_cohort_id = 3)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
connection <- connect(connectionDetails)
executeSql(connection, sql)

# let's check the table of interests
sql <- paste("SELECT cohort_definition_id, COUNT(*) AS count",
             "FROM @resultsDatabaseSchema.death",
             "GROUP BY cohort_definition_id")

sql <- render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
querySql(connection, sql)

sql <- paste("DROP TABLE alex_alexeyuk_results.Union_table AS (SELECT * FROM @resultsDatabaseSchema.death
        UNION ALL
        SELECT * FROM @resultsDatabaseSchema.obesity
        UNION ALL SELECT * FROM @resultsDatabaseSchema.hypertension)"
            )
sql <- render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
executeSql(connection, sql)

sql <- paste("SELECT COUNT(*) FROM @resultsDatabaseSchema.union_table
        "
)
sql <- render(sql, resultsDatabaseSchema = resultsDatabaseSchema)
sql <- translate(sql, targetDialect = connectionDetails$dbms)
querySql(connection, sql)


hypertension <- 38004624
obesyty <- 1177463
covSettings <- createDefaultCovariateSettings(excludedCovariateConceptIds = c(hypertension,
                                                                              obesyty),
                                              addDescendantsToExclude = TRUE)



#Load data:
cohortMethodData <- getDbCohortMethodData(connectionDetails = connectionDetails,
                                          cdmDatabaseSchema = cdmDatabaseSchema,
                                          tempEmulationSchema = resultsDatabaseSchema,
                                          targetId = 1,
                                          comparatorId = 2,
                                          outcomeIds = 3,
                                          studyStartDate = "",
                                          studyEndDate = "",
                                          exposureDatabaseSchema = resultsDatabaseSchema,
                                          exposureTable = "union_table",
                                          outcomeDatabaseSchema = resultsDatabaseSchema,
                                          outcomeTable = "death",
                                          cdmVersion = 5,
                                          firstExposureOnly = TRUE,
                                          removeDuplicateSubjects = TRUE,
                                          restrictToCommonPeriod = FALSE,
                                          washoutPeriod = 180,
                                          covariateSettings = covSettings)



summary(cohortMethodData)


#saveCohortMethodData(cohortMethodData, "obesityVsHypertensionDeath_fixed.zip")

# Defining the study population
cohortMethodData <- CohortMethod::loadCohortMethodData('obesityVsHypertensionDeath_fixed.zip')


studyPop <- createStudyPopulation(cohortMethodData = cohortMethodData,
                                  outcomeId = 3,
                                  firstExposureOnly = FALSE,
                                  restrictToCommonPeriod = FALSE,
                                  washoutPeriod = 0,
                                  removeDuplicateSubjects = "keep all",
                                  removeSubjectsWithPriorOutcome = TRUE,
                                  minDaysAtRisk = 1,
                                  riskWindowStart = 0,
                                  startAnchor = "cohort start",
                                  riskWindowEnd = 30,
                                  endAnchor = "cohort end")

getAttritionTable(studyPop)


# Fitting a propensity model

ps <- createPs(cohortMethodData = cohortMethodData, population = studyPop)
computePsAuc(ps)


plotPs(ps,
       scale = "preference",
       showCountsLabel = TRUE,
       showAucLabel = TRUE,
       showEquiposeLabel = TRUE)


getPsModel(ps, cohortMethodData)


trimmedPop <- trimByPsToEquipoise(ps)
plotPs(trimmedPop, ps, scale = "preference")

stratifiedPop <- stratifyByPs(ps, numberOfStrata = 5)
plotPs(stratifiedPop, ps, scale = "preference")

matchedPop <- matchOnPs(ps, caliper = 0.2, caliperScale = "standardized logit", maxRatio = 1)
plotPs(matchedPop, ps)


drawAttritionDiagram(matchedPop)


#Evaluating covariate balance

balance <- computeCovariateBalance(matchedPop, cohortMethodData)
plotCovariateBalanceScatterPlot(balance, showCovariateCountLabel = TRUE, showMaxLabel = TRUE)



plotCovariateBalanceOfTopVariables(balance)


#Inspecting select population characteristics

createCmTable1(balance)


insertDbPopulation(population = matchedPop,
                   cohortIds = c(101,100),
                   connectionDetails = connectionDetails,
                   cohortDatabaseSchema = resultsDatabaseSchema,
                   cohortTable = "coxibVsNonselVsGiBleed",
                   createTable = FALSE,
                   cdmVersion = cdmVersion)


computeMdrr(population = studyPop,
            modelType = "cox",
            alpha = 0.05,
            power = 0.8,
            twoSided = TRUE)

computeMdrr(population = matchedPop,
            modelType = "cox",
            alpha = 0.05,
            power = 0.8,
            twoSided = TRUE)


getFollowUpDistribution(population = matchedPop)


plotFollowUpDistribution(population = matchedPop)


# outcome Model
outcomeModel <- fitOutcomeModel(population = studyPop,
                                modelType = "cox")


outcomeModel <- fitOutcomeModel(population = matchedPop,
                                modelType = "cox",
                                stratified = TRUE)
outcomeModel


#Instead of matching or stratifying we can also perform Inverse Probability of Treatment Weighting (IPTW):
outcomeModel <- fitOutcomeModel(population = ps,
                                modelType = "cox",
                                inversePtWeighting = TRUE)
outcomeModel


# Adding interaction terms
interactionCovariateIds <- c(8532001, 201826210, 21600960413)
# 8532001 = Female
# 201826210 = Type 2 Diabetes
# 21600960413 = Concurent use of antithrombotic agents
outcomeModel <- fitOutcomeModel(population = matchedPop,
                                modelType = "cox",
                                stratified = TRUE,
                                interactionCovariateIds = interactionCovariateIds)


balanceFemale <- computeCovariateBalance(population = matchedPop,
                                         cohortMethodData = cohortMethodData,
                                         subgroupCovariateId = 8532001)
plotCovariateBalanceScatterPlot(balanceFemale)


# Adding covariates to the outcome model
outcomeModel <- fitOutcomeModel(population = matchedPop,
                                cohortMethodData = cohortMethodData,
                                modelType = "cox",
                                stratified = TRUE,
                                useCovariates = TRUE)
outcomeModel


#Inspecting the outcome model
#We can inspect more details of the outcome model

exp(coef(outcomeModel))

exp(confint(outcomeModel))



# K-M plot
plotKaplanMeier(population = matchedPop, includeZero = FALSE)


#Time-to-event plot

plotTimeToEvent(cohortMethodData = cohortMethodData,
                outcomeId = 3,
                firstExposureOnly = FALSE,
                washoutPeriod = 0,
                removeDuplicateSubjects = FALSE,
                minDaysAtRisk = 1,
                riskWindowStart = 0,
                startAnchor = "cohort start",
                riskWindowEnd = 30,
                endAnchor = "cohort end")

