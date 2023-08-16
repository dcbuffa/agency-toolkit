library(readxl)
library(dplyr)
library(likert)
library (purrr)
library(tidyverse)
library(psych)
library(pivottabler)

#Kate's computer
#Final_R <- read_excel("Documents/My Tableau Prep Repository/Datasources/Final_R.xlsx")

#Dani's computer
Final_R <- read_excel("C:/Users/danie/Desktop/PhDan/Agency/v2.xlsx") ## Use Cleaned Data.csv here
View(Final_R)  


#### remove columns to isolate household ID and Barriers #### 
colnames(Final_R)
df = subset(Final_R, select = -c(2:56) )
colnames(df)


#### rename barriers #### 
#groups:
 # (Skills) I_know_how_to_make_t_project_work_for_me -> If_I_need_help_with_can_get_that_support
  #(Marginalization) I_feel_like_project_service_as_possible -> I_feel_like_project_or_project_activity
  #(Expectations) I_am_worried_that_th_will_not_be_friendly -> I_am_worried_about_pro_topic_of_Y_than_I_do
  #(Pragmatics) I_have_concerns_abou_to_the_project_site -> I_have_concerns_abou_general_involvement

survey_cols <- c("id", paste0("skills_q", rep(1:9)), paste0("marginalization_q", rep(1:11)), paste0("expectations_q", rep(1:13)), paste0("pragmatics_q", rep(1:9)))
colnames(df) <- survey_cols 
which(colnames(df)=="expectations_q4")
df = subset(df, select = -c(25)) #remove funding not treating larger issue 
str(df)
na.omit(df)

#### Recode the Likert responses ####
likert_recode <- function(x) {
  as.numeric(case_when(
    x == "No problem" ~ 1,
    x == "Small problem" ~ 2,
    x == "Problem" ~ 3,
    x == "Big problem" ~ 4,
    x == "Very big problem" ~ 5,
  ))
}

survey_recoded <- df %>%
  select(-id) %>%
  mutate_all(likert_recode)

####  Overall Total Column #### 
survey_recoded$total <- rowSums(survey_recoded, na.rm=TRUE) #Create column of total score per person
survey_recoded <- cbind(id = df$id, survey_recoded)

#### Category Total Columns #### 
survey_recoded <- survey_recoded %>%
  rowwise() %>%
  mutate(skills_total = sum(skills_q1, skills_q2, skills_q3, skills_q4, skills_q5, skills_q6, skills_q7, skills_q8, skills_q9),
         marginalization_total = sum(marginalization_q1, marginalization_q2, marginalization_q3, marginalization_q4, marginalization_q5,
          marginalization_q6, marginalization_q7, marginalization_q8, marginalization_q9, marginalization_q10, marginalization_q11),
          expectations_total = sum(expectations_q1, expectations_q2, expectations_q3, expectations_q5,
                                    expectations_q6, expectations_q7, expectations_q8, expectations_q9, expectations_q10, expectations_q11, expectations_q12, expectations_q13),
         pragmatics_total = sum(pragmatics_q1, pragmatics_q2, pragmatics_q3, pragmatics_q4, pragmatics_q5, pragmatics_q6, pragmatics_q7, pragmatics_q8, pragmatics_q9)) %>%
  ungroup()

View(survey_recoded)
str(survey_recoded)

likert.data = subset(survey_recoded, select = c(2:42))
subset(Final_R, select = -c(2:42) )

#### Pivot Data ####

#variables must be factors with same number of levels.
#convert all columns to factors
col_names <- names(likert.data)
likert.data[,col_names] <- lapply(likert.data[,col_names], levels = c('1', '2', '3', '4', '5'), factor)
as.data.frame(likert.data)
                                  

write.csv(likert.data, "C:/Users/danie/Desktop/PhDan/Agency/likertdata.csv")






###########
#pivot
likert_long <- gather(likert.data, question, score, skills_q1:pragmatics_q9, factor_key = TRUE)
likert_long


plot(likert(as.data.frame(likert.data)), legend.position="right")

#pivot table
pt <- PivotTable$new()
pt$addData(likert_long)
pt$addRowDataGroups("question")
pt$addColumnDataGroups("score")
pt$defineCalculation(calculationName = "Counts", summariseExpression="n()", caption = "Count")
filterOverrides <- PivotFilterOverrides$new(pt, keepOnlyFiltersFor="question")
pt$defineCalculation(calculationName="QuestionTotals", filters=filterOverrides, 
                     summariseExpression="n()", caption="Question Total")
pt$defineCalculation(calculationName="Percents", type="calculation", 
                     basedOn=c("Counts", "QuestionTotals"),
                     calculationExpression="values$Counts/values$QuestionTotals", 
                     caption="Proportions")

pt$renderPivot()





#### Aggregate the Data ####
na.omit(survey_long) %>%
  group_by(category) %>%
  summarise(avg_response = mean(response), sd_response = sd(response))

#### Visualize the data ####
# Histogram of Total Score (!!! looks gross)
ggplot(survey_recoded, aes(x=total)) +
  geom_bar() 

# Histogram By Category
ggplot(survey_long, aes(x=response)) +
  geom_bar() +
  facet_wrap(~category)


# Average Response per Question (!!! x label should cover all questions in all domains, convert to question label where possible)
survey_long %>%
  group_by(question) %>%
  ggplot(., aes(x = question_no, y = response/length(survey_recoded))) +
  geom_col() +
  facet_wrap(~category) +
  ylim(0,5)
