library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
# Define the UI
ui <- fluidPage(
  downloadButton("downloadData", "Download Output as Excel"),
  titlePanel("Volunteer-Student Data Processing App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("vol_file", "Upload Volunteer/GivePulse File", accept = ".csv"),
      fileInput("stu_file", "Upload Student/CitySpan File", accept = ".csv"),
      fileInput("updated_names_file", "Upload Updated-Names File", accept = ".csv"),
      actionButton("process", "Process Data")
    ),
    
    mainPanel(
      h3("Summary Output"),
      tableOutput("summary_output"),
      
      h3("Student Count Output"),
      tableOutput("student_count_output"),
      
      h3("Teacher Student Count Output"),
      tableOutput("teacher_student_count_output"),
      #add text
      h3("Summary Statistics"),
      htmlOutput("stats_output")
    )
  )
)

# Define the server
server <- function(input, output) {
  library(writexl)
  processed_data <- reactiveValues()
  observeEvent(input$process, {
    
    # Load the data
    vol <- read.csv(input$vol_file$datapath)
    stu <- read.csv(input$stu_file$datapath)
    updated_names <- read.csv(input$updated_names_file$datapath) %>%
      filter(in_city_span == "yes") %>%
      select(join_id, updated_name_if_typo) %>%
      distinct()
    
    common_cols <- c(
      "First.Name", 
      "Last.Name", 
      "Group", 
      "Event.Name", 
      "Hours.Served", 
      "Start.Date", 
      "Full.Name.of.Student.B..CF...165903.", 
      "Full.Name.of.Student.C..CF...165906.", 
      "Full.Name.of.Student.D..CF...165904.",
      "Full.Name.of.StudentA..CF...165901.", 
      "StudentA.Content.Area..CF...163716.", 
      "StudentB.Content.Area..CF...165852.",
      "StudentC.Content.Area..CF...165858.", 
      "StudentD.Content.Area..CF...165862.", 
      "StudentA.Student.Time.spent.on.subject..CF...163717.",
      "StudentB.Student.Time.spent.on.subject..CF...165853.", 
      "StudentC.Student.Time.Spent.on.Subject..CF...165859.",
      "StudentD.Student.Time.Spent.on.Subject..CF...165863.", 
      "StudentA.Number.if.not.listed.above..CF...163718.",
      "StudentB.Number.if.not.listed.above..CF...165854.", 
      "StudentC.Number.if.not.listed.above..CF...165860.",
      "StudentD.Number.if.not.listed.above..CF...165864."
    )
    
    common_cols_2 = c("First.Name",
                      "Last.Name",
                      "Group",
                      "Event.Name",
                      "Hours.Served",
                      "Start.Date",
                      "student_index_abr")
    
    # Add the processing logic here (based on your original codes)
    vol_long_student_name <- vol %>%
      select(common_cols) %>%
      pivot_longer(cols = starts_with("Full.Name.of.Student"),
                   names_to = "student_index",
                   values_to = "student_name") %>%
      mutate(student_index_abr = ifelse(student_index == "Full.Name.of.StudentA..CF...165901", "StudentA", NA),
             student_index_abr = ifelse(student_index == "Full.Name.of.Student.B..CF...165903.", "StudentB", student_index_abr),
             student_index_abr = ifelse(student_index == "Full.Name.of.Student.C..CF...165906.", "StudentC", student_index_abr),
             student_index_abr = ifelse(student_index == "Full.Name.of.Student.D..CF...165904.", "StudentD", student_index_abr)) %>%
      select(common_cols_2, student_name)
     
    # By content area
    vol_long_content_area <- vol %>%
      select(common_cols) %>%
      pivot_longer(cols = contains("Content.Area"),
                   names_to = "student_index",
                   values_to = "content_area") %>%
      mutate(student_index_abr = substr(student_index, 1, 8)) %>%
      select(common_cols_2, content_area)
    
    # By time spent
    vol_long_time_spent <- vol %>%
      select(common_cols) %>%
      pivot_longer(cols = contains("Time.spent.on"),
                   names_to = "student_index",
                   values_to = "time_spent") %>%
      mutate(student_index_abr = substr(student_index, 1, 8)) %>%
      select(common_cols_2, time_spent)
    
    # By number if not listed
    vol_long_number_if_not_listed <- vol %>%
      select(common_cols) %>%
      pivot_longer(cols = contains("Number.if.not.listed"),
                   names_to = "student_index",
                   values_to = "number_if_not_listed") %>%
      mutate(student_index_abr = substr(student_index, 1, 8)) %>%
      select(common_cols_2, number_if_not_listed)
    
    # Put all the long tables together
    vol_long_all <- vol_long_student_name %>%
      left_join(vol_long_content_area) %>%
      left_join(vol_long_time_spent) %>%
      left_join(vol_long_number_if_not_listed) %>%
      mutate(teacher_name = trimws(str_extract(tolower(Event.Name), "(?<=:).*?(?=,)"), "left"),
             student_name = str_extract(tolower(student_name), "^[^ ]+"),
             student_name = str_replace_all(student_name, ",", ""))
    
    
    # --------------- Further Prep GivePulse data -------------------------- 
    # Prep the long-form volunteer spreadsheet ---------
    vol_long_all_time <- vol_long_all %>%
      mutate(number_if_not_listed_standard = ifelse(number_if_not_listed <= 5, number_if_not_listed, number_if_not_listed/60), # Create a column where if the number not listed
             time_spent_standard = ifelse(!is.na(as.numeric(time_spent)), as.numeric(time_spent)/60, number_if_not_listed_standard),
             time_spent_standard = ifelse(content_area == "Absent", 0, time_spent_standard),
             time_spent_standard = ifelse(time_spent == "More than one hour, specify below" & is.na(number_if_not_listed), 1, time_spent_standard)) %>%
      filter(content_area != "Absent") %>%
      filter(!is.na(student_name))
    
    vol_long_absent <- vol_long_all %>%
      mutate(number_if_not_listed_standard = ifelse(number_if_not_listed <= 5, number_if_not_listed, number_if_not_listed/60), # Create a column where if the number not listed
             time_spent_standard = ifelse(!is.na(as.numeric(time_spent)), as.numeric(time_spent)/60, number_if_not_listed_standard),
             time_spent_standard = ifelse(content_area == "Absent", 0, time_spent_standard)) %>%
      mutate(teacher_name = trimws(str_extract(tolower(Event.Name), "(?<=:).*?(?=,)"), "left"),
             student_name = str_extract(tolower(student_name), "^[^ ]+")) %>%
      filter(content_area == "Absent") %>%
      filter(!is.na(student_name))
    
    # ----------------- join_id appearts here!! -----------------
    # Summarise the time spent with a student by content area
    vol_summary_hours <- vol_long_all_time %>%
      mutate(teacher_name = tolower(str_extract(Event.Name, "(?<=:\\s)[^:,]+(?=,)")),
             school_name = tolower(str_remove(Group, " \\(.*")), 
             school_name = ifelse(school_name == "paul cuffee school", "paul cuffee lower school", school_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      left_join(updated_names, by = "join_id") %>%
      mutate(student_name = ifelse(!is.na(updated_name_if_typo), tolower(updated_name_if_typo), student_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      group_by(student_name, teacher_name, content_area, Group, Event.Name, join_id, school_name) %>%
      summarise(time_spent_in_content = sum(as.numeric(time_spent_standard))) # THIS IS ONLY THE TIME SPENT COLUMN, NO OTHER ADDITIONAL INFO ADDED FOR FULL PICTURE
    
    
    # Summarise the number of volunteers each student gets
    vol_summary_num_volunteers <- vol_long_all_time %>%
      mutate(teacher_name = tolower(str_extract(Event.Name, "(?<=:\\s)[^:,]+(?=,)")),
             school_name = tolower(str_remove(Group, " \\(.*")), 
             school_name = ifelse(school_name == "paul cuffee school", "paul cuffee lower school", school_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      left_join(updated_names, by = "join_id") %>%
      mutate(student_name = ifelse(!is.na(updated_name_if_typo), tolower(updated_name_if_typo), student_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      group_by(student_name, teacher_name) %>%
      summarise(number_of_volunteers = n_distinct(First.Name, Last.Name)) # ASSUMPTION IS THAT THERE IS NO VARIATION IN VOLUNTEER NAMES
    
    # ----------------------- Prep CitySpan Data -------------------------------------
    stu_clean <- stu %>%
      select(Person.ID,
             First.Name,
             Last.Name,
             School.Attending.24.25,
             Teacher) %>%
      mutate(teacher_name = str_remove(Teacher, ",.*"), # this is a difference with the mock code
             teacher_name = tolower(teacher_name),
             Group = tolower(str_remove(School.Attending.24.25, " \\(.*")),
             first_name_lower = tolower(First.Name),
             join_id = paste(teacher_name, Group, first_name_lower, sep = '_'))
    
    stu_clean_teacher_school <- stu_clean %>%
      distinct(teacher_name, Group) %>%
      mutate(join_id = paste(teacher_name, Group, sep = "_"))
    # ------------------------- Joining the GivePulse data with CitySpan data -------------------------
    # Prep the volunteer summary hours 
    vol_summary_hours <- vol_summary_hours %>%
      filter(student_name != "")
    
    # Joined table of volunteer and student information
    vol_stu_join <- vol_summary_hours %>%
      inner_join(stu_clean, by = "join_id")
    
    # Anti-joined table (those rows with no matching information) of student and volunteer information
    vol_stu_join_anti <- vol_summary_hours %>%
      anti_join(stu_clean, by = "join_id")
    vol_stu_join_sum <- vol_stu_join %>%
      filter(content_area %in% c("ELA", "Math")) %>%
      group_by(First.Name, Last.Name, School.Attending.24.25, Teacher, join_id, content_area) %>%
      summarise(time_spent_in_content = sum(time_spent_in_content))
    
    vol_stu_join_anti_sum <- vol_stu_join_anti %>%
      filter(content_area %in% c("ELA", "Math")) %>%
      group_by(student_name, Group, content_area) %>%
      summarise(time_spent_in_content = sum(time_spent_in_content))
    
    # Checking to see if there are any teachers and schools that don't match between the student and volunteer tables
    vol_summary_teacher_school <- vol_summary_hours %>%
      ungroup() %>%
      distinct(teacher_name, school_name) %>%
      mutate(join_id = paste(teacher_name, school_name, sep = "_"))
    
    teacher_school_not_matching <- vol_summary_teacher_school %>%
      anti_join(stu_clean_teacher_school, by = "join_id")
    # ------------------------- Printing out summary stats -------------------------
    # Putting together the requested tables -----
    # - TextOutput - 1. number of matching students / 2. number of students in cityspan
    number_of_matching_students <- length(unique(paste(vol_stu_join$First.Name, vol_stu_join$Last.Name)))
    number_of_students_in_city_span <- length(unique(paste(stu_clean$First.Name, stu_clean$Last.Name)))
    
    df_find_out_unique_students <- vol_long_all_time %>%
      mutate(teacher_name = tolower(str_extract(Event.Name, "(?<=:\\s)[^:,]+(?=,)")),
             school_name = tolower(str_remove(Group, " \\(.*")), 
             school_name = ifelse(school_name == "paul cuffee school", "paul cuffee lower school", school_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      left_join(updated_names, by = "join_id") %>%
      mutate(student_name = ifelse(!is.na(updated_name_if_typo), tolower(updated_name_if_typo), student_name),
             join_id = paste(teacher_name, school_name, student_name, sep = '_')) %>%
      select(student_name, teacher_name, school_name, join_id) %>%
      distinct()
    
    # - TextOutput - 3. number of students in GP including typos
    number_of_students_in_give_pulse_including_typos <- length(unique(df_find_out_unique_students$join_id))

    matched_hours <- sum(vol_stu_join_sum$time_spent_in_content, na.rm = TRUE)
    hours_spent_from_original_givepulse_data <- sum(vol$Hours.Served, na.rm = TRUE)
    hours_spent_from_pivoted_long <- sum(vol_long_all_time$time_spent_standard, na.rm = TRUE)
    # - TextOutput - 4. Match Rate for Students in CitySpan
    Match_Rate_for_Students_in_CitySpan = scales::percent(round(number_of_matching_students/number_of_students_in_city_span, 2))
    
    # - TextOutput - 5. Match Rate for Impacts in GivePulse
    Match_Rate_for_Impacts_in_GivePulse = scales::percent(round(number_of_matching_students/number_of_students_in_give_pulse_including_typos, 2))
    
    # - TextOutput - 6. Number of total hours accounted for in convergence
    Number_of_total_hours_accounted_for_in_convergence = scales::percent(round(number_of_matching_students/number_of_students_in_give_pulse_including_typos, 2))
    
    # ----- Printing out the total hours across students by school, whether they were matched or not ------
    vol_long_all_time %>%
      group_by(Group) %>%
      summarise(sum_hours_spent = sum(time_spent_standard, na.rm = TRUE)) %>%
      arrange(-sum_hours_spent)
    # --------- total hours across students by school and class ------
    vol_long_all_time %>%
      group_by(Group, teacher_name) %>%
      summarise(sum_hours_spent = sum(time_spent_standard, na.rm = TRUE)) %>%
      arrange(-sum_hours_spent)
    # ---- total sum of hours across all schools for students who were matched -----
    schools_match_hours <- vol_stu_join %>%
      group_by(Group.x, content_area) %>%
      summarise(time_spent = sum(time_spent_in_content, na.rm = TRUE)) %>%
      mutate(content_area = ifelse(content_area == "", "Content Area Not Specified", content_area),
             time_spent = ifelse(is.na(time_spent), 0, time_spent)) %>%
      pivot_wider(names_from = content_area, values_from = time_spent) %>%
      
      # Check and add missing columns with default 0
      mutate(
        ELA = ifelse("ELA" %in% colnames(.), ELA, 0),
        Math = ifelse("Math" %in% colnames(.), Math, 0),
        `Content Area Not Specified` = ifelse("Content Area Not Specified" %in% colnames(.), `Content Area Not Specified`, 0)
      ) %>%
      
      # Create total column
      mutate(Total = ELA + Math + `Content Area Not Specified`) %>%
      
      # Rename columns
      rename(School = Group.x,
             `ELA Hours` = ELA,
             `Math Hours` = Math,
             `Content Area Not Specified Hours` = `Content Area Not Specified`,
             `Total Hours` = Total)
    
    #
    sum(schools_match_hours$`Total Hours`)
    print(schools_match_hours)
    # ---- student count across all schools for students who were matched ---- 
    schools_match_student_count_total <- vol_stu_join %>%
      filter(time_spent_in_content != 0) %>%
      group_by(Group.x) %>%
      summarise(`Total Student Count` = n_distinct(First.Name, Last.Name)) %>%
      rename(School = Group.x)
    
    schools_match_student_count <- vol_stu_join %>%
      filter(time_spent_in_content != 0) %>%
      group_by(Group.x, content_area) %>%
      summarise(student_count = n_distinct(First.Name, Last.Name)) %>%
      mutate(content_area = ifelse(content_area == "", "Content Area Not Specified", content_area)) %>%
      pivot_wider(names_from = content_area, values_from = student_count) %>%
      #add codes here to set `Content Area Not Specified`= 0 if there's no `Content Area Not Specified`
      mutate(
        ELA = ifelse("ELA" %in% colnames(.), ELA, 0),
        Math = ifelse("Math" %in% colnames(.), Math, 0),
        `Content Area Not Specified` = ifelse("Content Area Not Specified" %in% colnames(.), `Content Area Not Specified`, 0)
      ) %>%
      # adding ends here
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      rename(School = Group.x,
             `ELA Student Count` = ELA,
             `Math Student Count` = Math,
             `Content Area Not Specified Student Count` = `Content Area Not Specified`
      ) %>%
      left_join(schools_match_student_count_total)
    # ------- total sum of hours across all schools and teachers for students who were matched -----
    schools_teachers_match_hours <- vol_stu_join %>%
      group_by(Group.x, teacher_name.x, content_area) %>%
      summarise(time_spent = sum(time_spent_in_content, na.rm = TRUE)) %>%
      mutate(content_area = ifelse(content_area == "", "Content Area Not Specified", content_area),
             time_spent = ifelse(is.na(time_spent), 0, time_spent)) %>%
      pivot_wider(names_from = content_area, values_from = time_spent) %>%
      #add
      mutate(
        ELA = ifelse("ELA" %in% colnames(.), ELA, 0),
        Math = ifelse("Math" %in% colnames(.), Math, 0),
        `Content Area Not Specified` = ifelse("Content Area Not Specified" %in% colnames(.), `Content Area Not Specified`, 0)
      ) %>%
      #add ends
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      mutate(Total = ELA + Math + `Content Area Not Specified`) %>%
      rename(School = Group.x,
             `ELA Hours` = ELA,
             `Math Hours` = Math,
             `Teacher` = teacher_name.x,
             `Content Area Not Specified Hours` = `Content Area Not Specified`,
             `Total Hours` = Total)
    
    sum(schools_teachers_match_hours$`Total Hours`)
    print(schools_teachers_match_hours)
    # ----- the student count across all schools for students who were matched -----
    schools_teachers_match_student_count_total <- vol_stu_join %>%
    filter(time_spent_in_content != 0) %>%
      group_by(Group.x, teacher_name.x) %>%
      summarise(`Total Student Count` = n_distinct(First.Name, Last.Name)) %>%
      rename(School = Group.x,
             `Teacher` = teacher_name.x)
    
    schools_teachers_match_student_count <- vol_stu_join %>%
      filter(time_spent_in_content != 0) %>%
      group_by(Group.x, teacher_name.x, content_area) %>%
      summarise(student_count = n_distinct(First.Name, Last.Name)) %>%
      mutate(content_area = ifelse(content_area == "", "Content Area Not Specified", content_area)) %>%
      pivot_wider(names_from = content_area, values_from = student_count) %>%
      #add
      mutate(
        ELA = ifelse("ELA" %in% colnames(.), ELA, 0),
        Math = ifelse("Math" %in% colnames(.), Math, 0),
        `Content Area Not Specified` = ifelse("Content Area Not Specified" %in% colnames(.), `Content Area Not Specified`, 0)
      ) %>%
      #add ends
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      rename(School = Group.x,
             `Teacher` = teacher_name.x,
             `ELA Student Count` = ELA,
             `Math Student Count` = Math,
             `Content Area Not Specified Student Count` = `Content Area Not Specified`
      ) %>%
      left_join(schools_teachers_match_student_count_total)
    
    print(schools_teachers_match_student_count)
    # ------ the student hours by content area by student ------
    student_match_hours <- vol_stu_join %>%
      group_by(First.Name, Last.Name, Person.ID, Group.x, teacher_name.x, content_area) %>%
      summarise(time_spent = sum(time_spent_in_content, na.rm = TRUE)) %>%
      mutate(content_area = ifelse(content_area == "", "Content Area Not Specified", content_area),
             time_spent = ifelse(is.na(time_spent), 0, time_spent)) %>%
      pivot_wider(names_from = content_area, values_from = time_spent) %>%
      #add
      mutate(
        ELA = ifelse("ELA" %in% colnames(.), ELA, 0),
        Math = ifelse("Math" %in% colnames(.), Math, 0),
        `Content Area Not Specified` = ifelse("Content Area Not Specified" %in% colnames(.), `Content Area Not Specified`, 0)
      ) %>%
      #add ends
      mutate(across(everything(), ~ replace_na(.x, 0))) %>%
      mutate(Total = ELA + Math + `Content Area Not Specified`) %>%
      rename(School = Group.x,
             `ELA Hours` = ELA,
             `Math Hours` = Math,
             `Teacher` = teacher_name.x,
             `Content Area Not Specified Hours` = `Content Area Not Specified`,
             `Total Hours` = Total,
             `Student First Name` = First.Name,
             `Student Last Name` = Last.Name)
    
    sum(student_match_hours$`Total Hours`)
  
    # save data into excel
    processed_data$summary_output <- schools_match_hours
    processed_data$student_count_output <- schools_match_student_count
    processed_data$teacher_student_count_output <- schools_teachers_match_student_count
    
    # Store statistics as text in a tibble for easy writing to Excel
    processed_data$stats_output <- tibble(
      Metric = c(
        "Number of matching students", 
        "Number of students in CitySpan", 
        "Number of students in GivePulse (including Typos)", 
        "Match Rate for Students in CitySpan", 
        "Match Rate for Impacts in GivePulse", 
        "Number of total hours accounted for"
      ),
      Value = c(
        number_of_matching_students, 
        number_of_students_in_city_span, 
        number_of_students_in_give_pulse_including_typos, 
        Match_Rate_for_Students_in_CitySpan, 
        Match_Rate_for_Impacts_in_GivePulse, 
        Number_of_total_hours_accounted_for_in_convergence
      )
    )
    
    # Render the results
    output$summary_output <- renderTable(schools_match_hours)
    output$student_count_output <- renderTable(schools_match_student_count)
    output$teacher_student_count_output <- renderTable(schools_teachers_match_student_count)
    output$stats_output <- renderUI({
      HTML(paste0(
        "<p style='margin-bottom: 10px;'><strong>Number of matching students:</strong> ", number_of_matching_students, "</p>",
        "<p style='margin-bottom: 10px;'><strong>Number of students in CitySpan:</strong> ", number_of_students_in_city_span, "</p>",
        "<p style='margin-bottom: 10px;'><strong>Number of students in GivePulse (including Typos):</strong> ", number_of_students_in_give_pulse_including_typos, "</p>",
        "<p style='margin-bottom: 10px;'><strong>Match Rate for Students in CitySpan:</strong> ", Match_Rate_for_Students_in_CitySpan, "</p>",
        "<p style='margin-bottom: 10px;'><strong>Match Rate for Impacts in GivePulse:</strong> ", Match_Rate_for_Impacts_in_GivePulse, "</p>",
        "<p style='margin-bottom: 10px;'><strong>Number of total hours accounted for in convergence:</strong> ", Number_of_total_hours_accounted_for_in_convergence, "</p>"))
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('processed_data_', Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {
        write_xlsx(list(
          Summary_Output = processed_data$summary_output,
          Student_Count_Output = processed_data$student_count_output,
          Teacher_Student_Count_Output = processed_data$teacher_student_count_output,
          Statistics = processed_data$stats_output  # Saving the statistics as a tibble
        ), file)
        
      }
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)



# Updates on 9/23
# ----- Remove Code Redundancy -----
# - several "select(First.name, Last.name, Group,etc.)" statements --> create 2 common-columns lists 
# to store these columns to be called to reuse

# ----- Modify the Wording of Uploading Section -----

# Updates on 9/24
# ----- Add More Statistics Results -----
#  Add:
#   - Number of matching student
#   - Number of students in city span
#   - Number of students in GP including typos
#   - The match rate of students in city span
#   - The match rate for impacts in GP
#   - The number of total hours accounted for convergence