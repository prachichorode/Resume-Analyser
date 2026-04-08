library(shiny)
library(shinydashboard)
library(ggplot2)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(stringr)
library(pdftools)
library(fmsb)

ui <- dashboardPage(
  dashboardHeader(title = "Resume Analyzer Pro"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Upload", tabName = "upload", icon = icon("upload")),
                menuItem("Dashboard", tabName = "analysis", icon = icon("chart-bar")),
                menuItem("Reports", tabName = "reports", icon = icon("file-alt")),
                menuItem("Settings", tabName = "settings", icon = icon("cog")),
                menuItem("Resume Builder", tabName = "build_resume", icon = icon("id-card"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # ---------------- UPLOAD ----------------
      tabItem(tabName = "upload",
              fileInput("file", "Upload Resume"),
              selectInput("role", "Select Role",
                          choices = c("Data Analyst","Data Scientist","Web Developer")),
              actionButton("analyze", "Analyze"),
              br(), br(),
              box(
                width = 12, status = "info", solidHeader = TRUE,
                h4("Welcome to Resume Analyzer Pro!"),
                p("Upload your resume (PDF or TXT), select your target role, and click Analyze to get insights."),
                p("Supported roles: Data Analyst, Data Scientist, Web Developer."),
                p("Make sure your resume is clear and updated for best results.")
              )
      ),
      
      # ---------------- DASHBOARD ----------------
      tabItem(tabName = "analysis",
              fluidRow(
                valueBoxOutput("score", width = 4),
                valueBoxOutput("skills_found", width = 4),
                valueBoxOutput("missing_skills", width = 4)
              ),
              
              fluidRow(
                box(title="📊 Skills Bar", width=6,
                    plotOutput("barPlot", height=220),
                    p("Comparison of found vs missing skills for your target role.")),
                
                box(title="🥧 Skills Pie", width=6,
                    plotOutput("pieChart", height=220),
                    p("Percentage of skills present versus missing in your resume."))
              ),
              
              fluidRow(
                box(title="📈 Resume Strength", width=6,
                    plotOutput("resumeBreakdownPie", height=220),
                    p("Contribution of Skills, Experience, and Education to overall score.")),
                
                box(title="📡 Radar Chart", width=6,
                    plotOutput("radarChart", height=220),
                    p("Visual representation of your resume strengths across key areas."))
              ),
              
              fluidRow(
                box(title="💼 Experience", width=6,
                    plotOutput("expChart", height=220),
                    p("Professional experience with roles, companies, and durations.")),
                
                box(title="🎓 Education", width=6,
                    plotOutput("eduChart", height=220),
                    p("Academic qualifications, degrees, and universities attended."))
              ),
              
              fluidRow(
                box(title="☁️ Word Cloud", width=12,
                    plotOutput("wordcloud", height=350),
                    p("Frequently used keywords in your resume to highlight expertise."))
              ),
              
              fluidRow(
                box(title="🔗 Links Detected", width=12, status="info", solidHeader=TRUE,
                    uiOutput("resume_links"),
                    p("Click any link to open in a new tab."))
              ),
              
              fluidRow(
                box(
                  title="💡 Tips & Recommendations", width=12, status="warning", solidHeader=TRUE,
                  h4("Resume Score Interpretation:"),
                  p("80–100% → Excellent! Strong match for your role."),
                  p("60–79% → Good, but consider adding missing skills or details."),
                  p("<60% → Needs improvement: update skills, experience, and formatting."),
                  h4("Skills Advice:"),
                  p("Focus on adding missing skills and highlighting achievements for better alignment with your target role.")
                )
              )
      ),
      
      # ---------------- REPORTS ----------------
      tabItem(tabName = "reports",
              fluidRow(
                box(title="📄 Download Analysis", width=12, status="primary", solidHeader=TRUE,
                    p("Download your resume analysis including skills, experience, education, and charts."),
                    downloadButton("downloadReport", "Download PDF/CSV"))
              ),
              fluidRow(
                box(title="Summary", width=12, status="info", solidHeader=TRUE,
                    p("This section will summarize your skills found, missing skills, total score, experience, and education."))
              )
      ),
      
      # ---------------- SETTINGS ----------------
      tabItem(tabName = "settings",
              fluidRow(
                box(title="⚙️ User Settings", width=12, status="primary", solidHeader=TRUE,
                    h4("Preferences"),
                    p("Choose your preferred theme, target roles, or save previous analyses."),
                    selectInput("theme", "Select Theme:", choices = c("Light", "Dark")),
                    checkboxGroupInput("save_analysis", "Save Analysis Options:", 
                                       choices = c("Save Skills", "Save Experience", "Save Education")))
              )
      ),
      
      # ---------------- RESUME BUILDER ----------------
      tabItem(tabName = "build_resume",
              fluidRow(
                box(title = "📝 Build Your Resume", width=6, status="primary", solidHeader = TRUE,
                    textInput("rb_name", "Full Name:", placeholder = "Enter your full name"),
                    textInput("rb_email", "Email:", placeholder = "Enter your email"),
                    textInput("rb_phone", "Phone Number:", placeholder = "Enter your phone number"),
                    textAreaInput("rb_summary", "Professional Summary:", "", rows = 3, placeholder="Briefly describe yourself"),
                    textAreaInput("rb_skills", "Skills (comma-separated):", "", rows = 3, placeholder="Python, R, SQL, Excel"),
                    textAreaInput("rb_experience", "Experience (Company | Role | Duration):", "", rows = 4, placeholder="Company A | Role | 2 yrs"),
                    textAreaInput("rb_education", "Education (Degree | University | Start-End):", "", rows = 4, placeholder="B.Tech | University A | 2018-2022"),
                    textAreaInput("rb_projects", "Projects / Achievements:", "", rows = 3, placeholder="Project A: Description"),
                    br(),
                    actionButton("rb_generate", "Generate Resume", icon = icon("magic"), class = "btn-success")
                ),
                
                box(title = "📄 Resume Preview", width=6, status="info", solidHeader = TRUE,
                    uiOutput("resume_preview"),
                    br(),
                    downloadButton("rb_download", "Download Resume as TXT", icon = icon("download"), class = "btn-primary")
                )
              ),
              
              fluidRow(
                box(title = "💡 Tips for a Strong Resume", width = 12, status="warning", solidHeader=TRUE,
                    tags$ul(
                      tags$li("Keep your professional summary concise and relevant."),
                      tags$li("Highlight key skills and use industry keywords."),
                      tags$li("Show achievements and measurable results."),
                      tags$li("Use consistent formatting and proper headings."),
                      tags$li("Tailor resume to the target role for better impact.")
                    ),
                    p(strong("Badge System:"), "Score your resume by completeness. The more fields you fill, the stronger your resume looks!")
                )
              )
      )
    )
  )
)

server <- function(input, output, session){
  
  resume_data <- reactiveVal(NULL)
  
  observeEvent(input$analyze, {
    req(input$file)
    
    if(grepl(".pdf", input$file$name, ignore.case = TRUE)){
      text <- pdf_text(input$file$datapath)
      text <- iconv(text, from="UTF-8", to="UTF-8", sub="")
    } else {
      text <- readLines(input$file$datapath, warn = FALSE, encoding="UTF-8")
      text <- iconv(text, from="UTF-8", to="UTF-8", sub="")
    }
    
    text <- paste(text, collapse=" ")
    text <- tolower(text)
    text <- gsub("[^[:alnum:]/:. ]", " ", text)  # Keep ":" and "/" for URLs
    text <- gsub("\\s+", " ", text)
    
    role_skills <- list(
      "Data Analyst" = c("excel","sql","power bi","r"),
      "Data Scientist" = c("python","machine learning","deep learning","r"),
      "Web Developer" = c("html","css","javascript","react")
    )
    
    skills <- role_skills[[input$role]]
    
    found <- skills[sapply(skills, function(skill)
      grepl(paste0("\\b", skill, "\\b"), text)
    )]
    
    missing <- setdiff(skills, found)
    skills_score <- round((length(found)/length(skills))*50)
    
    exp <- data.frame(
      Company=c("Company A","Company B"),
      Role=c("Role A","Role B"),
      Duration=c(2,1)
    )
    experience_score <- 30
    
    edu <- data.frame(
      Degree=c("B.Tech","M.Tech"),
      University=c("Uni A","Uni B"),
      StartYear=c(2018,2022),
      EndYear=c(2022,2024)
    )
    education_score <- 20
    
    total_score <- skills_score + experience_score + education_score
    
    resume_data(list(
      text=text,
      found=found,
      missing=missing,
      skills_score=skills_score,
      experience_score=experience_score,
      education_score=education_score,
      total_score=total_score,
      experience=exp,
      education=edu
    ))
    
    updateTabItems(session, "tabs", "analysis")
  })
  
  # ---------------- Dashboard Value Boxes ----------------
  output$score <- renderValueBox({
    req(resume_data())
    score <- resume_data()$total_score
    color <- if(score >= 80) "green" else if(score >= 60) "yellow" else "red"
    valueBox(paste0(score,"%"), "Score", color = color)
  })
  
  output$skills_found <- renderValueBox({
    req(resume_data())
    valueBox(length(resume_data()$found), "Skills Found", color = "blue")
  })
  
  output$missing_skills <- renderValueBox({
    req(resume_data())
    valueBox(length(resume_data()$missing), "Missing Skills", color = "red")
  })
  
  # ---------------- Dashboard Plots ----------------
  output$barPlot <- renderPlot({
    req(resume_data())
    df <- data.frame(
      Skill=c(resume_data()$found, resume_data()$missing),
      Status=c(rep("Found", length(resume_data()$found)),
               rep("Missing", length(resume_data()$missing)))
    )
    ggplot(df, aes(x=Skill, fill=Status)) +
      geom_bar() + coord_flip() + theme_minimal()
  })
  
  output$pieChart <- renderPlot({
    req(resume_data())
    df <- data.frame(
      category=c("Found","Missing"),
      count=c(length(resume_data()$found), length(resume_data()$missing))
    )
    ggplot(df, aes(x="", y=count, fill=category)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y") + theme_void()
  })
  
  output$resumeBreakdownPie <- renderPlot({
    req(resume_data())
    df <- data.frame(
      Category=c("Skills","Experience","Education"),
      Score=c(resume_data()$skills_score,
              resume_data()$experience_score,
              resume_data()$education_score)
    )
    ggplot(df, aes(x="", y=Score, fill=Category)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y") + theme_void()
  })
  
  output$radarChart <- renderPlot({
    req(resume_data())
    df <- data.frame(
      Skills=resume_data()$skills_score,
      Experience=resume_data()$experience_score,
      Education=resume_data()$education_score
    )
    df <- rbind(c(50,30,20), c(0,0,0), df)
    radarchart(df, axistype=1)
  })
  
  output$expChart <- renderPlot({
    req(resume_data())
    ggplot(resume_data()$experience,
           aes(x=Company, y=Duration, fill=Role)) +
      geom_bar(stat="identity") +
      coord_flip() + theme_minimal()
  })
  
  output$eduChart <- renderPlot({
    req(resume_data())
    ggplot(resume_data()$education,
           aes(x=StartYear, xend=EndYear,
               y=University, yend=University)) +
      geom_segment(size=5) + theme_minimal()
  })
  
  output$wordcloud <- renderPlot({
    req(resume_data())
    corpus <- Corpus(VectorSource(resume_data()$text))
    dtm <- TermDocumentMatrix(corpus)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m), decreasing=TRUE)
    d <- data.frame(word=names(v), freq=v)
    wordcloud(d$word, d$freq, random.order=FALSE, colors=brewer.pal(8,"Dark2"))
  })
  
  # ---------------- Dashboard Clickable Links ----------------
  output$resume_links <- renderUI({
    req(resume_data())
    text <- resume_data()$text
    
    urls <- unlist(str_extract_all(text, "(https?://[\\w\\-./?=&]+)"))
    urls <- unique(urls)
    
    if(length(urls) == 0){
      return(HTML("<i>No links detected in your resume.</i>"))
    }
    
    link_html <- paste0("<a href='", urls, "' target='_blank'>", urls, "</a><br>")
    HTML(paste(link_html, collapse=""))
  })
  
  # ---------------- Download Report ----------------
  output$downloadReport <- downloadHandler(
    filename = function() { "resume_analysis.txt" },
    content = function(file) {
      req(resume_data())
      writeLines(c(
        paste("Total Score:", resume_data()$total_score),
        paste("Skills Found:", paste(resume_data()$found, collapse=", ")),
        paste("Missing Skills:", paste(resume_data()$missing, collapse=", ")),
        "Experience:",
        paste(apply(resume_data()$experience, 1, paste, collapse=" | "), collapse="\n"),
        "Education:",
        paste(apply(resume_data()$education, 1, paste, collapse=" | "), collapse="\n")
      ), file)
    }
  )
  
  # ---------------- Resume Builder ----------------
  observeEvent(input$rb_generate, {
    output$resume_preview <- renderUI({
      make_clickable <- function(text) {
        text <- gsub("(https?://[\\w\\-./?=&]+)", "<a href='\\1' target='_blank'>\\1</a>", text)
        text <- gsub("\n", "<br>", text)
        text
      }
      
      HTML(paste0(
        "<h2 style='color:#2E86C1;'>", input$rb_name, "</h2>",
        "<b>Email:</b> ", input$rb_email, "<br>",
        "<b>Phone:</b> ", input$rb_phone, "<br><hr style='border-color:#2E86C1;'>",
        "<b style='color:#1ABC9C;'>Professional Summary:</b><br>", make_clickable(input$rb_summary), "<br><br>",
        "<b style='color:#1ABC9C;'>Skills:</b><br>", paste(strsplit(input$rb_skills, ",")[[1]], collapse=", "), "<br><br>",
        "<b style='color:#1ABC9C;'>Experience:</b><br>", make_clickable(input$rb_experience), "<br><br>",
        "<b style='color:#1ABC9C;'>Education:</b><br>", make_clickable(input$rb_education), "<br><br>",
        "<b style='color:#1ABC9C;'>Projects / Achievements:</b><br>", make_clickable(input$rb_projects)
      ))
    })
  })
  
  output$rb_download <- downloadHandler(
    filename = function() { paste0(gsub(" ", "_", input$rb_name), "_Resume.txt") },
    content = function(file) {
      lines <- c(
        paste("Name:", input$rb_name),
        paste("Email:", input$rb_email),
        paste("Phone:", input$rb_phone),
        "",
        "Professional Summary:",
        input$rb_summary,
        "",
        "Skills:",
        input$rb_skills,
        "",
        "Experience:",
        input$rb_experience,
        "",
        "Education:",
        input$rb_education,
        "",
        "Projects / Achievements:",
        input$rb_projects
      )
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)