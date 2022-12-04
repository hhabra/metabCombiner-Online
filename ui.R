library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinyjs)
source("helper.R")

ui <- fluidPage(
    theme = shinytheme("spacelab"),
    shinyjs::useShinyjs(),
    div(
        titlePanel(
            title = h1(strong(em("metabCombiner"), " Online"), align = "center"),
            windowTitle = "metabCombiner Online"
        )
    ),
    tabsetPanel(type = "tabs",
        tabPanel("metabCombine",
            tabsetPanel(type = "pills",
                tabPanel("Input Data",
                    div(style = "text-align:center",
                        fluidRow(
                            column(width = 6,
                                wellPanel(
                                    downloadLink("xdata_example", "Example Dataset"),
                                    fileInput(inputId = "file_x", label = "Dataset X",
                                              accept = c(".csv", ".txt", ".rds")),
                                    uiOutput("input_x"),
                                    uiOutput("button_x")
                                )
                            ),
                            column(width = 6,
                                wellPanel(
                                    downloadLink("ydata_example", "Example Dataset"),
                                    fileInput(inputId = "file_y", label = "Dataset Y",
                                              accept = c(".csv", ".txt", ".rda")),
                                    uiOutput("input_y"),
                                    uiOutput("button_y")
                                )
                            )
                        )
                    ),
                    fluidRow(
                        splitLayout(
                            column(width = 10,
                                verbatimTextOutput("xdata_summary")
                            ),
                            column(width = 10,
                                verbatimTextOutput("ydata_summary")
                            )
                        )
                    ),
                    sidebarLayout(
                        sidebarPanel(width = 6, style = "text-align: center",
                            h3("Form Paired Alignment Table", align = "center"),
                            numericInput("binGap", label = "m/z bin gap", value = 0.005, width = "75%"),
                            fluidRow(
                                splitLayout(
                                    uiOutput("xid_input"),
                                    uiOutput("yid_input")
                                )
                            ),
                            shinyjs::disabled(actionButton(inputId = "mzgroup",
                                        label = "group and align features",
                                        width = "50%", align = "center"))
                        ),
                        mainPanel(width = 6,
                            wellPanel(
                                h3("Object Summary", align = "center"),
                                verbatimTextOutput("object_summary")
                            )
                        )
                    )
                ),
                tabPanel("RT Mapping",
                    div(style = "text-align:center",
                        sidebarLayout(
                            sidebarPanel(width = 6,
                                h3("Anchor Selection", align = "center"),
                                checkboxInput("useID", "Use Matching IDs"),
                                fluidRow(
                                    h4("selection tolerances", align = "center"),
                                    splitLayout(
                                        numericInput("tolmz", "m/z", value = 0.003, width = "90%"),
                                        numericInput("tolQ", "Q", value = 0.3, width = "90%"),
                                        numericInput("tolrtq", "RT quantile", value = 0.3,
                                                     width = "90%")
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        numericInput("windx", "RT window (X)", value = 0.03,
                                                     width = "90%"),
                                        numericInput("windy", "RT window (Y)", value = 0.03,
                                                     width = "90%")
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        actionButton("anchors_select", "Select Anchors"),
                                        actionButton("plot_anchors", "Plot Anchors")
                                    )
                                ),
                                hr(style = "border-top: 1px solid #000000"),
                                h3("Model-Fitting", align = "center"),
                                textInput("k", "possible k values (comma-separated)",
                                          value = "12,14,16,18,20", width = "75%"),
                                fluidRow(
                                    splitLayout(
                                        radioButtons("family", "distribution family",
                                                     choices = c("scat" = "scat", "gaussian" = "gaussian")),
                                        radioButtons("outlier_method", "Outlier Detection Method",
                                                     choices = c("Mean Absolute Deviation" = "MAD",
                                                                 "boxplot" = "boxplot"),
                                                     selected = "Mean Absolute Deviation", width = "100%")
                                    )
                                ),
                                fluidRow(
                                    h4("Outlier Filtering Parameters", align = "center"),
                                    splitLayout(
                                        numericInput("iterFilter", "Filtering Iterations",
                                                     value = 2, width = "75%"),
                                        numericInput("outlier_coef", "Detection Coefficient",
                                                     value = 2, width = "75%"),
                                        numericInput("outlier_prop", "Detection Proportion",
                                                     value = 0.5, width = "75%")
                                    )
                                ),
                                fluidRow(
                                    h4("Retention Time Limits", align = "center"),
                                    splitLayout(
                                        textInput("rtx_min", "Minimum (X)",
                                                  value = "min", width = "90%"),
                                        textInput("rtx_max", "Maximum (X)",
                                                  value = "max", width = "90%"),
                                        textInput("rty_min", "Minimum (Y)",
                                                  value = "min", width = "90%"),
                                        textInput("rty_max", "Maximum (Y)",
                                                  value = "max", width = "90%")
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        actionButton("fit_gam", "Fit GAM Model"),
                                        actionButton("model_fit", "Plot Model Fit")
                                    )
                                ),
                                hr(style = "border-top: 1px solid #000000"),
                                h3("Plotting Options", align = "center"),
                                fluidRow(
                                    splitLayout(
                                        textInput("title", "plot title", width = "75%"),
                                        textInput("xlab", "x-axis label", value = "rtx",
                                                  width = "75%"),
                                        textInput("ylab", "y-axis label", value = "rty",
                                                  width = "75%")
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        selectInput("pcol", "point color", selected = "black",
                                                    width = "75%",
                                                    choices = c("black", "blue", "red",
                                                                "dark green", "purple")),
                                        selectInput("lcol", "line color", selected = "red",
                                                    width = "75%",
                                                    choices = c("black", "blue", "red",
                                                                "dark green", "purple"))
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        selectInput("outlier_opt", "filtered points option",
                                                    choices = c("highlight", "show", "remove"),
                                                    width = "75%"),
                                        selectInput("ocol", "filtered points color",
                                                    selected = "dark green", width = "75%",
                                                    choices = c("black", "blue", "red",
                                                                "dark green", "purple"))
                                    )
                                ),
                                fluidRow(
                                    splitLayout(
                                        numericInput("lwd", "line width", value = 3, width = "75%"),
                                        numericInput("cex", "point size", value = 1, width = "75%")
                                    )
                                )
                            ),
                            mainPanel(width = 5,
                                plotOutput("plot_fit", inline = TRUE, width = "90%", height = "90%"),
                                verbatimTextOutput("fit_progress")
                            )
                        )
                    )
                ),
                tabPanel("Scoring Options",
                    fluidRow(
                        splitLayout(
                            div(style = "text-align: center",
                                wellPanel(
                                    h3("Feature Pair Scoring", align = "center"),
                                    sliderInput(inputId = "A", label = "m/z weight", value = 75,
                                                min = 0, max = 150),
                                    sliderInput(inputId = "B", label = "RT weight", value = 10,
                                                min = 0, max = 30),
                                    sliderInput(inputId = "C", label = "Q weight", value = 0.25,
                                                min = 0, max = 1),
                                    actionButton("calcScores", "calculate scores",
                                                 style = "text-align: center")
                                )
                            ),
                            wellPanel(
                                div(style = "text-align: center",
                                    h3("Row Annotation & Reduction", align = "center"),
                                    checkboxInput("useID_labels", "Label Matching IDs"),
                                    sliderInput("minScore", "minimum score", value = 0.5, min = 0, max = 1),
                                    splitLayout(
                                        numericInput("maxRankX", "max X rank", value = 2, min = 1,
                                                     width = "75%"),
                                        numericInput("maxRankY", "max Y rank", value = 2, min = 1,
                                                     width = "75%")
                                    ),
                                    splitLayout(
                                        numericInput("delta", "delta score", value = 0.1, max = 1, min = 0,
                                                     width = "75%"),
                                        numericInput("maxRTerr", "max RT error (minutes)",
                                                     value = 10, min = 0.01, width = "75%")
                                    ),
                                    splitLayout(
                                        checkboxInput("update_X", "Unite Missing X Features", width = "75%"),
                                        checkboxInput("update_Y", "Unite Missing Y Features", width = "75%")
                                    ),
                                    actionButton("labelRows", "Annotate & Reduce", style = "text-align: center")
                                )
                            ),
                            wellPanel(
                                h3("Final Object", align = "center"),
                                verbatimTextOutput("object_summary_final")
                            )
                        )
                    )
                ),
                tabPanel("Output",
                    splitLayout(
                        wellPanel(
                            h3("Save Final Results", align = "center"),
                            downloadButton("combinedTable", "Combined Table (.csv)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("write2file", "Combined Table (special format)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("finalobject_results", "Download Object",
                                           style = "width:100%;")
                        ),
                        wellPanel(
                            h3("Save Full Results", align = "center"),
                            downloadButton("combinedTable_full", "Combined Table (.csv)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("write2file_full", "Combined Table (special format)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("object_results", "Download Object", style = "width:100%;")
                        )
                    ),
                    splitLayout(
                        wellPanel(
                            h3("Save X & Y Data Tables", align = "center"),
                            downloadButton("xdata_download", "xdata Table (.csv)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("ydata_download", "ydata Table (.csv)",
                                           style = "width:100%;")
                        ),
                        wellPanel(
                            h3("Save X & Y Objects", align = "center"),
                            downloadButton("xdata_object", "xdata Table (.rds)",
                                           style = "width:100%;"),
                            br(), br(),
                            downloadButton("ydata_object", "ydata Object (.rds)",
                                           style = "width:100%;")
                        )
                    ),
                    wellPanel(
                        h3("Use Final Results as Input", align = "center"),
                        splitLayout(
                            actionButton("enable_newdata", "Enable", width = "80%"),
                            disabled(actionButton("use_as_xdata", "Use Results as X Dataset",
                                                  width = "80%")),
                            disabled(actionButton("use_as_ydata", "Use Results as Y Dataset",
                                                  width = "80%"))
                        )
                    ),
                    verbatimTextOutput("newdata_warning"),
                    textOutput("use_as_xdata_message")



                ),
                tabPanel("Data View",
                    tabsetPanel(
                        tabPanel("xdata",
                            tableOutput("xdata_view")
                        ),
                        tabPanel("ydata",
                            tableOutput("ydata_view")
                        ),
                        tabPanel("combined table",
                            tableOutput("combinedTable_view")
                        ),
                        tabPanel("feature data",
                            tableOutput("featdata_view")
                        ),
                        tabPanel("anchors",
                            tableOutput("anchors_view")
                        ),

                    )
                )
            )
        ),
        ###########################  batchCombine ##################################
        tabPanel(
            "batchCombine",
            tabsetPanel(type = "pills",
                tabPanel("Input Data",
                    div(style = "text-align:center",
                        column(width = 12, align = "center",
                            wellPanel(
                                fileInput(inputId = "files_batches",
                                          label = "Upload Multiple Batch Dataset Files (use: shift + click or control + click) ",
                                          accept = c(".csv", ".txt"), multiple = TRUE, width = "70%"),
                                fluidRow(
                                    h4("General Options", align = "center"),
                                    splitLayout(
                                        textInput("batch_id", label = "batch ID prefix", value = "",
                                                 width = "50%"),
                                        selectInput("bcmode", label = "operation", width = "50%",
                                                    choices = c("intersection", "union"))
                                   )
                                ),
                               fluidRow(
                                   h4("Column Keywords", align = "center"),
                                   splitLayout(
                                       textInput("mz_batch", label = strong("m/z"), value = "[Mm][Zz]|[Mm]/[Zz]|[Mm].[Zz]|[Mm]ass",
                                                 width = '75%'),
                                       textInput("rt_batch", label = strong("retention time"), value = "[Rr][Tt]|[Rr]etention|[Tt]ime",
                                                 width = '75%'),
                                       textInput("id_batch", label = "identifiers", value = "[Ii][Dd]|[Nn]ame",
                                                 width = '75%'),
                                       textInput("adduct_batch", label = "adducts", value = "[Aa]dduct",
                                                 width = '75%')
                                   )
                               ),
                               fluidRow(
                                   div(
                                       splitLayout(
                                           textInput("samples_batch",  placeholder = "e.g. POOL",
                                                    label = "Samples Keyword(s) (separate by commas)",
                                                    width = "75%"),
                                           textInput("extra_batch", label = "Extra Keyword(s) (separate by commas)",
                                                     placeholder = "e.g. Blank,CHEAR", width = "75%")
                                       ),
                                       style = "font-size: 80%"
                                   )
                               ),
                               h4("Filters", align = "center"),
                               fluidRow(
                                   splitLayout(
                                       textInput("rtmin_batch", "min RT", value = "min", width = "75%"),
                                       textInput("rtmax_batch", "max RT", value = "max", width = "75%")
                                   )
                               ),
                               fluidRow(
                                   splitLayout(
                                       numericInput("misspc_batch", label = "% Missing Tolerance (0-99)",
                                                    value = 50, width = "75%"),
                                       checkboxInput("zero_batch", "Treat 0 as Missing Intensity Value",
                                                     width = "75%")
                                   )
                               ),
                               fluidRow(
                                   splitLayout(
                                       numericInput("dupmz_batch", "duplicate m/z tolerance",
                                                    value = "0.0025", width = "75%"),
                                       numericInput("duprt_batch", "duplicate RT tolerance",
                                                    value = "0.05", width = "75%")
                                   )
                               ),
                               fluidRow(
                                   h4("Stepwise Averaging", align = "center"),
                                   splitLayout(
                                       selectInput("mz_ave", label = "m/z", choices = c("observed", "average"),
                                                 width = '75%'),
                                       selectInput("rt_ave", label = "retention time", width = '75%',
                                                choices = c("observed", "average")),
                                       selectInput("Q_ave", label = "Q (relative abundance)", width = '75%',
                                                   choices = c("observed", "average"))
                                   )
                               ),
                               div(actionButton("process_batch", label = strong("process batches"),
                                                width = '200px', style = "text-align: center"))
                           ),
                        verbatimTextOutput("batch_summary")
                        )
                    )
                ),
                tabPanel("RT mapping",
                    div(style = "text-align:center",
                        column(width = 12, align = "center",
                            sidebarPanel(width = 12,
                                h3("Anchor Selection", align = "center"),
                                checkboxInput("useID_batch", "Use Matching IDs"),
                                fluidRow(
                                    h4("selection tolerances", align = "center"),
                                    splitLayout(
                                        numericInput("tolmz_batch", "m/z", value = 0.003, width = "50%"),
                                        numericInput("tolQ_batch", "Q", value = 0.3, width = "50%"),
                                        numericInput("tolrtq_batch", "RT quantile", value = 0.3,
                                                     width = "50%")
                                      )
                                ),
                                fluidRow(
                                    splitLayout(
                                        numericInput("windx_batch", "RT window (X)", value = 0.03,
                                                     width = "50%"),
                                        numericInput("windy_batch", "RT window (Y)", value = 0.03,
                                                     width = "50%")
                                    )
                                ),
                                hr(style = "border-top: 1px solid #000000"),
                                h3("Model-Fitting", align = "center"),
                                textInput("k_batch", "possible k values (comma-separated)",
                                          value = "12,14,16,18,20", width = "80%"),
                                fluidRow(
                                    splitLayout(
                                        radioButtons("family_batch", "distribution family",
                                            choices = c("scat" = "scat", "gaussian" = "gaussian")),
                                        radioButtons("outlier_method_batch", "Outlier Detection Method",
                                            choices = c("MAD" = "Mean Absolute Deviation",
                                                        "boxplot" = "boxplot"), selected = "MAD",
                                            width = "100%")
                                    )
                                ),
                                fluidRow(
                                    h4("Outlier Filtering Parameters", align = "center"),
                                    splitLayout(
                                        numericInput("iterFilter_batch", "Filtering Iterations",
                                                     value = 2, width = "75%"),
                                        numericInput("outlier_coef_batch", "Detection Coefficient",
                                                     value = 2, width = "75%"),
                                        numericInput("outlier_prop_batch", "Detection Proportion",
                                                     value = 0.5, width = "75%")
                                    )
                                ),
                                fluidRow(
                                    h4("Retention Time Limits", align = "center"),
                                    splitLayout(
                                        textInput("rtx_min_batch", "Minimum (X)",
                                                  value = "min", width = "90%"),
                                        textInput("rtx_max_batch", "Maximum (X)",
                                                  value = "max", width = "90%"),
                                        textInput("rty_min_batch", "Minimum (Y)",
                                                  value = "min", width = "90%"),
                                        textInput("rty_max_batch", "Maximum (Y)",
                                                  value = "max", width = "90%")
                                    )
                                ),
                            ),
                            mainPanel(width = 0)
                        )
                    )
                ),
                tabPanel("Scoring Options",
                    fluidRow(
                        splitLayout(
                            wellPanel(
                                div(style = "text-align: center",
                                    h3("Feature Pair Scoring", align = "center"),
                                    sliderInput(inputId = "A_batch", label = "m/z weight", value = 75,
                                                min = 0, max = 150),
                                    sliderInput(inputId = "B_batch", label = "RT weight", value = 30,
                                                min = 0, max = 50),
                                    sliderInput(inputId = "C_batch", label = "Q weight", value = 0.25,
                                                min = 0, max = 1)
                                )
                             ),
                             wellPanel(
                                div(style = "text-align: center",
                                    h3("Row Annotation & Reduction", align = "center"),
                                    checkboxInput("useID_batchlabels", "Use Matching IDs"),
                                    sliderInput("minScore_batch", "minimum score", value = 0.5,
                                                min = 0, max = 1),
                                    splitLayout(
                                        numericInput("maxRankX_batch", "maximum X rank",
                                                    value = 2, min = 1, width = "75%"),
                                        numericInput("maxRankY_batch", "maximum Y rank",
                                                    value = 2, min = 1, width = "75%")
                                     ),
                                    splitLayout(
                                        numericInput("delta_batch", "delta score", value = 0.1,
                                                     max = 1, min = 0, width = "75%"),
                                        numericInput("maxRTerr_batch", "maximum RT error (minutes)",
                                                     value = 0.5, min = 0.01, width = "75%")
                                    )
                                )
                             )
                        )
                    )
                ),
                tabPanel("Output",
                    div(style = "text-align: center",
                        fluidRow(
                             wellPanel(
                                 h3("Run batchCombine", align = "center"),
                                 disabled(actionButton("batchCombine_Run", "Run batchCombine",
                                              width = "80%")),
                                 br(), br(),
                                 h3("Save Output", align = "center"),
                                 disabled(downloadButton("combinedTable_batch",
                                        "Download Combined Table Output", style = "width:80%;")),
                                 br(), br(),
                                 disabled(downloadButton("object_results_batch",
                                                "Download metabCombiner Object", style = "width:80%;")),
                                 br(), br(),
                                 disabled(downloadButton("params_out_batch", "Download User Parameters",
                                                style = "width:80%;"))
                            ),
                            verbatimTextOutput("batchCombine_process")
                        )
                    )
                )
            )
        )
    )
)
