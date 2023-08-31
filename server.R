library(shiny)
library(shinyjs)
source("helper.R")

server <- function(input, output, session){
    options(shiny.maxRequestSize=5000*1024^2)

    state <- reactiveValues(object = NULL,
                            finalobject = NULL,
                            plotAnchors = FALSE,
                            plotFit = FALSE,
                            xdata = NULL,
                            ydata = NULL,
                            batches = NULL,
                            batchComb = NULL,
                            xfile = "",
                            yfile = ""
    )

    output$xdata_example <- downloadHandler(
        filename = "example_plasma30.csv",
        content = function(file){
            data("plasma30")
            write.csv(plasma30, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    output$ydata_example <- downloadHandler(
        filename = "example_plasma20.csv",
        content = function(file){
            data("plasma20")
            write.csv(plasma20, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    getExt_X <- reactive({
        state$xfile <- ifelse(length(input$file_x) > 0, tools::file_ext(input$file_x$datapath), "")
    })


    output$input_x <- renderUI({
        getExt_X()

        if(state$xfile %in% c("","rds")){


        }

        else{
            tagList(fluidRow(
                h4("Column Keywords", align = "center"),
                splitLayout(
                    textInput("mz_x", label = strong("m/z"), value = "[Mm][Zz]|[Mm]/[Zz]|[Mm].[Zz]|[Mm]ass", width = '75%'),
                    textInput("rt_x", label = strong("retention time"), value = "[Rr][Tt]|[Rr]etention|[Tt]ime", width = '75%'),
                    textInput("id_x", label = "identifiers", value = "[Ii][Dd]|[Nn]ame", width = '75%'),
                    textInput("adduct_x", label = "adducts", value = "[Aa]dduct", width = '75%')
                )
            ),
            fluidRow(
                div(
                    splitLayout(
                        textInput("samples_x", label = "Samples Keyword(s) (separate by commas)",
                                  placeholder = "e.g. POOL", width = "75%"),
                        textInput("extra_x", label = "Extra Keyword(s) (separate by commas)",
                                  placeholder = "e.g. Blank,CHEAR", width = "75%")
                    ),
                    style = "font-size: 80%",
                )
            ),
            h4("Filters", align = "center"),
            fluidRow(
                splitLayout(
                    textInput("rtmin_x", "Starting Retention Time", value = "min", width = "75%"),
                    textInput("rtmax_x", "Ending Retention Time", value = "max", width = "75%")
                )
            ),
            fluidRow(
                splitLayout(
                    numericInput("misspc_x", label = "% Missing Tolerance (0-99)",
                                 value = 50, width = "75%"),
                    checkboxInput("zero_x", "Treat 0 as Missing Intensity Value",
                                  width = "75%")
                )
            ),
            fluidRow(
                splitLayout(
                    numericInput("dupmz_x", "duplicate m/z tolerance",
                                 value = "0.0025", width = "75%"),
                    numericInput("duprt_x", "duplicate RT tolerance",
                                 value = "0.05", width = "75%"),
                    radioButtons("dup_opt_x", "duplicate feature options",
                                 choices = c("Keep Single Row" = "single", "Merge Values" = "merge"))
                )
            )
        )}
    })

    output$button_x <- renderUI({
        ext <- ifelse(length(input$file_x) > 0, tools::file_ext(input$file_x$datapath), "")
        if(ext == "")
            disabled(div(actionButton("data_x_inactive", label = strong("process X dataset"),
                             width = '200px', style = "text-align: center")))
        else
            div(actionButton("data_x", label = strong("process X dataset"),
                             width = '200px', style = "text-align: center"))
    })


    getExt_Y <- reactive({
        state$yfile <- ifelse(length(input$file_y) > 0, tools::file_ext(input$file_y$datapath), "")
    })

    output$input_y <- renderUI({
        getExt_Y()

        if(state$yfile %in% c("","rds")){

        }
        else{
            tagList(
                fluidRow(
                    h4("Column Keywords", align = "center"),
                    splitLayout(
                        textInput("mz_y", label = strong("m/z"), value = "[Mm][Zz]|[Mm]/[Zz]|[Mm].[Zz]|[Mm]ass", width = '75%'),
                        textInput("rt_y", label = strong("retention time"), value = "[Rr][Tt]|[Rr]etention|[Tt]ime", width = '75%'),
                        textInput("id_y", label = "identifiers", value = "[Ii][Dd]|[Nn]ame", width = '75%'),
                        textInput("adduct_y", label = "adducts", value = "[Aa]dduct", width = '75%')
                    )
                ),
                fluidRow(
                    div(
                        splitLayout(
                            textInput("samples_y", label = "Samples Keyword(s) (separate by commas)",
                                      placeholder = "e.g. POOL", width = "75%"),
                            textInput("extra_y", label = "Extra Keyword(s) (separate by commas)",
                                      placeholder = "e.g. Blank,CHEAR", width = "75%")
                        ),
                        style = "font-size: 80%",
                    )
                ),
                h4("Filters", align = "center"),
                fluidRow(
                    splitLayout(
                        textInput("rtmin_y", "Starting Retention Time", value = "min", width = "75%"),
                        textInput("rtmax_y", "Ending Retention Time", value = "max", width = "75%")
                    )
                ),
                fluidRow(
                    splitLayout(
                        numericInput("misspc_y", label = "% Missing Tolerance (0-99)",
                                     value = 50, width = "75%"),
                        checkboxInput("zero_y", "Treat 0 as Missing Intensity Value",
                                      width = "75%")
                    )
                ),
                fluidRow(
                    splitLayout(
                        numericInput("dupmz_y", "duplicate m/z tolerance",
                                    value = "0.0025", width = "60%"),
                        numericInput("duprt_y", "duplicate RT tolerance",
                                    value = "0.05", width = "60%"),
                        radioButtons("dup_opt_y", "duplicate feature options",
                                    choices = c("Keep Single Row" = "single", "Merge Values" = "merge"))
                    )
                )
            )
        }
    })

    output$button_y <- renderUI({
        ext <- ifelse(length(input$file_y > 0), tools::file_ext(input$file_y$datapath), "")
        if(ext == "")
            disabled(div(actionButton("data_y_inactive", label = strong("process Y dataset"),
                                      width = '200px', style = "text-align: center")))
        else
            div(actionButton("data_y", label = strong("process Y dataset"),
                             width = '200px', style = "text-align: center"))
    })

    observeEvent(input$data_x,{
        req(input$file_x)
        ext <- tools::file_ext(input$file_x$datapath)
        shiny::validate(need(ext %in% c("csv", "txt", "rds"),
                      "File Input Must be .csv, .txt, or .rds"))

        if(ext == "rds")
            state$xdata <- readRDS(input$file_x$datapath)

        else{
            rtbounds <- validateRTbounds(input$rtmin_x, input$rtmax_x)

            id_x <- input$id_x
            if(id_x == "") id_x <- NULL
            else id_x <- gsub(",", "|", id_x)

            adduct_x <- input$adduct_x
            if(adduct_x == "") adduct_x <- NULL
            else adduct_x <- gsub(",", "|", adduct_x)

            samples_x <- input$samples_x
            if(samples_x == "") samples_x <- NULL
            else samples_x <- gsub(",", "|", samples_x)

            extra_x <- input$extra_x
            if(extra_x == "") extra_x <- NULL
            else extra_x <- gsub(",", "|", extra_x)

            state$xdata <- attCatch(metabData(input$file_x$datapath, mz = input$mz_x,
                                        rt = input$rt_x, id = input$id_x, adduct = input$adduct_x,
                                        samples = samples_x, extra = extra_x,
                                        zero = input$zero_x, rtmin = rtbounds[[1]],
                                        rtmax = rtbounds[[2]], misspc = input$misspc_x,
                                        duplicate = opts.duplicate(input$dupmz_x, input$duprt_x, input$dup_opt_x)),
                                        state$xdata)
        }

        if(!is.null(state$xdata) & !is.null(state$ydata))
            enable("mzgroup")

    })

    observeEvent(input$data_y,{
        req(input$file_y)

        ext <- tools::file_ext(input$file_y$datapath)
        shiny::validate(need(ext %in% c("csv", "txt", "rds"),
                      "File Input Must be .csv, .txt, or .rds"))

        if(ext == "rds")
            state$ydata <- readRDS(input$file_y$datapath)

        else{
            id_y <- input$id_y
            if(id_y == "") id_y <- NULL
            else id_y <- gsub(",", "|", id_y)

            adduct_y <- input$adduct_y
            if(adduct_y == "") adduct_y <- NULL
            else adduct_y <- gsub(",", "|", adduct_y)

            samples_y <- input$samples_y
            if(samples_y == "") samples_y <- NULL
            else samples_y <- gsub(",", "|", samples_y)

            extra_y <- input$extra_y
            if(extra_y == "") extra_y <- NULL
            else extra_y <- gsub(",", "|", extra_y)

            rtbounds <- validateRTbounds(input$rtmin_y, input$rtmax_y)

            state$ydata <- attCatch(metabData(input$file_y$datapath, mz = input$mz_y,
                                        rt = input$rt_y, id = input$id_y, adduct = input$adduct_y,
                                        samples = samples_y,  extra = extra_y,
                                        zero = input$zero_y, rtmin = rtbounds[[1]],
                                        rtmax = rtbounds[[2]], misspc = input$misspc_y,
                                        duplicate = opts.duplicate(input$dupmz_y, input$duprt_y, input$dup_opt_y)),
                                        state$ydata)
        }

        if(!is.null(state$xdata) & !is.null(state$ydata))
            enable("mzgroup")

    })

    output$xid_input <- renderUI({
        if(is(state$xdata, "metabCombiner"))
            selectInput("xid", label = "Dataset X ID", choices = datasets(state$xdata),
                        selected = x(state$xdata), width = "75%")
        else if(is(state$xdata, "metabData")){
            if(is(state$ydata, "metabCombiner"))
                default <- as.character(length(datasets(state$ydata))+1)
            else
                default <- "1"

            textInput("xid", label = "Dataset X ID", value = default, width = "75%")
        }
    })

    output$yid_input <- renderUI({
        if(is(state$ydata, "metabCombiner"))
            selectInput("yid", label = "Dataset Y ID", choices = datasets(state$ydata),
                        selected = y(state$ydata), width = "75%")
        else if(is(state$ydata, "metabData")){
            if(is(state$xdata, "metabCombiner"))
                default <- as.character(length(datasets(state$xdata))+1)
            else
                default <- "2"

            textInput("yid", label = "Dataset Y ID", value = default, width = "75%")
        }
    })

    output$xdata_summary <- renderPrint({
        if(!is.null(state$xdata))
            print(state$xdata)
    })

    output$ydata_summary <- renderPrint({
        if(!is.null(state$ydata))
            print(state$ydata)
    })

    ##we will need to upgrade this when we use metabCombiner objects
    observeEvent(input$mzgroup, {
        state$object <- attCatch(metabCombiner(state$xdata, state$ydata, xid = input$xid,
                                       yid = input$yid, binGap = input$binGap), state$object)
        state$plotAnchors <- FALSE
        state$plotFit <- FALSE
        state$finalobject <- NULL
    })

    output$object_summary <- renderPrint({
        if(!is.null(state$object))
            print(state$object)
    })

    observeEvent(input$anchors_select,{
        state$object <- attCatch(selectAnchors(state$object, useID = input$useID,
                                tolmz = input$tolmz, tolQ = input$tolQ,
                                tolrtq = input$tolrtq, windx = input$windx,
                                windy = input$windy), state$object)
        state$plotAnchors <- TRUE
        state$plotFit <- FALSE
    })

    observeEvent(input$plot_anchors,{
        state <- updatePlot(state, TRUE, FALSE)
    })

    observeEvent(input$fit_gam,{
        k = as.numeric(strsplit(input$k, ",")[[1]])

        withCallingHandlers({
            shinyjs::html("fit_progress", "")
            state$object <- attCatch(fit_gam(state$object,
                                    iterFilter = input$iterFilter, k = k,
                                    family = input$family,
                                    rtx = c(input$rtx_min, input$rtx_max),
                                    rty = c(input$rty_min, input$rty_max),
                                    coef = input$outlier_coef,
                                    prop = input$outlier_prop,
                                    outlier = input$outlier_method), state$object)
            },
            message = function(m) {
                shinyjs::html(id = "fit_progress", html = m$message, add = TRUE)
            }
        )

        state <- updatePlot(state, FALSE, TRUE)
    })

    observeEvent(input$model_fit,{
        state <- updatePlot(state, FALSE, TRUE)
    })

    output$plot_fit <- renderPlot({
        if(state$plotAnchors){
            anchors <- getAnchors(state$object)
            plot(anchors$rtx, anchors$rty, main = input$title,
                 xlab = input$xlab, ylab = input$ylab,
                 col = input$pcol, cex = input$cex,
                 pch = 19)
        }
        else if(state$plotFit){
            plot(state$object, main = input$title,
                xlab = input$xlab, ylab = input$ylab,
                pcol = input$pcol, cex = input$cex,
                outlier = input$outlier_opt, lwd = input$lwd,
                lcol = input$lcol, ocol = input$ocol)
        }
        else{
            plot.new()
        }},
        width = 800, height = 800, res = 150
    )

    observeEvent(input$calcScores,{
        state$object <- attCatch(calcScores(state$object, A = input$A,
                                B = input$B, C = input$C), state$object)
    })

    observeEvent(input$labelRows,{
        state$object <- attCatch(labelRows(state$object, minScore = input$minScore,
                                    maxRankX = input$maxRankX, maxRankY = input$maxRankY,
                                    delta = input$delta, remove = FALSE,
                                    resolveConflicts = FALSE,  maxRTerr = input$maxRTerr,
                                    useID = input$useID_labels), state$object)

        state$finalobject <- attCatch(reduceTable(state$object, minScore = input$minScore,
                                    maxRankX = input$maxRankX,  maxRankY = input$maxRankY,
                                    delta = input$delta, maxRTerr = input$maxRTerr,
                                    useID = input$useID_labels), state$finalobject)

        if(isTRUE(input$update_X)){
            state$object <- attCatch(
                metabCombiner::updateTables(state$object, xdata = state$xdata),
                    state$object)
            state$finalobject <- attCatch(
                metabCombiner::updateTables(state$finalobject,
                    xdata = state$xdata), state$finalobject)
        }

        if(isTRUE(input$update_Y)){
            state$object <- attCatch(
                metabCombiner::updateTables(state$object, ydata = state$ydata),
                    state$object)
            state$finalobject <- attCatch(
                metabCombiner::updateTables(state$finalobject, ydata = state$ydata),
                    state$finalobject)
        }
    })

    output$object_summary_final <- renderPrint({
        if(!is.null(state$finalobject))
            print(state$finalobject)

        else if(!is.null(state$object) & is.null(state$finalobject))
            print(state$object)
    })


    output$combinedTable <- downloadHandler(
        filename = "results.csv",
        content = function(file){
            req(!is.null(state$finalobject))
            data <- combinedTable(state$finalobject)
            write.csv(data, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    output$write2file <- downloadHandler(
        filename = "results.csv",
        content = function(file){
            req(!is.null(state$finalobject))
            metabCombiner::write2file(state$finalobject, file)
        },
        contentType = "text/csv"
    )

    output$finalobject_results <- downloadHandler(
        filename = "results.rds",
        content = function(file){
            req(!is.null(state$finalobject))
            saveRDS(state$finalobject, file)
        }
    )
  #guh
    output$combinedTable_full <- downloadHandler(
        filename = "results.csv",
        content = function(file){
            req(!is.null(state$object))
            data <- combinedTable(state$object)
            write.csv(data, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    output$write2file_full <- downloadHandler(
        filename = "results.csv",
        content = function(file){
            req(!is.null(state$object))
            metabCombiner::write2file(state$object, file)
        },
        contentType = "text/csv"
    )

    output$object_results <- downloadHandler(
        filename = "results.rds",
        content = function(file){
            req(!is.null(state$object))
            saveRDS(state$object, file)
        }
    )

    output$xdata_download <- downloadHandler(
        filename = "xdata.csv",
        content = function(file){
            req(!is.null(state$xdata))
            if(is(state$xdata, "metabData"))
                data <- getData(state$xdata)
            else if(is(state$xdata, "metabCombiner"))
                data <- combinedTable(state$xdata)
            write.csv(data, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    output$ydata_download <- downloadHandler(
        filename = "ydata.csv",
        content = function(file){
            req(!is.null(state$ydata))
            if(is(state$ydata, "metabData"))
                data <- getData(state$ydata)
            else if(is(state$ydata, "metabCombiner"))
                data <- combinedTable(state$ydata)
            write.csv(data, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )


    output$xdata_results <- downloadHandler(
        filename = "xdata.rds",
        content = function(file){
            req(!is.null(state$xdata))
            saveRDS(state$xdata, file)
        }
    )

    output$ydata_results <- downloadHandler(
        filename = "ydata.rds",
        content = function(file){
            req(!is.null(state$ydata))
            saveRDS(state$ydata, file)
        }
    )

    observeEvent(input$enable_newdata, {
        shinyjs::enable("use_as_xdata")
        shinyjs::enable("use_as_ydata")
    }, once = TRUE)

    observeEvent(input$use_as_xdata, {
        req(!is.null(state$finalobject))
        state$xdata <- state$finalobject
        state$xfile <- ""
    })

    observeEvent(input$use_as_ydata, {
        req(!is.null(state$finalobject))
        state$ydata <- state$finalobject
        state$yfile <- ""
    })

    output$xdata_view <- renderTable(
        if(is(state$xdata, "metabData"))
            getData(state$xdata)[seq(1,min(20,nrow(getData(state$xdata)))),
                                 seq(1,min(50,ncol(getData(state$xdata))))]
        else if(is(state$xdata, "metabCombiner"))
            combinedTable(state$xdata)[seq(1,min(20,nrow(combinedTable(state$xdata)))),
                                       seq(1,min(50,ncol(combinedTable(state$xdata))))]

    )

    output$ydata_view <- renderTable(
        if(is(state$ydata, "metabData"))
            getData(state$ydata)[seq(1,min(20, nrow(getData(state$ydata)))),
                                       seq(1,min(50,ncol(getData(state$ydata))))]
        else if(is(state$ydata, "metabCombiner"))
            combinedTable(state$ydata)[seq(1,min(20,nrow(featData(state$ydata)))),
                                        seq(1,min(50,ncol(combinedTable(state$ydata))))]
    )

    output$combinedTable_view <- renderTable(
        if(!is.null(state$object)){
            combinedTable(state$object)[seq(1,min(20,nrow(featData(state$object)))),
                                        seq(1,min(100,ncol(combinedTable(state$object))))]
        }
    )

    output$featData_view <- renderTable(
        if(!is.null(state$object)){
            featData(state$object)[seq(1,min(20,nrow(featData(state$object)))),
                                    seq(1,min(100,ncol(featData(state$object))))]
        }
    )

    output$anchors_view <- renderTable(
        if(!is.null(state$object)){
            if(nrow(getAnchors(state$object) > 0))
                getAnchors(state$object)
      }
    )


    output$newdata_warning <- renderPrint(
        if(input$enable_newdata)
            print(paste("warning: clicking one of the above buttons will overwrite",
                        "the current X dataset or Y dataset object", sep = " "))

    )



    ###########################  batchCombine ##################################
    observeEvent(input$process_batch, {
        req(input$files_batches)
        ext <- sapply(input$files_batches$datapath, tools::file_ext)
        shiny::validate(need(all(ext %in% c("csv", "txt")),
                             "all input files must be .csv, .txt"))

        samples_batch <- input$samples_batch
        if(samples_batch == "") samples_batch <- NULL
        else samples_batch <- gsub(",", "|", samples_batch)

        extra_batch <- input$extra_batch
        if(extra_batch == "") extra_batch <- NULL
        else extra_batch <- gsub(",", "|", extra_batch)

        rtbounds <- validateRTbounds(input$rtmin_batch, input$rtmax_batch)
        state$batches <- attCatch(lapply(input$files_batches$datapath, metabCombiner::metabData,
                                  mz = input$mz_batch, rt = input$rt_batch, id = input$id_batch,
                                  adduct = input$adduct_batch, samples = samples_batch,
                                  extra = extra_batch, rtmin = rtbounds[1], rtmax = rtbounds[2],
                                  misspc = input$misspc_batch,
                                  duplicate = opts.duplicate(input$dupmz_batch, input$duprt_batch, input$dup_opt_batch)),
                                  state$batches)
        if(is.list(state$batches)){
            names(state$batches) = paste0(input$batch_id, seq(1,length(state$batches)))
            enable("batchCombine_Run")
        }
    })

    output$batch_summary <- renderPrint({
        req(!is.null(state$batches))
        total_batches <- length(state$batches)
        topline <- paste("Total Batches:", total_batches)
        batch_info <- paste(sapply(seq_along(state$batches), function(b){
            batch = state$batches[[b]]
            paste("Batch", b, ": Initial Features-", getStats(batch)$input_size,
                "; Final Feature Count-",  getStats(batch)$final_count)
        }), collapse = "\n")

        cat(batch_info)
    })

    paramlists <- eventReactive(input$batchCombine_Run,{
        req(!is.null(state$batches))

        k = as.numeric(strsplit(input$k_batch, ",")[[1]])

        saparam <- selectAnchorsParam(useID = input$useID_batch,
                                    tolmz = input$tolmz_batch,
                                    tolQ = input$tolQ_batch,
                                    tolrtq = input$tolrtq_batch,
                                    windx = input$windx_batch,
                                    windy = input$windy_batch)

        fitparam <- fitgamParam(useID = input$useID_batch, k = k,
                                iterFilter = input$iterFilter_batch,
                                outlier = input$outlier_method_batch,
                                coef = input$outlier_coef_batch,
                                prop = input$outlier_prop_batch,
                                family = input$family_batch,
                                rtx = c(input$rtx_min_batch,input$rtx_max_batch),
                                rty = c(input$rty_min_batch,input$rty_max_batch),
                                message = FALSE)

        scoreparam <- calcScoresParam(A = input$A_batch,
                                      B = input$B_batch,
                                      C = input$C_batch)

        reduceparam <- reduceTableParam(minScore = input$minScore_batch,
                                    maxRankX = input$maxRankX_batch,
                                    maxRankY = input$maxRankY_batch,
                                    delta = input$delta_batch,
                                    maxRTerr = input$maxRTerr_batch,
                                    useID = input$useID_batchlabels)

        params <- list(binGap = input$binGap_batch, saparam = saparam,
                       fitparam = fitparam, scoreparam = scoreparam,
                       reduceparam = reduceparam)

        return(params)
    })

    observeEvent(input$batchCombine_Run, {
        withCallingHandlers({
            shinyjs::html("batchCombine_progress", "")
            params <- attCatch(paramlists(), list())
            state$batchComb <- attCatch(batchCombine(state$batches, params$binGap,
                                                     "gam", means = TRUE, params$saparam,
                                                     union = input$bcmode == "union",
                                                     params$fitparam, params$scoreparam,
                                                     params$reduceparam), state$batchComb)
        },message = function(m) {
            shinyjs::html(id = "batchCombine_progress", html = m$message, add = TRUE)
        })

        enable("combinedTable_batch")
        enable("object_results_batch")
    })

    output$combinedTable_batch <- downloadHandler(
        filename = "results.csv",
        content = function(file){
            req(!is.null(state$batchComb))
            write.csv(state$batchComb$table, file, row.names = FALSE, na = "")
        },
        contentType = "text/csv"
    )

    output$object_results_batch <- downloadHandler(
        filename = "results.rds",
        content = function(file){
            req(!is.null(state$batchComb))
            saveRDS(state$batchComb$object, file)
        }
    )
}

