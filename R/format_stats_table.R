format_stats_table <- function( capture, model_description , outputname) {

    # parse html file of multiple lines
    newHtml<- ""
    for(i in 1:length(capture)) {

      newHtml<- paste (newHtml,capture[i] )
    }

    # transform it back in html
    my_df <- as.data.frame(xml2::read_html(newHtml) %>% rvest::html_table(fill=TRUE))

    # remove top line with useless info and bottom lines too
    my_df <- my_df[-c(1:6,8:9),]

    #gather pvalues threshold and remove bottom two lines
    pvaluesthresholds <- my_df[c(nrow(my_df)),2]
    my_df <- my_df[-c(nrow(my_df)),]
    my_df <- my_df[-c(nrow(my_df)),]

    #library(kableExtra)


    styling_function_1 <- function(x) {
      kableExtra::cell_spec(x, "html",
                            bold = ifelse( grepl("**",x, fixed = T) , TRUE, FALSE),
                            color = ifelse( grepl("*",x, fixed = T), "black", "#bfbfbf") ,
                            background = ifelse( grepl("*",x, fixed = T) & grepl("-",x, fixed = T) & grepl("(",x, fixed = T), "#d6ecfb", ifelse(grepl("*",x, fixed = T) & grepl("(",x, fixed = T), "#fbe5d6", "#ffffff")) )
    }

    styling_function_2 <- function(x) {
      kableExtra::cell_spec(x, "html",
                            bold = ifelse( grepl("**",x, fixed = T) , TRUE, FALSE),
                            color = ifelse( grepl("*",x, fixed = T) & grepl("-",x, fixed = T) & grepl("(",x, fixed = T), "#2372A7",
                                            ifelse(grepl("*",x, fixed = T) & grepl("(",x, fixed = T), "#B40000", "#cccccc")) )
    }


    table_title <- paste("<font color='black'> <b> Table for model:", model_description,"</b></font>")

    my.names <- my_df[1,]
    my.names$X1 <- "Coefficients"
    colnames(my_df) <- my.names
    my_df <- my_df[-c(1),]

    my_df %>% dplyr::mutate_at(c(2:ncol(my_df))  , styling_function_2  ) %>% kableExtra::kable(format = "html", escape = F, caption = table_title) %>%
      kableExtra::kable_styling("striped", full_width = F) %>% kableExtra::column_spec(1, bold = T, color="black")  %>%
      kableExtra::footnote(general = paste("stars:", pvaluesthresholds)
      ) %>%  kableExtra::save_kable(file = outputname)

}




# drafts


#aa <- newHtml %>% htmlTable()


