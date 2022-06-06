reading_df <- function (file_path){
  return (read.csv(file_path, na.strings = "."))
}

counting_unique <- function (vec) {
  return (length (unique (vec) ))
}

s_unique <- function (nums){
  return (sort (unique (nums)))
}

# return column names with class numeric or integer 
colnames_numint  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "integer" | 
                                       sapply (df, class ) == "numeric"] ))
}

colnames_char  <- function (df){
  return (names (sapply (df, class )[sapply (df, class ) == "character"] ))
}

colnames_unique  <- function (df, n = 20, less_than = TRUE){
  if (less_than){return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) < n] ))}
  else {return (names (sapply (df, counting_unique )[sapply (df, counting_unique ) > n] ))}
}


stat_box_data <- function(y) {
  return( 
    data.frame(
      y = 1.05*max(y),  #may need to modify this depending on your data
      label = paste('n =', length(y), '\n',
                    'median =', round(median(y, na.rm = T), 2), '\n')
    )
  )
}

stat_box_data2 <- function(y) {
  return( 
    data.frame(
      y = .95 * median(y),  #may need to modify this depending on your data
      label = round(median(y, na.rm = T), 2)
    )
  )
}

remove_duplicate <- function (df, column){
  return (df %>% filter (!duplicated (get(id_col))))
}

covariates_plot <- function (df, cats, conts){
  result <- list ()
  i <- 1
  for (cat in cats){
    for (cont in conts){
      result[[paste0 ("fig",i)]] <- ggplotly(ggplot (df) + 
                                               aes (x = as.character (get(cat)), y = get (cont), fill = as.character (get (cat))) + 
                                               geom_boxplot () + 
                                               stat_summary(fun.data = stat_box_data, geom = "text", hjust = 0.5,vjust = 0.9) +
                                               #stat_summary(fun.data = stat_box_data2, geom = "text", hjust = 0.5,vjust = 0.9) +
                                               scale_y_continuous(expand = expansion(mult = c(0,0.1))) +
                                               labs (x = cat, y = cont, fill = cat)  )
      i <- i + 1
    }
  }
  return (result)
}