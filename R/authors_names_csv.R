#' Create a list of authors
#' 
#' This function creates a list of authors/affiliations from a CSV file. The CSV should contain one authors per line, with the first column with the author full name, the subsequent for the affiliations (one affiliation per column only). The first column should be named "author".
#' 
#' @param file_path The full path of the CSV file
#' @return A list of author/affiliation to be used with coverpage()
#' 
#' @export
authors_names_csv <- function(file_path){
    author_df <- read.csv2(file_path, stringsAsFactors = F, encoding = "Latin-1")
    
    author_list <- list()
    
    for(num_author in seq(nrow(author_df))){
        affiliations <- author_df[num_author,-1]
        affiliations <- affiliations[which(affiliations != "")]
        author_list[[author_df[num_author,1]]] <- affiliations
    }
    
    return(author_list)
}


