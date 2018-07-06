#' Scientfic article coverpage builder
#' 
#' This function creates a proper frontpage for scientific articles, with superscript numbers 
#' refering to authors affiliations
#' 
#' @param author_list A named list of author/affiliations, with the author names as names of the 
#' list, and affiliations as the content of the list. Can be created with the functions 
#' authors_names_interactive() or author_names_csv()
#' @param path The directory path in which the coverpage will be saved (coverpage.docx)
#' @return Save a coverpage.docx with names and affiliations
#' 
#' @export
frontpage <- function(author_list, path){
    sorted_institutes <- vector()
    
    for(author in author_list){
        for(institute in author){
            if(!is.element(institute, sorted_institutes))
                sorted_institutes <- append(sorted_institutes, institute)
        }
    }
    
    numered_authors <- names(author_list)[1] + 
        pot(paste(which(sorted_institutes %in% author_list[[1]]), collapse = ", "),
            textProperties(vertical.align = "superscript"))
    
    for(author_name in names(author_list)[-1]){
        numered_author <- paste(",", author_name) + 
            pot(paste(which(sorted_institutes %in% author_list[[author_name]]), collapse = ", "),
                textProperties(vertical.align = "superscript"))
        numered_authors <- numered_authors + numered_author
    }
    
    doc <- docx()
    doc <- addParagraph(doc, numered_authors)
    
    for(i in seq(length(sorted_institutes))){
        doc <- addParagraph(doc, paste(i, sorted_institutes[i], sep = ". "))
    }

    writeDoc(doc, file.path(path, "frontpage.docx"))
}




