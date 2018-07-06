#' Create a list of authors
#' 
#' This function creates a list of authors/affiliations interactively. The function prompt queries asking for the author name and its affiliations. The resulting object can be used straight away in the frontpage() function or saved as a csv file for further use.
#' @param save Logical. TRUE if the list created should be saved as a csv file usable by frontpage().
#' @param path If save is TRUE, the full path of the csv file to be created ("path/filename.csv").
#' @return A list of author/affiliation to be used with frontpage()
#' 
authors_names_interactive <- function(save = FALSE, path = NULL){
    if(!exists("authors"))
        authors <- list()
    
    nb_authors <- 1
    author <- NULL
    
    while(TRUE){
        author <- readline(prompt = cat("Name of the", toOrdinal::toOrdinal(nb_authors), 
                                        "author (type 'quit' if there is no more authors):"))
        
        if(author == 'quit')
            return(authors)
        
        if(is.element(author, names(authors))){
            add_anyway <- readline("This author is already in the list. Do you want to add it anyway (y/n)?\n")
            if(add_anyway == "n")
                next()
            
        }
        
        affiliations <- vector()
        
        while(TRUE){
            affiliation <- readline(prompt = cat("Affiliations of", author, 
                                                 " ('quit' if there is no more affiliations for this author):\n"))
            
            if(affiliation == 'quit')
                break
            
            if(affiliation %in% affiliations){
                warning("This affiliation already exists for that author and will not be added",
                        immediate. = TRUE)
                affiliation <- NULL
            }
            
            affiliations <- c(affiliations, affiliation)
        }
        
        authors[[author]] <- affiliations

        nb_authors <- nb_authors + 1
    }
}
