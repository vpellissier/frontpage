#' Create a list of authors
#' 
#' This function creates a list of authors/affiliations interactively
#' 
#' @return A list of author/affiliation to be used with coverpage()
#' 
#' @export
authors_names_interactive <- function(){
    if(!exists("authors"))
        authors <- list()
    
    nb_authors <- 1
    author <- NULL
    
    while(TRUE){
        author <- readline(prompt = cat("Name of the", toOrdinal::toOrdinal(nb_authors), "author:"))
        
        if(is.element(author, names(authors))){
            add_anyway <- readline("This author is already in the list. Do you want to add it anyway (y/n)?\n")
            if(add_anyway == "n")
                next()
            
        }
        
        affiliations <- vector()
        
        while(TRUE){
            affiliation <- readline(prompt = cat("Affiliations of", author, ":\n"))
            
            if(affiliation %in% affiliations){
                warning("This affiliation already exists for that author and will not be added",
                        immediate. = TRUE)
                affiliation <- NULL
            }
            
            affiliations <- c(affiliations, affiliation)
            
            more_affiliations <- readline(prompt = cat("Does", author, 
                                                       "have more affiliations (y/n)?\n"))
            if(more_affiliations == "n")
                break()
        }
        
        authors[[author]] <- affiliations
        
        more_author <- readline(prompt = "Is there more authors to add (y/n)\n?")
        if(more_author == "n")
            return(authors)
        
        nb_authors <- nb_authors + 1
    }
}