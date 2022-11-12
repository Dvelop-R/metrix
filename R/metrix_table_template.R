#' Metrix data template
#'
#' Metrix compatible table format template generator
#'
#' This function creates a properly formatted table for being used with metrix functions. \cr The format of the input table must contain 8 columns that refer to the scientific and functional classification of the taxa. The first 7 columns refer to Class, Order, Family, Subfamily, Tribe, Genus and Species. Special care must be taken when entering the taxa nomenclature, because if it is misspelled, the package will not take this classification into account. It is not necessary to put genera and species in italics. Column 8 refers to the functional group of the taxa, which can be filtering collectors (FC), gathering collectors (GC), predators (P), scrapers (SCR) and shredders (SHR). After these columns, the places where you want to calculate the packet metrics are entered. It is essential that the site data are located after these taxonomic and functional classification columns. The user will be able to load the table with the amount of taxa and sites, as desired. \cr If store = TRUE the function will create a .csv file with properly named columns and saves it as template.csv on the current working directory.
#' @param store A logical value indicating if the user want to store the results in a file.#'

#' @return The function returns:
#' \item{template_table}{A table that can be used as input for other metrix functions.}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#' @export


metrix_table_template <- function(store = FALSE)
{
  template_table<-data.frame(Class = "Insecta", Order = "Diptera", Family = "Chironomidae",
                  Subfamily = "Chironominae", Tribe = "Tanytarsini", Genus = "Paratanytarsus",
                  Species = "", FG = "GC", Sample1 = 1, Sample2 = 0)
  if(store==TRUE){
    utils::write.table(x = template_table, file = "template.csv", sep = ";", col.names = TRUE, row.names = FALSE)
  }
  return(template_table)
}
