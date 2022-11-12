#' Read taxa data
#'
#' Load data from a formatted taxon table
#'
#' This function reads a formatted taxa .csv file and checks whether it is properly formatted. This function will determine which character to use as separator for data and decimals. \cr The format of the input table must contain 8 columns that refer to the scientific and functional classification of the taxa. The first 7 columns refer to Class, Order, Family, Subfamily, Tribe, Genus and Species. Special care must be taken when entering the taxa nomenclature, because if it is misspelled, the package will not take this classification into account. It is not necessary to put genera and species in italics. Column 8 refers to the functional group of the taxa, which can be filtering collectors (FC), gathering collectors (GC), predators (P), scrapers (SCR) and shredders (SHR). After these columns, the places where you want to calculate the packet metrics are entered. It is essential that the site data are located after these taxonomic and functional classification columns. The user will be able to load the table with the amount of taxa and sites, as desired. \cr Site columns with no entries and rows with no information of functional classification of the taxa will not be loaded. This function also has an autocorrect system that compares the words used to describe a taxon with a list of properly written words in order to find possible input errors. If correct = TRUE the autocorrect system will check all the names and perform corrections when possible (the file will not be modified). The autocorrect system will inform the user if it finds an issue that needs a manual check.
#'
#' @param file_name Name of formatted taxon table file. Use \code{metrix_table_template} to create a new formatted table file.
#' @param correct A logical value indicating if the auto correct system should be used (default \code{correct = TRUE}).
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return The function returns:
#' \item{dataset}{A table that can be used as input for other metrix functions.}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#' @export




read_data <- function(file_name, correct = TRUE, verbose = FALSE)
{

file_name->datafile

nf<-0

if (!(file.exists(datafile))){stop("File not found. Please check file name.")}


chk_sep <- readLines(file_name, n = 2)


if(verbose){message("Checking table format...")}


numfields <- utils::count.fields(textConnection(chk_sep[2]), sep = ";")

if (numfields == 1){

  spl<-strsplit(chk_sep[1],split=",")
  req_col_nam<-c("Class","Order","Family","Subfamily","Tribe","Genus","Species","FG")
  chk_form<-(spl[[1]][1:8] %in% req_col_nam)
  if(any(chk_form==FALSE)){
    message("Missing required columns:")
    message(req_col_nam[which(chk_form==FALSE)])
    stop("Check table format.")
  }


  dataset<-utils::read.csv(file_name, header = TRUE,strip.white = TRUE,blank.lines.skip = TRUE)
  if(is.character(dataset[1,9])){stop("Could not retrieve numerical values from table. Please check decimal separators.")}

}

if (numfields != 1){
  spl<-strsplit(chk_sep[1],split=";")
  req_col_nam<-c("Class","Order","Family","Subfamily","Tribe","Genus","Species","FG")
  chk_form<-(spl[[1]][1:8] %in% req_col_nam)
  if(any(chk_form==FALSE)){
    message("Missing required columns:")
    message(req_col_nam[which(chk_form==FALSE)])
    stop("Check table format.")
    }

  dataset<-utils::read.csv2(file_name, dec = ".", header = TRUE,strip.white = TRUE,blank.lines.skip = TRUE)
  if(is.character(dataset[1,9])){
    if(verbose){message ("Reading comma as decimal separator...")
    message (" ")}
    Sys.sleep(1)
    dataset<-utils::read.csv2(file_name, dec = ",", header = TRUE,strip.white = TRUE,blank.lines.skip = TRUE)
    if(is.character(dataset[1,9])){stop("Could not retrieve numerical values from table. Please check decimal separators.")}
  }
  if(verbose){message (" ")}
}

if(verbose){message("Checking empty rows and columns...")}
dataset[,9:ncol(dataset)][is.na(dataset[,9:ncol(dataset)])]<-0
emp_cat<-as.numeric(row.names(dataset[apply((dataset[,1:8]==""),1,all),]))
if(length(emp_cat)==0){emp_cat<-0
  }
if(ncol(dataset)==9){emp_val<-as.numeric(row.names(dataset[dataset[,9:ncol(dataset)]==0,]))
}else{
  emp_val<-as.numeric(row.names(dataset[apply((dataset[,9:ncol(dataset)]==0),1,all),]))
  }
if(length(emp_val)==0){emp_val<-0}
c_rows<-unique(c(emp_cat,emp_val))
if(any(c_rows==0)){c_rows<-c_rows[-which(c_rows==0)]}
if(length(c_rows)!=0){dataset=dataset[-c_rows,]}

cindex<-as.numeric(apply((dataset[,1:ncol(dataset)]==0),2,all))
emp_site<-which(cindex %in% 1)
emp_site_name<-colnames(dataset)[emp_site]
if(length(emp_site)!=0){dataset=dataset[,-emp_site]}

######
#Class

if(verbose){message ("Checking Class column from file...")}
if(anyNA(dataset$Class))
{
  if(verbose){message("No entries were registred on Class column.");
  message (" ")}

}else
{
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Class[chk_name_table$Class!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Class[dataset$Class!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
      if(identical(nf[[i]],integer(0))){next;}
      else{
        message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Class word database. Please check that entry.")
        if(correct==TRUE)
        {
          if(length(nf[[i]])>1){
            message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
            message("The matches are: ")
            for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
            stop("Please check that entry and modify it manually")
          }
          else{
            dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
            message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
          }
        }
        }
     }

  }
  if(verbose){message (" ")}
}
######
#ORDER

if(verbose){message ("Checking Order column from file...")}
if(anyNA(dataset$Order))
{
  if(verbose){message("No entries were registred on Order column.");
  message (" ")}

}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Order[chk_name_table$Order!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Order[dataset$Order!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
     if(identical(nf[[i]],integer(0)))
       {next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Order word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
      }

    }
}
  if(verbose){message(" ")}
}

######
#Family
if(verbose){message ("Checking Family column from file...")}
if(anyNA(dataset$Family))
{
  if(verbose){message("No entries were registred on Family column.");
  message (" ")}

}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Family[chk_name_table$Family!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Family[dataset$Family!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
      if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Family word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
    }
  }
}
  if(verbose){message (" ")}
}

######
#Subfamily
if(verbose){message ("Checking Subfamily column from file...")}
if(anyNA(dataset$Subfamily))
{
  if(verbose){message("No entries were registred on Subfamily column.");
  message (" ")}
}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Subfamily[chk_name_table$Subfamily!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Subfamily[dataset$Subfamily!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
      if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Subfamily word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
    }
  }
}
  if(verbose){message (" ")}
}
######
#Tribe
if(verbose){message ("Checking Tribe column from file...")}
if(anyNA(dataset$Tribe))
{
  if(verbose){  message("No entries were registred on Tribe column.");
  message (" ")}

}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Tribe[chk_name_table$Tribe!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Tribe[dataset$Tribe!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
    if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Tribe word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
    }

  }
}
  if(verbose){message (" ")}
}
######
#Genus
if(verbose){message("Checking Genus column from file...")}

if(anyNA(dataset$Genus))
{
  if(verbose){message ("No entries were registred on Genus column.");
  message (" ")}
}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Genus[chk_name_table$Genus!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Genus[dataset$Genus!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

    for (i in 1:length(nf)){
    if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Genus word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
    }
  }
}
  if(verbose){message (" ")}
}
######
#Species
if(verbose){message ("Checking Species column from file...")}
if(anyNA(dataset$Species))
{
  if(verbose){message ("No entries were registred on Species column.");
  message (" ")}


}else
  {
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Species[chk_name_table$Species!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Species[dataset$Species!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

  for (i in 1:length(nf)){
    if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ", uni_tlo_data[not_found==FALSE][i]," was not found in internal Species word database. Please check that entry.")
      if(correct==TRUE)
      {
        if(length(nf[[i]])>1){
          message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
          message("The matches are: ")
          for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
          stop("Please check that entry and modify it manually")
        }
        else{
          dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
          message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
        }
      }
    }
  }
}
  if(verbose){message (" ")}
}
######
#Functional.groups
if(verbose){message ("Checking FG column from file...")}
if(anyNA(dataset$FG))
{
  if(verbose){  message ("No entries were registred on FG column.");
  message (" ")}


}else{
  uni_tlo_chktable<-tolower(as.character(chk_name_table$Functional.groups[chk_name_table$Functional.groups!=""]))
  uni_tlo_data<-tolower(unique(as.character(dataset$Functional.groups[dataset$Functional.groups!=""])))

  not_found<- uni_tlo_data %in% uni_tlo_chktable

  if(!all(not_found)){
    nf<-lapply(uni_tlo_data[not_found==FALSE], agrep, x=uni_tlo_chktable)

  for (i in 1:length(nf)){
    if(identical(nf[[i]],integer(0))){next;}
    else{
      message("The word ",uni_tlo_data[not_found==FALSE][i]," was not found in internal Functional.groups word database. Please check that entry.")
        if(correct==TRUE)
        {
          if(length(nf[[i]])>1){
            message("The autocorrect system found ", length(nf[[i]])," possible matches for ", uni_tlo_data[not_found==FALSE][i]);
            message("The matches are: ")
            for(j in 1:length(nf[[i]])){message("\t", uni_tlo_chktable[nf[[i]][j]])}
            stop("Please check that entry and modify it manually")
          }
          else{
            dataset$Family[tolower(dataset$Family)==uni_tlo_data[not_found==FALSE][i]]<-stringr::str_to_title(uni_tlo_chktable[nf[[i]]])
            message("The autocorrect system replace the word ", uni_tlo_data[not_found==FALSE][i]," for ", stringr::str_to_title(uni_tlo_chktable[nf[[i]]]),".")
          }
        }
    }
  }
}
  if(verbose){message (" ")}
}

if(verbose){message("Data was succesfully read!")}

if(length(c_rows)!=0){if(verbose){message("Row/s removed: ", paste(c_rows, collapse = " "),".")}}
if(length(emp_site)!=0){if(verbose){message("Colum/s removed: ", paste(emp_site_name, collapse = " "),".")}}

return(dataset)
}

