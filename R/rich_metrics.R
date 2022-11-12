#' Richness metrics
#'
#' Calculates richness measures
#'
#' The richness measures reflect the diversity of the aquatic complex (Resh et al. 1995). Increased diversity correlates with increased assemblage health and suggests that niche space, habitat, and food source are adequate to support the survival and spread of many taxa. The number of taxa measures the general variety of the macroinvertebrate assemblage. Identities of major taxonomic groups are not derived from the total taxa metric, but the removal of taxa from naturally diverse systems can be detected easily (Barbour et al., 1996).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated richness measures:
#' \item{n_taxa}{N° total taxa.}
#' \item{n_fam}{N° of families.}
#' \item{n_gen}{N° of genus.}
#' \item{n_insec_fam}{N° of insects families.}
#' \item{n_non_insec_order}{N° of orders of invertebrates no insects.}
#' \item{n_dip_fam}{N° of Diptera families.}
#' \item{n_dip_gen}{N° of Diptera genus.}
#' \item{n_dip_chir_gen}{N° of Diptera Chironomidae genus.}
#' \item{n_chir_tax}{N° of Diptera Chironomidae taxa.}
#' \item{n_tany_tax}{N° of Tanytarisni taxa.}
#' \item{n_stemp_tax}{N° of Stempellina taxa.}
#' \item{n_non_chir_dip_tax}{N° of Diptera no Chironomidae taxa.}
#' \item{n_mol_tax}{N° of Mollusca taxa.}
#' \item{n_gastr_tax}{N° of Gastropoda taxa.}
#' \item{n_biv_tax}{N° of Bivalvia taxa.}
#' \item{n_crus_tax}{N° of Crustacea taxa.}
#' \item{n_crusmol}{N° of Crustacea + Mollusca taxa.}
#' \item{n_oligo_tax}{N° of Oligochaeta taxa.}
#' \item{n_ephetrich}{N° of Ephemeroptera + Trichoptera taxa.}
#'
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Resh VH, Norris RH & Barbour MT (1995). Design and implementation of rapid assessment approaches for water resource monitoring using benthic macroinvertebrates. \doi{10.1111/j.1442-9993.1995.tb00525.x}
#' @references Barbour MT, Gerritsen J, Griffith GE, Frydenborg R, McCarron E, White JS & Bastian ML (1996). A Framework for Biological Criteria for Florida Streams Using Benthic Macroinvertebrates. \doi{10.2307/1467948}
#'
#' @examples
#'#Load example data
#' example_data
#'
#'#Run rich_metrics with that example_data
#' richmetrics<-rich_metrics(example_data)
#'
#'#Check results
#' richmetrics
#' @export


rich_metrics <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Richness measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

  if(ncol(dataset)==9){
    n_taxa<-sum(dataset[,9]!=0)
    n_chir_tax<-numeric(length = length(9:ncol(dataset)))
    n_chir_tax<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                    dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                    dataset$Order=="Diptera")),9]!=0)
    n_non_chir_dip_tax<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                            dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                            dataset$Order=="Diptera")),9]!=0)
    n_mol_tax<-sum(dataset[dataset$Class=="Mollusca",9]!=0)
    n_oligo_tax<-sum(dataset[dataset$Order=="Oligochaeta",9]!=0)
    n_ephetrich<-sum(dataset[dataset$Order=="Trichoptera"|dataset$Order=="Ephemeroptera",9]!=0)
  }else{
    n_taxa<-apply(dataset[,9:ncol(dataset)],2,function(c)sum(c!=0))
    n_chir_tax<-numeric(length = length(9:ncol(dataset)))
    n_chir_tax<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                      dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                      dataset$Order=="Diptera")),
                              9:ncol(dataset)],2,function(c)sum(c!=0))
    n_non_chir_dip_tax<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                              dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                              dataset$Order=="Diptera")),
                                      9:ncol(dataset)],2,function(c)sum(c!=0))
    n_mol_tax<-apply(dataset[dataset$Class=="Mollusca",9:ncol(dataset)],2,function(c)sum(c!=0))
    n_oligo_tax<-apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,function(c)sum(c!=0))
    n_ephetrich<-apply(dataset[dataset$Order=="Trichoptera"|dataset$Order=="Ephemeroptera",
                               9:ncol(dataset)],2,function(c)sum(c!=0))}
  n_fam<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    fam=unique(as.character(aux$Family))
    n_fam[i-8]<-length(fam[fam!=""])
  }

  n_gen<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    gen=unique(as.character(aux$Genus))
    n_gen[i-8]<-length(gen[gen!=""])
  }

  n_insec_fam<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Family!="" & aux$Class=="Insecta",]
    fam=unique(as.character(aux$Family))
    n_insec_fam[i-8]<-length(fam)
  }

  n_non_insec_order<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Order!="" & aux$Class!="Insecta",]
    order=unique(as.character(aux$Order))
    n_non_insec_order[i-8]<-length(order)
  }

  n_dip_fam<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Family!="" & aux$Order=="Diptera",]
    fam=unique(as.character(aux$Family))
    n_dip_fam[i-8]<-length(fam)
  }

  n_dip_gen<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Genus!="" & aux$Order=="Diptera",]
    gen=unique(as.character(aux$Genus))
    n_dip_gen[i-8]<-length(gen)
  }

  n_dip_chir_gen<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Genus!="" & aux$Order=="Diptera" & aux$Family=="Chironomidae",]
    gen=unique(as.character(aux$Genus))
    n_dip_chir_gen[i-8]<-length(gen)
  }

  n_tany_tax<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    if(!anyNA(aux$Tribe)){
      aux<-aux[aux$Tribe=="Tanytarsini",]
      tri=as.character(aux$Tribe)
      n_tany_tax[i-8]<-length(tri)}
  }

  n_stemp_tax<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Genus=="Stempellina",]
    gen=as.character(aux$Genus)
    n_stemp_tax[i-8]<-length(gen)
  }
  n_gastr_tax<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Order=="Gastropoda",]
    order=as.character(aux$Order)
    n_gastr_tax[i-8]<-length(order)
  }

  n_biv_tax<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Order=="Bivalvia",]
    order=as.character(aux$Order)
    n_biv_tax[i-8]<-length(order)
  }

  n_crus_tax<-numeric(length = length(9:ncol(dataset)))
  for(i in 9:ncol(dataset))
  {
    aux<-dataset[dataset[,i]!=0,]
    aux<-aux[aux$Class=="Crustacea",]
    class=as.character(aux$Class)
    n_crus_tax[i-8]<-length(class)
  }
  n_crusmol<-n_crus_tax+n_mol_tax

  rcd<-data.frame(n_taxa, n_fam, n_gen, n_insec_fam, n_non_insec_order, n_dip_fam, n_dip_gen, n_dip_chir_gen,
                  n_chir_tax, n_tany_tax, n_stemp_tax, n_non_chir_dip_tax, n_mol_tax, n_gastr_tax, n_biv_tax,
                  n_crus_tax, n_crusmol, n_oligo_tax, n_ephetrich, stringsAsFactors = TRUE)
  richmetrics<-data.frame(t(rcd))
  colnames(richmetrics)<-colnames(dataset[9:ncol(dataset)])
  richmetrics<-round(richmetrics,2)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_rich_metric.csv",sep="")
    utils::write.table(x = richmetrics, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return(richmetrics)
    }
