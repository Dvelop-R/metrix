#' Composition metrics
#'
#' Calculates the relative abundance of particular taxa in the assemblage in percentage terms.
#'
#' Provides information on the makeup of the assemblage and the relative contribution of the populations to the total fauna (Barbour et al., 1996).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated composition measures:
#' \item{per_ephe}{\% Ephemeroptera.}
#' \item{per_molus}{\% Mollusca.}
#' \item{per_gastr}{\%Gastropoda.}
#' \item{per_biv}{\%Bivalvia.}
#' \item{per_crus}{\%Crustacea.}
#' \item{per_oli}{\% Oligochaeta.}
#' \item{per_amph}{\% Amphipoda.}
#' \item{per_ostr}{\% Ostracoda.}
#' \item{per_ephetricho}{\% Ephemeroptera + Trichoptera.}
#' \item{per_naid}{\% Naididae.}
#' \item{per_chir_dip}{\% Diptera Chironomidae.}
#' \item{per_non_chir_dip}{\%Diptera no Chironomidae.}
#' \item{per_polym}{\%Polymitarcidae.}
#' \item{per_hyal}{\%Hyalella.}
#' \item{per_coch}{\%Cochliopidae.}
#' \item{per_tricho}{\%Trichoptera.}
#' \item{per_subchiro}{\%Chironominae.}
#' \item{per_suborth}{\%Orthocladiinae.}
#' \item{per_subtany}{\%Tanypodinae.}
#'
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Barbour MT, Gerritsen J, Griffith GE, Frydenborg R, McCarron E, White JS & Bastian ML (1996). A Framework for Biological Criteria for Florida Streams Using Benthic Macroinvertebrates. \doi{10.2307/1467948}
#'
#' @examples
#'
#'#Load example data
#' example_data
#'
#'#Run comp_metrics with that example_data
#' compmetrics<-comp_metrics(example_data)
#'
#'#Check results
#' compmetrics
#' @export


comp_metrics <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Composition measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

  if(ncol(dataset)==9){
    den_t<-sum(dataset[,9])
    den_ephe<-sum(dataset[dataset$Order=="Ephemeroptera",9])
    den_molus<-sum(dataset[dataset$Class=="Mollusca",9])
    den_gastr<-sum(dataset[dataset$Order=="Gastropoda",9])
    den_biv<-sum(dataset[dataset$Order=="Bivalvia",9])
    den_crus<-sum(dataset[dataset$Class=="Crustacea",9])
    den_oli<-sum(dataset[dataset$Order=="Oligochaeta",9])
    den_amph<-sum(dataset[dataset$Order=="Amphipoda",9])
    den_ostr<-sum(dataset[dataset$Order=="Ostracoda",9])
    den_tricho<-sum(dataset[dataset$Order=="Trichoptera",9])
    den_nai<-sum(dataset[dataset$Family=="Naididae",9])
    den_chir_dip<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                  dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                  dataset$Order=="Diptera")),9])
    den_non_chir_dip<-sum(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                      dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                      dataset$Order=="Diptera")),9])
    den_polym<-sum(dataset[dataset$Family=="Polymitarcydae",9])
    den_hyal<-sum(dataset[dataset$Family=="Hyalellidae",9])
    den_coch<-sum(dataset[dataset$Family=="Cochliopidae",9])
    den_subchiro<-sum(dataset[dataset$Subfamily=="Chironominae",9])
    den_suborth<-sum(dataset[dataset$Subfamily=="Orthocladiinae",9])
    den_subtany<-sum(dataset[dataset$Subfamily=="Tanypodinae",9])
  }else{
    den_t<-apply(dataset[,9:ncol(dataset)],2,sum)
    den_ephe<-(apply(dataset[dataset$Order=="Ephemeroptera",9:ncol(dataset)],2,sum))
    den_molus<-(apply(dataset[dataset$Class=="Mollusca",9:ncol(dataset)],2,sum))
    den_gastr<-(apply(dataset[dataset$Order=="Gastropoda",9:ncol(dataset)],2,sum))
    den_biv<-(apply(dataset[dataset$Order=="Bivalvia",9:ncol(dataset)],2,sum))
    den_crus<-(apply(dataset[dataset$Class=="Crustacea",9:ncol(dataset)],2,sum))
    den_oli<-(apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum))
    den_amph<-(apply(dataset[dataset$Order=="Amphipoda",9:ncol(dataset)],2,sum))
    den_ostr<-(apply(dataset[dataset$Order=="Ostracoda",9:ncol(dataset)],2,sum))
    den_tricho<-(apply(dataset[dataset$Order=="Trichoptera",9:ncol(dataset)],2,sum))
    den_nai<-(apply(dataset[dataset$Family=="Naididae",9:ncol(dataset)],2,sum))
    den_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                        dataset$Order=="Diptera" & dataset$Family=="Chironomidae",
                                        dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
    den_non_chir_dip<-apply(dataset[(ifelse(dataset$Order=="Diptera" & dataset$Family!="",
                                            dataset$Order=="Diptera" & dataset$Family!="Chironomidae",
                                            dataset$Order=="Diptera")),9:ncol(dataset)],2,sum)
    den_polym<-(apply(dataset[dataset$Family=="Polymitarcydae",9:ncol(dataset)],2,sum))
    den_hyal<-(apply(dataset[dataset$Family=="Hyalellidae",9:ncol(dataset)],2,sum))
    den_coch<-(apply(dataset[dataset$Family=="Cochliopidae",9:ncol(dataset)],2,sum))
    den_subchiro<-(apply(dataset[dataset$Subfamily=="Chironominae",9:ncol(dataset)],2,sum))
    den_suborth<-(apply(dataset[dataset$Subfamily=="Orthocladiinae",9:ncol(dataset)],2,sum))
    den_subtany<-(apply(dataset[dataset$Subfamily=="Tanypodinae",9:ncol(dataset)],2,sum))
    }

  per_ephe<-den_ephe/den_t*100
  per_ephe[is.na(per_ephe)]<-0
  per_molus<-den_molus/den_t*100
  per_molus[is.na(per_molus)]<-0
  per_gastr<-den_gastr/den_t*100
  per_gastr[is.na(per_gastr)]<-0
  per_biv<-den_biv/den_t*100
  per_biv[is.na(per_biv)]<-0
  per_crus<-den_crus/den_t*100
  per_crus[is.na(per_crus)]<-0
  per_oli<-den_oli/den_t*100
  per_oli[is.na(per_oli)]<-0
  per_amph<-den_amph/den_t*100
  per_amph[is.na(per_amph)]<-0
  per_ostr<-den_ostr/den_t*100
  per_ostr[is.na(per_ostr)]<-0
  per_tricho<-den_tricho/den_t*100
  per_tricho[is.na(per_tricho)]<-0
  per_ephetricho<-(den_ephe+den_tricho)/den_t*100
  per_ephetricho[is.na(per_ephetricho)]<-0
  per_naid<-den_nai/den_t*100
  per_naid[is.na(per_naid)]<-0
  per_chir_dip<-den_chir_dip/den_t*100
  per_chir_dip[is.na(per_chir_dip)]<-0
  per_non_chir_dip<-den_non_chir_dip/den_t*100
  per_non_chir_dip[is.na(per_non_chir_dip)]<-0
  per_polym<-den_polym/den_t*100
  per_polym[is.na(per_polym)]<-0
  per_hyal<-den_hyal/den_t*100
  per_hyal[is.na(per_hyal)]<-0
  per_coch<-den_coch/den_t*100
  per_coch[is.na(per_coch)]<-0
  per_subchiro<-den_subchiro/den_t*100
  per_subchiro[is.na(per_subchiro)]<-0
  per_suborth<-den_suborth/den_t*100
  per_suborth[is.na(per_suborth)]<-0
  per_subtany<-den_subtany/den_t*100
  per_subtany[is.na(per_subtany)]<-0




  rcd<-data.frame(per_ephe, per_molus, per_gastr, per_biv, per_crus, per_oli, per_amph, per_ostr,
                  per_ephetricho, per_naid, per_chir_dip, per_non_chir_dip, per_polym, per_hyal,
                  per_coch, per_tricho, per_subchiro, per_suborth, per_subtany, stringsAsFactors = TRUE)
  compmetrics<-data.frame(t(rcd))
  colnames(compmetrics)<-colnames(dataset[9:ncol(dataset)])
  compmetrics<-round(compmetrics,2)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_comp_metrics.csv",sep="")
    utils::write.table(x = compmetrics, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return (compmetrics)
}
