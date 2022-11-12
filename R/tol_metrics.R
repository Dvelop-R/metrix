#' Tolerance metrics
#'
#' Indicate sensitivity of the assemblage and component species to various types of disturbance.
#'
#' Most of the metrics applied in the study of macroinvertebrates use as a key factor the tolerance or intolerance of the different taxa to a certain disturbance, normally organic contamination. The relationship between the number of organisms that are tolerant and intolerant to contamination is a common resource in the metrics used. Further metrics (multimetric indexes) can be derived from a combination of these primary metrics (Prat et al., 2009). The \emph{Limnodrilus hoffmeisteri}/total density ratio, which was developed by Marchese & Ezcurra de Drago (1999), increases in environments with organic contamination.
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a data.frame with all the calculated tolerance measures.:
#' \item{r_oligochir}{Oligochaeta/Chironomidae.}
#' \item{r_oligoset}{Oligochaeta with setaform chaetae/Oligochaeta without setaform chaetae.}
#' \item{r_tanychir}{Tanytarsini/Chironomidae.}
#' \item{den_t_lhoff}{\emph{Limnodrilus hoffmeisteri}/Total density.}
#' \item{den_t_bothr}{\emph{Bothrioneurum}/Total density.}
#' \item{den_t_tubi}{\emph{Tubifex}/Total density.}
#' \item{den_t_dero}{\emph{Dero}/Total density.}
#' \item{den_t_prist}{\emph{Pristina}/Total density.}
#' \item{den_t_chiro}{\emph{Chironomus}/Total density.}
#'
#'
#' @seealso \link[metrix]{read_data}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#'
#' @references Marchese M & Ezcurra de Drago I (1999). Use of benthic macroinvertebrates as organic pollution indicators in lotic environments of the Parana River drainage basin. \url{https://agro.icm.edu.pl/agro/element/bwmeta1.element.agro-article-e981d07b-e469-4460-a7fe-3239650cd089}
#' @references Prat N, Ríos B, Acosta R & Rieradevall M (2009). Los macroinvertebrados como indicadores de calidad de las aguas. \url{http://www.ub.edu/riosandes/docs/MacroIndLatinAmcompag0908.pdf}
#'
#' @examples
#'#Load example data
#' example_data
#'
#'#Run tol_metrics with that example_data
#' tolmetrics<-tol_metrics(example_data)
#'
#'#Check results
#' tolmetrics
#' @export


tol_metrics <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for Tolerance measures calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}


  #r_oligochir
  if(ncol(dataset)==9){
    den_oli<-sum(dataset[dataset$Order=="Oligochaeta",9])
    den_chironomidae<-sum(dataset[dataset$Family=="Chironomidae",9])
  }else{
    den_oli<-apply(dataset[dataset$Order=="Oligochaeta",9:ncol(dataset)],2,sum)
    den_chironomidae<-apply(dataset[dataset$Family=="Chironomidae",9:ncol(dataset)],2,sum)
  }
  r_oligochir<-den_oli/den_chironomidae
  r_oligochir[!is.finite(r_oligochir)] <- 0

  #r_oligoset
  non_q<-c("Limnodrilus", "Bothrioneurum" ,"Tubifex","Paranadrilus","Homochaeta",
           "Megadrili" , "Haplotaxis","Amphichaeta","Eiseniella","Brinkhurstia",
           "Chaetogaster","Bratislavia","Paranais")
  q<-c("Aulodrilus","Dero","Pristina","Slavina","Stphensoniana","Styllaria",
       "Allonais","Nais","Opistocysta","Trieminentia","Haemonais")
  if(ncol(dataset)==9){
    oli_q<-sum(dataset[dataset$Genus %in% q,9])
    oli_nq<-sum(dataset[dataset$Genus %in% non_q,9])
    }else{
    oli_q<-apply(dataset[dataset$Genus %in% q,9:ncol(dataset)],2,sum)
    oli_nq<-apply(dataset[dataset$Genus %in% non_q,9:ncol(dataset)],2,sum)
  }

  r_oligoset<-rep(0,ncol(dataset)-8)
  for(i in 1:length(r_oligoset)){
    if(oli_q[i]!=0 & oli_nq[i]!=0){
      r_oligoset[i]=oli_q[i]/oli_nq[i]}
    else{r_oligoset[i]<-0}
  }

  if(ncol(dataset==9)){
    r_tanychir<-rep(0,ncol(dataset)-8)
    if(!anyNA(dataset$Tribe))
    {
      den_tany<-sum(dataset[dataset$Tribe=="Tanytarsini",9])
      r_tanychir<-den_tany/den_chironomidae
      r_tanychir[!is.finite(r_tanychir)]<-0
    }
    den_t<-sum(dataset[,9])
    den_lhoff<-sum(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9])
    den_bothr<-sum(dataset[dataset$Genus=="Bothrioneurum",9])
    den_tubi<-sum(dataset[dataset$Genus=="Tubifex",9])
    den_dero<-sum(dataset[dataset$Genus=="Dero",9])
    den_prist<-sum(dataset[dataset$Genus=="Dero",9])
    den_chiro<-sum(dataset[dataset$Genus=="Chironomus",9])
    den_hele<-sum(dataset[dataset$Genus=="Heleobia",9])
  }else{
  r_tanychir<-rep(0,ncol(dataset)-8)
  if(!anyNA(dataset$Tribe))
  {
    den_tany<-apply(dataset[dataset$Tribe=="Tanytarsini",9:ncol(dataset)],2,sum)
    r_tanychir<-den_tany/den_chironomidae
    r_tanychir[!is.finite(r_tanychir)]<-0
  }
    den_t<-apply(dataset[,9:ncol(dataset)],2,sum)
    den_lhoff<-(apply(dataset[dataset$Species=="Limnodrilus hoffmeisteri",9:ncol(dataset)],2,sum))
    den_bothr<-(apply(dataset[dataset$Genus=="Bothrioneurum",9:ncol(dataset)],2,sum))
    den_tubi<-(apply(dataset[dataset$Genus=="Tubifex",9:ncol(dataset)],2,sum))
    den_dero<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))
    den_prist<-(apply(dataset[dataset$Genus=="Dero",9:ncol(dataset)],2,sum))
    den_chiro<-(apply(dataset[dataset$Genus=="Chironomus",9:ncol(dataset)],2,sum))
    den_hele<-(apply(dataset[dataset$Genus=="Heleobia",9:ncol(dataset)],2,sum))
  }

  den_t_lhoff<-den_lhoff/den_t
  den_t_lhoff<-as.double(den_t_lhoff)
  den_t_bothr<-den_bothr/den_t
  den_t_bothr<-as.double(den_t_bothr)
  den_t_tubi<-den_tubi/den_t
  den_t_tubi<-as.double(den_t_tubi)
  den_t_dero<-den_dero/den_t
  den_t_dero<-as.double(den_t_dero)
  den_t_prist<-den_prist/den_t
  den_t_prist<-as.double(den_t_prist)
  den_t_chiro<-den_chiro/den_t
  den_t_chiro<-as.double(den_t_chiro)
  den_t_hele<-den_hele/den_t
  den_t_hele<-as.double(den_t_hele)

  IdR<-data.frame(r_oligochir=r_oligochir,r_oligoset=r_oligoset, r_tanychir=r_tanychir, den_t_lhoff=den_t_lhoff,
                  den_t_bothr=den_t_bothr,den_t_tubi=den_t_tubi,den_t_dero=den_t_dero,den_t_prist=den_t_prist,
                  den_t_chiro=den_t_chiro,den_t_hele=den_t_hele)
  tolmetrics<-data.frame(t(IdR))
  colnames(tolmetrics)<-colnames(dataset[9:ncol(dataset)])
  tolmetrics<-round(tolmetrics,4)

  if(store==TRUE){
    result_fname<-paste(substitute(dataset),"_tol_metric.csv",sep="")
    utils::write.table(x = tolmetrics, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
    print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

  return(tolmetrics)

}
