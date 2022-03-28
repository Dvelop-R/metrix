#' BMWP prime prime index
#'
#' Calculate Biological Monitoring Working Party (BMWP) prime prime index (Loyola, 2000)
#'
#'The new BMWP’’ is an adaptation of the BMWP (Armitage et al. 1983) for the lotic environments of the Paraná River. This adaptation was based on the observation of the occurrence of the important families in the rivers in the region. Some families were added by ecological equivalence and others by similarity in the level of tolerance to contamination. The scores assigned to the different families were not changed (Loyola, 2000).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{Ibmwp_p_p_n}{The numerical BMWP’’ index (Loyola, 2000).}
#' \item{Ibmwp_p_p_c}{The BMWP’’ water quality classes (Loyola, 2000).}
#'
#' @seealso \link[metrix]{read_data}, \link{bmwp_ind}, \link{bmwp_p_ind}, \link{biotic_ind}
#' @author Juan Manuel Cabrera and Julieta Capeleti.
#' @references
#' \itemize{
#' \item{Armitage PD, Moss D, Wright JF & Furse MT (1983) \doi{10.1016/0043-1354(83)90188-4>}}
#' \item{Loyola RGN (2000) <https://www.iat.pr.gov.br/sites/agua-terra/arquivos_restritos/files/documento/2021-03/bioindicadores_qualidade_aguas_2001_2002.pdf>}
#' }
#' @examples
#'
#'#Example data is a properly formatted table with richness measures of two sites
#' example_data
#'
#'#Run bmwp_p_p_ind with that example_data
#' bmwpp<-bmwp_p_p_ind(example_data)
#'
#'#Check results
#' bmwpp$Ibmwp_p_p_n
#' bmwpp$Ibmwp_p_p_c
#' @export


bmwp_p_p_ind <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){ message("Checking table format for bmwp prime prime calculation...") }

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

score<-numeric(length = length(9:ncol(dataset)))
nscoringtaxa<-numeric(length = length(9:ncol(dataset)))

for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-bmwp_p_p_score$Score[match(tolower(unique(as.character(aux$Family))),
                                       tolower(as.character(bmwp_p_p_score$Taxa)))]
  m2<-bmwp_p_p_score$Score[match(tolower(unique(as.character(aux$Order))),
                                        tolower(as.character(bmwp_p_p_score$Taxa)))]
  score[i-8]<-sum(m[!is.na(m)])+sum(m2[!is.na(m2)])
}

score[is.na(score)]<-0
ind_bmwp_p_p_total<-score
ind_bmwp_p_p_class<-ind_bmwp_p_p_total
for (i in 1:length(ind_bmwp_p_p_total))
{
  if(ind_bmwp_p_p_total[i]>=150) ind_bmwp_p_p_class[i]="Class I Lilac"
  if(ind_bmwp_p_p_total[i]<=149 && ind_bmwp_p_p_total[i]>=101) ind_bmwp_p_p_class[i]="Class II Blue"
  if(ind_bmwp_p_p_total[i]<=100 && ind_bmwp_p_p_total[i]>=61) ind_bmwp_p_p_class[i]="Class III Green"
  if(ind_bmwp_p_p_total[i]<=60 && ind_bmwp_p_p_total[i]>=36) ind_bmwp_p_p_class[i]="Class IV Yellow"
  if(ind_bmwp_p_p_total[i]<=35 && ind_bmwp_p_p_total[i]>=16) ind_bmwp_p_p_class[i]="Class V Orange"
  if(ind_bmwp_p_p_total[i]<=15) ind_bmwp_p_p_class[i]="Class VI Red"

}

Ibmwp_p_p_n<-data.frame(ind_bmwp_p_p_total)
Ibmwp_p_p_n<-data.frame(t(Ibmwp_p_p_n))
colnames(Ibmwp_p_p_n)<-colnames(dataset[9:ncol(dataset)])
rownames(Ibmwp_p_p_n)<-c("ind_bmwp_p_p")
Ibmwp_p_p_c<-data.frame(ind_bmwp_p_p_class)
Ibmwp_p_p_c<-data.frame(t(Ibmwp_p_p_c))
colnames(Ibmwp_p_p_c)<-colnames(dataset[9:ncol(dataset)])

if(store==TRUE){
  result_fname<-paste(substitute(dataset),"_bmwp_p_p_ind.csv",sep="")
  utils::write.table(x = Ibmwp_p_p_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
  utils::write.table(x = Ibmwp_p_p_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
  print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
}

Ibmwp_p_p<-list(Ibmwp_p_p_n=Ibmwp_p_p_n, Ibmwp_p_p_c=Ibmwp_p_p_c)

return(Ibmwp_p_p)
}
