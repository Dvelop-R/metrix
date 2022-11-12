#' BMWP and ASPT index
#'
#' Calculate Biological Monitoring Working Party (BMWP) and Average Score Per Taxon (ASPT) indexes.
#'
#' The Biological Monitoring Working Party (BMWP) was set up in 1976. Its terms of reference were to recommend a system which could be used to assess the biological status of a river, and which would be suitable for presenting a broad picture of one aspect of the biological condition of rivers in the UK. Identification to family is sufficient to calculate the BMWP score. The average score per taxon (ASPT) is calculated by dividing the score by the total number of scoring taxa (Armitage et al. 1983).
#'
#' @param dataset A data.frame obtained from \code{read_data}.
#' @param store A logical value indicating if the user want to store the results in a file.
#' @param dec_c A character used for decimal separator on results file.
#' @param verbose A logical value indicating if progress messages should be given.
#'
#' @return This function returns a list with the following components:
#' \item{Ibmwp_n}{The numerical BMWP and ASPT index (Armitage et al. 1983).}
#' \item{Ibmwp_c}{The BMWP and ASPT water quality class (Armitage et al. 1983).}
#'
#' @seealso \link[metrix]{read_data}, \link{bmwp_p_ind}, \link{bmwp_p_p_ind}, \link{biotic_ind}
#' @author Juan Manuel Cabrera and Julieta Capeletti.
#' @references Armitage PD, Moss D, Wright JF & Furse MT (1983). The performance of a new biological water quality score system based on macroinvertebrates over a wide range of unpolluted running-water sites. \doi{10.1016/0043-1354(83)90188-4}
#'
#' @examples
#'
#'#Load example data
#' example_data
#'
#'#Run bmwp_ind with that example_data
#' bmwp<-bmwp_ind(example_data)
#'
#'#Check results
#' bmwp$Ibmwp_n
#' bmwp$Ibmwp_c
#' @export


bmwp_ind <- function(dataset, store = FALSE, dec_c = ".", verbose = FALSE)
{

  if(verbose){message("Checking table format for BMWP and ASPT index calculation...")}

  if (chkt_f(dataset) == FALSE) {stop("Check table format.")}

score<-numeric(length = length(9:ncol(dataset)))
nscoringtaxa<-numeric(length = length(9:ncol(dataset)))

for(i in 9:ncol(dataset))
{
  aux<-dataset[dataset[,i]!=0,]
  m<-bmwp_score$Score[match(tolower(unique(as.character(aux$Family))),
                           tolower(as.character(bmwp_score$Taxa)))]
  m2<-bmwp_score$Score[match(tolower(unique(as.character(aux$Order))),
                            tolower(as.character(bmwp_score$Taxa)))]
  nscoringtaxa[i-8]<-sum(tolower(as.character(unique(aux$Family))) %in% tolower(as.character(bmwp_score$Taxa))) +
    sum(tolower(as.character(unique(aux$Order))) %in% tolower(as.character(bmwp_score$Taxa)));
  score[i-8]<-sum(m[!is.na(m)])+sum(m2[!is.na(m2)])
}

score[is.na(score)]<-0
ind_bmwp_total<-score
ind_bmwp_class<-ind_bmwp_total
for (i in 1:length(ind_bmwp_total))
{
  if(ind_bmwp_total[i]>=151) ind_bmwp_class[i]="Class I Excellent"
  if(ind_bmwp_total[i]<151 && ind_bmwp_total[i]>=121) ind_bmwp_class[i]="Class II Excellent"
  if(ind_bmwp_total[i]<121 && ind_bmwp_total[i]>=91) ind_bmwp_class[i]="Class III Good"
  if(ind_bmwp_total[i]<91 && ind_bmwp_total[i]>=61) ind_bmwp_class[i]="Class IV Moderate"
  if(ind_bmwp_total[i]<61 && ind_bmwp_total[i]>=31) ind_bmwp_class[i]="Class V Moderate"
  if(ind_bmwp_total[i]<31 && ind_bmwp_total[i]>=15) ind_bmwp_class[i]="Class VI Bad"
  if(ind_bmwp_total[i]<15) ind_bmwp_class[i]="Class VII Very Bad"
}


ind_aspt<-ind_bmwp_total/nscoringtaxa
ind_aspt[is.na(ind_aspt)]<-0
ind_aspt_class<-ind_aspt
for (i in 1:length(ind_aspt))
{
  if(ind_aspt[i]>=6) ind_aspt_class[i]="Class I Excellent"
  if(ind_aspt[i]<6 && ind_aspt[i]>=5.5) ind_aspt_class[i]="Class II Excellent"
  if(ind_aspt[i]<5.5 && ind_aspt[i]>=5.1) ind_aspt_class[i]="Class III Good"
  if(ind_aspt[i]<5.1 && ind_aspt[i]>=4.6) ind_aspt_class[i]="Class IV Moderate"
  if(ind_aspt[i]<4.6 && ind_aspt[i]>=3.6) ind_aspt_class[i]="Class V Moderate"
  if(ind_aspt[i]<3.6 && ind_aspt[i]>=2.6) ind_aspt_class[i]="Class VI Bad"
  if(ind_aspt[i]<2.6) ind_aspt_class[i]="Class VII Very Bad"
}

Ibmwp_n<-data.frame(ind_bmwp_total, ind_aspt)
Ibmwp_n<-data.frame(t(Ibmwp_n))
colnames(Ibmwp_n)<-colnames(dataset[9:ncol(dataset)])
rownames(Ibmwp_n)<-c("ind_bmwp","ind_aspt")
Ibmwp_c<-data.frame(ind_bmwp_class, ind_aspt_class)
Ibmwp_c<-data.frame(t(Ibmwp_c))
colnames(Ibmwp_c)<-colnames(dataset[9:ncol(dataset)])

if(store==TRUE){
  result_fname<-paste(substitute(dataset),"_bmwp_ind.csv",sep="")
  utils::write.table(x = Ibmwp_n, file = result_fname, sep = ";", dec = dec_c, col.names =  NA)
  utils::write.table(x = Ibmwp_c, file = result_fname, sep = ";", append = TRUE, col.names = FALSE )
  print (paste("Results were stored in ",getwd(),"/",result_fname, sep=""))
  }

Ibmwp<-list(Ibmwp_n=Ibmwp_n, Ibmwp_c=Ibmwp_c)

return(Ibmwp)
}
