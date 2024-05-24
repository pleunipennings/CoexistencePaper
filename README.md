# CoexistencePaper
Explaining the stable coexistence of drug-resistant and -susceptible pathogens: a simple model reveals the chaos beneath the calm

##################
####
##Figure 1
####
##################

Figure 1A: 
Figure1A_ECDCPlot_ResistanceOverTime.png

Figure 1B: 
Figure1B_ECDCPlot_Correlation.png

Figure 1C: 
Copied from other paper (Van Nouhuijs et al, 2023 Biorxiv). 

Data needed: 
ECDC_surveillance_data_Antimicrobial_resistance.csv
2015_data_Table_D6_J01M_quinolone antibacterials_trend_community_sparklines.csv
(Last one also available as excel file) 

Code: 
MakeECDCFigure.R

##################
####
##Figure 2
####
##################

Figure 2: 
Figure2_EvolutionVsFractionResistant.png

Code: 
FiguresMathModel_1.R


##################
####
##Figure 3
####
##################

Figure 3A: 
Figure_3A_Resistance_Trends_Gladstone.png


Figure 3B: 
Figure_3B_Norway_NumClusters.png


Figure 3C: 
Figure_3C_Norway_LongevityClusters.png


Figure 3D: 


Data needed: 
For 3B and C
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Piperacillin_tazobactam_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Cefotaxim_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Gentamicin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Ampicillin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Ciprofloxacin_SIR.csv

For 3A
MetaData Norway: NorwayGladstoneData/data.csv
ECDC_surveillance_data_Antimicrobial_resistance_complete_DownloadApril2024.csv


Code: 
Norway_Clusters_Tree_4Drugs.R determines singe and longevity of resistance clusters
##This needs as input the metadata and the tree file. 

Figure3ABC_NorwaySimpleNumClustersPlots.R

##Still needed: code for 3D. 






