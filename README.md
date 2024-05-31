# CoexistencePaper
Explaining the stable coexistence of drug-resistant and -susceptible pathogens: a simple model reveals the chaos beneath the calm

##################
####
## Figure 1
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
## Figure 2
####
##################

Figure 2: 
Figure2_EvolutionVsFractionResistant.png

Code: 
FiguresMathModel_1.R


##################
####
## Figure 3
####
##################

####
Figure 3A: 
Figure_3A_Resistance_Trends_Gladstone.png


####
Figure 3B: 
Figure_3B_Norway_NumClusters.png


####
Figure 3C: 
Figure_3C_Norway_LongevityClusters.png


####
Figure 3D: 
Figure_3D_Norway_TurnoverClusters.png

####
Data and code needed for 3A
MetaData Norway: NorwayGladstoneData/data.csv
ECDC data: ECDC_surveillance_data_Antimicrobial_resistance_complete_DownloadApril2024.csv


####
Data and code needed for 3B, C and D

Tree file Norway: Gladstone2021_tree.nwk
Code to infer origins of resistance and resistance strain lifespans: Rscript_Norway_Clusters_Tree_4Drugs.R
##This needs as input the metadata and the tree file. 

This script writes the following files: 

NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Piperacillin_tazobactam_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Cefotaxim_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Gentamicin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Ampicillin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/CLusterSizeDF_Ciprofloxacin_SIR.csv

NorwayGladstoneData/SummaryDataClusters4Drugs/FractionsDF_Ampicillin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/FractionsDF_Cefotaxim_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/FractionsDF_Gentamicin_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/FractionsDF_Piperacillin_tazobactam_SIR.csv
NorwayGladstoneData/SummaryDataClusters4Drugs/FractionsDF_Ciprofloxacin_SIR.csv

And the following figures: 

 [1] "Norway_Ampicillin_SIR_ClusterSizes.png"               [2] "Norway_Ampicillin_SIR_Frequencies.png"                [3] "Norway_Ampicillin_SIR_SurvivalTimes.png"              [4] "Norway_Cefotaxim_SIR_ClusterSizes.png"                [5] "Norway_Cefotaxim_SIR_Frequencies.png"                 [6] "Norway_Cefotaxim_SIR_SurvivalTimes.png"               [7] "Norway_Ciprofloxacin_SIR_ClusterSizes.png"            [8] "Norway_Ciprofloxacin_SIR_Frequencies.png"             [9] "Norway_Ciprofloxacin_SIR_SurvivalTimes.png"          [10] "Norway_Gentamicin_SIR_ClusterSizes.png"              [11] "Norway_Gentamicin_SIR_Frequencies.png"               [12] "Norway_Gentamicin_SIR_SurvivalTimes.png"             [13] "Norway_Piperacillin_tazobactam_SIR_ClusterSizes.png" [14] "Norway_Piperacillin_tazobactam_SIR_Frequencies.png"  [15] "Norway_Piperacillin_tazobactam_SIR_SurvivalTimes.png"[16] "Tree_NorwayAmpicillin_SIR.pdf"                       [17] "Tree_NorwayCefotaxim_SIR.pdf"                        [18] "Tree_NorwayCiprofloxacin_SIR.pdf"                    [19] "Tree_NorwayGentamicin_SIR.pdf"                       [20] "Tree_NorwayPiperacillin_tazobactam_SIR.pdf"          

####
Code to make the figures for 3B, C and D
Figure3ABC_NorwaySimpleNumClustersPlots.R
RscriptFigure3_D_NorwayTurnoverPlots.R







