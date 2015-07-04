# DA2-Avengers-and-Pascal
This is the public repository of the case study in Data Analytics 2 lecture at WWU Muenster. It includes methods for missing values and imputation like MCAR, MAR, MNAR as well as MICE, Regression and Classification techniques.  
                                              
#If you are not familiar with gitHub 
... you can download the whole repository from the following release as a .zip file. 
https://github.com/derursm/DA2-Avengers-and-Pascal/releases
                                              
                                              
                                              
                                              ##############################
                                              ### Please read this first ###
                                              ##############################


                                                    .7                           
                                         \       , //                   
                                         |\.--._/|//                  
                                        /\ ) ) ).'/                    
                                       /(  \  // /                    
                                      /(   J`((_/ \                  
                                     / ) | _\     /                 
                                    /|)  \  eJ    L                          
                                   |  \ L \   L   L                 
                                  /  \  J  `. J   L                      
                                  |  )   L   \/   \                  
                                 /  \    J   (\   /               
               _....___         |  \      \   \```            
        ,.._.-'        '''--...-||\     -. \   \                 _______             ____               ______
      .'.=.'                    `         `.\ [ Y               ||       \          /    \            /        \    
     /   /                                  \]  J               ||        \        /      \          |   ___    |    
    Y / Y                                    Y   L              ||   __    \      /    _   \         |__|  /   /    
  | | |          \                         |   L                ||  |  |    |    /    | |   \             /   /     
  | | |           Y                        A  J                 ||  |__|    |   /     |_|    \           /   /
  |   I           |                       /I\ /                 ||         /   /      ___     \         /   /     
  |    \          I             \        ( |]/|                 ||        /   /     /    \     \     _./   /______       
  J     \         /._           /        -tI/ |                 ||_______/   /_____/      \_____\   |_____________|
  L     )       /   /'-------'J           `'-:.                         
  J   .'      ,'  ,' ,     \   `'-.__          \                  
     \ T      ,'  ,'   )\    /|        ';'---7   /                      
      \|    ,'L  Y...-' / _.' /         \   /   /   
       J   Y  |  J    .'-'   /         ,--.(   /     
        L  |  J   L -'     .'         /  |    /\    
        |  J.  L  J     .-;.-/       |    \ .' /    
        J   L`-J   L____,.-'`        |  _.-'   |    
         L  J   L  J                  ``  J    |     
         J   L  |   L                     J    |     
          L  J  L    \                    L    \      
          |   L  ) _.'\                    ) _.'\     
          L    \('`    \                  ('`    \      
           ) _.'\`-....'                   `-....'       
          ('`    \      
           `-.___/  



Project 5 – Missing Values & Classification & Regression


Given Task:
0.Artificially remove pairs of values from 10 observations using MCAR, MAR, MNAR pairs should consist of a numerical and a categorical feature for each of the approaches, impute the missing values using:
1. MICE
2. First a regression on the numerical feature, followed by a classification on the categorical feature (already using the imputed values from the regression)
3. first a classification, followed by a regression
4. classification and regression in parallel (without imputed values)
compare the imputed values to the original ones


####################
###Documentation


1. Annotation notes syntax: 

Headline 1: #=== This is the headline 1 ===#   "Notation for end of block":  #======#
Headline 2: ##### Headline 2        "Notation for end of block":  #####
Inline-Comment: # 



2. Call order of files:

DA2_avengers_install.R 
DA2_avengers_require.R 
Seed.R

--> load output_all.Rdata

FirstDataAnalysis.R
MCAR_working_set.R
MNAR_working_set.R
MAR_working_set.R

mice_analysis_mar.R
mice_analysis_mcar.R
mice_analysis_mnar.R

regression_analysis.R
classification_analysis.R
regression_analysis.R
classification_analysis.R

MCAR_repeat.R


3. Management review 
The dataset output_all implemented on google maps: https://www.google.com/maps/d/edit?mid=z_N-SAjgBb5o.kyFyx1-XJNic

After analysing the given data set we chose to work with the given data from Tripadvisor platform as this data has most observations.

Chosen features for MCAR, MAR and MNAR:

MCAR 
  - numerical: X102_DistanceToBeach
  - factorial: X004_City->change!

MAR
  - numerical: X012_NoRooms
  - factorial: X003_Postcode
  based on X099_DistanceCathedralLeSeu

MNAR
  - numerical: X100_DistanceAlcudiaOldTown
  - factorial: X046_PayOptions


#MICE
  - In total 3 files for each Missing data mechanism (mice_analysis_mcar/_mar/_mnar)
  - Same procedures for every method
  - Start with MICE imputations for different methods
  - Different number of imputations for method PMM (used for calculation of numeric features)
  - Important: Chose your Mice Object for further investigation - right under the listings of the mice objects. Each of the mice objects
    with different methods will be saved into its own object. In case you want to switch between objects in your investigation, you do
    not have to go through the whole time consuming mice computation
  - Further we investigate how close the imputations represent the original structure of the dataset regarding mean and variance.
  - After each investigation step there is a tiny section for visalisation in regards to the aforementioned section
  - The best fitting imputation is selected and depicted for 5; 10; 50; and 100 imputations.

#Regression 
The regression analysis splits up into the three different sections "MCAR", "MAR" and "MNAR". 
For each of these sections, the methods apply a linear model, a regression tree and a random forest.
For each of these methods, an imputation for all values and an imputation for the ten missing values is conducted.

The results are displayed in plots showing original and imputed values, the decision trees and the importance of features in the random forest.

Features we have taken for regression

#Classification 
The classification analysis splits up into the three different sections "MCAR", "MAR" and "MNAR". 
For each of these sections, the methods apply a linear model, a regression tree and a random forest.
For each of these methods, an imputation for all values and an imputation for the ten missing values is conducted.

The results are displayed in plots showing ... 

Features we have taken for regression


#The follow Up 

Regression based on classification uses imputed values of Location Beach. These are deleted beforehand via MCAR and imputed using a linear model for classification.
Taking the feature City instead wasn't feasible since it did contain too many factorial levels.

Classification based on regression takes the output of the regression via random forest and performs a classification for the features City, Postcode and Pay Options.

After all a multiple mcar regression (file:mcar_repeat.R) is  conducted where the mse of the three different appraoches linear model, regression tree and randomForrest for 1000 runs are analysed.
