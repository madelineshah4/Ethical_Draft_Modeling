# Summary
This project addresses discriminatory bias in NFL draft recommendation models and prevention strategies. While teams increasingly adopt machine learning for scouting optimization, these systems risk perpetuating historical discrimination embedded in evaluation data. Rather than developing another potentially biased model, this study systematically investigates existing bias and provides frameworks for improving the ethics of current methods for identifying players for the NFL draft. A 3-stage analysis of 4,347 NFL prospects (2018-2025) demonstrated systematic discrimination at each draft pipeline level. County-level Black population share significantly predicts draft selection across all eight position groups, while individual race probability correlates with approximately one-round draft penalties at four positions—effects persisting after controlling for college performance. Conference affiliation creates additional barriers, with Group of 5 and FCS players facing significant disadvantages despite comparable production. A 4-stage experimental framework testing progressively riskier features revealed that optimizing on historical data inherently validates past discrimination. Models appearing more precise may systematically exclude qualified candidates based on protected characteristics.

The analysis proves historical NFL evaluation data is ethically compromised. Therefore, any recommendation model must account for this before deployment to avoid perpetuating discriminatory patterns. The study demonstrates that achieving both competitive advantage and ethical evaluation requires explicit bias mitigation. Auditing bias and establishing mandatory third-party testing should be required before deploying any high stakes recommendation models. 

# Information on Github Files and Folders
There are 3 folders in the main file system: 
  - 1.Data_Collection_prep
  - 2.Historical_Analysis
  - 3.Bias_Modeling_Framework

Folder #1 contains the incomplete data collection and preparation process. API pulls and data preparation files were omitted from this folder and only the cleaned final datasets remain. This was done to limit runtime in the case that the files would be run.

Folder #2 contains the historical analysis of discrimination in the NFL draft. The file within this folder named 'Historical Data Preparation.R' must be run before the file named 'Historical Analysis.R'. To run these files without error, you must modify the line of code near the top of the file to set the working directory to the correct path on your devide.

Folder #3 contains the proof-of-concept experimental framework to test progressively riskier features in NFL recommendation models.

# Neecessary Packages
This project invovles the use of many libraries. 

For R:
  - tidyverse
  - progressr
  - tidycensus
  - wru
  - nflreadr
  - broom
  - logistf

For Python:
  - pandas
  - glob
  - os
  - numpy
  - sklearn
  - matplotlib
  - seaborn
  - xgboost
  - imblearn
  - collections
  - pickle

To install packages in R run the line ``install.packages("NAME_OF_PACKAGE")``
To install libraries in Python run the line ``pip install NAME_OF_PACKAGE`` or if using an ipynb cell, then run ``!pip install NAME_OF_PACKAGE``

