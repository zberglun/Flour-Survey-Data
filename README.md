# Flour-Survey-Models
## Note: The data for this model is a privately owned dataset and not avialable for parties outside of the original owners and Purdue University. Do note a current project is in the works for an original publication. If you are a researcher who wishes to collaborate on a project with this data or view the data after this publication (est. June 2024 for publication). Please email yfengchi@purdue.edu. 
### Model List
- Recall Heard (Consumer has heard of flour recalls)
- Behavior Score (Overall score for proper food safety practices)
- Recall Behavior Score (Score of how safe consumers behave when hearing about recalls)
- Sanitization Behavior Score ( Score of how well consumers practice sanitization practices
- Risk Behavior Frequency (How often consumers consume raw dough or batter, lower values correspond to more frequency)
### Data
Uploaded data is generated from the original data. Data points were randomized between rows, breaking any true associations between variables. Additionally, the data was sabotaged intentionally, replacing data points with randomly generated, valid data points that do not represent any valid findings or distributions of the survey.
This completely fake data serves as an example to highlight the project's data manipulation work process while protecting the data source. For access to the original data, please email yfengchi@purdue.edu. For more information on the data and the complete survey, please review https://doi.org/10.4315/JFP-19-562.
### Modeling Information
The models follow three different templates. 

1) Recall Heard is a raw R-code file and uses a model ensemble consisting of:
   1) Mono-layer Neural Network Classifier
   2)  CART Random Forest Classifier
   3)  a logistical regression.
   4)  These are combined by the default trained linear regression, essentially a weighted average.
2) Behavior Score is a raw R-code file and uses a model ensemble consisting of:
   1) Mono-layer Neural Network Regression
   2) CART Random Forest Regression
   3) These are combined by the default trained linear regression, essentially a weighted average.
3) Recall Behavior Score, Sanitization Behavior Score, and Risk Behavior Frequency are R Markdowns that explain the code and other information to assist new collaborators in learning R and the project with their directions. These contain a model ensemble of:
   1) Mono-layer Neural Network Regression
   2) CART Random Forest Regression
   3) These are combined by the default trained linear regression, essentially a weighted average.

### Cleaning, transformation and feature selection
The data is cleaned and processed in excel and the following was done: 
1) Data in Excel source was extracted from the survey source
2) Categorical variables were one-hot encoded
3) a variable dictionary with encoded values and keys was created.
4) composite variables for analysis were created.
5) Pertinent variables were selected for a new Excel document for data cleaning
6) Rows with missing values were removed
7) Rows with duplicate values were removed
8) Data was extracted as a CSV for easier importing into code

The imported data was manipulated using code for more complex operations.
1) Correlations were conducted, and highly correlated variables were removed (Ps >0.7) (used Spearman's and Kendal's tau and Tetrachoric correlations)
2) Variables with low variance (Var <0.001) were removed
3) Other variables involved in calculating the composite variables were removed since that provides little meaningful conclusions
4) We conducted feature extraction via statistical tests, Chi-squared for variables to see if they are dependent, and serial-wise Wilcoxian for mean comparisons with categoricals.

### Model Structure and Optimization

The models were created using a 5-fold cross-validation with default parameters. Model selection/optimization used a simple brute force technique. For scores and frequencies, 5000 seeds were run, and the top five models with the lowest RMSE were selected. For recall heard, five models with accuracy that were statistically significant for being greater than their no information rate were selected. These models were then summarized and analyzed to account for randomness in model creation.   
