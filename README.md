# Flour-Survey-Models
## Note: The data for this model is a privately owned dataset and not avialable for parties outside of the original owners and Purdue University. Do note a current project is in the works for an original publication. If you are a researcher who wishes to collaborate on a project with this data or view the data after this publication (est. May 2024 for publication). Please email yfengchi@purdue.edu. 
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
  2) CART Random Forest Classifier
  3) a logistical regression.
  These are combined by the default trained linear regression, essentially a weighted average.
2) Behavior Score is a raw R-code file and uses a model ensemble consisting of:
   1) Mono-layer Neural Network Regression
   2) CART Random Forest Regression
   These are combined by the default trained linear regression, essentially a weighted average.
3) Recall Behavior Score, Sanitization Behavior Score, and Risk Behavior Frequency are R Markdowns that explain the code and other information to assist new collaborators in learning R and the project with their directions. These contain a model ensemble of:
   1) Mono-layer Neural Network Regression
   2) CART Random Forest Regression
   These are combined by the default trained linear regression, essentially a weighted average.


