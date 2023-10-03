# Predicting Job Change Intentions Among Data Scientist Candidates

## Summary
Understanding why employees consider leaving their roles is crucial, especially in specialized fields like Data Science. 
This study employs a multilevel modeling approach to identify factors influencing job change intentions among data scientist candidates. 
Our findings could help HR to develop strategies while optimizing training resources and hiring decisions.


## Setup
This project is developed using R and R Studio. Refer to `mice_project-report.RMD` for the required packages.


## Data
Our dataset, sourced from Kaggle, consists of 19,158 observations across 12 key columns, capturing attributes from individual 
experience to city metrics. The primary outcome is the 'target' column, a binary indicator of job change intention.

Link to Data: https://www.kaggle.com/datasets/arashnic/hr-analytics-job-change-of-data-scientists


## File Descriptions
- `OptimalModelExploration.R`: An R script for data exploration and determining the best model framework.
- `job_change_prediction.Rmd`: R Markdown document detailing the analysis and modeling process.
- `job_change_prediction.pdf`: PDF report generated from the R Markdown file.
- `test_data.csv`: Dataset used for model validation.
- `train_data.csv`: Primary dataset used for model training.


## Key Results
* Candidates from lower-developed cities tend to stay in their current jobs.
* University-enrolled candidates are less inclined to seek new employment.
* Those who've changed jobs within the past 2-4 years, or not at all, show a higher tendency to consider new roles.
* Public sector employees are more open to job transitions compared to those in start-up companies.


### For inquiries or further discussion, please reach out to me at [xihaocao@163.com](mailto:xihaocao@163.com). Thank you!
