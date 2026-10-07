
## Repo Organization 
### Sub-directories 
['data/'](data): contains all data for this project.
['plots/'](plot): contains all figures for this project (excluding tables)

### R Scripts
-   '0a_spatial_key.R': notated work on joining of Chicago Demographic Data set 
with Chicago neighborhoods and zip codes, with code provided by Professor Sass. 
-   '0b_data_cleaning.R': notated work on cleaning and tidying Chicago Public Schools 
and joined with Chicago demographic data 
-   '1_univariate_analysis.R': notated work on univariate analysis for research 
question 1 and 2.
-   '2_multivariate_analysis.R': notated work on multivariate analysis for research 
question 1 and 2.

### Reports
-   'Villamil_Sarah_final_report.qmd': file for creating final project
-   'Villamil_Sarah_final_report.html': rendered html for final project
-   'Villamil_Sarah_executive_summary.qmd': file for creating executive summary
-   'Villamil_Sarah_executive_summary.html': rendered html for executive summary
  
# Chicago Public Schools College Enrollment Analysis
## Overview
In this project, I explored factors associated with college enrollment outcomes across Chicago Public Schools.
Using Chicago Public Schools performance data and Chicago demographic data, I investigated how school type, attendance, graduation outcomes, and community characteristics relate to college enrollment rates. The goal of this analysis was to identify factors that schools could potentially target to improve post-secondary outcomes.
 
## Research Questions
### Research Question 1
What variables are associated with higher college enrollment rates among Chicago Public School students?
### Research Question 2
How does school type relate to student post-secondary outcomes?

## Dataset
### Chicago Public Schools Performance Data
- 649 schools
- 183 variables
- School performance and outcome metrics
- Student attendance
- Graduation outcomes
- College enrollment outcomes
- School characteristics
### Chicago Demographic Data
- Community demographic characteristics
- Income information
- Race and ethnicity distributions
- Population data
### Final Analytical Dataset
- 1,298 observations
- 39 variables
- Combined education and demographic information
---
 
## Tools 
- R
- dplyr
- tidyr
- ggplot2
- Statistical Analysis
- Data Visualization
---
 
## Data Preparation
To create the final analytical dataset, I:
- Joined CPS performance data with Chicago demographic data
- Removed variables with little analytical value
- Addressed missingness through filtering and variable selection
- Focused on high schools to ensure college enrollment information was available
- Reduced missingness in the college enrollment variable to less than 7%
---
 
## Analysis
The project included:
- Univariate Analysis
- Bivariate Analysis
- Multivariable Analysis
- Correlation Analysis
- Data Visualization
 
I evaluated relationships among:
- School type
- Community income levels
- Student attendance
- Graduation rates
- College persistence rates
- Mobility rates
- Chronic truancy
- College enrollment outcomes
---
 
## Key Findings
### Positive Associations with College Enrollment
The strongest positive relationships were observed between:
- Student attendance
- College persistence rates
- Four-year graduation rates
- Five-year graduation rates
 
### Negative Associations with College Enrollment
The strongest negative relationships were observed between:
- Chronic truancy
- Mobility rates
- One-year dropout rates
 
### School Type Differences
Selective enrollment schools consistently demonstrated:
- Higher college enrollment rates
- Higher graduation rates
- Higher college persistence rates
- Higher average SAT scores
 
### Community Characteristics
Schools located in higher-income communities generally exhibited higher average college enrollment rates, suggesting that neighborhood characteristics may play an important role in post-secondary outcomes.
 
## Conclusions
The analysis suggests that student attendance is one of the most actionable variables associated with college enrollment outcomes.
While graduation and college persistence rates were strongly related to college enrollment, attendance and chronic truancy may represent more practical intervention points for schools seeking to improve student outcomes.
The findings also highlight substantial differences across school types and community contexts, suggesting opportunities for future research into equity and access across Chicago Public Schools. 
