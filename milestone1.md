## Milestone #1

2) Identify the question that your team in interested in answering with the survey. The aim of the survey should be to try to answer one specific and testable question.

_**How does distance from campus influence arrival time to lectures?**_

specific questions to ask in the survey:

- How far from Hugh Dempster do you live in kilometers?

  _we will include a Google maps link with DMP for easy access_

  _we will remind people to choose the appropriate mode of transportation on Google maps_

- What time do you typically arrive to Hugh Dempster on Mondays and Wednesdays?

- What time do you typically arrive to Hugh Dempster on Tuesdays and Thursdays?

3) Identify the other questions you plan to ask in your survey to identify confounding variables and justify/explain why you plan to include them.

- What is your typical mode of transit?

  _multiple choice_

  - drive
  - public transit
  - walk
  - bike

justification for asking this question:

Mode of transit is a direct influencer on both arrival time and distance. A confounder is a variable that influences both the dependent variable (arrival time) and independent variable (distance).

4) Describe how you plan to analyze the survey results (e.g., what statistical test(s) do you plan to employ?)

  We will first fit a linear regression using the distance as the predictor variable and the arrival time as
the response variable. We will compare this linear regression model with a null model through an ANOVA test.
To validate the estimates of the frequentist approach, given the possibility of a small sample size, we will
use a Bayesian linear regression. After this, we will move on to using the mode of transportation as a confounder
variable and fit a linear regression model with these variables and the interaction between the variables. We
will compare this model with the null model through an ANOVA test.


5) Discuss the aspects of the UBC Office of Research Ethics document on Using Online Surveys that are relevant
to your proposed survey.

  The document on Using Online Surveys includes instructions on the adequate procedures of handling personal
  information. As part of our project we are not collecting personal information given the questions we’re asking.
  This means that we won’t include a consent form as it is not required. We will be collecting information on the
  distance someone lives from the Hugh Dempster Building; however, this provides a radius of distance of where
  people live so it makes it non-identifiable. The other variable is the average arrival time that cannot be combined
  with other variables to identify individuals. The last variable is typical mode of transportation, which is composed
  of 4 categories {bike, car, walk, bus} which means that there is low probability of one individual using any particular category.
  As for the IP addresses of the individuals, they will not be collected as we will turn off this feature in the UBC
  Survey Tool developed by Qualtrics.
