For decades now, higher education has had a survey-problem, and one of the annual contributors 
the survey glut in higher education is the National Survey of Student Engagement (NSSE). 
Throughout my career, I’ve worked at three different colleges, and all three participate in the survey 
on a yearly or semi-yearly basis. Last year, around 500 institutions across the country were involved. 

The challenge in using the data is that is unwieldy - lots of missing data, incomplete responses, 
highly correlated variables, etc. - so its difficult to find the set of maximum signal variables 
out of the hundreds of options in the dataset. This is data-wrangling, feature-selection
part of a script that I wrote a little script that helped get the data off the shelf 
and into some decision-maker's hands.

It's WIP, but for anyone working with NSSE data in higher  education research, it provides 
an approach for identifying the most important variables for predicting overall 
satisfaction among respondents. And because NSSE standardizes their 
datasets (since 2014), it should work on any NSSE file, for any college or university.