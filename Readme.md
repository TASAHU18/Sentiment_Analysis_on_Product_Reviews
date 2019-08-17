# Sentiment Analysis on Product Reviews 
## Decsription of the dataset
The dataset consists of the following columns:
1. Id: This is the review id number. Basically row number
2. ProductId: The id number of the product being reviewed. The number is not unique. There are several products that have been reviewed multiple times
3. UserId: The id number of user providing the reviews. The number is not unique. There are several users that have reviewed multiple products
4. ProfileName: The profile name of the user writing the reviews
5. HelpfulnessNumerator: Number of users who found the review helpful
6. HelpfulnessDenominator: Number of users who indicated that they found the review helpful
7. Score: Rating of the product from 1 to 5
8. Time: Timestamp for the review
9. Summary: Summary of the review
10. Text: Actual review itself

## Dictionary used to calculate the sentiment score
AFINN-111 which is a list of English words rated for valence with an integer between minus five (negative) and plus five (positive)

## Tasks Performed
* Performed Natural Language Processing on 500K+ food reviews from amazon to interpret customer sentiments for the purchased products
* Developed a dashboard app to display actual reviews, normalized reviews, word-frequency, word clouds, scatter plots of relation between user rating and calculated sentiment score 

## Author
_Takesh Sahu_
