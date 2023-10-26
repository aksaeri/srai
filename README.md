# Safe and Responsible AI consultation submissions

## Summary
Data and code to analyse submissions to the Safe and Responsible AI consultation conducted by the Australian Department of Industry, Science and Resources in 2023.

## About the repo
I scraped the public submissions to the [Safe and Responsible AI consultation](https://consult.industry.gov.au/supporting-responsible-ai/submission/list) and have been cleaning some of the data for it. My intention is for this data to be analysed to understand:

- The landscape of actors interested in AI governance in Australia
- Discourse around risks and governance of AI - what risks are common, what are missing; what approaches are being recommended for action
- Pathways towards more effective policy & governance research and development.

## Repo contents
- A full cleaned dataset which includes the text of any uploaded documents, as well as answers to each question
- A dataset with all responses, but with each answer / text response capped at 500 characters [since some are well over 10,000 words long]
- A 'wide' dataset, with each consultation question/answer in its own column, and no document text.
- R code for reproducing
- Original dataset scraped from SRAI website

## Things that could be done with the 'wide' dataset now:
- Organisations need to be correctly & manually coded for type, sector, size, or other relevant info and more obscure groups researched
- Many of the question - by - question responses say things like "see attached submission", fixing this would improve the data quality
- Many of the written submissions actually answer the questions, so their responses need to be copied out into the spreadsheet.

## How you could help:
- If you are expert / know someone expert in topic modelling / quantitative text analysis, work with me to investigate the data.
- It would be very useful to know a little more about some of the people & organisations who made a submission. This would involve manually / AI-assisted coding of the data and some web searching for information.

The majority of submissions were from individuals, vs. groups/organisations.
Peak bodies and associations were the most likely type of organisation to make a submission. Representation is extremely diverse, from medicine to media production, retail, technology, etc.
