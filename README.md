# Regulating Reliability-Usability Trade-Offs for Few-Shot Inference: 
## General Supply-Inspect Cost Framework and Threshold Choice Methods

(Work submitted for review)

### Description

Language models and other recent machine learning paradigms blur the distinction between generative and discriminative tasks, in a continuum that is regulated by the degree of pre- and post-supervision that is required from users, as well as the tolerated level of error. In few-shot inference, we need to find a trade-off between the number and cost of the solved examples that have to be supplied, those that have to be inspected (some of them accurate but others needing correction) and those that are wrong but pass undetected. In this paper, we define a new %utility function
Supply-Inspect Cost framework, associated graphical representations and comprehensive metrics that consider all these elements. We also introduce novel algorithms beyond the concept of reject that statically or dynamically optimise few-shot inference for the desired operating condition. We illustrate the effectiveness of all these elements for a transformative domain, data wrangling, for which language models can have a huge impact if we are able to properly regulate the reliability-usability trade-off, as we do in this paper.
