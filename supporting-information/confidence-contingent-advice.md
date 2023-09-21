# Confidence-contingent advisors

A fourth advisor contrast, not detailed in the main report, is included here for completeness.
These advisors had advice profiles that were substantially more complicated than those in the main experiments, because they depended on the confidence of the participant's initial estimate as well as the accuracy.
These advisors borrow from Pescetelli and Yeung ([2009](http://doi.apa.org/getdoi.cfm?doi=10.1037/xge0000960)).

These two advisors are a bias-sharing advisor and an anti-bias advisor.
The bias-sharing advisor is more likely to agree with the participant when the participant is confident in their own decisions, while the anti-bias advisor is more likely to agree with the participant when the participant is less confident in their own decisions.
Crucially, these advisors are equally accurate and equally likely to agree overall, and also equally accurate and likely to agree when the participant expresses medium confidence in their initial estimate.

This experiment was preregistered at https://osf.io/h6yb5.

## Theory

The motivation behind these advisors was to test an elaboration of the basic theory that agreement serves as a proxy for accuracy in the absence of feedback. 
The use of agreement as a proxy for accuracy may depend crucially upon confidence in the initial estimate.
If someone agrees with me when I am sure I am correct, I am likely to be more sure that they are also correct.
Conversely, if someone agrees with me when I know I am guessing, I have no more insight into whether they really are identifying the right answer than I did before I received the advice.
As with experiments where objective feedback is not 100% accurate, confidence may have a role in determining how much to update an estimate of the reliability of a source of information.

## Advice structure

For trials where the participant's initial estimate is incorrect, both advisors agree 30% of the time. 
For trials where the participant's initial estimate is correct, the advisors' agreement depends upon the initial estimate confidence: 

|- Advisor      | Agree rate -| When Low Conf -| When Med Conf -| When High Conf -| Accuracy rate -|
| Bias-sharing  |       58.4  |             50 |              70|               90|             70 |
| Anti-bias     |       58.4  |             90 |              70|               50|             70 |


## Hypothesis testing

We expected that participants would develop a systematic preference for the advisor who agreed with them when they were more confident (bias-sharing advisor). 
We did not see this effect $t(49) = -1.524$, $p = 0.1338$, $mu = 0.472$ $[0.435, 0.509]$ vs $mu_0 = 0.500$, $d = -0.216$.

The advisors differed in agreement rates dependent upon confidence, so we also examined whether participants preferred the bias-sharing advisor _when their initial estimates were medium confidence_. 
Again, we did not see a systematic difference in pick rates $t(49) = -0.908$, $p = 0.3681$, $mu = 0.478$ $[0.429, 0.527]$ vs $mu_0 = 0.500$, $d = -0.128$.
