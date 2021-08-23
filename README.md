**A Simulation Study to Compare the Bias and Efficiency of the Sample Mean Obtained from Simple Random (SRS) and Convenience Samples (CS)**

Non probability sampling is a typically cheap, easy to implement and often used sampling method in health related studies, such as in clinical trials where volunteers are used.
Although non probability sampling is very practical, it is known that the samples might not be representative and it can cause sampling bias. There are very few studies in literature that compare probability sampling to nonprobability sampling via simulations. In this project, we assess the impact of convenience sampling on the estimation of the
population mean, especially in regards to its bias and efficiency, compared to simple random sampling using an extensive Monte Carlo study.

**Simulation Study Design:**

<p align="center">
  <img width="800" height="800" src="https://github.com/hrmazumder/probability-and-nonprobability-sampling/blob/main/Simulation%20Design/simulation-study-design.PNG">
</p>


**Output from the Simulation Study:**
<p align="center">
  <img width="800" height="400" src="https://github.com/hrmazumder/probability-and-nonprobability-sampling/blob/main/Output/Output-1.PNG">
</p>
<p align="center">
  <img width="800" height="400" src="https://github.com/hrmazumder/probability-and-nonprobability-sampling/blob/main/Output/Output-2.PNG">
</p>
<p align="center">
  <img width="800" height="400" src="https://github.com/hrmazumder/probability-and-nonprobability-sampling/blob/main/Output/Output-2.PNG">
</p>

**Discussion from the Output:**

• Increasing the sample size has no effect on the bias of the sample mean (zbar) from CS • Increasing the 𝛼 value decreases the bias in the sample mean (zbar) 𝑧obtained from CS. This is due to the fact that increasing 𝛼 values increase the selection probability • Increasing the 𝛼 values increase the REs, i.e. CS becomes competitive with SRS as 𝛼 values increase • Increasing the 𝛽 value, however causes an increase in the bias of the sample mean (zbar) obtained from CS • Similarly, increasing the 𝛽 value, decreases the RE (i.e. SRS’s MSE gets better) • While increasing the 𝛾 values also causes an increase in the bias of the sample mean (zbar) obtained from CS, this increase stabilizes quickly. However, the REs get smaller with higher 𝛾 values • We want to highlight that for very high 𝛼 values, CS sampling becomes pretty competitive with SRS • For such high 𝛼 values the biases from CS and SRS become very close • For small 𝛾 values RE values become greater than 1

**Conclusion:**

Our study confirms that the bias and the MSE of the sample mean from CS is higher than that of SRS. However, we observed that there are cases, such as when there is a correlation between an outcome and a covariate where the covariate is also correlated with selection probability, if this probability is high, then CS becomes competitive with SRS. Since the distinction between probability and non probability sampling has been rapidly diminishing due to high nonresponse rates (general) and reduced coverage (phone surveys). Besides, as long as some adjustment techniques are applied (such as post stratification) the selection bias from CS can be eliminated. Thus, CS offers a good alternative within the non probability sampling methods.


Reference:
https://ww2.amstat.org/meetings/jsm/2021//onlineprogram/AbstractDetails.cfm?abstractid=318968

Presented in Joint Statistical Meetings (JSM) 2021, American Statistical Association.

**My contribution: writing the entire R code for analysis (SRS, CS, Simulation Study and Making Plots) and providing with insights from analysis!

