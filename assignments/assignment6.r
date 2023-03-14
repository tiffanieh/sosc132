#' ---
#' title: "Assignment 6, Social Science Inquiry II (SOSC13200-W23-2)"
#' author: "Tiffanie Huang"
#' date: "Monday 2/13/22 at 5pm"
#' ---
#' This assignment based on analysis in:
#' 
#' Butler, Daniel M., & Broockman, David E. (2011). Do politicians racially 
#' discriminate against constituents? A field experiment on state legislators. 
#' *American Journal of Political Science.* 
#' 
#' # 1. Go through the replication file for the Butler and Broockman study. 
#' 
#' Replication files are available here:
#' https://isps.yale.edu/research/data/d037
#' 
#' (If you have issues downloading the files, they are also posted on the class
#' GitHub repository.)
#' 
#' You will only need to use the R program file and the Excel .csv file (files 
#' D037F04 and D037F02) respectively, as well as potentially the Stata .dta file 
#' (file D037F01). 
#' 
#' Run the whole replication file in R on your local computer. Note that Michael 
#' used an absolute directory path when he wrote the replication file, you will 
#' need to reset the working directory for your computer or use appropriate 
#' relative file paths. He also used the Stata .dta version of the dataset. You 
#' can use that version and read it in with the appropriate package, or you can 
#' adapt the code to read in the .csv file. 
#' 
#' 
#reading in data
library(foreign)
data <- read.dta("../data/Butler_Broockman_AJPS_2011_public_dta.dta", convert.factors=F)
#' 
#' Once you have run the replication file, write two paragraphs here on your 
#' findings. Was the replication file well written and self explanatory? Or do 
#' you think it should have included more notes for users to understand how to 
#' use it? Interpret some of your findings from the replicated tables. 
#' 
#' 
#' One thing I appreciated about the replication file was the similar formatting and names for all the linear regression models. It was easy to refer back to the literature tables based on what the vector names were in the file. Most people looking at the replication file are probably already familiar with the study's analysis and findings, and can carefully compare the table to the replication file to match up percentage values, but if someone were not as familiar to the different methods they used and how these tables were constructed, I think this replication file could be a bit difficult to navigate. At first glance, there seem to be many different treatments (true, but confusing) and it can be hard to see which treatment names are actually related. For example, "treat_jake" and "treat_deshawn" are mutually exclusive and dependent on each other because they are just 2 possibilities of the same random treatment (putatively white/Black aliases). There aren't any instances in the replication file where reply_atall is regressed on treat_jake, since that's included when regressing on treat_deshawn, so I'm not sure if a separate column for the Jake Mueller treatment is necessary when treat_deshawn==0 suffices. Alternatively, if both columns are kept, there could be comments included explaining that data[data$treat_jake==1] is the same thing as data[data$treat_deshawn==0] to ease the interpretation process for anyone less familiar to the study; I liked the simplicity of the Card+Krueger data/names. I think the same could be applied to the "treat_noprimary" and "treat_primary" -- maybe consolidating everything into "treat_primary" might make it easier for someone overwhelmed by the large amount of variable names in the replication file, if a comment is included explaining what treat_primary=1 or 0 would mean. I also think the name could be clarified, since all the emails are inquiring about primary elections anyways even if someone isn't signaling partisanship, and we can just clarify that in a separate comment. For instance, "primary" could be renamed as "treat_signal" (or "treat_partsignal") and "treat_repprimary" could be "treat_repsignal" to avoid confusion between partisanship signaling and the partisanship of the legislators and improve readability.
#' 
#' The results of this study show evidence that discriminatory behavior can be only partially explained by strategic considerations based on a constituent’s partisanship. Even after controlling for partisan preferences observing an increase in both Democratic and Republican legislators’ responsiveness to a constituent after they express interest in the legislator’s party for the primary, Republican legislators continue to show lower levels of responsiveness to an email from the black alias indicating interest in voting in the Republican primary. It was interesting that within both parties, those expressing no partisanship led to the largest rate differential (significant for Republicans). Table 2 also shows Democrat legislators seeming to favor the DeShawn alias a significantly more amount than the Jake alias if they signaled Democratic partisanship and overall there is a positive combined effect for race differential when partisanship is signaled. This is then broken down by our replication data results for Table 3 and Appendix C, which provide some explanation for the large significant party differential value for the DeShawn alias compared to the Jake alias. The authors initially gathered that Democratic legislators did not discriminate based on race due to positive (not significant) race differential for their party as a whole. However, once they test for heterogeneous treatment effects by party/race of legislators (Table 3/Appendix C), we observe White Democrats responding to the DeShawn alias signaling no partisanship (out.c.c1: -0.06847) at a lower rate, with a race differential comparable to that of White Republicans (-0.0795). Meanwhile, non-white Democratic legislators had a larger positive race differential of 0.16507 (out.3.2) and generally responded more to the DeShawn alias. The similar differences in levels of racial discrimination for both parties point to the legislator’s own race as a new predictor for discrimination that might be even more consequential than the constituent’s partisan preference. These discoveries ultimately call attention to the presence of taste-based discrimination as well as the significance of minority representation in the U.S. government in fairly representing minority constituents, regardless of political party. We can learn from this study the importance of racial composition in politics and further explore the potential impact that has on other political domains such as voter turnout or legislation related to minority rights.
#' 