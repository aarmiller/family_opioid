#  Assessing the association between exposure to opioids prescribed to family members and risk of overdose (family_opioid)

## Research question
* If a family member in a household has been prescribe opioids, does that increase the risk of overdose for other family member in that household?
	
## Study population
* Insured population capture in Truven from 2001-2017 with >= 2 family member in insurance plan (about 142 million). 
	
## Exposure
* Opioid prescriptions in household vs. no prescriptions in household in the past 30 days (prior to overdose or start of the month for those that have never overdosed)?
* Need to come up with opioid ndc codes 
* **Conversion of days supply and strength into one metric**
	* Strength per Unit  X  (Number of Units/ Days Supply)  X  MME conversion factor  =  	MME/Day
	* Apparent prescribed daily  = Strength per EU x (quantity/days supply) = dose (PDD)
* **Exposure as a function of high vs low risk opioid’s and days supply**
	* Instead of high vs low look at interaction between MME per pill and number of pills
	* So maybe first look at MME overall, then interaction between MME per pill and number of pills (or MME per patch for patched or MME per ml for liquids)
		* Aggregate over all family member in the past 30 days.
		* Consider dose response 
		* More/stronger opioids based on MME or quantity dispensed = increased IRR of overdose 
			* The longer duration of continuous prescriptions = increased IRR of overdose? (e.g.individuals exposed to a family member who has been receiving continuous prescriptions for opioids for 60 days may have greater risk than exposed to a family member who has been receiving continuous prescriptions for opioids for 30 days. Need to think of allowable gaps, maybe 30 days?) 
		* Look at stockpiling/oversupply (total days supply left over from fill i -  total days supply for i +1) (hypothesis family member may be less likely to notice missing medication during times of oversupply?)
	* Type of opioid?
	* Look at regions across the use that effect vary in high vs low prevalence regions
* **Chronic vs acute** 
	* Based on continuous prescription > 30 days (using allowable gap) to define chronic prescriptions vs acute prescriptions (<30 days of continuous prescription)
	* Use conditions to define acute and chronic pain managment. e.g. use most common surgical procedures that prescibe opioids or conditions like kidney stones to classify acute vs chronic (chronic would be like cancer). 
	* Effects should be attenuated among chronic users as they are more likely to take all of their meds (less likely to have meds left over that family members can take an overdose on)
	* Effects should be greater among acute as they may not complete the course of their meds (more likely to have meds left over that family members can take an overdose on)

## Outcome
* Nonfatal and fatal pharmaceutical opioid overdoses
* Stratify by fatal and non-fatal?
* Use index case of overdose
* Limit to ED and inpatient settings only?
* Might want to look at dependence abuse as well, not just overdose

## Potential covariates
* Age
* Year
* Month
* Sex
* Comorbidities?
* Risk factors for overdose
	* HIV
	* Liver, kidney, or lung disease 
	* Mental health conditions
	* Tobacco use disorder
	* Drug use disorder
	* Alcohol use disorder
	* SES (rural/urban MSA indicator, median income in MSA?)
	* Race not available 

## Prior studies
* https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2763605#:~:text=with%20a%20prescription.-,Exposure%20to%20family%20members%20with%20opioid%20prescriptions%20in%20the%20past,CI%2C%203.39%2D12.91.
* In this cohort study of 72 040 *adolescents and young adults* (from Kaiser Permanente Colorado health plan in 2006 and observed through June 2018), exposure to family members with opioid prescriptions in the past month was associated with a 2-fold increase in the risk of overdose
* Youth’s own prescriptions were associated with a more than 6-fold increase in risk. 
* Concurrent exposure to prescriptions of family members and youth themselves was associated with a nearly 13-fold increase in overdose risk.
* Only worked with a total of 103 overdoses
* **Exposure**:
	* Summed the total morphine milligram equivalents (MME) of all prescription opioids dispensed in the past month to all family members and to index youth. 
	* Total MME in the past month was subsequently divided into approximate terciles separately for family members (ie, >0 to <200 MME, 200 to <600 MME, and ≥600 MME) and index youth (ie, >0 to <120 MME, 120 to <225 MME, and ≥225 MME).
* **Outcome**:
	* nonfatal pharmaceutical opioid overdoses in emergency department and inpatient settings from medical billing claims
	* ICD-9 codes 965.0, 965.00, 965.02, 965.09, E850.1, and E850.2 and ICD-10 codes T40.0X1 to T40.0X4, T40.2X1 to T40.2X4, T40.3X1 to T40.3X4, and T40.4X1 to T40.4X4. 
	* ICD-10 codes that indicate drug poisoning as the underlying cause of death (ie, X40-X44, X60-X64, X85, Y10-Y14) and pharmaceutical opioid involvement as a contributing cause (ie, T40.2-T40.3)
* **Covariates**:
	* Charlson Comorbidity index
	* Drug use disorder
	* Alcohol use disorder
	* Tobacco use
	* Major depressive disorder
	* Anxiety disorder
	* Mood disorder
	* Pain diagnosis
	* Prescriptions for benzodiazepines, stimulants or antidepressants
	* Gender
	* Race
	* Ethnicity
	* Age
	* Family unit size
	* Tract-level median family income 

## Innovations
* Truven nationwide data, large sample size. Not limiting to just youths or adolescents.
* Instead of just using MME, intereact number of pills (or pathes or ml) by MME/pill (or per patch or per ml) to get at the amont dispensed. We could use days supply but after talking to Ryan, he made me realize that days supply is not really accurate, especially for use as needed opioid prescription. 
* Look at interaction between exposure and age (like in the CDIFF paper)
* If possible stratify by fatal and non-fatal?
* Maybe also look at both overdoses and dependence abuse sperately?
* Use data driven approach to classify chronic and acute opioid use (< 30 day as cutoff for acute?), then stratify by chronic vs acute (or add interaction term)



