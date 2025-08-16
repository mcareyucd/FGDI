#' Amputee Combined Gait Data
#'
#' Kinematic data for 18 individuals with unilateral above-knee amputations and 42 healthy controls.
#' Each entry contains 18 kinematic variables measured at 1% intervals over the full gait cycle. 
#' control data precedes amputee data. Amputee data from Hood et al. (2020); control data from
#' Fukuchi et al. (2018).
#' @format A list containing the 18 kinematic variables with each list containing a data.frame with N rows and T variables, where N is 
#' the total number of subjects and T is the total number of time points. 
#' \describe{
#'   \item{Left Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Left Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Left Foot Progression Angle}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Right Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Right Foot Progression Angle}{numeric data.frame with N rows and T variables}
#' }
#' @source Internal gait study
"A_Data"

#' Amputee Left-Side Gait Data
#'
#' Kinematic data for 18 individuals with unilateral above-knee amputations (left side only) and 42 healthy controls.
#' Each entry includes pelvic, hip, knee, ankle, and foot angles, sampled at 1% intervals
#' over the full gait cycle. Amputee data from Hood et al. (2020); control data from Fukuchi et al. (2018).
#' @format A list containing the 9 kinematic variables with each list containing a data.frame with N rows and T variables, where N is 
#' the total number of subjects and T is the total number of time points. 
#' \describe{
#'   \item{Left Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Left Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Left Foot Progression Angle}{numeric data.frame with N rows and T variables}
#' }
"A_DataL"

#' Amputee Right-Side Gait Data
#'
#' Kinematic data for 18 individuals with right-side above-knee amputations and 42 healthy controls.
#' Includes pelvic, hip, knee, ankle, and foot angles, sampled at 1% intervals
#' over the full gait cycle. Amputee data from Hood et al. (2020); control data from Fukuchi et al. (2018).
#' @format A list containing the 9 kinematic variables with each list containing a data.frame with N rows and T variables, where N is 
#' the total number of subjects and T is the total number of time points. 
#' \describe{
#'   \item{Right Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Right Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Right Foot Progression Angle}{numeric data.frame with N rows and T variables}
#' }
"A_DataR"

#' Metadata for Amputee Subjects
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{Subject Code}{Unique identifier for each subject (e.g., TF01, TF02).}
#'   \item{Age(yrs)}{Age of the subject in years.}
#'   \item{Gender}{Biological sex (e.g., Male, Female).}
#'   \item{Mass(kg)}{Body mass in kilograms.}
#'   \item{Height(m)}{Height in meters.}
#'   \item{Amputation side}{Side of amputation (Left or Right).}
#'   \item{Etiology}{Cause of amputation (e.g., Traumatic, Infection, Dysvascular).}
#'   \item{Age of Amputation (yrs)}{Age at which the subject underwent amputation.}
#'   \item{K-Level}{Medicare Functional Classification Level (e.g., K2, K3), indicating the subject’s mobility level.}
#'   \item{Prescribed Prosthesis Knee}{Type/model of the prosthetic knee prescribed (e.g., C-Leg Obk, Plie FI).}
#'   \item{Prescribed Prosthesis Ankle}{Type/model of the prosthetic ankle prescribed (e.g., AllPro FI, Triton Obk).}
#'   \item{Socket Suspension}{Suspension mechanism used for the prosthetic socket (e.g., Suction, Lanyard, Pin Lock).}
#'   \item{Training? (#)}{Indicates whether the subject had gait training and, if so, how many sessions (e.g., "Yes (2)", "No").}
#'   \item{Hand-rails?}{Indicates whether hand-rails were used during gait assessment (e.g., "Yes, All", "No").}
#' }
"A_data_info"

#' Parkinson Combined Gait Data
#'
#' Kinematic data for Parkinson's subjects.The PD dataset is publicly accessible, as detailed in Shida, T. K. F., Costa, T. M., de Oliveira, 
#' C. E. N., de Castro Treza, R., Hondo, S. M., Los Angeles, E., ... & Coelho, D. B. (2023).. It includes data from 21
#' right-handed idiopathic PD individuals. Measurements are taken at 1% intervals throughout the entire 100% gait cycle.
#' @format A list containing the 18 kinematic variables with each list containing a data.frame with N rows and T variables, where N is 
#' the total number of subjects and T is the total number of time points. 
#' \describe{
#'   \item{Left Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Left Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Left Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Left Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Left Foot Progression Angle}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Pelvis Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D2}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D1}{numeric data.frame with N rows and T variables}
#'   \item{Right Hip Angles D3}{numeric data.frame with N rows and T variables}
#'   \item{Right Knee Angles}{numeric data.frame with N rows and T variables}
#'   \item{Right Ankle Angle}{numeric data.frame with N rows and T variables}
#'   \item{Right Foot Progression Angle}{numeric data.frame with N rows and T variables}
#' }
"P_Data"

#' Metadata for Parkinson Subjects
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{ID}{Subject identifier (e.g., SUB01).}
#'   \item{Gender}{Sex of the subject (M/F).}
#'   \item{Age}{Age in years.}
#'   \item{Height (cm)}{Height in centimeters.}
#'   \item{Weight (kg)}{Body weight in kilograms.}
#'   \item{BMI (kg/m2)}{Body Mass Index.}
#'   \item{Ortho-Prosthesis}{Indicates use of orthotic or prosthetic aids.}
#'   \item{Years of formal study}{Years of formal education completed.}
#'   \item{Disease duration (years)}{Number of years since Parkinson’s diagnosis.}
#'   \item{L-Dopa equivalent units (mg•day-1)}{Daily medication dosage in L-Dopa equivalents.}
#'   \item{FoG group}{Classification as Freezer or Non-Freezer based on presence of Freezing of Gait (FoG).}
#'   \item{FoG-Q (score)}{Freezing of Gait Questionnaire total score.}
#'   \item{Initial symptoms}{Description of first Parkinson’s disease symptoms.}
#'   \item{Is there a family history of PD? Who?}{Family history of Parkinson’s, if applicable.}
#'   \item{Do you feel improvement after using the antiparkinsonian medicine?}{Subjective report of medication benefit.}
#'   \item{Have you ever had any type of surgery? Which?}{Surgical history.}
#'   \item{Any rehabilitation or physical activity?}{Engagement in physical therapy or activity.}
#'   \item{Other disease (cardiovascular, bone, etc.)}{Reported comorbidities.}
#'   \item{Handedness}{Dominant hand.}
#'   \item{ON/OFF - Hoehn & Yahr}{Clinical staging of Parkinson’s disease severity (ON and OFF medication).}
#'   \item{ON/OFF - MoCA}{Montreal Cognitive Assessment scores.}
#'   \item{ON/OFF - mini-BESTest}{Mini Balance Evaluation Systems Test scores.}
#'   \item{ON/OFF - FES-I}{Falls Efficacy Scale - International.}
#'   \item{ON/OFF - UPDRS-II, III}{Unified Parkinson’s Disease Rating Scale Part II (daily living) and Part III (motor).}
#'   \item{ON/OFF - UPDRS subsections}{Subscores for walking, rigidity, and asymmetry.}
#'   \item{ON/OFF - PIGD or TD}{Phenotype classification: Postural Instability Gait Disorder (PIGD) or Tremor Dominant (TD).}
#'   \item{ON/O}{(Unclear — consider clarifying or removing if unused.)}
#' }
"P_data_info"
