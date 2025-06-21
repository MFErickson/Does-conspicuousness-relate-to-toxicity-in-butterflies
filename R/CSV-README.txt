Contents of the conspicuousness.csv file

Each row describes a single species, unless the species is sexually
dimorphic, in which case there are 2 rows for the species, one for the
male and one for the female.

Column descriptions

Binomial                - species scientific name 
Key			- describes the morphotypes used in our analyses
N.daphnia		- number of Daphnia used in assays
deaths			- number of daphnia killed in the assays
Daphnia.mortality.p	- deaths/N.daphnia giving the percentage of deaths of toxicity assays
Phylo			- closest phylogeny match of the Kawahara et. al 2012 phylogenetic tree
Sex                     - NA if the species is not sexually dimorphic, otherwise "female" or "male"
Time.difference         - Difference in detection time (milliseconds) for the dorsal and ventral photos (Dorsal.time - Ventral.time)
Dorsal.photo            - Numeric ID of the dorsal image file for the species (or species and sex)
Dorsal.URL              - Name of the dorsal image file for the species (or species and sex)
Dorsal.time             - Mean detection time (milliseconds) for the dorsal image photo on the background that 
                          had the longest mean detection time for this photo (i.e. the most cryptic background)
Dorsal.cryptic.BG       - Name of the most cryptic background for this photo
Dorsal.Time.lower       - 95% confidence interval (CI) lower limit for detection time for this photo
Dorsal.Time.upper       - 95% confidence interval (CI) upper limit for detection time for this photo
Dorsal.Rank             - Rank of detection time for this photo relative to all other photos (dorsal 
                          and ventral). Rank 1 has shortest detection time (most conspicuous); 
                          rank 123 has the longest detection time (most cryptic)
Dorsal.Rank.lower       - 95% confidence interval (CI) lower limit for rank for this photo
Dorsal.Rank.upper       - 95% confidence interval (CI) upper limit for rank for this photo
Dorsal.Class            - "Cryptic" or "Conspicuous"
Dorsal.Time.range.pc    - Range of detection times across all backgrounds / maximum detection time (as a percentage)

Ventral.photo           - Numeric ID of the ventral image file for the species (or species and sex)
Ventral.URL             - Name of the ventral image file for the species (or species and sex)
Ventral.time            - Mean detection time (milliseconds) for the ventral image photo on the background that 
                          had the longest mean detection time for this photo (i.e. the most cryptic background)
Ventral.crytic.BG       - Name of the most cryptic background for this photo
Ventral.Time.lower      - 95% confidence interval (CI) lower limit for detection time for this photo
Ventral.Time.upper      - 95% confidence interval (CI) upper limit for detection time for this photo
Ventral.Rank            - Rank of detection time for this photo relative to all other photos (dorsal 
                          and ventral). Rank 1 has shortest detection time (most conspicuous); 
                          rank 123 has the longest detection time (most cryptic)
Ventral.Rank.lower      - 95% confidence interval (CI) lower limit for rank for this photo
Ventral.Rank.upper      - 95% confidence interval (CI) upper limit for rank for this photo
Ventral.class           - "Cryptic" or "Conspicuous"
Ventral.Time.range.pc   - Range of detection times across all backgrounds / maximum detection time (as a percentage)

Remaining columns are the average detection time (milliseconds) for the photo on each 
background, e.g. column "Dorsal.bush1" is the average detection time of the dorsal 
photo on the background "bush1", and "Ventral.litter2" is the average detection time 
of the ventral photo on the background "litter2"