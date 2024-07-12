* Encoding: UTF-8.

Dataset name df.
Dataset Activate df.

compute case_id= $casenum.
exe.

*https://www.ryentes.com/careless/intro.html.
*Psychometric Synonyms and Antonyms.
Compute err = $sysmis.
exe.
if es4=1 & es5=1 | es4=5&es5=5 err=5.
fre err.
Compute SCOR2=err.
fre scor2.

Compute err = $sysmis.
if ((any(es6,4,5)&any(es7,4,5) & any(es8,4,5))&es10=1) |((any(es6,1,2)&any(es7,1,2) & any(es8,1,2))&es10=5) err=2.
fre err. 
Compute SCOR3=err.

Compute err=$sysmis.
if pss3=4 &es9=1 | pss3=0 &es9=5 err=1.
fre err.
Compute SCOR4=err.

Compute err=$sysmis.
if pss7=4 &es7=1 | pss7=0 &es7=5 err=1.
fre err.
Compute SCOR5=err.

Compute err=$sysmis.
if es1=5 &es2=1 | es1=1 &es2=5 err=1.
fre err.
Compute SCOR6=err.

*Intra-individual Response Variability by scales with reversed items and extreme cases.
do if as1=1 |as1=5.
recode row_sd_As (0=3) (else=sysmis) into SCOR7.
else.
recode row_sd_As (0=2) (else=sysmis) into SCOR7.
end if.
do if any(soc1,1,7).
RECODE row_sd_Soc (0=3)(else=sysmis) into SCOR8.
else.
RECODE row_sd_Soc (0=2)(else=sysmis) into SCOR8.
end if.
do if es1=1|es1=5.
recode row_sd_Es (0=3) (else=sysmis) into SCOR9.
else.
recode row_sd_Es (0=2) (else=sysmis) into SCOR9.
end if.
exe.

if flagged=1 SCOR10=3.
exe.

if flagged1=1 SCOR11=2.
if flagged2=1 SCOR12=2.
if flagged3=1 SCOR13=2.
if flagged4=1 SCOR14=2.
if flagged5=1 SCOR15=2.
if flagged6=1 SCOR16=2.
exe.

fre longstr.

recode longstr (9 THRU hi = 2) into SCOR17.
exe.
fre scor11.

if even_odd_results>-0.3 SCOR18=5.
fre SCOR12.

del var err.
exe.
Compute tot_SCOR =sum(scor1, scor2 to scor18).
fre tot_scor.

temp.
select if tot_SCOR>5.
SUMMARIZE id /format=list.

fre school.

SAVE OUTFILE='C:\Users\iulii\Desktop\internal review HIT\новые опрос китайцев\данные\чистка.sav'
  /COMPRESSED.

*check what the answers of a particular respondent are, example with id=5.
temp.
select if case_id=5.
list as1 to cca3 soc1 to soc13 es1 to es10 pss1 to pss10 longstr.




