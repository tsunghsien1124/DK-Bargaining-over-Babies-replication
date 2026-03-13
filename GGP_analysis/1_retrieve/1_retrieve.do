clear all

* generate wave 1 data
do "1_retrieve/2_wave1.do"

* generate wave 2 data (includes merging between waves)
do "1_retrieve/3_wave2.do"
