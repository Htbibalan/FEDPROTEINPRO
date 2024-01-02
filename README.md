# FEDPROTEINPRO
This repository stores data files and code scripts for FEDXC experiment where we examined the dynamics of food intake through a longer period compared to our previouse experiment (i.e. FEDX A anb B [FEDProtein](https://github.com/Htbibalan/FEDProtein/tree/BNA) )

##### Number of animals:  12 male mice
##### Age: 6 weeks at the time of arrival, 7 weeks at the onset of the experiment
##### Strain:  C57BL/6NRj




![paradigm](https://github.com/Htbibalan/FEDPROTEINPRO/blob/main/source/FEDXC_paradigm.png)
*The figure shows the overall paradigm of the experiment. Ctl stands for control group that received 20% casein pellets for 4 weeks and Exp stands for experimental group that received 20% and 5% casein pellets alternatively each week. However, the experiment was initially designed for a 4 weeks study(4 epochs) but since we ran out of NR pellets at the end of the 3rd week, we had to exclude the data for Ctl group. Nevertheless it should not be a problem to compare the 4 epochs of Exp group.*

##### Cohorts
* The experiment includes one cohort of 12 male mice, FEDXC01-06 are the experimental group and FEDXC07-12 are the control group.

##### Food and bodyweight measurment
* Number of pellets delieverd were logged by FEDs and read and registered everyday, however for the final analysis we just rely on the number of pellets logged by the FEDs not the manual readings.
* bodyweghit of the mice was measured everyday between 08-10 am.
* We were supposed to log the number of discarded/hoarded pellets, but we did not notice any particular hoarding behaviour in this experiment, however the detailed data for each mouse can be found in the file named METAFILE FEDXC DATA SHEETS.xls stored in folder /source. Moreover, in the 4th week of the experiment, since we were running out of NR pellets, we used expired NR pellets for the Ctl group which led to several discarded pellets that we can not count as "hoarding".

##### Experiment protocol details
* All mice start with 2 days of training with FEDs loaded with NR 20% casein pellets.
* FEDXC01-06: after the first 2 days , receive 20% casein pellets for 1 week, at the begining of the 2nd week these mice switch to 5% casein pellets and then switch to 20% at the 3rd week and again to 5% for the last week.
* FEDXC07-12: These mice start with 2 days of training with FEDs loaded with NR 20% casein pellets and receieve the same pellet through out the course of the experiment, i.e. from week 1 to week 4, however as mentioned earlier, the data from the 4th week is excluded due to usage of expired pellets. 


##### GitHub repository structure
* **/data** stores data files obtained from FEDs
* **/notebooks** stores python scripts either for data analysis or creating poster figures
* **/source** stores function helper files and the original data sheet collected manually by the experimenter
* **/results** mainly stores data extracted from python scripts and converted for JASP

Look at **FEDXC_Metafile** to have an overview of the FED files attributed to each mouse and each epoch, the same file also stores bodyweight data of all mice.