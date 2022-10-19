#%%
# This is where I might put helper functions and scripts
import csv
from datetime import datetime
import numpy as np
import trompy as tp

def get_FEDevents(filename, eventname):
    
    formats = ['%Y-%m-%d %H:%M:%S', '%m/%d/%Y %H:%M:%S']

    file = open (filename)
    csvreader= csv.reader(file)
    next(csvreader)
    rows= []
    for row in csvreader:
        rows.append(row)

    # works out which format should be used
    try:
        tmp_date_time_obj = datetime.strptime(rows[0][0], formats[0])
        date_format = formats[0]
    except ValueError:
        tmp_date_time_obj = datetime.strptime(rows[0][0], formats[1])
        date_format = formats[1]

    timestamps = []
    for row in rows: 
        if row[7] == eventname:
            date_time_obj = datetime.strptime(row [0], date_format)
            timestamps.append (date_time_obj)
           
    t0 = rows[0] [0] 
    day = t0.split()[0] 
    lightson = day + " 07:00:00" 
    refpoint = datetime.strptime(lightson, date_format)
    
    pellettimes = []
    for t in timestamps:
        Deltat = t-refpoint
        Deltatinseconds = Deltat.total_seconds()
        Deltatinhours = Deltatinseconds/3600
        pellettimes.append(Deltatinhours)

    return pellettimes

def get_intermealinterval (pellettimes):
    IPIs = np.diff(pellettimes)
    IMI= np.mean([x for x in IPIs if x > (1/60)])
    return IMI
#%%

# function to get timestamps from fed csv files
metafile = "..\\FEDXA DATA SHEETS METAFILE .xls"
rows, header = tp.metafilereader(metafile, sheetname="METAFILE")

mice = {}
for row in rows:
    mouse_id = row[1]
    if mouse_id not in mice.keys():
        mice[mouse_id] = {}

for key in mice.keys():
    for row in rows:
        if "sex" not in mice[key].keys():
            mice[key]["sex"] = row[4]
            mice[key]["order"] = row[5]

        if row[1] == key and row[3] == "FF":
            filename = "..\\data\\{}\\{}".format(row[1], row[0])
            if row[2] == "GRAIN":
                mice[key]["grain_timestamps"] = get_FEDevents(filename, "Pellet")
            elif row[2] == "PR":
                mice[key]["pr_timestamps"] = get_FEDevents(filename, "Pellet")
            elif row[2] == "NR":
                mice[key]["nr_timestamps"] = get_FEDevents(filename, "Pellet")
            else:
                print(row[2], "is not a valid type of pellet for", key)


# %%
## gets bodyweights and adds to dictionary
metafile = "..\\FEDXA DATA SHEETS METAFILE .xls"
rows, header = tp.metafilereader(metafile, sheetname="METAFILE_BW")

n_days = len(rows[0])

for row in rows:
    mouse_id = row[0]
    mice[mouse_id]["bodyweight"] = [row[i] for i in range(1, n_days)]

# %%
## gets hoarded pellets and adds to dictionary
metafile = "..\\FEDXA DATA SHEETS METAFILE .xls"
rows, header = tp.metafilereader(metafile, sheetname="METAFILE_HO")

n_days = len(rows[0])

for row in rows:
    mouse_id = row[0]
    mice[mouse_id]["hoarding"] = [row[i] for i in range(1, n_days)]

# %%
# to get average pellets per day
for key in mice.keys():
    mice[key]["grain_avg_pellets"] = len(mice[key]["grain_timestamps"]) / 3
    mice[key]["pr_avg_pellets"] = len(mice[key]["pr_timestamps"]) / 7
    mice[key]["nr_avg_pellets"] = len(mice[key]["nr_timestamps"]) / 7

# %%
# 
def get_pellets_per_day(timestamps, start_time=4):
    print("will divide pellets up into the number per day")

"""
make dictionary for each mouse with the following fields:
    # order
    # sex
    # bodyweight
    # hoarding
    # grain_timestamps
    # pr_timestamps
    # nr_timestamps
    grain_pellets_per_day
    pr_pellets_per_day
    nr_pellets_per_day

    pr_intermeal_interval
    nr_intermeal_interval
    pr_mealsize
    nr_mealsize
    
how to show bodyweight?
how to show FR and FR-R - work out rolling % correct and determine when it reaches 95%
how to show choice? - work out proportion of pellets of each type per day
                    - work out average ratio reached for each pellet by day

should maybe roll out new release of trompy or give hamid instructions to get dev version

"""
# %%
