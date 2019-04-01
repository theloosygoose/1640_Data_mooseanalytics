import csv
import os

inputFileName = "Bensalem2019 - Sheet1.csv"
outputFileName = os.path.splitext("Bensalem2019")[0] +".csv"

with open(inputFileName, newline='') as inFile, open(outputFileName, 'w', newline='') as outfile:    
    r = csv.reader(inFile)
    w = csv.writer(outfile)

    next(r, None)  # skip the first row from the reader, the old header
    # write new header
    w.writerow(["Match","Pos","Scout_Initials","Team","Is_Present","Starting_Location","Piece_Holding","Crossed_Baseline","Activity_Type","SS_LR_HATCH_LH","SS_LR_HATCH_RH","SS_LR_HATCH_LM","SS_LR_HATCH_RM","SS_LR_HATCH_LL","SS_LR_HATCH_RL","SS_RR_HATCH_LH","SS_RR_HATCH_RH","SS_RR_HATCH_LM","SS_RR_HATCH_RM","SS_RR_HATCH_LL","SS_RR_HATCH_RL","SS_LR_CARGO_H1","SS_LR_CARGO_H2","SS_LR_CARGO_M1","SS_LR_CARGO_M2","SS_LR_CARGO_L1","SS_LR_CARGO_L2","SS_RR_CARGO_H1","SS_RR_CARGO_H2","SS_RR_CARGO_M1","SS_RR_CARGO_M2","SS_RR_CARGO_L1","SS_RR_CARGO_L2","SS_CS_HATCH_L3","SS_CS_HATCH_L2","SS_CS_HATCH_L1","SS_CS_HATCH_FL","SS_CS_HATCH_FR","SS_CS_HATCH_R1","SS_CS_HATCH_R2","SS_CS_HATCH_R3","SS_CS_CARGO_L3","SS_CS_CARGO_L2","SS_CS_CARGO_L1","SS_CS_CARGO_FL","SS_CS_CARGO_FR","SS_CS_CARGO_R1","SS_CS_CARGO_R2","SS_CS_CARGO_R3","SS_FEEDER_L","SS_FEEDER_R","LR_HATCH_LH","LR_HATCH_RH","LR_HATCH_LM","LR_HATCH_RM","LR_HATCH_LL","LR_HATCH_RL","RR_HATCH_LH","RR_HATCH_RH","RR_HATCH_LM","RR_HATCH_RM","RR_HATCH_LL","RR_HATCH_RL","LR_CARGO_H1","LR_CARGO_H2","LR_CARGO_M1","LR_CARGO_M2","LR_CARGO_L1","LR_CARGO_L2","RR_CARGO_H1","RR_CARGO_H2","RR_CARGO_M1","RR_CARGO_M2","RR_CARGO_L1","RR_CARGO_L2","CS_HATCH_L3","CS_HATCH_L2","CS_HATCH_L1","CS_HATCH_FL","CS_HATCH_FR","CS_HATCH_R1","CS_HATCH_R2","CS_HATCH_R3","CS_CARGO_L3","CS_CARGO_L2","CS_CARGO_L1","CS_CARGO_FL","CS_CARGO_FR","CS_CARGO_R1","CS_CARGO_R2","CS_CARGO_R3","FEEDER_L","FEEDER_R","Level_Ended","Number_Assisted","Assist_Method","Assisted_By_Other","General_Success","Defensive_Success","Success_v_Defense","Efficient_Placing","Dropped_Pieces","Floor_Pickup_Hatch","Floor_Pickup_Cargo","Fouls?","Problems?"])

    for row in r:
        w.writerow(row)