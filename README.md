# Introduction (Project in Development)
The present repository will contain the work and code developed within a collaboration between Mohammad Royapoor (University of Newcastle) and myself alongside Hailiang Du and Michael Goldstein (University of Durham). The collaboration has only recently started; the content of the repository is therefore partial and subject to changes.

# The Collaboration in Short
A computer model is available, which simulates the energy consumption (gas and electricity) in a property. Data on actual consumption are available. Via Bayes linear emulation, the aim is to find the parameters of the model whose outputs match the data (calibration), while accounting for the uncertainties affecting both sources (measurement errors, discrepancy between simulator and real world).

# Code
The script "Explore_gas_dataset.R" is the main one, where inputs and outputs of the model are explored, and a first emulator is built (details and comments inside the script).
The script uses routines which are defined within the other two files, "Various_Functions.R" and "emulation.R".
