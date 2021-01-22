# N15-LIM-BLOOFINZ-GoM
Linear inverse model with MCMC+15N for the BLOOFINZ Gulf of Mexico Project

This code was written to solve a linear inverse ecosystem model that includes non-linear approximate equality constraints (the δ15N of each model compartment).  It uses a Markov Chain Monte Carlo approach adapted from the MCMC+15N approach developed in Stukel et al. (2018a).  The MCMC+15N, approach was itself an adaptation of the MCMC approach of Van Den Meersche et al.  (2009).  The MCMC+15N approach added a second simultaneous random walk through δ15N space.  The δ15N vector in turn affected the approximate equalities (Ax≈b) in Van Den Meersche et al. (Van den Meersche et al., 2009).  For a thorough discussion of the MCMC+15N approach, I refer readers to Stukel et al. (2018a; 2018b).  This version of the code includes multiple adaptations necessary to model a substantially larger number of ecosystem flows associated with the oligotrophic, open-ocean Gulf of Mexico planktonic environment as part of the BLOOFINZ-GoM project.

Goal
The goal of this code was to develop an approach sufficient to constrain an underdetermined ecosystem model for the open-ocean Gulf of Mexico with a specific focus on trophic pathways of larval Atlantic Bluefin tuna that live in the upper euphotic zone.  The model differentiates between a lower euphotic zone (extending from the depth of the deep chlorophyll maximum to a depth of 50 m) and the upper euphotic zone (surface to 50 m depth).  The model includes a total of 302 unknown ecosystem nitrogen flows to be solved for.  It includes 44 mass balance (exact equality) equations,  82 approximate equality constraints (including 38 field rate measurements and 44 nitrogen istope mass balance equations), and 254 inequality constraints.  An overview of the model is shown in Fig. 1.  Full details of the model are available in Stukel et al. (2021).  If you do not have access to that manuscript, a pre-print is available: (Stukel et al., 2020).

Files

N15InverseModelRW.GOM.xlsx and N15InverseModelRW-C5.GOM:  These files contain the model structure in an easily readable excel format.  Note that because of slight differences in measurements between the two cycles, they differ slightly between Cycle 1 and Cycle 5 (to different Lagrangian experiments from which the data are derived).  

Inputs.xlsx: This excel file includes all the field data used to constrain the inverse model.

SetMatricesN15GoM.m and SetMatricesN15GoMC5.m:  These Matlab files can be used to read the aforementioned excel files and create the matrices needed to run the LIM-N15-BLOOFINZ-GoM model.  The files that they will create are called N15GoMInverseCycle1.mat and N15GoMInverseCycle5.mat

N15GoMInverseCycle1.mat and N15GoMInverseCycle5.mat:  These files (created by SetMatricesN15GoM.m and SetMatricesN15GoMC5.m) contain the matrices that must be solved by the LIM+15N procedure, as well as the δ15N values that add additional constrain to the random walk.

RunN15InverseRW.R and RunN15InverseRWcycle5.R:  These are the codes that run the model in R.  They rely on the presence of the input files (the .mat files mentioned previously) as well as the xsampleN15outputs.R and ExternalFunctions.R files.  They will create files named N15GoMInverseCycle1.mat or N15GomInverseCycle5.mat that contained the model solutions.  These solutions are stored in the variables MCMCmat and del15N.  Please note that the model included online is configured to run with only 200,000 iterations so that users can test to ensure that it is working.  However, for the manuscript we ran it with >100,000,000 iterations.  

xsampleN15outputs.R:  This is the file that actually runs the MCMC+15N inverse.  It is called by the RunN15InverseRW.R and RunN15InverseRWcycle5.R files.  

ExternalFunctions.R:  This file is called by xsampleN15outputs.R and serves to re-set the approximate equality matrix (Aa) based on changes in the estimated δ15N of each model compartment as stored in the d15N vector.

References
Stukel, M.R., Décima, M. and Kelly, T.B., 2018a. A new approach for incorporating 15N isotopic data into linear inverse ecosystem models with Markov Chain Monte Carlo sampling. PloS one, 13(6): e0199123.
Stukel, M.R., Décima, M., Landry, M.R. and Selph, K.E., 2018b. Nitrogen and isotope flows through the Costa Rica Dome upwelling ecosystem: The crucial mesozooplankton role in export flux. Global Biogeochemical Cycles, 32: 1815-1832.
Stukel, M.R. et al., 2020. Plankton food webs of the Gulf of Mexico spawning grounds of Atlantic Bluefin tuna. bioRxiv: 2020.07.29.227116.
Stukel, M.R. et al., 2021. Plankton food webs of the Gulf of Mexico spawning grounds of Atlantic Bluefin tuna. Journal of Plankton Research.
Van den Meersche, K., Soetaert, K. and Van Oevelen, D., 2009. xSample(): An R function for sampling linear inverse problems. Journal of Statistal Software, Code Snippets, 30(1): 1-15.

