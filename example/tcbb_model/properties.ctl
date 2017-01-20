##################################################################################
# Properties representing basic properties of G1/S transition switch.	         #
##################################################################################
# 1. E2F1 protein is required to stay in high or low concentration mode.         #
# 2. E2F1 protein is required to reach the high or low concentration mode.       #
# 3. E2F1 protein is required to be able to reach both modes (bistability).      #
##################################################################################

# atomic propositions declaration
#
# high state of E2F1 (observed in cancer cells)
high = E2F1 > 3	
# low state of E2F1 (observed in healthy cells)
low  = E2F1 < 3			
###
# Warning: thresholds used in atomic propositions should be defined also in the model.    
#          If not, the tool will do their addition automatically.

# CTL properties declaration
#								
# Property requiring the concentration of E2F1 to never exceed 3. 
:?stay_low = AG low

# Property requiring the concentration of E2F1 to never drop below 3.							
:?stay_high = AG high

# Property requiring the concentration of E2F1 to eventually exceed 3.						
:?reach_high = EF high

# Property requiring the concentration of E2F1 to eventually drop below 3.	
:?reach_low = EF low		

# Property requiring the concentration of E2F1 to eventually drop below 3 and remain there.
:?reach_and_stay_low = EF(AG low)					

# Property requiring the concentration of E2F1 to eventually exceed 3 and remain there.		
:?reach_and_stay_high = EF(AG high)						

# Property requiring the concentration of E2F1 to eventually drop below 3 and remain there
# or to eventually exceed 3 and remain there (the so-called bistable switch, states satisfying 
# this give the cell the ability to decide by fine-tuning the initial concentration of E2F1).
:?bistability = reach_and_stay_high && reach_and_stay_low
