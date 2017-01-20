##################################################################################
# Properties representing basic properties of 2-gene repressilator               #
##################################################################################
# 1. Protein is required to stay in high or low concentration mode.              #
# 2. Protein is required to reach the high or low concentration mode. 	         #
# 3. Protein is required to be able to reach both modes (bistability).           #
##################################################################################

# atomic propositions declaration
#
# high state of y
high = y > 8		
# low state of y										
low  = y < 4												
###
# Warning: thresholds used in atomic propositions should be defined also in the model.
#          If not, the tool will do their addition automatically.

# CTL properties declaration
#
# Property requiring the concentration of y to never exceed 4.
#
:?stay_low = AG low											

# Property requiring the concentration of y to never drop below 8.
#
:?stay_high = AG high

# Property requiring the concentration of y to eventually exceed 8.
#
:?reach_high = EF high		

# Property requiring the concentration of E2F1 to eventually drop below 4.
#
:?reach_low = EF low	

# Property requiring the concentration of y to eventually drop below 4 and remain there.
#
:?reach_and_stay_low = EF(AG low)

# Property requiring the concentration of E2F1 to eventually exceed 8 and remain there.
#
:?reach_and_stay_high = EF(AG high)

# Property requiring the concentration of y to be able to eventually drop below 4 and remain there
# and also to eventually exceed 8 and remain there.
# The property holds in a state where both situations are reachable.
# 
:?bistability = reach_and_stay_high && reach_and_stay_low
