##################################################################################
# This is an example of intersting properties for model of cell-cycle arrest     #
# Warning: numeric thresholds used in properties should be defined also in model #
#          if not, tool will do completion automatically                         #
##################################################################################

high = E2F1 > 3												# Atomic proposition with explicitly defined threshold
low  = E2F1 < 3												# Atomic proposition with explicitly defined threshold

:?stay_low = AG low											# Property standing for concentration of 'y' never exceeds 3
:?stay_high = AG high										# Property standing for concentration of 'y' never drops below 3
:?reach_high = EF high										# Property standing for concentration of 'y' eventually exceeds 3
:?reach_low = EF low										# Property standing for concentration of 'y' eventually drops below 3
:?reach_and_stay_low = EF(AG low)							# Property standing for concentration of 'y' eventually drops below 3 and stays there
:?reach_and_stay_high = EF(AG high)							# Property standing for concentration of 'y' eventually exceeds 3 and stays there
:?bistability = reach_and_stay_high && reach_and_stay_low	# Property standing for concentration of 'y' can either eventually drops below 3 and stays there or eventually exceeds 3 and stays there
