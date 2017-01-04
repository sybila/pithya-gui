################################################################################
# This is an example of intersting properties for model of 2D repressilator    #
# Warning: numeric thresholds used in properties must be defined also in model #
################################################################################

high = y > 8	# Atomic proposition with explicitly defined threshold
low  = y < 4	# Atomic proposition with explicitly defined threshold

:?stay_low = AG low		# Property standing for concentration of 'y' never exceeds 4
:?stay_high = AG high	# Property standing for concentration of 'y' never drops below 8
:?reach_high = EF high	# Property standing for concentration of 'y' eventually exceeds 8
:?reach_low = EF low		# Property standing for concentration of 'y' eventually drops below 4
:?reach_and_stay_low = EF(AG low)	# Property standing for concentration of 'y' eventually drops below 4 and stays there
:?reach_and_stay_high = EF(AG high)	# Property standing for concentration of 'y' eventually exceeds 8 and stays there
:?bistability = reach_and_stay_high && reach_and_stay_low	# Property standing for concentration of 'y' can either eventually drops below 4 and stays there or eventually exceeds 8 and stays there
