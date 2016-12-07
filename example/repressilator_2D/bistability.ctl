################################################################################
# This is an example of intersting properties for model of 2D repressilator    #
# Warning: numeric thresholds used in properties must be defined also in model #
################################################################################

high = y > 8
low  = y < 4

stay_low? = AG low
stay_high? = AG high
reach_high? = EF high	
reach_low? = EF low		
reach_and_stay_low? = EF(AG low)	
reach_and_stay_high? = EF(AG high)
bistability? = reach_and_stay_high? && reach_and_stay_low?
