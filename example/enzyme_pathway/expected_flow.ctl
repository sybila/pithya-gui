##################################################################################
# Properties representing desired constraints on the metabolic mass flow.        #
##################################################################################
# 1. Substrate SUB is required to entirely convert into the product PROD.	 #
# 2. Intermediates cannot exceed certain concentration levels during 		 #
#    the conversion.								 #
##################################################################################

# atomic propositions declaration
#
st = SUB > 1.9			
et = SUB < 0.01			
sg = PROD < 0.01		
eg = PROD > 1.9			
bd = INT1 < 0.5			
be = INT2 < 0.5		
###
# Warning: thresholds used in atomic propositions should be defined also in the model.    
#          If not, the tool will do their addition automatically.

# CTL properties declaration
#
# Complete and fluent mass conversion of substrate into product. 
:?p0 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))))		

# Complete and fluent mass conversion of substrate into product while additionally 
# requiring the intermediate INT1 not to exceed concentration 0.5. 				
:?p1 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd))			

# Complete and fluent mass conversion of substrate into product while additionally 
# requiring each of the intermediates INT1 and INT2 not to exceed concentration 0.5. 	
:?p2 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd) && (AG be))
