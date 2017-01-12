##################################################################################
# This is an example of expected mass-flow during degradation of substrate into  #
# product while intermediates don't exceed defined level of concentration        #
# Warning: numeric thresholds used in properties should be defined also in model #
#          if not, tool will do completion automatically                         #
##################################################################################

st = SUB > 1.9			# Atomic proposition with explicitly defined threshold
et = SUB < 0.01			# Atomic proposition with explicitly defined threshold
sg = PROD < 0.01		# Atomic proposition with explicitly defined threshold
eg = PROD > 1.9			# Atomic proposition with explicitly defined threshold
bd = INT1 < 0.5			# Atomic proposition with explicitly defined threshold
be = INT2 < 0.5			# Atomic proposition with explicitly defined threshold

:?p0 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))))						# Property standing for fluent mass-flow from substrate into product
:?p1 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd))			# Property standing for fluent mass-flow from substrate into product while INT1 doesn't exceed concentration of 0.5
:?p2 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd) && (AG be))	# Property standing for fluent mass-flow from substrate into product while both intermediates don't exceed concentration of 0.5
