##################################################################################
# Advanced properties of repressilator model.									 #
##################################################################################
# 1. Identify all stable states of the system.									 #
# 2. Two different stable states exists in the system (bistability).             #
##################################################################################
# Warning: The declared formulae lead to computationally hard tasks,			 #
#  	   high-performance hardware is required!							         # 
##################################################################################

# HUCTL properties declaration

# Identifying all stable states.
# An expressive general HUCTL formula independent of atomic propositions.
#
:?stable = bind x: AG EF x

# Existence of two different stable states wrt y. 
# A HUCTL formula precisely formalising the systems bistability.
# 
:?bistable = exists s in stable: exists t in stable: {!y+} EF s && {!y-} EF t && at t: !EF s
