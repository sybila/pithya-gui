##################################################################################
# Advanced properties of G1/S transition switch.				 #
##################################################################################
# 1. Identify all stable states of the system.					 #
# 2. E2F1 protein is required to reach the high or low concentration mode.       #
# 3. E2F1 protein is required to be able to reach both modes (bistability).      #
##################################################################################
# Warning: The declared formulae lead to computationally hard tasks,		 #
#  	   high-performance hardware is required!			         # 
##################################################################################

# Identifying all stable states.
# An expressive general HUCTL formula independent of atomic propositions.
#
:?stable = bind x: AG EF x

# Existence of two different stable states wrt E2F1. 
# A HUCTL formula precisely formalising the systems bistability.
# 
:?bistable = exists s in stable: exists t in stable: {!E2F1+} EF s && {!E2F1-} EF t && at t: !EF s
