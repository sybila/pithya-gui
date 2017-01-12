##################################################################################
# This is an example of intersting property for model of cell-cycle arrest       #
# Warning: numeric thresholds used in properties should be defined also in model #
#          if not, tool will do completion automatically                         #
# Warning 2: this property is computationally exhausting !!!                     #
##################################################################################

# Property standing for presence of all stable states in model
:?stable = bind x: AG EF x

# Property standing for presence of all pairs of distinct stable states in model
:?bistable = exists s in stable: exists t in stable: {!E2F1+} EF s && {!E2F1-} EF t && at t: !EF s
