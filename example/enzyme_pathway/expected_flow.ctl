st = SUB > 1.9
et = SUB < 0.01
sg = PROD < 0.01
eg = PROD > 1.9
bd = INT1 < 0.5
be = INT2 < 0.5

:?p0 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))))
:?p1 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd))
:?p2 = ((st AU (AF (AG et))) && (sg AU (AF (AG eg))) && (AG bd) && (AG be))
