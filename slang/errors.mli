exception Error of string

val complain : string -> 'a
val complainf : ('a, Format.formatter, unit, 'b) format4 -> 'a
