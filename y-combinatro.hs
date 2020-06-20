y :: (t -> t) -> t
y = \f -> f (y f)
