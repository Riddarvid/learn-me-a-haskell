
Difference lists are an efficient way of appending.

With normal lists we have:

(la ++ lb) ++ lc -> la ++ lb, then lalb ++ lc. This is slow since list concatenation is dependent on the length of the first list. In this case the first part is first la, then lalb.

la ++ (lb ++ lc) -> lb ++ lc, then la ++ lblc. This is fast since the first part is first lb, then la, both short.

With difference lists:

(da ++ db) ++ dc = (\\xs -> la ++ (lb ++ xs)) ++ dc = \\xs -> la ++ (lb ++ (lc ++ xs)). What would be a slow left associative operation with normal lists is transformed into an efficient right associative operation.

da ++ (db ++ dc) = da ++ (\\xs -> lb ++ (lc ++ xs)) = \\xs -> la ++ (lb ++ (lc ++ xs)). A right assocative operation remains right associative.

Conclusion: With difference lists, appending will always be right associative, which is the most efficient way of appending multiple lists together.
