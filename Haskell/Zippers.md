
"In general, a single breadcrumb should contain all the data needed to reconstruct the parent node. So it should have the information from all the paths that we didn't take and it should also know the direction that we did take, but it must not contain the sub-tree that we're currently focusing on."

"That's because we already have that sub-tree in the first component of the tuple, so if we also had it in the breadcrumbs, we'd have duplicate information."

		A zipper is a pair of a focused part of a data structure and its surroundings.