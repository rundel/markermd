# Pure helpers for rubric item groups
#
# A group is a named, contiguous run of a question's rubric items whose
# combined points are bounded together (see markermd_rubric_group). Every
# helper here works on the flat display order of a question's item ids plus
# each item's group id (NA when ungrouped), so the mark app, the database
# writers and the serializers all reason about the same structure: a list of
# top-level blocks, each either a single ungrouped item or a group with its
# member items in order. Contiguity is an invariant that every order writer
# restores through normalize_item_order(); it keeps the UI a list of sections
# and lets the YAML nest a group's items under it.

# Stable permutation that makes each group's members contiguous, at the
# position of the group's first member: members keep their relative order and
# are pulled up to sit behind the first of them; ungrouped items stay put.
#
# group_of: Character vector of group ids in display order (NA = ungrouped)
# Returns: Integer permutation of seq_along(group_of)

group_contiguous_order = function(group_of) {
  group_of = as.character(group_of)
  n = length(group_of)
  if (n == 0) {
    return(integer(0))
  }
  first = vapply(seq_len(n), function(i) {
    g = group_of[i]
    if (is.na(g)) i else min(which(!is.na(group_of) & group_of == g))
  }, numeric(1))
  order(first, seq_len(n))
}

# Reorder a named list of markermd_rubric_item so each group's members are
# contiguous (see group_contiguous_order()). A no-op for an already
# normalized list.
#
# items: Named list of markermd_rubric_item in display order

normalize_item_order = function(items) {
  if (length(items) == 0) {
    return(items)
  }
  items[group_contiguous_order(vapply(items, function(item) item@group, character(1)))]
}

# Split a question's flat item order into top-level blocks: each maximal run
# of items sharing a group id becomes a group block holding those ids, and
# every ungrouped item is a block of its own. Groups with no members (listed
# in group_ids but absent from group_of) are appended as empty group blocks,
# in group_ids order, so they render after the last item.
#
# ids: Character vector of item ids in display order
# group_of: Character vector of the items' group ids (NA = ungrouped),
#   aligned with ids
# group_ids: Character vector of every group id of the question (optional)
# Returns: List of blocks, each list(type = "item", id) or
#   list(type = "group", group, ids)

rubric_blocks = function(ids, group_of, group_ids = character(0)) {
  ids = as.character(ids)
  group_of = as.character(group_of)
  n = length(ids)
  blocks = list()
  i = 1
  while (i <= n) {
    g = group_of[i]
    if (is.na(g)) {
      blocks[[length(blocks) + 1]] = list(type = "item", id = ids[i])
      i = i + 1
      next
    }
    j = i
    while (j <= n && !is.na(group_of[j]) && group_of[j] == g) {
      j = j + 1
    }
    blocks[[length(blocks) + 1]] = list(type = "group", group = g, ids = ids[i:(j - 1)])
    i = j
  }
  for (g in setdiff(group_ids, group_of[!is.na(group_of)])) {
    blocks[[length(blocks) + 1]] = list(type = "group", group = g, ids = character(0))
  }
  blocks
}

# Flatten blocks back to the item id order they describe
#
# blocks: List of blocks from rubric_blocks()

blocks_to_ids = function(blocks) {
  as.character(unlist(lapply(blocks, function(b) if (b$type == "item") b$id else b$ids)))
}

# Move element k of x one step in direction, with the mark app's wrap rule:
# moving up from the top sends it to the end and moving down from the bottom
# sends it to the front (a rotation), otherwise it swaps with its neighbour.
#
# x: Vector or list
# k: Index of the element to move
# direction: -1 for up, 1 for down

move_within = function(x, k, direction) {
  n = length(x)
  if (n <= 1) {
    return(x)
  }
  if (direction == -1 && k == 1) {
    return(x[c(2:n, 1)])
  }
  if (direction == 1 && k == n) {
    return(x[c(n, 1:(n - 1))])
  }
  j = k + direction
  x[c(k, j)] = x[c(j, k)]
  x
}

# Move an item or a whole group one step up or down in the display order,
# keeping groups contiguous. An item inside a group moves within its group
# (wrapping within it); an ungrouped item moves among the top-level blocks,
# jumping over a whole group rather than entering it; a group id moves its
# whole block among the top-level blocks. Membership never changes here: that
# is move_item_to_group()'s job. Returns the ids unchanged when there is
# nothing to move.
#
# ids: Character vector of item ids in display order
# group_of: Character vector of the items' group ids (NA = ungrouped)
# id: An item id (in ids) or a group id
# direction: -1 for up, 1 for down
# Returns: Character vector of item ids in the new display order

move_in_blocks = function(ids, group_of, id, direction) {
  ids = as.character(ids)
  group_of = as.character(group_of)
  blocks = rubric_blocks(ids, group_of)

  block_index = function(test) {
    which(vapply(blocks, test, logical(1)))
  }

  if (id %in% ids) {
    g = group_of[match(id, ids)]
    if (!is.na(g)) {
      b = block_index(function(b) b$type == "group" && identical(b$group, g))
      members = blocks[[b]]$ids
      blocks[[b]]$ids = move_within(members, match(id, members), direction)
      return(blocks_to_ids(blocks))
    }
    b = block_index(function(b) b$type == "item" && identical(b$id, id))
  } else {
    b = block_index(function(b) b$type == "group" && identical(b$group, id))
    if (length(b) == 0) {
      return(ids)
    }
  }

  blocks_to_ids(move_within(blocks, b, direction))
}

# Move an item into a group, or out of its group, appending it to the end of
# the target run so the order stays contiguous: into a group it lands after
# the group's last member (or at the end of the list when the group has no
# members yet); out of a group it lands right after the group it left (or
# stays where it was if that group has no other members).
#
# ids: Character vector of item ids in display order
# group_of: Character vector of the items' group ids (NA = ungrouped)
# id: The item id to move
# target: Group id to move it into, or NA to make it ungrouped
# Returns: List with ids (new display order) and group_of (named by ids)

move_item_to_group = function(ids, group_of, id, target) {
  ids = as.character(ids)
  group_of = as.character(group_of)
  k = match(id, ids)
  old = group_of[k]
  target = as.character(target)

  unchanged = (is.na(old) && is.na(target)) || (!is.na(old) && !is.na(target) && old == target)
  if (unchanged) {
    return(list(ids = ids, group_of = stats::setNames(group_of, ids)))
  }

  rest_ids = ids[-k]
  rest_groups = group_of[-k]
  anchor = if (!is.na(target)) target else old
  members = which(!is.na(rest_groups) & rest_groups == anchor)
  after = if (length(members) > 0) {
    max(members)
  } else if (!is.na(target)) {
    length(rest_ids)
  } else {
    k - 1
  }

  new_ids = append(rest_ids, id, after = after)
  new_groups = append(rest_groups, target, after = after)
  list(ids = new_ids, group_of = stats::setNames(new_groups, new_ids))
}
