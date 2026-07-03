# Shared mechanics for the question module's dynamic row machines (the rules
# machine in mod_question_rules.R and the filters machine in
# mod_question_filters.R). Both keep a keyed list of rows ("1".."n") whose
# structured inputs are captured back into state before any structural
# change; these helpers hold the pieces of that pattern that carry no
# per-machine policy, so the invariants live in one place.

# Re-key a list sequentially as "1".."n" (positional row ids)
#
# x: A named list to re-key

reindex_keys = function(x) {
  stats::setNames(unname(x), as.character(seq_along(x)))
}

# Structural render trigger: bump() on load/add/delete (and edits that swap a
# row's control) so the row UI re-renders on structural changes but not on
# every value edit. Re-rendering on a value edit would rebuild a row's
# node-type multiselect mid-interaction; its menu lives on <body>
# (dropdownParent = "body"), so the rebuild orphans the open menu and
# subsequent clicks are lost.
#
# Returns list(depend = function() establishing the reactive dependency,
# bump = function() firing the trigger)

make_render_trigger = function() {
  counter = shiny::reactiveVal(0L)
  list(
    depend = function() counter(),
    bump = function() counter(shiny::isolate(counter()) + 1L)
  )
}

# Seen-widget registry for asynchronously initialising selectize
# multiselects: an empty multiselect reports NULL, which is also the
# not-yet-initialised state, so a key is noted once its widget has reported
# a non-NULL value at least once. A later NULL for a noted key is a
# deliberate clear; for an un-noted key it means the widget has not reported
# yet, so the stored value must be kept. Keys are positional row ids;
# bookkeeping only, so reads/writes are isolated from reactivity.
#
# Whether stale keys after a delete's re-indexing are harmful depends on the
# machine's NULL-when-seen semantics, so the remap policy deliberately
# differs between the two machines:
# - The RULES machine treats a deliberate clear as the meaningful catch-all
#   "Any node", so a stale key would turn a not-yet-reported widget's NULL
#   into a spurious clear; it calls remap() on every delete.
# - The FILTERS machine treats an empty kind set as invalid (it would match
#   nothing), keeps the stored value, and snaps the widget back to it, so a
#   stale key cannot corrupt state; it does not remap.
#
# Returns list(note = function(key), has = function(key),
# remap = function(surviving_keys) mapping the k-th surviving old key to new
# key k and dropping the rest)

make_seen_registry = function() {
  seen = shiny::reactiveVal(character(0))
  list(
    note = function(key) {
      cur = shiny::isolate(seen())
      if (!(key %in% cur)) {
        seen(c(cur, key))
      }
    },
    has = function(key) {
      key %in% shiny::isolate(seen())
    },
    remap = function(surviving_keys) {
      cur = shiny::isolate(seen())
      seen(as.character(which(surviving_keys %in% cur)))
    }
  )
}

# Sync a keyed collection of row observers against the active row keys:
# destroy entries for keys that no longer exist and create entries for new
# ones. An entry may be a single observer or a list of observers (the
# filters machine stores an add/delete pair per group).
#
# observers: Named list of existing entries
# active_keys: Character vector of keys that should have entries
# create: Function(key) returning the entry for a new key
#
# Returns the updated named list

sync_keyed_observers = function(observers, active_keys, create) {
  for (stale_key in setdiff(names(observers), active_keys)) {
    entry = observers[[stale_key]]
    if (!is.list(entry)) entry = list(entry)
    for (observer in entry) observer$destroy()
    observers[[stale_key]] = NULL
  }
  for (key in active_keys) {
    if (!key %in% names(observers)) {
      observers[[key]] = create(key)
    }
  }
  observers
}
