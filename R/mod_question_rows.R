# Shared mechanics for the question module's dynamic row machines (the rules
# machine in mod_question_rules.R and the filters machine in
# mod_question_filters.R). Both keep a keyed list of rows whose keys are
# stable: a row's key is minted from a monotonic counter when the row is
# created and is never reused or renumbered within the session, so a row's
# input ids stay bound to that row for its whole life and the list order
# alone carries the row order. Structured inputs are captured back into state
# before any structural change; these helpers hold the pieces of that pattern
# that carry no per-machine policy, so the invariants live in one place.

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

# Seen-widget registry for asynchronously initializing selectize
# multiselects: an empty multiselect reports NULL, which is also the
# not-yet-initialized state, so a key is noted once its widget has reported
# a non-NULL value at least once. What a later NULL for a noted key means is
# the machine's call (the rules machine reads it as the deliberate clear
# "Any node", the filters machine keeps the stored kinds since an empty kind
# set would match nothing); for an un-noted key it means the widget has not
# reported yet, so the stored value must be kept. Keys are the stable row
# keys, so a deleted row's note can never be inherited by another row;
# forget() just keeps the registry tidy. Bookkeeping only, so reads/writes
# are isolated from reactivity.
#
# Returns list(note = function(key), has = function(key),
# forget = function(keys))

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
    forget = function(keys) {
      seen(setdiff(shiny::isolate(seen()), keys))
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
