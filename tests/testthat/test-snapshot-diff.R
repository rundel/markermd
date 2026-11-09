# The snapshot diff over plain exchange lists: items and selections matched
# by id, groups matched by id, and the one scorer behind snapshots and the
# live state. Fixtures from helper-snapshots.R.

sd_payload = function(d) {
  markermd:::with_database(d, markermd:::snapshot_payload)
}

# Set an item's description throughout a plain rubric list
sd_describe = function(rubric, old, new) {
  rubric$questions = lapply(rubric$questions, function(q) {
    q$items = lapply(q$items, function(entry) {
      if (!is.null(entry$description) && entry$description == old) entry$description = new
      if (!is.null(entry$items)) {
        entry$items = lapply(entry$items, function(m) {
          if (m$description == old) m$description = new
          m
        })
      }
      entry
    })
    q
  })
  rubric
}

# Rewrite one pair's selections (descriptions and ids together) in a plain
# marks list
sd_select = function(marks, repo, question, items, ids) {
  marks$repos = lapply(marks$repos, function(r) {
    if (r$name != repo) return(r)
    r$questions = lapply(r$questions, function(q) {
      if (q$name != question) return(q)
      q$items = as.list(items)
      q$item_ids = as.list(ids)
      q
    })
    r
  })
  marks
}

sd_pair = function(marks, repo, question) {
  for (r in marks$repos) {
    if (r$name != repo) next
    for (q in r$questions) if (q$name == question) return(q)
  }
  NULL
}

test_that("a deleted item whose description another item takes over is still a lost selection", {
  d = sn_graded_project()
  pay = sd_payload(d)
  q1 = pay$rubric$questions[[1]]
  ids = vapply(q1$items, function(e) if (is.null(e$id)) NA_character_ else e$id, character(1))
  descs = vapply(q1$items, function(e) if (is.null(e$description)) NA_character_ else e$description, character(1))
  issue_a = ids[which(descs == "Issue A")]
  looks_good = ids[which(descs == "Looks good")]

  # team01 selects both; then Issue A is deleted and Looks good is reworded to "Issue A"
  from_marks = sd_select(pay$marks, "hw01-team01", "Q1", c("Issue A", "Looks good"), c(issue_a, looks_good))
  to_rubric = pay$rubric
  to_rubric$questions[[1]]$items = Filter(function(e) is.null(e$id) || e$id != issue_a, to_rubric$questions[[1]]$items)
  to_rubric = sd_describe(to_rubric, "Looks good", "Issue A")
  to_marks = sd_select(from_marks, "hw01-team01", "Q1", "Issue A", looks_good)

  rubric = markermd:::diff_rubric(pay$rubric, to_rubric, from_marks, to_marks)
  expect_true("item removed" %in% rubric$changes$change)
  expect_true("item reworded" %in% rubric$changes$change)

  marks = markermd:::diff_marks(from_marks, to_marks, rubric$renames, markermd:::rubric_item_labels(to_rubric, pay$rubric))
  sel = marks[marks$change == "selection" & marks$repo == "hw01-team01", ]
  expect_equal(nrow(sel), 1)
  expect_equal(sel$before, "Issue A")
  expect_equal(sel$after, "")
})

test_that("selections of items with identical descriptions are compared by id", {
  d = sn_graded_project()
  pay = sd_payload(d)
  q1 = pay$rubric$questions[[1]]
  ids = vapply(q1$items, function(e) if (is.null(e$id)) NA_character_ else e$id, character(1))
  descs = vapply(q1$items, function(e) if (is.null(e$description)) NA_character_ else e$description, character(1))
  first = ids[which(descs == "Issue A")]
  second = ids[which(descs == "Looks good")]

  rubric = sd_describe(pay$rubric, "Looks good", "Issue A")
  from_marks = sd_select(pay$marks, "hw01-team01", "Q1", "Issue A", first)
  to_marks = sd_select(pay$marks, "hw01-team01", "Q1", "Issue A", second)

  marks = markermd:::diff_marks(from_marks, to_marks, list(), markermd:::rubric_item_labels(rubric))
  sel = marks[marks$change == "selection" & marks$repo == "hw01-team01", ]
  expect_equal(nrow(sel), 1)
  expect_equal(sel$before, "Issue A")
  expect_equal(sel$after, "Issue A")
})

test_that("swapping two descriptions is two rewords and no selection change", {
  d = sn_graded_project()
  pay = sd_payload(d)
  to_rubric = sd_describe(sd_describe(sd_describe(pay$rubric, "Issue A", "TMP"), "Looks good", "Issue A"), "TMP", "Looks good")
  swap = function(x) {
    x[x == "Issue A"] = "TMP"
    x[x == "Looks good"] = "Issue A"
    x[x == "TMP"] = "Looks good"
    x
  }
  to_marks = pay$marks
  to_marks$repos = lapply(to_marks$repos, function(r) {
    r$questions = lapply(r$questions, function(q) {
      q$items = as.list(swap(as.character(unlist(q$items))))
      q
    })
    r
  })

  rubric = markermd:::diff_rubric(pay$rubric, to_rubric, pay$marks, to_marks)
  expect_equal(sum(rubric$changes$change == "item reworded"), 2)
  marks = markermd:::diff_marks(pay$marks, to_marks, rubric$renames, markermd:::rubric_item_labels(to_rubric, pay$rubric))
  expect_equal(nrow(marks[marks$change == "selection", ]), 0)
})

test_that("marks without ids fall back to descriptions mapped through renames", {
  d = sn_graded_project()
  pay = sd_payload(d)
  strip = function(marks) {
    marks$repos = lapply(marks$repos, function(r) {
      r$questions = lapply(r$questions, function(q) {
        q$item_ids = NULL
        q
      })
      r
    })
    marks
  }
  to_rubric = sd_describe(pay$rubric, "Issue A", "Issue A, restated")
  to_marks = strip(pay$marks)
  to_marks$repos = lapply(to_marks$repos, function(r) {
    r$questions = lapply(r$questions, function(q) {
      items = as.character(unlist(q$items))
      items[items == "Issue A"] = "Issue A, restated"
      q$items = as.list(items)
      q
    })
    r
  })
  rubric = markermd:::diff_rubric(pay$rubric, to_rubric, strip(pay$marks), to_marks)
  marks = markermd:::diff_marks(strip(pay$marks), to_marks, rubric$renames)
  expect_equal(nrow(marks[marks$change == "selection", ]), 0)
})

test_that("a relabeled group is one rename, not a removal, an addition and a move per member", {
  d = sn_graded_project()
  pay = sd_payload(d)
  to_rubric = pay$rubric
  to_rubric$questions[[1]]$items = lapply(to_rubric$questions[[1]]$items, function(entry) {
    if (!is.null(entry$group)) entry$group = "Severity"
    entry
  })
  changes = markermd:::diff_rubric(pay$rubric, to_rubric, pay$marks, pay$marks)$changes
  expect_equal(changes$change, "group renamed")
  expect_equal(changes$item, "Severity")
  expect_equal(changes$detail, "was: Tiers")

  # The History tab and the console show it
  suppressMessages(snapshot_create(d, label = "Before"))
  group_id = markermd:::with_database(d, function(conn) markermd:::load_all_item_groups(conn))$group_id[1]
  markermd:::with_database(d, function(conn) {
    DBI::dbExecute(conn, "UPDATE item_groups SET label = 'Severity' WHERE group_id = ?", params = list(group_id))
  })
  diff = snapshot_diff("Before", project = d)
  expect_equal(diff@rubric$change, "group renamed")
  expect_match(paste(cli::cli_fmt(print(diff)), collapse = "\n"), "group renamed")
})

test_that("a group's bounds change is reported under its new label when it is also renamed", {
  d = sn_graded_project()
  pay = sd_payload(d)
  to_rubric = pay$rubric
  to_rubric$questions[[1]]$items = lapply(to_rubric$questions[[1]]$items, function(entry) {
    if (!is.null(entry$group)) {
      entry$group = "Severity"
      entry$exclusive = FALSE
    }
    entry
  })
  changes = markermd:::diff_rubric(pay$rubric, to_rubric)$changes
  expect_setequal(changes$change, c("group renamed", "group bounds"))
  expect_equal(unique(changes$item), "Severity")
})

test_that("a snapshot's scores and the live scores come from one scorer", {
  d = sn_graded_project()
  proj = project_config(d)
  questions = c("Q1", "Q2")
  repos = c("hw01-team01", "hw01-team02")

  live = collect_score_data(d, questions, repos)
  snap = suppressMessages(snapshot_create(d, label = "s1"))
  cached = snapshot_scores(snap, project = d)
  current = snapshot_scores("current", project = d)
  key = function(df) paste(df$assignment_repo, df$question_name, sep = "\x1f")
  for (df in list(cached, current)) {
    pairs = df[df$question_name != "(total)", ]
    expect_equal(pairs$score[match(key(live), key(pairs))], live$score)
  }

  # The database frame and the snapshot frame are the same shape and agree
  frames_db = markermd:::with_database(d, markermd:::grading_frames_from_conn)
  payload = sd_payload(d)
  frames_snap = markermd:::grading_frames_from_parts(payload$rubric, payload$marks)
  expect_setequal(names(frames_db), names(frames_snap))
  expect_equal(
    markermd:::score_pairs_from_frames(frames_db, questions, repos),
    markermd:::score_pairs_from_frames(frames_snap, questions, repos)
  )
  pair_keys = function(pairs) sort(paste(pairs$question_name, pairs$assignment_repo, sep = "\x1f"))
  expect_equal(
    pair_keys(markermd:::graded_pairs_from_frames(frames_db, repos)),
    pair_keys(markermd:::graded_pairs_from_frames(frames_snap, repos))
  )

  # Rewording an item (same id) changes neither side
  suppressMessages(rubric_item_set("Q1", "Issue A", description = "Issue A, restated", project = d))
  after = collect_score_data(d, questions, repos)
  expect_equal(after$score, live$score)
  cached_after = snapshot_scores(snap, project = d)
  expect_equal(cached_after$score, cached$score)
  expect_equal(nrow(snapshot_diff("s1", project = d)@scores), 0)
})

test_that("graded pairs agree between the database and the frame helpers, optional questions included", {
  d = sn_graded_project()
  repos = c("hw01-team01", "hw01-team02")
  markermd:::with_database(d, function(conn) {
    frames = markermd:::grading_frames_from_conn(conn)
    expect_equal(
      markermd:::graded_question_pairs_conn(conn, repos),
      markermd:::graded_pairs_from_frames(frames, repos)
    )
    expect_equal(
      markermd:::graded_question_pairs_conn(conn),
      markermd:::graded_pairs_from_frames(frames)
    )
  })
  graded = graded_question_pairs(d, repos)
  expect_equal(nrow(graded), 3)
})
