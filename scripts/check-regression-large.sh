#!/usr/bin/env bash

# Compare one selected large-regression CSV with its local baseline.

set -u

baseline="${1:-}"
current="${2:-}"
label="${3:-large}"
perf_threshold="${SQBRICKS_LARGE_PERF_THRESHOLD:-1.25}"
min_slowdown_seconds="${SQBRICKS_LARGE_MIN_SLOWDOWN_SECONDS:-5}"

if [[ -z "$baseline" || -z "$current" ]]; then
	echo "Usage: $0 <baseline.csv> <current.csv> [label]" >&2
	exit 1
fi

if [[ ! -f "$baseline" ]]; then
	echo "Missing large regression baseline: $baseline" >&2
	exit 1
fi

if [[ ! -f "$current" ]]; then
	echo "Missing large regression result: $current" >&2
	exit 1
fi

awk -F';' \
	-v label="$label" \
	-v perf_threshold="$perf_threshold" \
	-v min_slowdown_seconds="$min_slowdown_seconds" '
function is_time(value) {
	return value ~ /^[0-9]+([,.][0-9]+)?$/
}

function seconds(value, copy) {
	copy = value
	gsub(",", ".", copy)
	return copy + 0
}

function status_rank(value) {
	if (is_time(value)) return 3
	if (value == "SubCircuitInconclusive" || value == "Entanglement1" ||
	    value == "Entanglement2" || value == "GlobalPhaseInconclusive" ||
	    value == "FullCircuitInconclusive" || value == "FullCircuitInconclusiveAmp" ||
	    value == "FullCircuitInconclusiveKet" || value ~ /^NotEquivDiff/) return 2
	if (value == "") return 0
	return 1
}

function row_key() {
	if (NF == 14) return $1 ";" $2 ";" $3 ";" $4 ";" $5 ";" $6
	return $1 ";" $2 ";" $3 ";" $4 ";" $5
}

function case_key() {
	if (NF == 14) return $1 ";" $2 ";" $3 ";" $4
	return $1 ";" $2 ";" $3
}

function row_opt() {
	if (NF == 14) return $6
	return $5
}

function is_case_level_row() {
	return row_opt() == "Skip" || row_opt() == "Conversion"
}

function remember_message(messages, count, text) {
	if (count <= 20) messages[count] = text
}

function compare_case_shape_change(ck, baseline, current, baseline_text, current_text, base_rank, current_rank) {
	base_rank = status_rank(baseline)
	current_rank = status_rank(current)
	if (base_rank > current_rank) {
		if (ck in case_regression_seen) return
		case_regression_seen[ck] = 1
		functional_failure_count++
		remember_message(functional_failure_messages, functional_failure_count,
			ck ": baseline " baseline_text " " baseline ", current " current_text " " current)
		return
	}
	if (base_rank < current_rank) {
		if (ck in case_improvement_seen) return
		case_improvement_seen[ck] = 1
		improvement_count++
		remember_message(improvement_messages, improvement_count,
			ck ": baseline " baseline_text " " baseline ", current " current_text " " current)
	}
}

function compare_status(key, baseline, current, base_rank, current_rank, base, actual, ratio, slowdown) {
	base_rank = status_rank(baseline)
	current_rank = status_rank(current)

	if (base_rank > current_rank) {
		functional_failure_count++
		remember_message(functional_failure_messages, functional_failure_count,
			key ": baseline " baseline ", current " current)
		return
	}

	if (base_rank < current_rank) {
		improvement_count++
		remember_message(improvement_messages, improvement_count,
			key ": baseline " baseline ", current " current)
		return
	}

	if (is_time(baseline) && is_time(current)) {
		timing_count++
		base = seconds(baseline)
		actual = seconds(current)
		if (base > 0) {
			ratio = actual / base
			slowdown = actual - base
			if (ratio > perf_threshold && slowdown > min_slowdown_seconds) {
				perf_failure_count++
				remember_message(perf_failure_messages, perf_failure_count,
					key ": " sprintf("%.6f", actual) "s vs " sprintf("%.6f", base) \
					"s baseline, +" sprintf("%.6f", slowdown) "s and ratio " \
					sprintf("%.6f", ratio))
			}
		}
	}
}

function read_baseline_row(key, ck, status, case_level) {
	key = row_key()
	ck = case_key()
	status = $NF
	sub(/\r$/, "", status)
	case_level = is_case_level_row()
	if (key in baseline_status) {
		duplicate_baseline_count++
		duplicate_baseline_messages[duplicate_baseline_count] = key
		return
	}
	baseline_status[key] = status
	baseline_seen[key] = 1
	baseline_key_case[key] = ck
	baseline_key_is_case_level[key] = case_level
	if (case_level) {
		baseline_case_level_seen[ck] = 1
		baseline_case_level_status[ck] = status
	} else {
		baseline_mode_seen[ck] = 1
		if (!(ck in baseline_mode_best_rank) || status_rank(status) > baseline_mode_best_rank[ck]) {
			baseline_mode_best_rank[ck] = status_rank(status)
			baseline_mode_best_status[ck] = status
		}
	}
	baseline_count++
}

function compare_current_row(key, ck, current, case_level, baseline) {
	key = row_key()
	ck = case_key()
	current = $NF
	sub(/\r$/, "", current)
	case_level = is_case_level_row()
	if (key in current_seen) {
		duplicate_current_count++
		duplicate_current_messages[duplicate_current_count] = key
		return
	}
	current_seen[key] = 1
	if (case_level) {
		current_case_level_seen[ck] = 1
	} else {
		current_mode_seen[ck] = 1
	}
	current_count++

	if (!(key in baseline_seen)) {
		if (!case_level && (ck in baseline_case_level_seen)) {
			compare_case_shape_change(ck, baseline_case_level_status[ck], current,
				"case row", "mode row")
			return
		}
		if (case_level && (ck in baseline_mode_seen)) {
			compare_case_shape_change(ck, baseline_mode_best_status[ck], current,
				"mode row", "case row")
			return
		}
		if (case_level && (ck in baseline_case_level_seen)) {
			baseline = baseline_case_level_status[ck]
			compare_status(ck, baseline, current)
			return
		}
		missing_baseline_count++
		remember_message(missing_baseline_messages, missing_baseline_count, key)
		return
	}

	compare_status(key, baseline_status[key], current)
}

function print_messages(title, messages, count, i) {
	if (count == 0) return
	print title ":" > "/dev/stderr"
	for (i = 1; i <= count && i <= 20; i++) {
		print "  - " messages[i] > "/dev/stderr"
	}
	if (count > 20) {
		print "  - ... " (count - 20) " more" > "/dev/stderr"
	}
}

BEGIN {
	format_error_count = 0
	duplicate_baseline_count = 0
	duplicate_current_count = 0
	missing_baseline_count = 0
	missing_current_count = 0
	functional_failure_count = 0
	perf_failure_count = 0
	improvement_count = 0
	current_count = 0
	timing_count = 0
}

{
	# mawk 1.3.4 may leave NF stale until a positional field is read.
	first_field = $1
}

NR == FNR {
	if ($0 == "" || $0 == "\r" || $0 ~ /^Program/) next
	if (NF != 13 && NF != 14) {
		format_error_count++
		format_messages[format_error_count] = FILENAME ":" FNR ": " NF " fields"
		next
	}
	read_baseline_row()
	next
}

FNR == 1 {
	next
}

$0 == "" || $0 == "\r" {
	next
}

{
	if (NF != 13 && NF != 14) {
		format_error_count++
		format_messages[format_error_count] = FILENAME ":" FNR ": " NF " fields"
		next
	}
	compare_current_row()
}

END {
	for (key in baseline_seen) {
		if (!(key in current_seen)) {
			ck = baseline_key_case[key]
			if (baseline_key_is_case_level[key] && (ck in current_mode_seen || ck in current_case_level_seen)) {
				continue
			}
			if (!baseline_key_is_case_level[key] && (ck in current_case_level_seen)) {
				continue
			}
			missing_current_count++
			remember_message(missing_current_messages, missing_current_count, key)
		}
	}

	total_failures = format_error_count + duplicate_baseline_count + \
		duplicate_current_count + missing_baseline_count + \
		missing_current_count + functional_failure_count + perf_failure_count

	if (total_failures > 0) {
		print "Large regression check FAILED for " label ": " \
			missing_current_count " missing current row(s), " \
			missing_baseline_count " missing baseline row(s), " \
			functional_failure_count " functional regression(s), " \
			perf_failure_count " performance regression(s)." > "/dev/stderr"
		print_messages("Format errors", format_messages, format_error_count)
		print_messages("Duplicate baseline rows", duplicate_baseline_messages, duplicate_baseline_count)
		print_messages("Duplicate current rows", duplicate_current_messages, duplicate_current_count)
		print_messages("Missing current rows", missing_current_messages, missing_current_count)
		print_messages("Missing baseline rows", missing_baseline_messages, missing_baseline_count)
		print_messages("Functional regressions", functional_failure_messages, functional_failure_count)
		print_messages("Performance regressions", perf_failure_messages, perf_failure_count)
		exit 1
	}

	print "Large regression check OK for " label ": " current_count \
		" rows compared, " timing_count " timed row(s), " \
		improvement_count " functional improvement(s)."
	if (improvement_count > 0) {
		print_messages("Functional improvements", improvement_messages, improvement_count)
	}
}
' "$baseline" "$current"
