#!/usr/bin/env bash

# This file is part of SQbricks.
#
# Copyright (C) 2022-2026
# CEA (Commissariat a l'energie atomique et aux energies alternatives)
# Universite Paris-Saclay
#
# you can redistribute it and/or modify it under the terms of the GNU
# Lesser General Public License as published by the Free Software
# Foundation, version 2.1.
#
# It is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# See the GNU Lesser General Public License version 2.1
# for more details (enclosed in the file licenses/LGPLv2.1).

set -u

debug="${SQBRICKS_LONG_DEBUG:-false}"
version="${1:-}"
timeout_seconds="${SQBRICKS_LONG_TIMEOUT:-600}"
timeout_seconds="${timeout_seconds%s}"
memory_kb="${SQBRICKS_LONG_MEMORY_KB:-6291456}"
progress_mode="${SQBRICKS_LONG_PROGRESS:-auto}"

if [[ -z "$version" ]]; then
	echo "Usage: $0 <sanity-unit|sanity-hybrid|sanity-partial|unit-vs-hybrid|veriqc|qiskit-hybrid|owm|tele|owm-vs-tele|owm-vs-qiskit>" >&2
	exit 1
fi

case "$version" in
sanity-unit | sanity-hybrid | sanity-partial | unit-vs-hybrid | veriqc | qiskit-hybrid | owm | tele | owm-vs-tele | owm-vs-qiskit) ;;
*)
	echo "Unknown SQbricks benchmark version: $version" >&2
	exit 1
	;;
esac

export DUNE_BUILD_DIR="${DUNE_BUILD_DIR:-$(pwd)/_build/sqbricks-long/$version}"

tmp_dir="_tmp/sqbricks-long/$version"
path_file="${SQBRICKS_LONG_PATH_FILE:-scripts/paths/paths_${version}.txt}"

# Current input row number, used only for progress reporting.
case_index=0

# Whether the progress bar is printed for this run.
progress_enabled="false"

# Number of completed cases displayed by the progress bar.
progress_current=0

# Total number of non-empty path rows selected for this family.
progress_total=0

# True after a carriage-return progress line has been printed.
progress_line_open="false"

# Last wrapped command stdout, captured so callers can parse it.
cmd_stdout=""

# Last wrapped command stderr, captured so callers can classify failures.
cmd_stderr=""

# Last wrapped command exit status.
cmd_status=0

# Ordered-size series of the current input case. Empty means isolated.
series_key=""

# Series/mode pairs already stopped after a timeout or memory limit.
declare -A stopped_series_modes

if [[ ! -f "$path_file" ]]; then
	echo "Missing path file: $path_file" >&2
	exit 1
fi

case "$progress_mode" in
auto | always | never) ;;
*)
	echo "SQBRICKS_LONG_PROGRESS must be auto, always, or never." >&2
	exit 1
	;;
esac

case "$version" in
qiskit-hybrid | owm-vs-qiskit)
	if ! python3 -c 'from qiskit import qasm2; from qiskit.circuit import QuantumCircuit; from qiskit.transpiler.preset_passmanagers import generate_preset_pass_manager' >/dev/null 2>&1; then
		echo "Missing Python dependency: qiskit is required for $version." >&2
		exit 1
	fi
	;;
esac

mkdir -p "$tmp_dir"

# Apply the same limits to every command launched by this script.
ulimit -v "$memory_kb" || exit 1
ulimit -t "$timeout_seconds" || exit 1

# Remove surrounding whitespace and CR characters from command outputs.
trim() {
	local value="$1"
	value="${value//$'\r'/}"
	printf "%s" "$value" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//'
}

# Convert a decimal point to a decimal comma for the historical CSV format.
csv_time() {
	printf "%s" "$1" | sed 's/\./,/'
}

# Map a failed command status and stderr to the compact CSV status.
status_from_failure() {
	local status="$1"
	local stderr="$2"

	if [[ "$status" -eq 124 || "$status" -eq 152 ]]; then
		printf "TO"
	elif [[ "$status" -eq 137 || "$stderr" == *"allocation failure during minor GC"* || "$stderr" == *"Out_of_memory"* ]]; then
		printf "OutOfMemory"
	else
		printf "Err%s" "$status"
	fi
}

is_resource_failure() {
	[[ "$1" == "TO" || "$1" == "OutOfMemory" ]]
}

# Return true when this mode was stopped for the current ordered series.
series_mode_stopped() {
	local mode="$1"
	local key

	if [[ -z "$series_key" ]]; then
		return 1
	fi
	key="$series_key|$mode"
	[[ -n "${stopped_series_modes[$key]:-}" ]]
}

# Stop only one verification mode for the current ordered series.
stop_series_mode() {
	local mode="$1"
	local key

	if [[ -z "$series_key" ]]; then
		return 0
	fi
	key="$series_key|$mode"
	stopped_series_modes["$key"]="true"
}

# A conversion failure is independent of the equivalence algorithm.
stop_all_series_modes() {
	stop_series_mode "Sequence"
	stop_series_mode "Parallel"
}

# Return true only when no verification mode remains runnable for this case.
all_series_modes_stopped() {
	if [[ "$version" == "owm-vs-tele" ]]; then
		series_mode_stopped "Parallel"
	else
		series_mode_stopped "Sequence" && series_mode_stopped "Parallel"
	fi
}

# Return the ordered-size series for cases where a larger case is expected to be
# harder than a smaller one. Empty means that the case is treated as isolated.
case_series_key() {
	local name="$1"
	local path_row="$2"
	local base="${name%%;*}"
	local source_path="${path_row%%;*}"
	local source_dir
	local family=""

	source_dir="$(dirname -- "$source_path")"

	if [[ "$base" =~ ^DQC_PE_[0-9]+($|_) || "$base" =~ ^dqc_pe_[0-9]+($|_) ]]; then
		family="dqc_pe"
	elif [[ "$base" =~ ^DQC_qft_[0-9]+($|_) || "$base" =~ ^dqc_qft_[0-9]+($|_) ]]; then
		family="dqc_qft"
	elif [[ "$base" =~ ^pe_[0-9]+($|_) ]]; then
		family="pe"
	elif [[ "$base" =~ ^qft_[0-9]+($|_) ]]; then
		family="qft"
	elif [[ "$base" =~ ^adder_n[0-9]+($|_) ]]; then
		family="adder"
	elif [[ "$base" =~ ^bv_[0-9]+($|_) ]]; then
		family="bv"
	elif [[ "$base" =~ ^grover_[0-9]+($|_) ]]; then
		family="grover"
	elif [[ "$base" =~ ^gf2\^[0-9]+_?mult($|_) ]]; then
		family="gf2_mult"
	elif [[ "$base" =~ ^hwb[0-9]+($|_) ]]; then
		family="hwb"
	elif [[ "$base" =~ ^barenco_tof_[0-9]+($|_) ]]; then
		family="barenco_tof"
	elif [[ "$base" =~ ^tof_[0-9]+($|_) ]]; then
		family="tof"
	elif [[ "$base" =~ ^mod_adder_[0-9]+($|_) ]]; then
		family="mod_adder"
	fi

	if [[ -n "$family" ]]; then
		printf "%s|%s|%s" "$version" "$source_dir" "$family"
	fi
}

# Run one command and capture stdout, stderr, and exit status in cmd_stdout,
# cmd_stderr, and cmd_status.
run_command() {
	local stdout_file
	local stderr_file

	stdout_file="$(mktemp "$tmp_dir/stdout.XXXXXX")" || exit 1
	stderr_file="$(mktemp "$tmp_dir/stderr.XXXXXX")" || exit 1

	{ "$@"; } >"$stdout_file" 2>"$stderr_file"

	# Keep the command result in globals so callers can parse stdout or inspect
	# stderr without mixing diagnostics into the CSV output.
	cmd_status=$?
	cmd_stdout="$(cat "$stdout_file")"
	cmd_stderr="$(cat "$stderr_file")"

	# Temporary files are only needed to capture both streams cleanly.
	rm -f "$stdout_file" "$stderr_file"

	return "$cmd_status"
}

# Run SQbricks through dune with the common command wrapper.
run_sqbricks_command() {
	if [[ "$debug" == "true" ]]; then
		echo "dune exec -- ./bin/main.exe $*" >&2
	fi

	run_command dune exec -- ./bin/main.exe "$@"
}

# Run the Qiskit transformation used to generate optimized comparison circuits.
run_qiskit_transform() {
	local input="$1"
	local output="$2"

	if [[ "$debug" == "true" ]]; then
		echo "python3 scripts/qiskit-tr.py $input $output" >&2
	fi

	run_command python3 scripts/qiskit-tr.py "$input" "$output"
}

# Return the SQbricks gate-count CSV fragment, or empty gate fields on failure.
gate_count() {
	local path1="$1"
	local path2="$2"

	if run_sqbricks_command -nb_gates_csv "$path1" "$path2"; then
		trim "$cmd_stdout"
	else
		printf ";;;;;;"
	fi
}

# Convert a QASM file through SQbricks -sql with the requested mode.
run_sql() {
	local mode="$1"
	local input="$2"
	local output="$3"

	run_sqbricks_command -sql "$mode" "$input" "$output"
}

# Build an OWM or teleportation transformed circuit.
run_transform() {
	local kind="$1"
	local input="$2"
	local output="$3"

	run_sqbricks_command "-qasm_to_${kind}" "$input" "$output" "false"
}

# Build the final unitary OWM circuit without intermediate QASM files.
run_owm_ium_transform() {
	local input="$1"
	local output="$2"

	run_sqbricks_command -qasm_to_owm "$input" "$output" "true"
}

# Split a transformation result of the form "inputs,outputs".
split_transform_output() {
	local value="$1"
	local __inputs="$2"
	local __outputs="$3"
	local extracted_inputs
	local extracted_outputs

	extracted_inputs="${value%%,*}"
	extracted_outputs="${value##*,}"
	printf -v "$__inputs" "%s" "$(trim "$extracted_inputs")"
	printf -v "$__outputs" "%s" "$(trim "$extracted_outputs")"
}

# Emit one CSV row when an intermediate conversion step failed.
emit_conversion_error() {
	local name="$1"
	local lift="$2"
	local result="ErrConv"
	local failure_status

	failure_status="$(status_from_failure "$cmd_status" "$cmd_stderr")"
	if is_resource_failure "$failure_status"; then
		result="$failure_status"
		stop_all_series_modes
	fi

	echo "$name;SQbricks;2025;$lift;Conversion;;;;;;;;$result"
}

# Emit one CSV row for a case skipped because a smaller case in the same ordered
# series already hit the timeout or memory limit.
emit_skipped_case() {
	local name="$1"

	echo "$name;SQbricks;2025;skipped;Skip;;;;;;;;SKIP_AFTER_RESOURCE_FAILURE"
}

# Run SQbricks equivalence checks and emit Sequence/Parallel CSV rows.
run_equiv_sqbricks() {
	local nb_gate="$1"
	local name="$2"
	local path1="$3"
	local path2="$4"
	local lift="$5"
	local inputs1="${6:-}"
	local inputs2="${7:-}"
	local outputs1="${8:-}"
	local outputs2="${9:-}"
	local meas1="${10:-}"
	local meas2="${11:-}"

	run_equiv_mode() {
		local algo="$1"
		local label="$2"
		local result

		if series_mode_stopped "$label"; then
			result="SKIP_AFTER_RESOURCE_FAILURE"
		elif run_sqbricks_command -sqv "$algo" s \
			"$path1" "$path2" \
			"$inputs1" "$inputs2" "$outputs1" "$outputs2" \
			"$meas1" "$meas2"; then
			result="$(csv_time "$(trim "$cmd_stdout")")"
		else
			result="$(status_from_failure "$cmd_status" "$cmd_stderr")"
			if is_resource_failure "$result"; then
				stop_series_mode "$label"
			fi
		fi

		echo "$name;SQbricks;2025;$lift;$label;$nb_gate;$result"
	}

	if [[ "$version" != "owm-vs-tele" ]]; then
		run_equiv_mode seq "Sequence"
	fi
	run_equiv_mode par "Parallel"
}

# Check two plain QASM circuits without lifting.
test_unit() {
	local path1="$1"
	local path2="$2"
	local name="$3"
	local nb_gate

	nb_gate="$(gate_count "$path1" "$path2")"
	run_equiv_sqbricks "$nb_gate" "$name" "$path1" "$path2" "standalone"
}

# Convert both circuits to unitary form, then compare the converted files.
test_lifted_pair() {
	local path1="$1"
	local path2="$2"
	local path1_unitary="$3"
	local path2_unitary="$4"
	local name="$5"
	local nb_gate

	if ! run_sql u "$path1" "$path1_unitary"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	if ! run_sql u "$path2" "$path2_unitary"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	nb_gate="$(gate_count "$path1_unitary" "$path2_unitary")"
	run_equiv_sqbricks "$nb_gate" "$name" "$path1_unitary" "$path2_unitary" "lifting"
}

# Generate a Qiskit-optimized circuit, then compare original and optimized
# circuits after SQbricks lifting.
test_qiskit_hybrid() {
	local path_original="$1"
	local path_optimized="$2"
	local path_original_unitary="$3"
	local path_optimized_unitary="$4"
	local name="$5"

	if ! run_qiskit_transform "$path_original" "$path_optimized"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	test_lifted_pair "$path_original" "$path_optimized" \
		"$path_original_unitary" "$path_optimized_unitary" "$name"
}

# Compare an OWM or teleportation transformation against the original unitary
# circuit, using the partial-equivalence metadata returned by the transform.
test_transformed_against_unitary() {
	local kind="$1"
	local path_original="$2"
	local path_original_unitary="$3"
	local path_transformed="$4"
	local path_transformed_ium="$5"
	local name="$6"
	local transform_output
	local inputs
	local outputs
	local meas1
	local nb_gate

	# First lift the original circuit to a unitary form.
	if ! run_sql u "$path_original" "$path_original_unitary"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	# Build the transformed circuit from that unitary form. SQbricks prints the
	# input/output qubit lists needed later for partial equivalence.
	if ! run_transform "$kind" "$path_original_unitary" "$path_transformed"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	transform_output="$(trim "$cmd_stdout")"

	# Lift the transformed circuit too; SQbricks prints the measurement list.
	if ! run_sql u "$path_transformed" "$path_transformed_ium"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	meas1="$(trim "$cmd_stdout")"

	# The equivalence check compares transformed IUM vs original unitary, using
	# the transform metadata for the first circuit and empty metadata for the
	# original circuit.
	split_transform_output "$transform_output" inputs outputs
	nb_gate="$(gate_count "$path_transformed_ium" "$path_original_unitary")"
	run_equiv_sqbricks "$nb_gate" "$name" \
		"$path_transformed_ium" "$path_original_unitary" "lifting" \
		"$inputs" "[]" "$outputs" "[]" "$meas1" "[]"
}

# Generate both OWM and teleportation circuits from the same original circuit,
# then compare their lifted IUM forms.
test_owm_vs_tele() {
	local path_original="$1"
	local path_original_unitary="$2"
	local path_owm="$3"
	local path_owm_ium="$4"
	local path_tele="$5"
	local path_tele_ium="$6"
	local name="$7"
	local output_owm
	local output_tele
	local inputs1
	local inputs2
	local outputs1
	local outputs2
	local meas1
	local meas2
	local nb_gate

	if ! run_sql u "$path_original" "$path_original_unitary"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	if ! run_transform owm "$path_original_unitary" "$path_owm"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	output_owm="$(trim "$cmd_stdout")"
	if ! run_transform tele "$path_original_unitary" "$path_tele"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	output_tele="$(trim "$cmd_stdout")"
	if ! run_sql u "$path_owm" "$path_owm_ium"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	meas1="$(trim "$cmd_stdout")"
	if ! run_sql u "$path_tele" "$path_tele_ium"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	meas2="$(trim "$cmd_stdout")"
	split_transform_output "$output_owm" inputs1 outputs1
	split_transform_output "$output_tele" inputs2 outputs2
	nb_gate="$(gate_count "$path_owm_ium" "$path_tele_ium")"
	run_equiv_sqbricks "$nb_gate" "$name" \
		"$path_owm_ium" "$path_tele_ium" "lifting" \
		"$inputs1" "$inputs2" "$outputs1" "$outputs2" "$meas1" "$meas2"
}

# Generate a Qiskit-optimized circuit and an OWM circuit from the original,
# then compare their lifted IUM forms.
test_owm_vs_qiskit() {
	local path_original="$1"
	local path_optimized="$2"
	local path_optimized_ium="$3"
	local path_owm_ium="$4"
	local name="$5"
	local output_owm
	local remaining_metadata
	local inputs1
	local outputs1
	local meas1
	local nb_gate

	if ! run_qiskit_transform "$path_original" "$path_optimized"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	if ! run_owm_ium_transform "$path_original" "$path_owm_ium"; then
		emit_conversion_error "$name" "lifting"
		return
	fi
	output_owm="$(trim "$cmd_stdout")"
	if ! run_sql u "$path_optimized" "$path_optimized_ium"; then
		emit_conversion_error "$name" "lifting"
		return
	fi

	# The direct OWM command returns "inputs,outputs,measurements".
	inputs1="$(trim "${output_owm%%,*}")"
	remaining_metadata="${output_owm#*,}"
	outputs1="$(trim "${remaining_metadata%%,*}")"
	meas1="$(trim "${remaining_metadata#*,}")"
	nb_gate="$(gate_count "$path_owm_ium" "$path_optimized_ium")"
	run_equiv_sqbricks "$nb_gate" "$name" \
		"$path_owm_ium" "$path_optimized_ium" "lifting" \
		"$inputs1" "" "$outputs1" "" "$meas1" ""
}

# Derive all temporary file paths needed by the current benchmark family.
prepare_paths() {
	local path_original="$1"

	filename="$(basename -- "$path_original")"
	filename_no_ext="${filename%.qasm}"

	case "$version" in
	sanity-unit | sanity-hybrid | sanity-partial | unit-vs-hybrid | veriqc)
		IFS=';' read -r path1 path2 _ <<<"$path_original"
		filename1="$(basename -- "$path1")"
		filename2="$(basename -- "$path2")"
		filename1_no_ext="${filename1%.qasm}"
		filename2_no_ext="${filename2%.qasm}"
		path1_unitary="$tmp_dir/${filename1_no_ext}_unitary.qasm"
		path2_unitary="$tmp_dir/${filename2_no_ext}_unitary.qasm"
		;;
	qiskit-hybrid)
		path_optimized="$tmp_dir/${filename_no_ext}_optimize.qasm"
		path_original_unitary="$tmp_dir/${filename_no_ext}_unitary.qasm"
		path_optimized_unitary="$tmp_dir/${filename_no_ext}_optimize_unitary.qasm"
		;;
	owm | tele)
		path_original_unitary="$tmp_dir/${filename_no_ext}_${version}_original_unitary.qasm"
		path_by_meas="$tmp_dir/${filename_no_ext}_${version}_by_meas.qasm"
		path_by_meas_ium="$tmp_dir/${filename_no_ext}_${version}_by_meas_ium.qasm"
		;;
	owm-vs-tele)
		path_original_unitary="$tmp_dir/${filename_no_ext}_original_unitary.qasm"
		path_owm="$tmp_dir/${filename_no_ext}_${version}_owm.qasm"
		path_owm_ium="$tmp_dir/${filename_no_ext}_${version}_owm_ium.qasm"
		path_tele="$tmp_dir/${filename_no_ext}_${version}_tele.qasm"
		path_tele_ium="$tmp_dir/${filename_no_ext}_${version}_tele_ium.qasm"
		;;
	owm-vs-qiskit)
		path_optimized="$tmp_dir/${filename_no_ext}_optimize.qasm"
		path_optimized_ium="$tmp_dir/${filename_no_ext}_optimize_ium.qasm"
		path_owm_ium="$tmp_dir/${filename_no_ext}_${version}_owm_ium.qasm"
		;;
	esac
}

# Draw or refresh the single-line progress bar.
render_progress() {
	local label="$1"
	local bar_width=30
	local percent
	local filled
	local pending_width
	local complete
	local pending
	local bar
	local cols
	local line
	local max_width

	if [[ "$progress_enabled" != "true" || "$progress_total" -eq 0 ]]; then
		return
	fi

	percent=$((progress_current * 100 / progress_total))
	filled=$((progress_current * bar_width / progress_total))
	pending_width=$((bar_width - filled))
	printf -v complete "%*s" "$filled" ""
	printf -v pending "%*s" "$pending_width" ""
	bar="${complete// /#}${pending// /-}"

	cols="${COLUMNS:-$(tput cols 2>/dev/null || printf 80)}"
	if ! [[ "$cols" =~ ^[0-9]+$ ]] || (( cols < 20 )); then
		cols=80
	fi

	printf -v line 'SQbricks long %-14s [%s] %3d%% %d/%d - %.48s' \
		"$version" "$bar" "$percent" "$progress_current" "$progress_total" "$label"

	max_width=$((cols - 1))
	line="${line:0:max_width}"

	printf '\r\033[2K%s' "$line" >&2
	progress_line_open="true"
}

# Show the current case before it starts running.
begin_progress_case() {
	local name="$1"

	progress_current=$((case_index - 1))
	render_progress "$name"
}

# Mark the current case as completed in the progress bar.
finish_progress_case() {
	local name="$1"

	progress_current="$case_index"
	render_progress "$name"
}

# Close the progress line before printing normal messages.
finish_progress() {
	if [[ "$progress_line_open" == "true" ]]; then
		printf "\n" >&2
		progress_line_open="false"
	fi
}

# Enable or disable progress according to SQBRICKS_LONG_PROGRESS.
configure_progress() {
	progress_total="$total_cases"
	case "$progress_mode" in
	always) progress_enabled="true" ;;
	auto)
		if [[ -t 2 ]]; then
			progress_enabled="true"
		fi
		;;
	never) progress_enabled="false" ;;
	esac

	render_progress "starting"
}

# Print the CSV header matching the current family shape.
case "$version" in
qiskit-hybrid | owm | tele | owm-vs-tele | owm-vs-qiskit)
	echo "Program;Tool;Version;Lift;Opt;CH;CS;CZ;CCZ;CCX;CU1;Gates;Time"
	;;
*)
	echo "Program1;Program2;Tool;Version;Lift;Opt;CH;CS;CZ;CCZ;CCX;CU1;Gates;Time"
	;;
esac

# Load the selected path manifest, ignoring blank separator lines.
mapfile -t path_originals < <(grep -v '^[[:space:]]*$' "$path_file")
total_cases="${#path_originals[@]}"
configure_progress

# Main loop: prepare paths, run the family-specific benchmark, and separate
# each input case by an empty CSV line, as the historical script does.
for path_original in "${path_originals[@]}"; do
	case_index=$((case_index + 1))
	prepare_paths "$path_original"

	case "$version" in
	sanity-unit)
		name="$filename1_no_ext;$filename2_no_ext"
		;;
	sanity-hybrid | sanity-partial | unit-vs-hybrid | veriqc)
		name="$filename1_no_ext;$filename2_no_ext"
		;;
	qiskit-hybrid)
		name="$filename_no_ext"
		;;
	owm | tele)
		name="$filename_no_ext"
		;;
	owm-vs-tele)
		name="$filename_no_ext"
		;;
	owm-vs-qiskit)
		name="$filename_no_ext"
		;;
	esac

	# Keep the original Feynman filename on disk, but disambiguate its CSV key
	# from the VeriQbench circuit that is also named grover_5.
	if [[ "$path_original" == "benchmarks/Feynman/grover_5.qasm" ]]; then
		name="grover_5_feynman"
	fi

	series_key="$(case_series_key "$name" "$path_original")"
	begin_progress_case "$name"
	if all_series_modes_stopped; then
		emit_skipped_case "$name"
	else
		case "$version" in
		sanity-unit)
			test_unit "$path1" "$path2" "$name"
			;;
		sanity-hybrid | sanity-partial | unit-vs-hybrid | veriqc)
			test_lifted_pair "$path1" "$path2" "$path1_unitary" "$path2_unitary" "$name"
			;;
		qiskit-hybrid)
			test_qiskit_hybrid "$path_original" "$path_optimized" \
				"$path_original_unitary" "$path_optimized_unitary" "$name"
			;;
		owm | tele)
			test_transformed_against_unitary "$version" "$path_original" \
				"$path_original_unitary" "$path_by_meas" "$path_by_meas_ium" "$name"
			;;
		owm-vs-tele)
			test_owm_vs_tele "$path_original" "$path_original_unitary" \
				"$path_owm" "$path_owm_ium" "$path_tele" "$path_tele_ium" "$name"
			;;
		owm-vs-qiskit)
			test_owm_vs_qiskit "$path_original" "$path_optimized" \
				"$path_optimized_ium" "$path_owm_ium" "$name"
			;;
		esac
	fi
	finish_progress_case "$name"
	echo ""
done

finish_progress
