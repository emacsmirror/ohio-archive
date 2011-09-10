;;; dcsh-mode.el --- major mode for editing Synopsys dcsh (dc_shell) scripts

;; Copyright (C) 1999 Reto Zimmermann, Synopsys Inc.

;; Author:      Reto Zimmermann  <reto@gnu.org>
;; Maintainer:  Reto Zimmermann  <reto@gnu.org>
;; Version:     1.1
;; Keywords:    languages dcsh dc_shell
;; WWW:         http://www.emacs.org/hdl/dcsh-mode.html

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides a simple Emacs major mode for editing scripts for
;; Synopsys' Design Compiler shell (dcsh, dc_shell).
;; It includes the following features:

;;   - Syntax highlighting
;;   - Indentation
;;   - Word/command completion
;;   - Block commenting
;;   - Works under GNU Emacs and XEmacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `dcsh-mode' or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation:

;; Put `dcsh-mode.el' into the `site-lisp' directory of your Emacs installation
;; or into an arbitrary directory that is added to the load path by the
;; following line in your Emacs start-up file (`.emacs'):

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `dcsh-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  dcsh-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile dcsh-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):

;;   (autoload 'dcsh-mode "dcsh-mode" "Dcsh Mode" t)
;;   (setq auto-mode-alist (cons '("\\.scr\\'" . dcsh-mode) auto-mode-alist))

;; If you do NOT want Dcsh Mode to be automatically loaded for files with
;; extension ".scr", delete the last line and invoke Dcsh Mode manually using
;; the command `M-x dcsh-mode'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup dcsh nil
  "Customizations for Dcsh Mode."
  :prefix "dcsh-"
  :group 'languages)

(defcustom dcsh-basic-offset 2
  "*Amount of basic offset used for indentation."
  :type 'integer
  :group 'dcsh)

(defcustom dcsh-underscore-is-part-of-word nil
  "*Non-nil means consider the underscore character `_' as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations.  All parts of an identifier separated by underscore
are treated as single words otherwise."
  :type 'boolean
  :group 'dcsh)


(defconst dcsh-version "1.1"
  "Dcsh Mode version number.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar dcsh-mode-map ()
  "Keymap for Dcsh Mode.")

(setq dcsh-mode-map (make-sparse-keymap))
;; backspace/delete key bindings
(define-key dcsh-mode-map [backspace] 'backward-delete-char-untabify)
(unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
  (define-key dcsh-mode-map [delete]       'delete-char)
  (define-key dcsh-mode-map [(meta delete)] 'kill-word))
;; mode specific key bindings
(define-key dcsh-mode-map "\M-\C-\\" 'dcsh-indent-region)
(define-key dcsh-mode-map "\C-c\C-c" 'dcsh-comment-uncomment-region)
(define-key dcsh-mode-map "\C-c\C-f" 'dcsh-fontify-buffer)
(define-key dcsh-mode-map "\C-c\C-h" 'dcsh-doc-mode)
(define-key dcsh-mode-map "\C-c\C-v" 'dcsh-version)
(define-key dcsh-mode-map "\M-\t"    'tab-to-tab-stop)
;; electric key bindings
(define-key dcsh-mode-map "\t"       'dcsh-electric-tab)
(define-key dcsh-mode-map "\r"       'dcsh-electric-return)
(define-key dcsh-mode-map "}"        'dcsh-electric-closing-brace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu

(defvar dcsh-mode-menu-list
  '("Dcsh"
     ["(Un)Comment Out Region"	dcsh-comment-uncomment-region (mark)]
     "--"
     ["Indent Line"		dcsh-indent-line t]
     ["Indent Region"		dcsh-indent-region (mark)]
     ["Indent Buffer"		dcsh-indent-buffer t]
     "--"
     ["Fontify Buffer"		dcsh-fontify-buffer t]
     "--"
     ["Documentation"		dcsh-doc-mode :keys "C-c C-h"]
     ["Version"			dcsh-version t]
     ["Bug Report..."		dcsh-submit-bug-report t]
     "--"
     ["Customize..."		dcsh-customize t]
     )
  "Dcsh Mode menu.")

(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar dcsh-mode-syntax-table nil
  "Syntax table used in `dcsh-mode' buffers.")

(setq dcsh-mode-syntax-table (make-syntax-table))
;; punctuation
(modify-syntax-entry ?\# "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\$ "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\% "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\& "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\' "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\* "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\- "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\+ "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\. "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\/ "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\: "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\; "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\< "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\= "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\> "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\\ "."    dcsh-mode-syntax-table)
(modify-syntax-entry ?\| "."    dcsh-mode-syntax-table)
;; string
(modify-syntax-entry ?\" "\""   dcsh-mode-syntax-table)
;; underscore
(when dcsh-underscore-is-part-of-word
  (modify-syntax-entry ?\_ "w"    dcsh-mode-syntax-table))
;; parentheses to match
(modify-syntax-entry ?\( "()"   dcsh-mode-syntax-table)
(modify-syntax-entry ?\) ")("   dcsh-mode-syntax-table)
(modify-syntax-entry ?\[ "(]"   dcsh-mode-syntax-table)
(modify-syntax-entry ?\] ")["   dcsh-mode-syntax-table)
(modify-syntax-entry ?\{ "(}"   dcsh-mode-syntax-table)
(modify-syntax-entry ?\} "){"   dcsh-mode-syntax-table)
;; comment
(modify-syntax-entry ?\/ ". 14" dcsh-mode-syntax-table)
(modify-syntax-entry ?\* ". 23" dcsh-mode-syntax-table)
;; newline and CR
(modify-syntax-entry ?\n "> b"    dcsh-mode-syntax-table)
(modify-syntax-entry ?\^M "> b"   dcsh-mode-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

;;;###autoload
(defun dcsh-mode ()
  "Major mode for editing Synopsys dcsh (dc_shell) scripts.

Usage:
------

- INDENTATION:  Typing `\\[dcsh-electric-tab]' at the beginning of a line indents the line.
  The amount of indentation is specified by variable `dcsh-basic-offset'.
  Indentation can be done for an entire region \(`\\[dcsh-indent-region]') or buffer (menu).

- WORD/COMMAND COMPLETION:  Typing `\\[dcsh-electric-tab]' after a (not completed) word looks
  for a dcsh command or a word in the buffer that starts alike, inserts it
  and adjusts case.  Re-typing `\\[dcsh-electric-tab]' toggles through alternative word
  completions.

  Typing `\\[dcsh-electric-tab]' after a non-word character inserts a tabulator stop (if
  not at the beginning of a line).  `\\[tab-to-tab-stop]' always inserts a tabulator stop.

- COMMENTS:  `\\[dcsh-comment-uncomment-region]' comments out a region if not commented out, and
  uncomments a region if already commented out.

- HIGHLIGHTING (fontification):  Dcsh commands and their options, predefined
  attributes, and variable declarations, as well as comments and strings are
  highlighted using different colors.


Maintenance:
------------

To submit a bug report, use the menu entry within Dcsh Mode.
Add a description of the problem and include a reproducible test case.

Feel free to send questions and enhancement requests to <reto@gnu.org>.

Official distribution is at <http://www.emacs.org/hdl/dcsh-mode.html>.


                                                  The Dcsh Mode Maintainer
                                               Reto Zimmermann <reto@gnu.org>

Key bindings:
-------------

\\{dcsh-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'dcsh-mode)
  (setq mode-name "Dcsh")
  ;; set maps and tables
  (use-local-map dcsh-mode-map)
  (set-syntax-table dcsh-mode-syntax-table)
  ;; set local variables
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) " */")
  (set (make-local-variable 'comment-padding) 0)
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "/*+ *")
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent)
  (set (make-local-variable 'end-comment-column) 79)
  (set (make-local-variable 'paragraph-start) "^$")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'dcsh-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; initialize font locking
  (require 'font-lock)
  (set (make-local-variable 'font-lock-defaults)
       '(dcsh-font-lock-keywords nil t ((?\_ . "w"))))
  (turn-on-font-lock)
  ;; add menu
  (easy-menu-add dcsh-mode-menu-list) ; for XEmacs
  (easy-menu-define dcsh-mode-menu dcsh-mode-map
		    "Menu keymap for Dcsh Mode." dcsh-mode-menu-list)
  (run-hooks 'menu-bar-update-hook)
  ;; miscellaneous
  (message "Dcsh Mode %s.  Type C-c C-h for documentation." dcsh-version)
  ;; run hooks
  (run-hooks 'dcsh-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dcsh definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords

;; adjust variable bindings size
(when (< max-specpdl-size 800)
  (setq max-specpdl-size 800))

(defconst dcsh-commands
  '(
    "add_module" "alias" "all_clocks" "all_cluster_cells" "all_clusters"
    "all_connected" "all_critical_cells" "all_critical_pins" "all_designs"
    "all_fanin" "all_fanout" "all_inputs" "all_nets" "all_outputs"
    "all_registers" "allocate_budgets" "analyze" "annotate"
    "balance_buffer" "balance_registers" "bc_check_design"
    "bc_dont_register_input_port" "bc_dont_ungroup" "bc_report_memories"
    "bc_set_margin" "bc_time_design" "bc_view" "break"
    "cd" "cell_of" "chain_operations" "change_link" "change_names"
    "characterize" "check_bindings" "check_bsd" "check_design"
    "check_implementations" "check_scan" "check_synlib" "check_test"
    "check_timing" "check_unmapped" "compare_design" "compare_fsm"
    "compare_lib" "compile" "connect_net" "context_check" "continue"
    "copy_attributes" "copy_design" "create_bsd" "create_bsd_patterns"
    "create_bus" "create_cache" "create_cell" "create_clock" "create_cluster"
    "create_data_handler" "create_design" "create_event" "create_model"
    "create_multibit" "create_net" "create_operating_conditions" "create_port"
    "create_schematic" "create_test_clock" "create_test_patterns"
    "create_testsim_model" "create_tp" "create_wire_load" "current_design"
    "current_file" "current_instance"
    "db_stats" "dbatt" "dcsim_test" "define_design_lib" "define_name_rules"
    "delete_test" "derive_clock_parameters" "derive_clocks"
    "derive_timing_constraints" "diff_lib" "disconnect_net"
    "distribute_capacitance" "dont_chain_operations" "drive_of"
    "echo" "eco_align_design" "eco_analyze_design" "eco_compile"
    "eco_correspond_design" "eco_current_design_pair" "eco_funcor"
    "eco_implement" "eco_merge_steer" "eco_netlist_diff" "eco_recycle"
    "eco_remap_registers" "eco_report" "eco_report_cell"
    "eco_reset_directives" "eco_set_script" "eco_show_fanin"
    "eco_write_script" "elaborate" "else" "encode_fsm" "encrypt_lib"
    "execute" "exit" "externalize_cell" "extract" "extract_clock_tree"
    "fault_simulate" "filter" "find" "fmgi_read" "foreach" "fv_control"
    "get_attribute" "get_design_lib_path" "get_design_parameter"
    "get_license" "get_timing_paths" "get_ultra_license" "get_unix_variable"
    "group" "group_clock_tree" "group_path" "group_variable"
    "help" "highlight_path" "history" "hold_check"
    "if" "ignore_array_loop_precedences" "ignore_array_precedences"
    "ignore_memory_loop_precedences" "ignore_memory_precedences" "include"
    "insert_bsd" "insert_clock_tree" "insert_dft" "insert_jtag" "insert_pads"
    "insert_scan" "insert_test" "isolate_timing_path"
    "lib2saif" "license_users" "link" "list" "list_dc" "list_designs"
    "list_duplicate_designs" "list_instances" "list_libs" "list_subshells"
    "load_of" "log_to_phys"
    "map_design" "max_capacitance" "max_fall_delay" "max_fanout"
    "max_rise_delay" "max_transition" "mem" "mem_dump_group" "min_fall_delay"
    "min_rise_delay" "minimize_fsm" "model"
    "optimize_bsd" "optimize_registers"
    "parent_cluster" "partition_dp" "pause" "pipeline_design" "pipeline_loop"
    "plot" "prepare_design_for_compile" "prepare_testsim_vectors" "preschedule"
    "preview_bsd" "preview_dft" "preview_scan" "print_constraint_env"
    "propagate_constraints" "pwd"
    "quit" "quit_subshells"
    "read" "read_bsd_init_protocol" "read_bsd_protocol" "read_clusters"
    "read_file" "read_init_protocol" "read_lib" "read_pin_map" "read_saif"
    "read_sdf" "read_test_protocol" "read_toggle"
    "reduce_fsm" "register_control"
    "remove_analysis_info" "remove_annotated_check" "remove_annotated_delay"
    "remove_attribute" "remove_bsd_instruction" "remove_bsd_port"
    "remove_bsd_signal" "remove_bsd_specification" "remove_bsr_cell_type"
    "remove_bus" "remove_cache" "remove_cell" "remove_clock"
    "remove_clock_gating_check" "remove_clock_latency"
    "remove_clock_transition" "remove_clock_uncertainty" "remove_clusters"
    "remove_collar_element" "remove_constraint" "remove_design"
    "remove_dft_configuration" "remove_dft_signal" "remove_driving_cell"
    "remove_highlighting" "remove_ideal_net" "remove_input_delay" "remove_lib"
    "remove_license" "remove_multibit" "remove_net" "remove_output_delay"
    "remove_pads" "remove_path_group" "remove_pin_map" "remove_port"
    "remove_port_configuration" "remove_propagated_clock"
    "remove_scan_register_type" "remove_scan_specification"
    "remove_scheduling_constraints" "remove_test_component"
    "remove_test_global_connect_type" "remove_unconnected_ports"
    "remove_variable" "remove_wire_load_min_block_size"
    "remove_wire_load_model" "remove_wire_load_selection_group"
    "rename_design" "reoptimize_design" "replace_fpga" "replace_synthetic"
    "report" "report_annotated_check" "report_annotated_delay" "report_area"
    "report_attribute" "report_bus" "report_cache" "report_cell" "report_clock"
    "report_clusters" "report_compile_options" "report_constraint"
    "report_delay_calculation" "report_design" "report_design_lib"
    "report_dynamic_synlib" "report_fpga" "report_fsm" "report_hierarchy"
    "report_iddq" "report_internal_loads" "report_lib" "report_multibit"
    "report_multicycles" "report_name_rules" "report_names" "report_net"
    "report_packages" "report_path_group" "report_port" "report_power"
    "report_qor" "report_reference" "report_resource_estimates"
    "report_resources" "report_routability" "report_schedule"
    "report_scheduling_constraints" "report_synlib" "report_templates"
    "report_test" "report_timing" "report_timing_requirements"
    "report_transitive_fanin" "report_transitive_fanout"
    "report_ultra_optimization" "report_wire_load" "report_xref"
    "res_info" "reset_compare_design_script" "reset_design"
    "reset_iddq_invalid_state" "reset_path" "restore_test" "rtl2saif"
    "rtl_analyzer" "rtldrc"
    "schedule"
    "set_annotated_check" "set_annotated_delay" "set_arrival" "set_attribute"
    "set_auto_ideal_nets" "set_autofix_clock" "set_autofix_configuration"
    "set_autofix_element" "set_balance_registers" "set_behavioral_async_reset"
    "set_behavioral_reset" "set_boundary_optimization" "set_bsd_compliance"
    "set_bsd_configuration" "set_bsd_control_register" "set_bsd_instruction"
    "set_bsd_intest" "set_bsd_path" "set_bsd_port" "set_bsd_register"
    "set_bsd_runbist" "set_bsd_signal" "set_bsr_cell_type" "set_case_analysis"
    "set_cell_degradation" "set_cell_internal_power" "set_clock"
    "set_clock_gating_check" "set_clock_gating_signals"
    "set_clock_gating_style" "set_clock_latency" "set_clock_skew"
    "set_clock_transition" "set_clock_uncertainty" "set_collar_element"
    "set_combinational_type" "set_common_resource" "set_compare_design_script"
    "set_compile_directives" "set_connection_class" "set_constraint_env"
    "set_cost_priority" "set_critical_range" "set_cycles"
    "set_decompose_for_retiming" "set_default_flip_flop_type"
    "set_design_license" "set_dft_configuration" "set_dft_signal"
    "set_disable_timing" "set_dont_touch" "set_dont_touch_network"
    "set_dont_use" "set_dpcm_libraries" "set_drive" "set_drive_resistance"
    "set_driving_cell" "set_eco_accept_endpoint" "set_eco_align"
    "set_eco_dont_reuse" "set_eco_dont_tap" "set_eco_dont_target"
    "set_eco_isomorphic" "set_eco_obsolete" "set_eco_probe" "set_eco_recycle"
    "set_eco_reuse" "set_eco_tap_order" "set_eco_target" "set_eco_target_order"
    "set_eco_unique" "set_equal" "set_exclusive_use" "set_fall_arrival"
    "set_fall_drive" "set_false_path" "set_fanout_load" "set_fix_hold"
    "set_fix_multiple_port_nets" "set_flatten" "set_flatten_effort"
    "set_flatten_minimize" "set_flatten_phase" "set_flip_flop_type"
    "set_fsm_encoding" "set_fsm_encoding_style" "set_fsm_minimize"
    "set_fsm_order" "set_fsm_preserve_state" "set_fsm_state_vector"
    "set_iddq_invalid_state" "set_ideal_net" "set_impl_priority"
    "set_implementation" "set_input_delay" "set_input_parasitics"
    "set_input_transition" "set_isolation_operations" "set_jtag_implementation"
    "set_jtag_instruction" "set_jtag_manufacturer_id" "set_jtag_part_number"
    "set_jtag_port" "set_jtag_port_mode" "set_jtag_port_routing_order"
    "set_jtag_port_type" "set_jtag_version_number" "set_layer" "set_load"
    "set_local_link_library" "set_logic_dc" "set_logic_one" "set_logic_zero"
    "set_map_only" "set_margin" "set_max_area" "set_max_capacitance"
    "set_max_cycles" "set_max_delay" "set_max_dynamic_power" "set_max_fanout"
    "set_max_leakage_power" "set_max_time_borrow" "set_max_transition"
    "set_memory_input_delay" "set_memory_output_delay" "set_min_capacitance"
    "set_min_cycles" "set_min_delay" "set_min_fanout" "set_min_fault_coverage"
    "set_min_library" "set_min_porosity" "set_minimize_tree_delay"
    "set_model_drive" "set_model_load" "set_model_map_effort" "set_model_scale"
    "set_multibit_options" "set_multicycle_path" "set_operand_isolation_style"
    "set_operating_conditions" "set_opposite" "set_optimize_registers"
    "set_output_delay" "set_pad_type" "set_pipeline_stages"
    "set_port_configuration" "set_port_fanout_number" "set_port_is_pad"
    "set_prefer" "set_processor_implementation_binding" "set_propagated_clock"
    "set_register_type" "set_resistance" "set_resource_allocation"
    "set_resource_implementation" "set_rise_arrival" "set_rise_drive"
    "set_scan" "set_scan_chain" "set_scan_configuration" "set_scan_element"
    "set_scan_link" "set_scan_path" "set_scan_register_type" "set_scan_segment"
    "set_scan_signal" "set_scan_style" "set_scan_transparent" "set_share_cse"
    "set_signal_type" "set_simple_compile_mode" "set_size_only"
    "set_state_for_retiming" "set_structure" "set_switching_activity"
    "set_test_assume" "set_test_component" "set_test_dont_fault"
    "set_test_global_connect_type" "set_test_hold" "set_test_initial"
    "set_test_isolate" "set_test_mask_fault" "set_test_methodology"
    "set_test_require" "set_test_routing_order" "set_test_signal"
    "set_test_unmask_fault" "set_testsim_input_delay"
    "set_testsim_output_strobe" "set_timing_ranges"
    "set_transform_for_retiming" "set_true_delay_case_analysis"
    "set_ultra_optimization" "set_unconnected" "set_ungroup"
    "set_unix_variable" "set_view_object" "set_wire_load"
    "set_wire_load_min_block_size" "set_wire_load_mode" "set_wire_load_model"
    "set_wire_load_selection_group" "set_wired_logic_disable"
    "setup_check" "sh" "simplify_constants" "syntax_check"
    "tcl" "test_keep_fault_data" "test_test_compiler" "time" "trace_nets"
    "transform_csa" "translate"
    "unalias" "ungroup" "uniquify" "unschedule" "untrace_nets"
    "update_clusters" "update_lib" "update_script" "update_timing"
    "valid_vhdl_name"
    "which" "while"
    "write" "write_bsd_protocol" "write_bsdl" "write_clusters"
    "write_compare_design_script" "write_constraints" "write_design_lib_paths"
    "write_designlist" "write_file" "write_lib" "write_parasitics"
    "write_power" "write_script" "write_sdc" "write_sdf" "write_test"
    "write_test_protocol" "write_testsim_lib" "write_timing"
    )
  "List of dcsh commands.")

(defconst dcsh-attributes
  '(
    "area"
    "ba_net_resistance" "boundary_optimization"
    "default_flip_flop_type" "default_flip_flop_type_exact"
    "default_latch_type" "default_values" "design_type" "disable_timing"
    "dont_touch" "dont_touch_network" "dont_use" "driven_by_logic_one"
    "driven_by_logic_zero" "driving_cell_dont_scale" "driving_cell_fall"
    "driving_cell_from_pin_fall" "driving_cell_from_pin_rise"
    "driving_cell_library_fall" "driving_cell_library_rise"
    "driving_cell_multiplier" "driving_cell_pin_fall"
    "driving_cell_pin_rise" "driving_cell_rise"
    "fall_delay" "fall_drive" "fanout_load" "fix_hold" "flatten"
    "flatten_effort" "flatten_minimize" "flatten_phase" "flip_flop_type"
    "flip_flop_type_exact"
    "is_black_box" "is_combinational" "is_hierarchical" "is_mapped"
    "is_sequential" "is_test_circuitry" "is_unmapped"
    "k_process_values" "k_temp_values" "k_volt_values"
    "latch_type" "latch_type_exact" "load" "local_link_library"
    "max_capacitance" "max_fanout" "max_time_borrow" "max_transition"
    "minus_uncertainty"
    "nom_process" "nom_temperature" "nom_voltage"
    "output_not_used"
    "pad_location" "part" "period" "pin_direction" "plus_uncertainty"
    "port_direction" "port_is_pad" "preferred" "propagated_clock"
    "ref_name" "rise_delay" "rise_drive"
    "structure" "subtract_pin_load"
    "ungroup"
    "wired_and" "wired_logic_disable" "wired_or"
    "xnf_init" "xnf_loc"
    )
  "List of dcsh predefined attributes.")

;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst dcsh-commands-regexp
  (concat "\\<\\(" (regexp-opt dcsh-commands) "\\)\\>")
  "Regexp for dcsh commands.")

(defconst dcsh-attributes-regexp
  (concat "\\<\\(" (regexp-opt dcsh-attributes) "\\)\\>")
  "Regexp for dcsh predefined attributes.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dcsh-font-lock-keywords
  (list
   ;; highlight commands
   (list dcsh-commands-regexp 1 'font-lock-keyword-face)
   ;; highlight command options
   '("\\W\\(-[a-zA-Z]\\w*\\)\\>" 1 'font-lock-function-name-face)
   ;; highlight predefined attributes
   (list dcsh-attributes-regexp 1 'font-lock-type-face)
   ;; highlight variable declaration names
   '("\\<\\(\\w+\\)\\s-*=[^=]" 1 'font-lock-variable-name-face)
   )
  "Regular expressions to highlight in Dcsh Mode.")

(defun dcsh-fontify-buffer ()
  "Fontify buffer."
  (interactive)
  (font-lock-fontify-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dcsh-offsets-alist
  '((comment      . dcsh-lineup-C-comments)
    (block-intro  . +)
    (block-close  . 0)
    (command      . 0)
    (command-cont . +))
  "Association list of syntactic element symbols and indentation offsets.
Adapted from `c-offsets-alist'.")

(defun dcsh-evaluate-offset (offset langelem symbol)
  "Offset can be a number, a function, a variable, a list, or one of
the symbols + or -."
  (cond
   ((eq offset '+)         (setq offset dcsh-basic-offset))
   ((eq offset '-)         (setq offset (- dcsh-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 dcsh-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- dcsh-basic-offset))))
   ((eq offset '*)         (setq offset (/ dcsh-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- dcsh-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (dcsh-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		0
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset))))
  offset)

(defun dcsh-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
dcsh-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol dcsh-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(setq offset 0
	      relpos 0)
      (setq offset (dcsh-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (save-excursion (beginning-of-line) (point))))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (dcsh-evaluate-offset offset langelem symbol))))

(defsubst dcsh-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line
  eol  -- end of line
  boi  -- back to indentation
  ionl -- indentation of next line
  iopl -- indentation of previous line
  bonl -- beginning of next line
  bopl -- beginning of previous line
This function does not modify point or mark."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl) (forward-line -1) (back-to-indentation))
     ((eq position 'ionl) (forward-line 1) (back-to-indentation))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun dcsh-in-comment-p ()
  "Determine if point is in a dcsh comment."
  (save-excursion
    (re-search-backward "\\(/\\*\\)\\|\\(\\*/\\)" nil t)
    (match-string 1)))

(defun dcsh-beginning-of-command ()
  "Go to beginning of current command."
  (beginning-of-line)
  (let (pos)
    (while (save-excursion (dcsh-backward-syntactic-ws)
			   (setq pos (point))
			   (= (preceding-char) ?\\))
      (goto-char pos)
      (beginning-of-line))))

(defun dcsh-forward-syntactic-ws (&optional lim)
  "Forward skip of syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)))))

(defun dcsh-backward-syntactic-ws (&optional lim)
  "Backward skip over syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (if (< lim (point))
	  (progn
	    (narrow-to-region lim (point))
	    (while (/= here (point))
	      (setq here (point))
	      (forward-comment hugenum)))))))

(defsubst dcsh-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of langelem's relpos.
Leaves point at the relpos unless preserve-point is non-nil."
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here)))))

(defun dcsh-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Nicked from `c-lineup-C-comments'."
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (dcsh-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (dcsh-point 'eol) t))
	  (progn
	    (if (not (looking-at "[*]+"))
		(progn
		  ;; we now have to figure out where this comment begins.
		  (goto-char here)
		  (back-to-indentation)
		  (if (looking-at "[*]+/")
		      (progn (goto-char (match-end 0))
			     (forward-comment -1))
		    (goto-char (cdr langelem))
		    (back-to-indentation))))
	    (- (current-column) langelem-col))
	(if (zerop stars)
	    (progn
	      (skip-chars-forward " \t")
	      (- (current-column) langelem-col))
	  ;; how many stars on comment opening line?  if greater than
	  ;; on current line, align left.  if less than or equal,
	  ;; align right.  this should also pick up Javadoc style
	  ;; comments.
	  (if (> (length (match-string 1)) stars)
	      (progn
		(back-to-indentation)
		(- (current-column) -1 langelem-col))
	    (- (current-column) stars langelem-col)))))))

(defmacro dcsh-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in symbol to the syntax list.
try to increase performance by using this macro."
  `(setq syntax (cons (cons ,symbol ,relpos) syntax)))

(defun dcsh-guess-basic-syntax ()
  "Determine syntactic context of current line of code."
  (save-excursion
    (let (syntax placeholder)
      (cond
       ;; comment
       ((dcsh-in-comment-p)
	(while (and (zerop (forward-line -1))
		    (looking-at "^\\s-*$")))
	(dcsh-add-syntax 'comment (dcsh-point 'boi)))
       ;; block closing brace
       ((save-excursion (beginning-of-line) (looking-at "\\s-*}"))
	(goto-char (match-end 0))
	(backward-sexp)
	(back-to-indentation)
	(dcsh-add-syntax 'block-close (point)))
       ;; block opening brace
       ((save-excursion (dcsh-backward-syntactic-ws)
			(setq placeholder (point))
			(= (preceding-char) ?{))
	(goto-char placeholder)
	(dcsh-add-syntax 'block-intro (dcsh-point 'boi)))
       ;; command continuation
       ((save-excursion (dcsh-backward-syntactic-ws)
			(= (preceding-char) ?\\))
	(dcsh-beginning-of-command)
	(dcsh-add-syntax 'command-cont (dcsh-point 'boi)))
       ;; new command
       (t
	(beginning-of-line)
	(dcsh-backward-syntactic-ws)
	(dcsh-beginning-of-command)
	(dcsh-add-syntax 'command (dcsh-point 'boi))))
      syntax)))

(defun dcsh-indent-line ()
  "Indent the current line as dcsh code. Optional SYNTAX is the
syntactic information for the current line. Returns the amount of
indentation change (in columns)."
  (interactive)
  (let* ((syntax (dcsh-guess-basic-syntax))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'dcsh-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (unless (zerop shift-amt)
      (beginning-of-line)
      (delete-region (point) (dcsh-point 'boi))
      (indent-to indent))
    (if (< (point) (dcsh-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    shift-amt))

(defun dcsh-indent-buffer ()
  "Indent whole buffer as dcsh code.
Calls `indent-region' for whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

(defun dcsh-indent-region (start end column)
  "Indent region as dcsh code."
  (interactive "r\nP")
  (indent-region start end column))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electrifications

(defun dcsh-electric-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'."
  (interactive "*P")
  (cond ((memq (char-syntax (preceding-char)) '(?w ?_))
	 (let ((case-fold-search t)
	       (case-replace nil))
	   (dcsh-expand-abbrev prefix-arg)))
	((> (current-column) (current-indentation))
	 (tab-to-tab-stop))
	((and (or (eq last-command 'dcsh-electric-tab)
		  (eq last-command 'dcsh-electric-return))
	      (/= 0 (current-indentation)))
	 (backward-delete-char-untabify dcsh-basic-offset nil))
	(t (dcsh-indent-line)))
  (setq this-command 'dcsh-electric-tab))

(defun dcsh-electric-return ()
  "Insert newline and indent."
  (interactive)
  (newline-and-indent))

(defun dcsh-electric-closing-brace ()
  "Outdent closing brace."
  (interactive)
  (insert "}")
  (dcsh-indent-line))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of dcsh commands)

(defvar dcsh-abbrev-list
  (append (list nil) dcsh-commands
	  (list nil) dcsh-attributes)
  "Predefined abbreviations for dcsh.")

(defvar dcsh-expand-upper-case nil)

(eval-when-compile (require 'hippie-exp))

(defun dcsh-try-expand-abbrev (old)
  "Try expanding abbreviations from `dcsh-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list dcsh-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
;		  (equal (car he-expand-list) he-search-string)))
    (unless (stringp (car he-expand-list))
      (setq dcsh-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if dcsh-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; function for expanding abbrevs and dabbrevs
(defun dcsh-expand-abbrev (arg))
(fset 'dcsh-expand-abbrev (make-hippie-expand-function
			   '(try-expand-dabbrev
			     try-expand-dabbrev-all-buffers
			     dcsh-try-expand-abbrev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun dcsh-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end -1)
    (comment-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun dcsh-customize ()
  "Call the customize function with `dcsh' as argument."
  (interactive)
  (customize-browse 'dcsh))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst dcsh-mode-help-address "Dcsh Mode Maintainer <reto@gnu.org>"
  "Address for Dcsh Mode bug reports.")

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun dcsh-submit-bug-report ()
  "Submit via mail a bug report on Dcsh Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on Dcsh Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    dcsh-mode-help-address
    (concat "Dcsh Mode " dcsh-version)
    (list
     ;; report all important variables
     'dcsh-basic-offset
     'dcsh-underscore-is-part-of-word
     )
    nil nil
    "Dear Dcsh Mode maintainer,")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dcsh-version ()
  "Echo the current version of Dcsh Mode in the minibuffer."
  (interactive)
  (message "Using Dcsh Mode version %s" dcsh-version))

(defun dcsh-doc-mode ()
  "Display Dcsh Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'dcsh-mode))
    (unless (string-match "XEmacs" emacs-version)
      (help-setup-xref (list #'dcsh-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dcsh-mode)

;;; dcsh-mode.el ends here
