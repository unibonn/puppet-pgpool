(*
Module: Pgpool
  Parses pgpool.conf

Author: Alex Schultz <aschultz@next-development.com>

About: Reference
   http://www.pgpool.net/docs/latest/pgpool-en.html

About: Usage Example
(start code)
    augtool> set /augeas/load/Pgpool/lens "Pgpool.lns"
    augtool> set /augeas/load/Pgpool/incl "/etc/pgpool/pgpool.conf"
    augtool> load

    augtool> get /files/etc/pgpool/pgpool.conf/port
    /files/etc/pgpool/pgpool.conf/port = 9999

    augtool> set /files/etc/pgpool/pgpool.conf/port 9990
    augtool> save
    Saved 1 file(s)

    $ grep port  /etc/pgpool/pgpool.conf
    port = 9990
(end code)

About: Configuration files
   This lens applies to pgconf.conf. See <filter>.

About: Examples
   The <Test_Pgpool> file contains various examples and tests.
*)


module Pgpool =
  autoload xfm

(* View: sep
     Key and values are separated
     by either spaces or an equal sign *)
let sep = del /([ \t]+)|([ \t]*=[ \t]*)/ " = "

(* Variable: bool_word_re
     The boolean words from the pgpool configuration (on|off) *)
let bool_word_re = /on|off/

(* Variable: log_word_re
     The log words from the pgpool configuration *)
let log_word_re = "info"
                | "notice"
                | "warning"
                | "error"
                | "log"
                | "fatal"
                | "panic"
                | /debug[1-5]/
                | "terse"
                | "default"
                | "verbose"

(* Variable: number_re
     An integer only *)
let number_re = Rx.integer

(* View: number
     Storing <number_re>, with or without quotes *)
let number = store number_re

(* View: bool_word
     Store the <bool_word_re> without quotes *)
let bool_word = store bool_word_re

(* View: log_word
     Store the <log_word_re> without quotes *)
let log_word = store log_word_re

(* View: word_quot
     Anything other than <bool_word_re> or <number> or <log_word_re>
     Quotes are mandatory *)
let word_quot =
     let esc_squot = /\\\\'/
  in let no_quot = /[^#'\n]/
  in let forbidden = number_re | bool_word_re | log_word_re
  in let value = (no_quot|esc_squot)* - forbidden
  in Quote.do_squote (store value)

(* View: word_unquot
   Any unquoted word that is not handled by <log_word_re> or <bool_word_re>.
   Needed to support newly introduced enums *)
let word_unquot =
     let unquot_forbidden = bool_word_re | log_word_re | number_re
  in let unquot_value = Rx.word - unquot_forbidden
  in store unquot_value

(* Variable: enum_vars_re
   Keywords in the pgpool configuration related to enum values *)
let enum_vars_re = "relcache_query_target"
                 | "check_temp_table"

(* Variable: vars_re
   All possible pgpool keywords, except for enum related ones *)
let vars_re = Rx.word - enum_vars_re

(* View: entry_gen
     Builder to construct entries non-enum keywords*)
let entry_gen (lns:lens) =
  Util.indent . Build.key_value_line_comment vars_re sep lns Util.comment_eol

(* View: enum_entry_gen
     Builder to construct entries for enum keywords *)
let enum_entry_gen (lns:lens) =
  Util.indent . Build.key_value_line_comment enum_vars_re sep lns Util.comment_eol

(* View: entry *)
let entry = entry_gen number
          | entry_gen bool_word
          | entry_gen log_word
          | entry_gen word_quot
          | enum_entry_gen word_unquot

(* View: lns *)
let lns = (Util.empty | Util.comment | entry)*

(* Variable: filter *)
let filter = incl "/etc/pgpool/pgpool.conf"
           . incl "/etc/pgpool-II-*/pgpool.conf"
           . incl "/etc/pgpool2/pgpool.conf"

let xfm = transform lns filter
