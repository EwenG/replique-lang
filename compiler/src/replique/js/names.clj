(ns replique.js.names
  "The JavaScript spellings of names, from replique-cljs's clojure.cljs.names
  (doc/cljs-compiler.md §5.3 there): which spelling a name gets depends on
  who owns it.

  A name the host owns, a property or a method, is spelled as the host
  spells it: `host-name`, ClojureScript's character map, lossy on purpose.

  A name we own is spelled by `munge`, which is injective: distinct names
  give distinct JavaScript names. A var is a property of its namespace
  object named after it (doc/compilation.md §2.2), which other forms and the
  REPL compute from the symbol alone, so two vars must never share one.

  The identifiers we declare have four shapes, so none can be another:

    local       <munged>__<n>     ends in digits, contains __
    temporary   t$<n>             never contains __
    constant    c$<n>             a keyword or symbol, made once per form
    ns alias    <munged>$...$ns   ends in ns

  None is ever a bare identifier, so there is no reserved-word list, and a
  local can't shadow a JS global."
  (:refer-clojure :exclude [munge])
  (:require [clojure.string :as str]))

(def char-map
  "cljs.compiler's mapping, used as-is for host names."
  {\- "_" \: "_COLON_" \+ "_PLUS_" \> "_GT_" \< "_LT_" \= "_EQ_" \~ "_TILDE_"
   \! "_BANG_" \@ "_CIRCA_" \# "_SHARP_" \' "_SINGLEQUOTE_" \" "_DOUBLEQUOTE_"
   \% "_PERCENT_" \^ "_CARET_" \& "_AMPERSAND_" \* "_STAR_" \| "_BAR_"
   \{ "_LBRACE_" \} "_RBRACE_" \[ "_LBRACK_" \] "_RBRACK_" \/ "_SLASH_"
   \\ "_BSLASH_" \? "_QMARK_"})

(def code-map
  "char-map with its codes delimited by $, which makes munge injective: a $
  always delimits a code, and a _ always came from a hyphen.

    -  _      _  $US$      $  $DL$      >  $GT$ ..."
  (into {\- "_", \_ "$US$", \$ "$DL$"}
        (map (fn [[c code]] [c (str "$" (subs code 1 (dec (count code))) "$")]))
        (dissoc char-map \-)))

(defn host-name
  "The spelling of a name the host owns: (.-foo-bar o) reaches foo_bar."
  [s]
  (apply str (map #(get char-map % %) (str s))))

(defn munge
  "The spelling of a name we own: a var's property, the base of a local."
  [s]
  (apply str (map #(get code-map % %) (str s))))

(def identifier-re #"^[A-Za-z_$][A-Za-z0-9_$]*$")

(defn identifier? [s] (boolean (re-matches identifier-re s)))

(defn property
  "`.p` or `[\"p\"]`, reading property `p` of an object."
  [p string-literal]
  (if (identifier? p) (str "." p) (str "[" (string-literal p) "]")))

(defn ns-alias
  "The identifier a form binds namespace `ns`'s object to: app.core is
  app$core$ns."
  [ns]
  (str (str/join "$" (map munge (str/split (str ns) #"\."))) "$ns"))

(def ^:private auto-gensym-re #"^(.+)__\d+__auto__$")

(defn local-name
  "A local binding of `sym`, numbered `n`. An auto-gensym's suffix is
  dropped, since the number already makes the name unique; a base that
  doesn't munge to an identifier (a dot, say) becomes `l`."
  [sym n]
  (let [s (str sym)
        base (munge (if-let [[_ b] (re-matches auto-gensym-re s)] b s))]
    (str (if (identifier? base) base "l") "__" n)))
