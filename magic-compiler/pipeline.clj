(ns pipeline
  "Render a Clojure form's pipeline (form -> AST -> symbolic IL). Public
   function: `show`."
  (:require [magic.analyzer :as ana]
            [magic.core :as magic]
            [clojure.walk :as walk]
            [clojure.string :as str]))

(def ^:private output-dir
  "Relative path where EDN dumps are written."
  "target")

(def ^:private ast-noise-keys
  "Analyzer bookkeeping that obscures an AST's semantic shape:
   `:env` (lexical context), `:raw-forms` (pre-macroexpand history),
   `:children` (structural metadata, useful for navigation but visual
   noise once you have the keys themselves), source-position info,
   etc. Everything else flows through `ast-skeleton`."
  #{:env :raw-forms :children :top-level :containing-fn-name
    :throws? :outside-type? :constant? :loop-id :meta
    :line :column :file :end-line :end-column :source-span})

(defn- ast-skeleton
  "Recursively `dissoc` every analyzer-bookkeeping key from each AST
   node in `ast`, leaving the semantic shape intact. Non-AST values
   pass through unchanged."
  [ast]
  (walk/postwalk
   (fn [x]
     (if (and (map? x) (:op x))
       (reduce dissoc x ast-noise-keys)
       x))
   ast))

(defn- mage-instruction?
  "Return true when `x` is a map carrying a `:mage.core/opcode` key."
  [x]
  (and (map? x) (contains? x :mage.core/opcode)))

(defn- flatten-il
  "Return a flat vector of mage instruction maps from `il` in emission
   order. Structural nils that mage uses for alignment are discarded.
   Walks only into sequential collections so the contents of each
   instruction map stay opaque."
  [il]
  (->> (tree-seq sequential? seq il)
       (filter mage-instruction?)
       vec))

(defn- render-method-info
  "Return a readable signature string for a `MethodInfo` or
   `ConstructorInfo`, e.g. \"Numbers/isZero(Int64) -> Boolean\"."
  [m]
  (let [mname  (.Name m)
        decl   (.Name (.DeclaringType m))
        ret    (when (instance? System.Reflection.MethodInfo m)
                 (.Name (.ReturnType m)))
        params (str/join "," (map #(.Name (.ParameterType %))
                                  (.GetParameters m)))]
    (str decl "/" mname "(" params ")" (when ret (str " -> " ret)))))

(defn- render-argument
  "Return a readable string for a mage opcode argument. Handles nil,
   `MethodBase` instances, mage local/field/label argument maps, and
   falls back to `pr-str` for anything else."
  [argument]
  (cond
    (nil? argument) ""

    (instance? System.Reflection.MethodBase argument)
    (render-method-info argument)

    (map? argument)
    (let [{:mage.core/keys [local field label type]} argument]
      (cond
        local (str local " :type " type)
        field (str field " :type " type)
        label (str label)
        :else (pr-str argument)))

    :else (pr-str argument)))

(defn- render-instruction
  "Return a single-line pseudo-MSIL string for one mage instruction map."
  [{:mage.core/keys [opcode argument]}]
  (let [op-str (if (instance? clojure.lang.Named opcode)
                 (name opcode)
                 (str opcode))]
    (str/trim (str op-str " " (render-argument argument)))))

(defn- header
  "Print `s` surrounded by banner lines on stdout."
  [s]
  (let [bar (apply str (repeat 64 "="))]
    (println (str "\n" bar "\n" s "\n" bar))))

(defn- ensure-dir
  "Create `dir` and any missing parents. Return `dir`."
  [dir]
  (System.IO.Directory/CreateDirectory dir)
  dir)

(defn- edn-safe
  "Return `v` with every value that prints in a non-EDN form replaced
   by its string representation. Clojure produces exactly two
   non-EDN-readable print shapes: `#object[Class id \"toString\"]`
   (the default fallback for any type without a registered print-method)
   and `#'ns/name` (the var reference form). System.Type gets a nicer
   `.FullName` instead of the default `#object[...]` wrapping. Walks
   the structure so only leaf values get checked."
  [v]
  (walk/postwalk
   (fn [x]
     (cond
       (coll? x) x
       (instance? System.Type x) (.FullName x)
       :else (let [s (pr-str x)]
               (if (or (str/starts-with? s "#object[")
                       (str/starts-with? s "#'"))
                 (str x)
                 x))))
   v))

(defn- spit-edn
  "Write `v` to `path` as EDN, fully expanded (no print-length /
   print-level truncation). Reflection objects in `v` are coerced to
   strings so the output is parseable by any EDN reader."
  [path v]
  (binding [*print-length* nil *print-level* nil]
    (System.IO.File/WriteAllText path (pr-str (edn-safe v)))))

(defn- show-pipeline
  "Print `form`, its AST skeleton, and its symbolic IL to stdout, and
   write the full AST and IL as EDN under `out-dir`."
  ([form] (show-pipeline form output-dir))
  ([form out-dir]
   (ensure-dir out-dir)
   (header (str "FORM   " (pr-str form)))
   (let [ast (ana/analyze form)]
     (header "AST (skeleton)")
     (prn (ast-skeleton ast))
     (let [ast-path (str out-dir "/pipeline-ast.edn")
           il       (magic/compile ast)
           il-path  (str out-dir "/pipeline-il.edn")
           insts    (flatten-il il)]
       (spit-edn ast-path ast)
       (spit-edn il-path il)
       (header (str "SYMBOLIC IL (" (count insts) " instructions)"))
       (doseq [inst insts]
         (println (str "  " (render-instruction inst))))
       (header "EDN dumps")
       (println " " ast-path)
       (println " " il-path)))))

(defn show
  "Join `args` with spaces, read the result as a single Clojure form,
   and show its pipeline."
  [& args]
  (when (empty? args)
    (throw (ex-info "usage: nos pipeline/show '<clojure-form>'" {})))
  (show-pipeline (read-string (str/join " " args))))
