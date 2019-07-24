(ns icfpc.speed.string)

;;Cribbed from spork.util.general rather than
;;pulling in the whole dependency.
;;https://github.com/joinr/spork/blob/master/src/spork/util/general.clj

;;#Faster String Building
;;clojure.core/string has a lot of inefficiencies, mostly due to 
;;a lot of vararg usage.  If you're building strings a lot, i.e. 
;;passing messages in a simulation using strings, or logging, etc.
;;then make-string is roughly 5x faster.  It's a drop-in replacement 
;;for clojure.core/str.
(defn ^String simple-str [^Object x]
  (if (nil? x) "" (.toString x)))

(defmacro build-string [& args]
  `(let [x#  ~(first args)
         sb# (StringBuilder. (simple-str x#))]
     (.toString
      (doto sb#
        ~@(for [a (rest args)]
            `(.append (simple-str  ~a)))))))

(def ^:constant +max-params+ 15)
(defn string-func-body [n]
  (let [obj (with-meta (gensym "obj") {:tag 'Object})]
    (assert (>= n 0))
    (if (<= n +max-params+)
      (case n 
        0 `(~(with-meta [] {:tag 'String}) "")
        1 `(~(with-meta [obj] {:tag 'String})
            (if (nil? ~obj) "" (. ~obj (toString))))    
        (let [args (-> (vec (for [x (range n)] (gensym "str")))
                       (with-meta {:tag String}))]    
          `(~args
            (build-string ~@args))))
      (let [baseargs (vec (for [x (range (+ 2 +max-params+))] (gensym "str")))
            args (-> baseargs
                     (conj '&)
                     (conj 'rest)
                     (with-meta {:tag String}))] 
      `(~args
        (build-string (build-string ~@baseargs) 
                      (apply clojure.core/str ~'rest)))))))

;;Positional definitions of str, to eliminate arrayseq overhead due 
;;to varargs version of str.  Since we're making lots of strings,
;;it's stupid to incur the varargs cost here...
(let [bodies (for [n (range (+ +max-params+ 2))]
               (string-func-body n))]
  (eval
   `(defn ~'make-string
           "Drop-in replacement for clojure.core/str, designed for faster 
            string concatenation when creating strings is on a critical 
            performance path.

            With no args, returns the empty string. With one arg x, returns
            x.toString().  (str nil) returns the empty string. With more than
            one arg, returns the concatenation of the str values of the args.
            When creating strings - many times over - using arity > 2, avoids
            the overhead of calls to first/next that clojure.core/str invokes.
            Roughly 33% faster for concatenating 3 strings, approaching  
            60% faster for larger arities, up to 15."
      ~@bodies)))
