(ns kibe.core)

(defprotocol IFailure
  (fail-unwrap
    [this]))

(defprotocol ISuccess
  (success-unwrap
    [this]))

(defprotocol MonadCallable
  (call
   [this args]))

(defn failure?
  [i]
  (satisfies? IFailure i))

(defn success?
  [i]
  (satisfies? ISuccess i))

(defn failure
  [i]
  (reify IFailure (fail-unwrap [this] i)))

(defn success
  [i]
  (reify ISuccess (success-unwrap [this] i)))

(defmacro defh
  [name doc-string attr-list & forms]
  (assert (symbol? name) "First form must be a symbol.")
  (assert (string? doc-string) "Second form must be a doc string.")
  (assert (and (vector? attr-list) (map symbol? attr-list))
          "Third form must be a vector of symbols.")
  `(def ~name
     (reify
       MonadCallable
       (call [this# args#]
         (let [[~@attr-list] args#]
           (try
             (let [retval# (do ~@forms)]
               (assert
                (or (failure? retval#) (success? retval#))
                (str "Function " ~name " did not a return an explict"
                     " success or failure value."))
               retval#)
             (catch Exception e#
               (failure {:exception e#}))))))))

(defmacro handle
  [[val-name [fh & args]] success-path failure-path]
  `(let [i# (call ~fh [~@args])]
     (if (failure? i#)
       (let [~val-name (fail-unwrap i#)]
         ~failure-path)
       (let [~val-name (success-unwrap i#)]
         ~success-path))))

(defmacro cond?>
  [i & forms]
  (if (seq forms)
    (let [[[f & r] & forms] forms]
      `(handle
        [r# (~f ~i ~@r)]
        (cond?> r# ~@forms)
        (failure r#)))
    `(success ~i)))

(defmacro cond?>>
  [i & forms]
  (if (seq forms)
    (let [[[f & r] & forms] forms]
      `(handle
        [r# (~f ~@r ~i)]
        (cond?>> r# ~@forms)
        (failure r#)))
    `(success ~i)))
