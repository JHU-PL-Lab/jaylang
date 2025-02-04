(lang dune 3.17)

(profile dev)

(env
 (release
  (ocamlopt_flags (:standard -O3))))

(context
 (default
  (name profile)
  (instrument_with landmarks)
  (env
   (_
    (env-vars
     ("OCAML_LANDMARKS" "auto"))))))
