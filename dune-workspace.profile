(lang dune 3.0)

(profile dev)

(context
 (default
  (name profile)
  (instrument_with landmarks)
  (env
   (_
    (env-vars
     ("OCAML_LANDMARKS" "auto"))))))
