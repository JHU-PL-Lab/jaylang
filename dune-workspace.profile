(lang dune 3.0)

(profile dev)

(env
 ; (dev
 ;  (ocamlopt_flags (:standard -O3)))
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
