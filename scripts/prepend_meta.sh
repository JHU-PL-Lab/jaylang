#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <file>"
  exit 1
fi

file="$1"
tmp="$(mktemp)"

cat > "$tmp" <<'EOF'
(***
  (
    (features (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Deterministic_functions Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (reasons (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Deterministic_functions Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (speed <Fast or Slow>)
    (typing <Well_typed or Ill_typed>)
    (flags "")
  )
*)
EOF

cat "$file" >> "$tmp"
mv "$tmp" "$file"
