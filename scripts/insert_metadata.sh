#!/bin/bash

# Check if directory argument is provided
if [ -z "$1" ]; then
  echo "Usage: $0 <directory>"
  exit 1
fi

TARGET_DIR="$1"

# Verify that the argument is a directory
if [ ! -d "$TARGET_DIR" ]; then
  echo "Error: '$TARGET_DIR' is not a directory."
  exit 1
fi

# Block to prepend
read -r -d '' header <<'EOF'
(***
  (
    (features (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (reasons (Polymorphic_types Refinement_types Dependent_types Modules Mu_types Parametric_types First_class_types Variants Records Recursive_functions Higher_order_functions Subtyping OOP_style Return_error Usage_error Other))
    (speed <Fast or Slow>)
    (typing <Well_typed or Ill_typed>)
  )
*)
EOF

# Find all regular files under the directory (recursively)
find "$TARGET_DIR" -type f | while IFS= read -r file; do
  tmp=$(mktemp)
  {
    echo "$header"
    cat "$file"
  } > "$tmp" && mv "$tmp" "$file"
done
