let max x1 x2 = 
    if x1 > x2 then x1 else x2
in
let abs n = 
    if n > 0 then n else (0-1) * n
in
let rec get_height t = 
    match t with
    | {left = l, right = r, _} ->
        let l_height = get_height l in
        let r_height = get_height r in
        let children_height = max l_height r_height in
        children_height + 1
    | {leaf = x, _} ->
        1
    end
in
let rec is_balanced t = 
    match t with
    | {left = l, right = r, _} ->
        if is_balanced l then
            if is_balanced r then
                let l_height = get_height l in
                let r_height = get_height r in
                (abs (l_height - r_height)) <= 1
            else false
        else
            false
    | {leaf = x, _} -> true
    end
in
let tree_type = Mu tt. ({: left : tt, right : tt :} || {: leaf : int :}) in
let (x : {. tree_type | is_balanced }) = 
    { left = 
        { left = 
            { left = 
                { left = {leaf = 0}, right = {leaf = 0}} , 
              right = {leaf = 0}} , 
          right = {leaf = 0} }, 
      right = { leaf = 0 } }
in x
