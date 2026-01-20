(fun (d : int + int) -> 
    case d of 
      | inl (x : int) -> x + 2 
      | inr (y : int) -> 17 
    ) (inl int 99) 
