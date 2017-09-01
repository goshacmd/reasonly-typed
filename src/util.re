let joinList = fun sep lst => List.fold_left (fun str x => str ^ sep ^ x) "" lst;
