let joinList = fun sep lst => List.fold_left (fun str x => if (str === "") { x } else { str ^ sep ^ x }) "" lst;

let maybeFind = fun fn lst => {
  let filtered = List.filter fn lst;
  if ((List.length filtered) > 0) {
    Some (List.hd filtered);
  } else {
    None
  }
};
