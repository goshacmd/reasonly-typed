let mapSome = fun fn optn => switch optn {
  | Some x => Some (fn x)
  | None => None
};
