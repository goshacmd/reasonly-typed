type either 'a 'b = Left 'a | Right 'b;
let bindEither = fun either fn => switch either {
  | Right x => fn x
  | Left x => Left x
};
let mapEither = fun fn either => switch either {
  | Right x => Right (fn x)
  | Left x => Left x
};
let mapLeft = fun fn either => switch either {
  | Left x => Left (fn x)
  | Right x => Right x
};
