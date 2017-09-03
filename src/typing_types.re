type type_ =
  | NumberType
  | StringType
  | AnyType
  | GenericType (list string) type_
  | GenericLabel string
  | SimpleFnType type_ type_
  ;

let rec typeToString = fun type_ => switch type_ {
  | NumberType => "number";
  | StringType => "string";
  | AnyType => "any";
  | GenericType labels wrappedType => "<" ^ (Util.joinList "," labels) ^ ">" ^ (typeToString wrappedType);
  | GenericLabel label => label;
  | SimpleFnType from to_ => (typeToString from) ^ " => " ^ (typeToString to_);
};

let isAny = fun type_ => switch type_ {
  | AnyType => true
  | _ => false
};
let isGenericVar = fun type_ => switch type_ {
  | GenericLabel _ => true
  | _ => false
};
