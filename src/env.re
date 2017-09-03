open Option;
open Typing_types;

type env = list (string, type_);
type errors = list string;
type resultEnv = (env, errors);

let lookUpType = fun env varName =>
  env
  |> Util.maybeFind (fun (var, _) => var == varName)
  |> mapSome (fun (_, type_) => type_);

let addVar = fun varName type_ env errors => ([(varName, type_), ...env], errors);

let defaultEnv = [
  ("+", SimpleFnType NumberType (SimpleFnType NumberType NumberType)),
  ("-", SimpleFnType NumberType (SimpleFnType NumberType NumberType)),
];

let emptyResultEnv = (defaultEnv, []);

let addError = fun error env errors => (env, errors @ [error]);
