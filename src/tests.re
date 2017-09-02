let formatTypes = fun env => Util.joinList "\n" (List.map (fun (varName, type_) => "- " ^ varName ^ ": " ^ Typing.typeToString(type_)) env);
let formatErrors = fun errors => Util.joinList "\n" (List.map (fun error => "- " ^ error) errors);
let expect = fun label program types errors => {
  let (_actualTypes, actualErrors) = Typing.buildEnv program;

  let actualTypes = List.filter (fun (varName, _) => varName != "+" && varName != "-") _actualTypes;

  if (types == actualTypes && errors == actualErrors) {
    Js.log ("OK: " ^ label);
  } else {
    Js.log ("Error: " ^ label);

    Js.log (Ast.formatProgram program);

    if (types != actualTypes) {
      Js.log "Expected types to be:";
      Js.log (formatTypes types);
      Js.log "";
      Js.log "Actual types:";
      Js.log (formatTypes actualTypes);
    };

    if (errors != actualErrors) {
      Js.log "Expected errors to be:";
      Js.log (formatErrors errors);
      Js.log "";
      Js.log "Actual errors:";
      Js.log (formatErrors actualErrors);
    };

    Js.log "";
  }
};

expect "Proper literal types" Ast.([
  Statement (VarAssignment "x" (NumberLiteral 10)),
  Statement (VarAssignment "name" (StringLiteral "Gosha")),
]) Typing.([
  ("name", StringType),
  ("x", NumberType),
]) [];

expect "Proper type for number addition" Ast.([
  Statement (VarAssignment "x" (NumberLiteral 10)),
  Statement (VarAssignment "y" (Plus (VarReference "x") (NumberLiteral 1))),
]) Typing.([
  ("y", NumberType),
  ("x", NumberType),
]) [];

expect "Error for adding a number with a string" Ast.([
  Statement (VarAssignment "x" (NumberLiteral 10)),
  Statement (VarAssignment "y" (Plus (VarReference "x") (StringLiteral "Yo"))),
]) Typing.([
  ("x", NumberType),
]) [
  {|Type mismatch in '(x + "Yo")', expected a number, got a string|}
];

expect "Properly inferred types of function arguments" Ast.([
  Statement (VarAssignment "megaAdd" (SimpleFn "x" (SimpleFn "y" (Plus (VarReference "x") (VarReference "y"))))),
  Statement (VarAssignment "noOp" (SimpleFn "x" (SimpleFn "y" (Plus (NumberLiteral 1) (NumberLiteral 2))))),
]) Typing.([
  ("noOp", SimpleFnType AnyType (SimpleFnType AnyType NumberType)),
  ("megaAdd", SimpleFnType NumberType (SimpleFnType NumberType NumberType)),
]) [];
