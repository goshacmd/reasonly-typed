open Ast;
open Typing;

let testProgram = [
  Statement (VarAssignment "x" (NumberLiteral 10)),
  Statement (VarAssignment "name" (StringLiteral "Gosha")),
  Statement (VarAssignment "y1" (Plus (VarReference "name") (NumberLiteral 2))),
  Statement (VarAssignment "y2" (Plus (VarReference "x") (NumberLiteral 2))),
  Statement (VarAssignment "y3" (Minus (VarReference "name") (VarReference "name"))),
  Statement (VarAssignment "y4" (Minus (VarReference "x") (NumberLiteral 1))),
  Statement (VarAssignment "add1" (SimpleFn "x" (Plus (VarReference "x") (NumberLiteral 1)))),
  Statement (VarAssignment "res1" (FnCall (VarReference "add1") (NumberLiteral 5))),
  Statement (VarAssignment "res2" (FnCall (VarReference "x") (NumberLiteral 5))),
];

let analyzeProgram = fun program => {
  let (env, errors): resultEnv = buildEnv testProgram;

  Js.log "Program:";
  Js.log (formatProgram program);
  Js.log "";
  Js.log "Types:";
  Js.log (Util.joinList "\n" (List.map (fun (varName, type_) => "- " ^ varName ^ ": " ^ typeToString(type_)) env));
  if ((List.length errors) == 0) {
    Js.log "No errors";
  } else {
  Js.log "";
  Js.log "Errors:";
    Js.log (Util.joinList "\n" (List.map (fun error => "- " ^ error) errors));
  };
  Js.log "";
};

analyzeProgram testProgram;
