open Ast;

type type_ =
  | NumberType
  | StringType
  | AnyType
  | SimpleFnType type_ type_
  ;

type typeResult =
  | Type type_
  | TypeMismatch expression type_ type_
  | NotAFunction expression expression
  | NoVariable expression string
  ;

type env = list (string, type_);

let lookUpType = fun env varName =>
switch (Util.maybeFind (fun (var, _) => var == varName) env) {
  | Some (_, type_) => Some type_
  | None => None
};

let addVar = fun varName type_ env errors => ([(varName, type_), ...env], errors);

let rec typeToString = fun type_ => switch type_ {
  | NumberType => "number";
  | StringType => "string";
  | AnyType => "any";
  | SimpleFnType from to_ => (typeToString from) ^ " => " ^ (typeToString to_);
};

let isAny = fun type_ => switch type_ {
  | AnyType => true
  | _ => false
};

let typeErrorToString = fun expr expected got => "Type mismatch in '" ^ (formatExpression expr) ^ "', expected a " ^ (typeToString expected) ^ ", got a " ^ (typeToString got);
let notAFnToString = fun expr fnExpr => "In '" ^ (formatExpression expr) ^ "', '" ^ (formatExpression fnExpr) ^ "' is not a function";
let noVariableToString = fun expr varName => "No variable '" ^ varName ^ "', in '" ^ (formatExpression expr) ^ "'";

let withType = fun typeResult fn =>
switch typeResult {
  | Type type_ => fn type_;
  | x => x;
};

let withTwoTypes = fun typeA_ typeB_ fn =>
withType typeA_ (fun typeA => withType typeB_ (fun typeB => fn typeA typeB));

let doesMatchType = fun expectedType givenType => {
  expectedType == givenType || (isAny givenType)
};

let rec transformPlusMinus = fun expr => switch expr {
  | Plus a b => FnCall (FnCall (VarReference "+") (transformPlusMinus a)) (transformPlusMinus b)
  | Minus a b => FnCall (FnCall (VarReference "-") (transformPlusMinus a)) (transformPlusMinus b)
  | x => x
};

let rec inferType = fun expr varName env => switch expr {
  | FnCall fnName arg1 => {
    if (isVar arg1 varName) {
      switch (typeOf fnName env) {
        | Type x => switch x {
          | SimpleFnType from _ => from
          | _ => AnyType
        }
        | _ => AnyType
      }
    } else if (isFnCall fnName) {
      inferType fnName varName env
    } else {
      AnyType
    }
  }
  | SimpleFn _ bodyExpr => inferType (transformPlusMinus bodyExpr) varName env
  | _ => AnyType
}

and typeOf = fun expr env =>
switch expr {
  | NumberLiteral _ => Type NumberType
  | StringLiteral _ => Type StringType
  | Plus a b => switch (typeOf (FnCall (FnCall (VarReference "+") a) b) env) {
    | TypeMismatch _ exp actual => TypeMismatch expr exp actual
    | x => x
  }
  | Minus a b => switch (typeOf (FnCall (FnCall (VarReference "-") a) b) env) {
    | TypeMismatch _ exp actual => TypeMismatch expr exp actual
    | x => x
  }
  | SimpleFn varName bodyExpr => {
    let varType = inferType (transformPlusMinus bodyExpr) varName env;
    let returnTypeResult = typeOf bodyExpr [(varName, varType), ...env];
    withType returnTypeResult (fun returnType => Type (SimpleFnType varType returnType))
  }
  | FnCall fnName arg1 => {
    withType (typeOf fnName env) (fun fnType => switch fnType {
      | SimpleFnType expectedArgType returnType => withType (typeOf arg1 env) (fun arg1Type => {
        if (doesMatchType expectedArgType arg1Type) {
          Type returnType
        } else {
          TypeMismatch expr expectedArgType arg1Type
        }
      })
      | _ => NotAFunction expr fnName
    })
  }
  | VarReference varName => switch (lookUpType env varName) {
    | Some type_ => Type type_
    | None => NoVariable expr varName
  }
};

type errors = list string;
type resultEnv = (env, errors);

let plusType = SimpleFnType NumberType (SimpleFnType NumberType NumberType);
let minusType = SimpleFnType NumberType (SimpleFnType NumberType NumberType);
let defaultEnv = [
  ("+", plusType),
  ("-", minusType),
];

let emptyResultEnv = (defaultEnv, []);

let addError = fun error env errors => (env, errors @ [error]);

let buildEnv = fun program => List.fold_left (fun (env, errors) element => {
  switch element {
    | Expression expr => {
      let type_ = typeOf expr env;
      switch type_ {
        | Type _ => (env, errors);
        | TypeMismatch x a b => addError (typeErrorToString x a b) env errors;
        | NotAFunction expr fnExpr => addError (notAFnToString expr fnExpr) env errors;
        | NoVariable expr varName => addError (noVariableToString expr varName) env errors;
      }
    };
    | Statement stmt => switch stmt {
      | VarAssignment varName expr => {
        let type_ = typeOf expr env;
        switch type_ {
          | Type x => addVar varName x env errors;
          | TypeMismatch x a b => addError (typeErrorToString x a b) env errors;
          | NotAFunction expr fnExpr => addError (notAFnToString expr fnExpr) env errors;
          | NoVariable expr varName => addError (noVariableToString expr varName) env errors;
        }
      }
    }
  }
}) emptyResultEnv program;
