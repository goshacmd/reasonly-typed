open Ast;

type type_ =
  | NumberType
  | StringType
  | AnyType
  | SimpleFnType type_ type_
  ;


type either 'a 'b = Left 'a | Right 'b;
let bindEither = fun either fn => switch either {
  | Right x => fn x
  | Left x => Left x
};
let mapEither = fun either fn => switch either {
  | Right x => Right (fn x)
  | Left x => Left x
};
let mapLeft = fun either fn => switch either {
  | Left x => Left (fn x)
  | Right x => Right x
};


type typeError =
  | TypeMismatch expression type_ type_
  | NotAFunction expression expression
  | NoVariable expression string
  ;

type typeResult = either typeError type_;

type env = list (string, type_);

let lookUpType = fun env varName =>
switch (Util.maybeFind (fun (var, _) => var == varName) env) {
  | Some (_, type_) => Some type_
  | None => None
};

let addVar = fun varName type_ env errors => ([(varName, type_), ...env], errors);

let changeMismatchExpr = fun newExpr error => switch error {
  | TypeMismatch _ exp actual => TypeMismatch newExpr exp actual
  | x => x
};

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

let formatError = fun error => switch error {
  | TypeMismatch expr expected got => "Type mismatch in '" ^ (formatExpression expr) ^ "', expected a " ^ (typeToString expected) ^ ", got a " ^ (typeToString got);
  | NotAFunction expr fnExpr => "In '" ^ (formatExpression expr) ^ "', '" ^ (formatExpression fnExpr) ^ "' is not a function";
  | NoVariable expr varName => "No variable '" ^ varName ^ "', in '" ^ (formatExpression expr) ^ "'";
};

let doesMatchType = fun expectedType givenType => {
  expectedType == givenType || (isAny givenType)
};

let rec inferType = fun expr varName env => switch expr {
  | FnCall fnName arg1 => {
    if (isVar arg1 varName) {
      switch (typeOf fnName env) {
        | Right x => switch x {
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
  | SimpleFn _ bodyExpr => inferType bodyExpr varName env
  | _ => AnyType
}

and typeOf = fun expr env =>
switch expr {
  | NumberLiteral _ => Right NumberType
  | StringLiteral _ => Right StringType
  | Plus a b => mapLeft (typeOf (FnCall (FnCall (VarReference "+") a) b) env) (changeMismatchExpr expr)
  | Minus a b => mapLeft (typeOf (FnCall (FnCall (VarReference "-") a) b) env) (changeMismatchExpr expr)
  | SimpleFn varName bodyExpr => {
    let varType = inferType (transformPlusMinus bodyExpr) varName env;
    let returnTypeResult = typeOf bodyExpr [(varName, varType), ...env];
    mapEither returnTypeResult (fun returnType => SimpleFnType varType returnType)
  }
  | FnCall fnName arg1 => {
    let fnTypeResult = typeOf fnName env;
    bindEither fnTypeResult (fun fnType => switch fnType {
      | SimpleFnType expectedArgType returnType => bindEither (typeOf arg1 env) (fun arg1Type => {
        if (doesMatchType expectedArgType arg1Type) {
          Right returnType
        } else {
          Left (TypeMismatch expr expectedArgType arg1Type)
        }
      })
      | _ => Left (NotAFunction expr fnName)
    })
  }
  | VarReference varName => switch (lookUpType env varName) {
    | Some type_ => Right type_
    | None => Left (NoVariable expr varName)
  }
};

type errors = list string;
type resultEnv = (env, errors);

let defaultEnv = [
  ("+", SimpleFnType NumberType (SimpleFnType NumberType NumberType)),
  ("-", SimpleFnType NumberType (SimpleFnType NumberType NumberType)),
];

let emptyResultEnv = (defaultEnv, []);

let addError = fun error env errors => (env, errors @ [error]);

let buildEnv = fun program => List.fold_left (fun (env, errors) element => {
  switch element {
    | Expression expr => {
      switch (typeOf expr env) {
        | Right _ => (env, errors);
        | Left x => addError (formatError x) env errors;
      }
    };
    | Statement stmt => switch stmt {
      | VarAssignment varName expr => {
        switch (typeOf expr env) {
          | Right x => addVar varName x env errors;
          | Left x => addError (formatError x) env errors;
        }
      }
    }
  }
}) emptyResultEnv program;
