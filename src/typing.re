open Ast;

type type_ =
  | NumberType
  | StringType
  | AnyType
  | GenericType (list string) type_
  | GenericLabel string
  | SimpleFnType type_ type_
  ;


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

let mapSome = fun fn optn => switch optn {
  | Some x => Some (fn x)
  | None => None
};


type typeError =
  | TypeMismatch expression type_ type_
  | NotAFunction expression expression
  | NoVariable expression string
  ;

type typeResult = either typeError type_;

type env = list (string, type_);

let lookUpType = fun env varName =>
  env
  |> Util.maybeFind (fun (var, _) => var == varName)
  |> mapSome (fun (_, type_) => type_);

let addVar = fun varName type_ env errors => ([(varName, type_), ...env], errors);

let changeMismatchExpr = fun newExpr error => switch error {
  | TypeMismatch _ exp actual => TypeMismatch newExpr exp actual
  | x => x
};

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

let formatError = fun error => switch error {
  | TypeMismatch expr expected got => "Type mismatch in '" ^ (formatExpression expr) ^ "', expected a " ^ (typeToString expected) ^ ", got a " ^ (typeToString got);
  | NotAFunction expr fnExpr => "In '" ^ (formatExpression expr) ^ "', '" ^ (formatExpression fnExpr) ^ "' is not a function";
  | NoVariable expr varName => "No variable '" ^ varName ^ "', in '" ^ (formatExpression expr) ^ "'";
};

let doesMatchType = fun expectedType givenType => (expectedType == givenType) || (isAny givenType) || (isAny expectedType);

let rec replaceGenericWithSpecific = fun type_ genericLabel replacementType => switch type_ {
  | GenericLabel label => if (label == genericLabel) { replacementType } else { type_ }
  | SimpleFnType argType retType => SimpleFnType (replaceGenericWithSpecific argType genericLabel replacementType) (replaceGenericWithSpecific retType genericLabel replacementType)
  | x => x
};

let computeFnCallType = fun expr expectedArgType returnType arg1Type => {
  let expectedArg1Type = if (isGenericVar expectedArgType) {
    arg1Type
  } else {
    expectedArgType
  };

  let actualReturnType = if (isGenericVar expectedArgType) {
    replaceGenericWithSpecific returnType "A" arg1Type /* FIXME */
  } else {
    returnType
  };

  if (doesMatchType expectedArg1Type arg1Type) {
    Right actualReturnType
  } else {
    Left (TypeMismatch expr expectedArg1Type arg1Type)
  }
};

let rec getFnArgType = fun possiblyFnType => switch possiblyFnType {
  | SimpleFnType from _ => from
  | GenericType _ wrapped => getFnArgType wrapped
  | _ => AnyType
};

let rec getFnRetType = fun possiblyFnType arg1 expr fnName env => switch possiblyFnType {
  | SimpleFnType expectedArgType returnType =>
    bindEither (typeOf arg1 env) (fun arg1Type => computeFnCallType expr expectedArgType returnType arg1Type)
  | GenericType _ wrapped => getFnRetType wrapped arg1 expr fnName env
  | _ => Left (NotAFunction expr fnName)
}

and inferType = fun (expr: expression) (varName: string) (env: env) : type_ => switch expr {
  | VarReference refVarName => if (refVarName == varName) { GenericLabel "A" } else { AnyType } /* FIXME */
  | FnCall fnName arg1 => {
    if (isVar arg1 varName) {
      switch (typeOf fnName env) {
        | Right x => getFnArgType x
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

and typeOf = fun (expr: expression) (env: env) : typeResult =>
switch expr {
  | NumberLiteral _ => Right NumberType
  | StringLiteral _ => Right StringType
  | Plus a b => (typeOf (FnCall (FnCall (VarReference "+") a) b) env) |> mapLeft (changeMismatchExpr expr)
  | Minus a b => (typeOf (FnCall (FnCall (VarReference "-") a) b) env) |> mapLeft (changeMismatchExpr expr)
  | SimpleFn varName bodyExpr => {
    let varType = inferType (transformPlusMinus bodyExpr) varName env;

    typeOf bodyExpr [(varName, varType), ...env]
    |> mapEither (fun returnType => switch varType {
      | GenericLabel label => GenericType [label] (SimpleFnType varType returnType)
      | _ => SimpleFnType varType returnType
    });
  }
  | FnCall fnName arg1 => {
    let fnTypeResult = typeOf fnName env;
    bindEither fnTypeResult (fun fnType => getFnRetType fnType arg1 expr fnName env)
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
