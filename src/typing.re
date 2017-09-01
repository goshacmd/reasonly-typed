open Ast;

type type_ =
  | NumberType
  | StringType
  | AnyType
  | SimpleFnType type_ type_;

type typeResult = Type type_ | TypeMismatch expression type_ type_;

type env = list (string, type_);

let lookUpType = fun env varName => {
  let (_, type_) = List.find (fun (var, _) => var == varName) env;
  type_
};
let addVar = fun varName type_ env errors => ([(varName, type_), ...env], errors);

let rec typeToString = fun type_ => switch type_ {
  | NumberType => "number";
  | StringType => "string";
  | AnyType => "any";
  | SimpleFnType from to_ => (typeToString from) ^ " => " ^ (typeToString to_);
};

let typeErrorToString = fun expr expected got => "Type mismatch in '" ^ (formatExpression expr) ^ "', expected a " ^ (typeToString expected) ^ ", got a " ^ (typeToString got);

let withType = fun typeResult fn =>
switch typeResult {
  | TypeMismatch e x y => TypeMismatch e x y;
  | Type type_ => fn type_;
};

let withTwoTypes = fun typeA_ typeB_ fn =>
withType typeA_ (fun typeA => withType typeB_ (fun typeB => fn typeA typeB));

let isAny = fun type_ => switch type_ {
  | AnyType => true;
  | _ => false;
};

let doesMatchType = fun expectedType givenType => {
  if ((isAny expectedType) || (isAny givenType)) {
    true
  } else {
    expectedType == givenType
  }
};

let isVar = fun expr varName => switch expr {
  | VarReference vn => varName == vn;
  | _ => false;
};

let makeSingleVarEnv = fun varName => [(varName, AnyType)];

let rec inferType = fun expr varName =>
switch expr {
  | Plus a b => {
    if ((isVar a varName) || (isVar b varName)) {
      switch (typeOf a (makeSingleVarEnv varName)) {
        | Type typeA => switch (typeOf b (makeSingleVarEnv varName)) {
          | Type typeB => {
            if (typeA === NumberType || typeB === NumberType) {
              NumberType
            } else if (typeB === StringType || typeB === StringType) {
              StringType
            } else {
              AnyType
            }
          }
          | _ => AnyType;
        }
        | _ => AnyType;
      }
    } else {
      AnyType
    }
  }
  | Minus a b => {
    if ((isVar a varName) || (isVar b varName)) {
      switch (typeOf a (makeSingleVarEnv varName)) {
        | Type typeA => switch (typeOf b (makeSingleVarEnv varName)) {
          | Type typeB => {
            if (typeA === NumberType || typeB === NumberType) {
              NumberType
            } else {
              AnyType
            }
          }
          | _ => AnyType;
        }
        | _ => AnyType;
      }
    } else {
      AnyType
    }
  }
  | _ => AnyType;
}

and typeOf = fun expr env =>
switch expr {
  | NumberLiteral _ => Type NumberType
  | StringLiteral _ => Type StringType
  | Plus a b => {
    withTwoTypes (typeOf a env) (typeOf b env) (fun typeA typeB => {
      (doesMatchType typeA typeB) ? (Type typeA) : (TypeMismatch expr typeA typeB)
    });
  }
  | Minus a b => {
    withTwoTypes (typeOf a env) (typeOf b env) (fun typeA typeB => {
      (doesMatchType typeA typeB) ? ((doesMatchType NumberType typeA) ? (Type typeA) : (TypeMismatch expr NumberType typeA)) : (TypeMismatch expr typeA typeB)
    });
  }
  | SimpleFn varName expr => {
    let varType = inferType expr varName;
    let returnTypeResult = typeOf expr [(varName, varType), ...env];
    withType returnTypeResult (fun returnType => {
      Type (SimpleFnType varType returnType)
    })
  }
  | FnCall fn arg1 => {
    let fnType_ = typeOf fn env;
    let arg1Type_ = typeOf arg1 env;
    withType arg1Type_ (fun actualArg1Type => {
      withType fnType_ (fun fnType => switch fnType {
        | SimpleFnType arg1Type retType => {
          if (doesMatchType arg1Type actualArg1Type) {
            Type retType
          } else {
            TypeMismatch expr arg1Type actualArg1Type
          }
        }
        | _ => TypeMismatch expr (SimpleFnType AnyType AnyType) fnType
      })
    })
  }
  | VarReference varName => Type (lookUpType env varName)
};

type errors = list string;
type resultEnv = (env, errors);

let emptyResultEnv = ([], []);

let addError = fun error env errors => (env, errors @ [error]);

let buildEnv = fun program => List.fold_left (fun (env, errors) element => {
  switch element {
    | Expression expr => {
      let type_ = typeOf expr env;
      switch type_ {
        | Type _ => (env, errors);
        | TypeMismatch x a b => addError (typeErrorToString x a b) env errors;
      }
    };
    | Statement stmt => switch stmt {
      | VarAssignment varName expr => {
        let type_ = typeOf expr env;
        switch type_ {
          | Type x => addVar varName x env errors;
          | TypeMismatch x a b => addError (typeErrorToString x a b) env errors;
        }
      }
    }
  }
}) emptyResultEnv program;
