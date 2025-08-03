import {
  BuiltInTypes,
  isNumericType,
  isValidType,
  NumericSuffixTypes,
  TypeKinds,
  type NumericType,
  type Type,
} from "../ast/types";
import { SymbolTable } from "./SymbolTable";
import { TokenTypes } from "../tokens";
import wabt from "wabt";
import { CompilerError } from "../errors";
import type { FunctionDeclaration } from "../ast/nodes/FunctionDeclaration";
import type { ExpressionStatement } from "../ast/nodes/ExpressionStatement";
import type { VariableDeclaration } from "../ast/nodes/VariableDeclaration";
import { NodeKinds, type AnyAstNode } from "../ast/nodes";
import type { CallExpression } from "../ast/nodes/CallExpression";
import type { Identifier } from "../ast/nodes/Identifier";
import type { BinaryExpression } from "../ast/nodes/BinaryExpression";
import { StringLiteral } from "../ast/nodes/StringLiteral";
import type { NumberLiteral } from "../ast/nodes/NumberLiteral";
import runtime from "../../assets/runtime.wat?raw";
import type { InterpolatedString } from "../ast/nodes/InterpolatedString";
import { FormattedExpression } from "../ast/nodes/FormattedExpression";
import { DEFAULT_FORMAT } from "../ast/formats";
import type { ReturnStatement } from "../ast/nodes/ReturnStatement";
import { FunctionTable } from "./FunctionTable";
import type { IfStatement } from "../ast/nodes/IfStatement";
import { LocalTable } from "./LocalTable";
import type { VariableAssignment } from "../ast/nodes/VariableAssignment";
import type { BoolLiteral } from "../ast/nodes/BoolLiteral";
import type { TernaryExpression } from "../ast/nodes/TernaryExpression";
import type { FieldAssignment } from "../ast/nodes/FieldAssignment";
import { StructTable } from "./StructTable";
import {
  WASM_CONSTANT_EMITS,
  WASM_LOAD_EMITS,
  WASM_NUMERIC_FORMATTERS,
  WASM_OPERATION_OPERANDS,
  WASM_BINARY_OPERATIONS,
  WASM_STORE_EMITS,
  WASM_TYPES,
  type OperandSize,
  WASM_UNARY_OPERATIONS,
} from "./types";
import type { FieldExpression } from "../ast/nodes/FieldExpression";
import { ReturnTable } from "./ReturnTable";
import type { LoopStatement } from "../ast/nodes/LoopStatement";
import type { FixExpression } from "../ast/nodes/FixExpression";
import type { BinaryOperator, UnaryOperator } from "../ast/operators";
import type { UnaryExpression } from "../ast/nodes/UnaryExpression";

interface StringLiteralInfo {
  offset: number;
  length: number;
}

const BUILTIN_FUNCTIONS = [
  {
    name: "print",
    params: [
      {
        name: "message",
        type: BuiltInTypes.string,
      },
    ],
    type: BuiltInTypes.void,
  },
  {
    name: "error",
    params: [
      {
        name: "message",
        type: BuiltInTypes.string,
      },
      {
        name: "index",
        type: BuiltInTypes.int,
      },
    ],
    type: BuiltInTypes.void,
  },
];

export class Compiler {
  #wat: string[] = [];
  #offset = 0;
  #strings = new Map<string, StringLiteralInfo>();
  #scope: SymbolTable | null = null;
  #structs = new StructTable();
  #functions = new FunctionTable();
  #locals = new LocalTable();
  #returns = new ReturnTable();

  #spacing = 0;

  constructor() {
    for (const func of BUILTIN_FUNCTIONS) {
      this.#functions.add(func);
    }
  }

  #block(opener: string, body: () => void) {
    this.#write(opener);
    this.#spacing += 2;
    body();
    this.#spacing -= 2;
    this.#write(")");
  }

  async compile(ast: AnyAstNode[]): Promise<Uint8Array> {
    this.#block("(module", () => {
      for (const func of BUILTIN_FUNCTIONS) {
        this.#block(`(import "env" "${func.name}"`, () => {
          this.#block(`(func $${func.name}`, () => {
            for (let i = 0; i < func.params.length; i++) {
              const param = func.params[i];
              this.#write(this.#getParamDeclaration(param.name, param.type)!);
            }
          });
        });
      }

      this.#write('(memory (export "memory") 1)');
      this.#write("(global $__next_free_address (mut i32) (i32.const 0))");

      this.#write(runtime);

      this.#collectStringLiterals(ast);

      this.#strings.forEach((info, value) => {
        const bytes = new TextEncoder().encode(value);
        const hexBytes = Array.from(bytes)
          .map((b) => `\\${b.toString(16).padStart(2, "0")}`)
          .join("");
        this.#write(`(data (i32.const ${info.offset}) "${hexBytes}")`);
      });

      this.#block("(func $__init_heap", () => {
        this.#write(
          `(global.set $__next_free_address (i32.const ${this.#offset}))`
        );
      });

      this.#write("(start $__init_heap)");

      ast.forEach((node) => {
        if (node.kind === NodeKinds.STRUCT_DECLARATION) {
          if (
            !this.#structs.add({
              name: node.name,
              fields: node.fields,
            })
          ) {
            throw new CompilerError(
              `Struct "${node.name}" has already been declared`,
              node.offset
            );
          }
        }
      });

      ast.forEach((node) => {
        if (node.kind === NodeKinds.FUNCTION_DECLARATION) {
          if (
            !this.#functions.add({
              name: node.name,
              params: node.params,
              type: node.type,
            })
          ) {
            throw new CompilerError(
              `Function "${node.name}" has already been declared`,
              node.offset
            );
          }
        }
      });

      ast.forEach((node) => {
        if (node.kind === NodeKinds.FUNCTION_DECLARATION) {
          this.#compileFunctionDeclaration(node);
        }
      });
    });

    const wat = this.#wat.join("\n");

    console.log(wat);

    const module = await wabt();
    const parsed = module.parseWat("module.wat", wat);
    const { buffer } = parsed.toBinary({ log: true });

    return buffer;
  }

  #write(line: string, offset?: number) {
    const indented = " ".repeat(this.#spacing).concat(line);

    if (offset) {
      this.#wat.splice(offset, 0, indented);
    } else {
      this.#wat.push(indented);
    }
  }

  #collectStringLiterals(node: AnyAstNode | AnyAstNode[] | null) {
    if (Array.isArray(node)) {
      node.forEach((n) => this.#collectStringLiterals(n));
      return;
    }

    if (!node) return;

    switch (node.kind) {
      case NodeKinds.INTERPOLATED_STRING:
        for (const part of node.parts) {
          if (!(part instanceof StringLiteral)) continue;

          if (!this.#strings.has(part.value)) {
            const length = new TextEncoder().encode(part.value).length;
            const aligned = Math.ceil(this.#offset / 4) * 4;
            this.#strings.set(part.value, {
              offset: aligned,
              length,
            });
            this.#offset = aligned + length;
          }
        }
        break;
      case NodeKinds.STRING_LITERAL:
        if (!this.#strings.has(node.value)) {
          const length = new TextEncoder().encode(node.value).length;
          const aligned = Math.ceil(this.#offset / 4) * 4;
          this.#strings.set(node.value, {
            offset: aligned,
            length,
          });
          this.#offset = aligned + length;
        }
        break;
      default:
        this.#collectStringLiterals(node.children());
    }
  }

  #getLocalDeclaration(name: string, type: Type) {
    if (isValidType(type)) {
      return [`$${name} ${WASM_TYPES[type.kind]}`];
    }
    return null;
  }

  #getParamDeclaration(name: string, type: Type) {
    if (isValidType(type)) {
      return `(param $${name} ${WASM_TYPES[type.kind]})`;
    }
    return null;
  }

  #getResultDeclaration(type: Type) {
    if (isValidType(type)) {
      return `(result ${WASM_TYPES[type.kind]})`;
    }
    return null;
  }

  #emitConst(type: NumericType, value: bigint) {
    this.#write(`(${WASM_CONSTANT_EMITS[type.kind]} ${value})`);
  }

  #emitLoad(size: OperandSize) {
    this.#write(`(${WASM_LOAD_EMITS[size]})`);
  }

  #emitStore(size: OperandSize) {
    this.#write(`(${WASM_STORE_EMITS[size]})`);
  }

  #emitLoadLocal(name: string) {
    this.#write(`(local.get $${name})`);
  }

  #emitStoreLocal(name: string) {
    this.#write(`(local.set $${name})`);
  }

  #emitBinaryOperation(type: Type, operator: BinaryOperator, offset: number) {
    const operation = WASM_BINARY_OPERATIONS[type.kind]?.[operator];

    if (!operation) {
      throw new CompilerError(
        `Type mismatch for "${operator}" operator: ${type.kind}`,
        offset
      );
    }
    this.#write(`(${operation})`);
  }

  #emitUnaryOperation(type: Type, operator: UnaryOperator, offset: number) {
    const operation = WASM_UNARY_OPERATIONS[type.kind]?.[operator];

    if (!operation) {
      throw new CompilerError(
        `Type mismatch for "${operator}" operator: ${type.kind}`,
        offset
      );
    }
    this.#write(`(${operation})`);
  }

  #compileFunctionDeclaration(node: FunctionDeclaration) {
    this.#scope = new SymbolTable();

    this.#scope.setFunction(this.#functions.get(node.name));

    this.#locals.clear();
    this.#returns.clear();

    const exportPart = node.name === "main" ? [`(export "${node.name}")`] : [];
    const paramsPart = node.params.map((param) => {
      const decl = this.#getParamDeclaration(param.name, param.type);

      if (!decl) {
        throw new CompilerError(
          `Invalid type "${param.type.kind}" for parameter declaration`,
          param.offset
        );
      }

      this.#scope!.add(param.name, param.name, param.type, param.offset);

      return decl;
    });

    this.#collectLocals(node.body);
    this.#collectReturns(node);

    const declaredResult = node.type !== BuiltInTypes.void;

    const hasResult = this.#returns.returns(node);

    if (declaredResult && !hasResult) {
      throw new CompilerError("Missing return statement", node.offset);
    }

    const resultDecl = this.#getResultDeclaration(node.type);
    const resultPart = hasResult && resultDecl ? [resultDecl] : [];

    if (hasResult && !resultPart) {
      throw new CompilerError(
        `Invalid type "${node.type}" for return type`,
        node.offset
      );
    }

    const signature = [
      `(func $${node.name}`,
      ...exportPart,
      ...paramsPart,
      ...resultPart,
    ];

    this.#block(signature.join(" "), () => {
      const start = this.#wat.length;

      node.body.forEach((stmt) => this.#compileNode(stmt));

      for (const [, local] of this.#locals.entries()) {
        const decl = this.#getLocalDeclaration(local.mangled, local.type);

        if (!decl) {
          throw new CompilerError(
            `Invalid type "${local.type}" for variable declaration`,
            local.offset
          );
        }
        this.#write(`(local ${decl})`, start);
      }
    });

    this.#scope = null;
  }

  #collectLocals(body: AnyAstNode[]) {
    body.forEach((node) => {
      if (node.kind === NodeKinds.VARIABLE_DECLARATION) {
        this.#locals.add(node.name, node.type, node.offset);
      }
      this.#collectLocals(node.children());
    });
  }

  #collectReturns(node: AnyAstNode): boolean {
    if (node.kind === NodeKinds.RETURN_STATEMENT) {
      this.#returns.set(node.offset, true);
      return true;
    }

    const branches = node.branches();

    if (branches.length > 0) {
      const all = branches.every(
        (branch) =>
          branch.length > 0 &&
          branch.every((child) => this.#collectReturns(child))
      );
      this.#returns.set(node.offset, all);
      return all;
    } else {
      const children = node.children();

      if (children.length === 0) {
        this.#returns.set(node.offset, false);
        return false;
      }

      const any = children.some((child) => this.#collectReturns(child));
      this.#returns.set(node.offset, any);
      return any;
    }
  }

  #compileNode(node: AnyAstNode) {
    switch (node.kind) {
      case NodeKinds.FIELD_ASSIGNMENT:
        this.#compileFieldAssignment(node);
        break;
      case NodeKinds.RETURN_STATEMENT:
        this.#compileReturnStatement(node);
        break;
      case NodeKinds.IF_STATEMENT:
        this.#compileIfStatement(node);
        break;
      case NodeKinds.LOOP_STATEMENT:
        this.#compileLoopStatement(node);
        break;
      case NodeKinds.EXPRESSION_STATEMENT:
        this.#compileExpressionStatement(node);
        break;
      case NodeKinds.VARIABLE_DECLARATION:
        this.#compileVariableDeclaration(node);
        break;
      case NodeKinds.VARIABLE_ASSIGNMENT:
        this.#compileVariableAssignment(node);
        break;
      default:
        throw new CompilerError(
          `Invalid node type for statement: ${node.kind}`,
          node.offset
        );
    }
  }

  #struct(object: string, member: string, offset: number) {
    if (!this.#scope) {
      throw new CompilerError("No active scope for struct resolution", offset);
    }

    const symbol = this.#scope.resolve(object);

    if (!symbol) {
      throw new CompilerError(`Struct "${object}" not found in scope`, offset);
    }

    if (symbol.type.kind !== TypeKinds.STRUCT) {
      throw new CompilerError(`Variable "${object}" is not a struct`, offset);
    }

    const struct = this.#structs.get(symbol.type.name);

    if (!struct) {
      throw new CompilerError(
        `Struct definition for "${symbol.type.name}" not found`,
        offset
      );
    }

    const field = struct.fields.get(member);

    if (!field) {
      throw new CompilerError(
        `Field "${member}" not found in struct "${struct.name}"`,
        offset
      );
    }

    this.#emitLoadLocal(symbol.mangled);
    this.#write(`(i32.const ${field.offset})`);
    this.#write("(i32.add)");

    return field;
  }

  #compileFieldAssignment(node: FieldAssignment) {
    const field = this.#struct(node.object, node.member, node.offset);
    this.#compileExpression(node.expression);
    this.#emitStore(field.size);
  }

  #compileReturnStatement(node: ReturnStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for return statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "Return statement outside of a function",
        node.offset
      );
    }

    const type = node.expression
      ? this.#getExpressionResultType(node.expression)
      : BuiltInTypes.void;

    if (type !== func.type) {
      throw new CompilerError(
        `Return type mismatch in function "${func.name}": expected ${func.type.kind}, got ${type.kind}`,
        node.offset
      );
    }

    if (node.expression) {
      this.#compileExpression(node.expression);
    }
    this.#write("(return)");
  }

  #compileIfStatement(node: IfStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for if statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "If statement outside of a function",
        node.offset
      );
    }

    const condition = this.#getExpressionResultType(node.condition);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.condition.offset
      );
    }

    const hasAlternate = node.alternate !== null;
    const hasResult = this.#returns.returns(node);
    const resultDecl = this.#getResultDeclaration(func.type);
    const resultPart =
      hasAlternate && hasResult && resultDecl ? [resultDecl] : [];

    const signature = ["(if", ...resultPart];

    this.#compileExpression(node.condition);
    this.#block(signature.join(" "), () => {
      this.#block("(then", () => {
        this.#scope = this.#scope!.enter();
        node.consequent.forEach((stmt) => this.#compileNode(stmt));
        this.#scope = this.#scope!.exit();
      });

      if (node.alternate) {
        this.#block("(else", () => {
          if (Array.isArray(node.alternate)) {
            this.#scope = this.#scope!.enter();
            node.alternate.forEach((stmt) => this.#compileNode(stmt));
            this.#scope = this.#scope!.exit();
          } else {
            this.#compileIfStatement(node.alternate!);
          }
        });
      }
    });
  }

  #compileLoopStatement(node: LoopStatement) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for if statement compilation",
        node.offset
      );
    }

    const func = this.#scope.getFunction();

    if (!func) {
      throw new CompilerError(
        "If statement outside of a function",
        node.offset
      );
    }

    if (node.declaration) {
      this.#compileNode(node.declaration);
    }

    if (node.condition) {
      const condition = this.#getExpressionResultType(node.condition);

      if (condition !== BuiltInTypes.bool) {
        throw new CompilerError(
          `Invalid type "${condition}" as conditional expression`,
          node.condition.offset
        );
      }
    }

    const identifier = `loop_${node.offset}`;
    this.#block(`(loop $${identifier}`, () => {
      this.#scope = this.#scope!.enter();
      node.body.forEach((stmt) => this.#compileNode(stmt));

      if (node.step) {
        this.#compileNode(node.step);
      }

      if (node.condition) {
        this.#compileExpression(node.condition);
        this.#write(`(br_if $${identifier})`);
      } else {
        this.#write(`(br $${identifier})`);
      }
      this.#scope = this.#scope!.exit();
    });
  }

  #compileExpressionStatement(node: ExpressionStatement) {
    this.#compileExpression(node.expression);

    if (this.#isResultExpression(node.expression)) {
      this.#write("(drop)");
    }
  }

  #compileVariableDeclaration(node: VariableDeclaration) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for variable declaration compilation",
        node.offset
      );
    }

    const local = this.#locals.get(node.name, node.offset);

    if (!local) {
      throw new CompilerError(
        `Variable "${node.name}" not found in scope during compilation`,
        node.offset
      );
    }

    this.#scope.add(local.name, local.mangled, local.type, local.offset);

    if (node.expression) {
      const type = this.#getExpressionResultType(node.expression);

      if (type !== node.type) {
        throw new CompilerError(
          `Type mismatch in assignment to "${node.name}": expected ${node.type.kind}, got ${type.kind}`,
          node.offset
        );
      }

      this.#compileExpression(node.expression);

      this.#emitStoreLocal(local.mangled);
    } else if (node.type.kind === TypeKinds.STRUCT) {
      const object = this.#structs.get(node.type.name);

      if (!object) {
        throw new CompilerError(
          `Object "${node.type.name}" not found in scope during compilation`,
          node.offset
        );
      }

      this.#write(`(i32.const ${object.size})`);
      this.#write("(call $__malloc)");

      this.#emitStoreLocal(local.mangled);
    }
  }

  #compileVariableAssignment(node: VariableAssignment) {
    const type = this.#getExpressionResultType(node.expression);

    this.#compileExpression(node.expression);

    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for variable declaration compilation",
        node.offset
      );
    }
    const symbol = this.#scope.resolve(node.name);

    if (!symbol) {
      throw new CompilerError(
        `Variable "${node.name}" not found in scope during compilation`,
        node.offset
      );
    }

    if (type !== symbol.type) {
      throw new CompilerError(
        `Type mismatch in assignment to "${node.name}": expected ${symbol.type.kind}, got ${type.kind}`,
        node.offset
      );
    }

    this.#emitStoreLocal(symbol.mangled);
  }

  #compileExpression(node: AnyAstNode) {
    switch (node.kind) {
      case NodeKinds.FIELD_EXPRESSION:
        this.#compileFieldExpression(node);
        break;
      case NodeKinds.CALL_EXPRESSION:
        this.#compileCallExpression(node);
        break;
      case NodeKinds.BINARY_EXPRESSION:
        this.#compileBinaryExpression(node);
        break;
      case NodeKinds.FIX_EXPRESSION:
        this.#compileFixExpression(node);
        break;
      case NodeKinds.UNARY_EXPRESSION:
        this.#compileUnaryExpression(node);
        break;
      case NodeKinds.TERNARY_EXPRESSION:
        this.#compileTernaryExpression(node);
        break;
      case NodeKinds.IDENTIFIER:
        this.#compileIdentifier(node);
        break;
      case NodeKinds.BOOL_LITERAL:
        this.#compileBoolLiteral(node);
        break;
      case NodeKinds.NUMBER_LITERAL:
        this.#compileNumberLiteral(node);
        break;
      case NodeKinds.STRING_LITERAL:
        this.#compileStringLiteral(node);
        break;
      case NodeKinds.INTERPOLATED_STRING:
        this.#compileInterpolatedString(node);
        break;
      case NodeKinds.FORMATTED_EXPRESSION:
        this.#compileFormattedExpression(node);
        break;
      default:
        throw new CompilerError(
          `Invalid node type for expression: ${node.kind}`,
          node.offset
        );
    }
  }

  #compileFieldExpression(node: FieldExpression) {
    const field = this.#struct(node.object, node.member, node.offset);
    this.#emitLoad(field.size);
  }

  #compileCallExpression(node: CallExpression) {
    const func = this.#functions.get(node.callee);

    if (!func) {
      throw new CompilerError(
        `Call to undeclared function: ${node.callee}`,
        node.offset
      );
    }

    // Special handling of the error function
    if (node.callee === "error" && node.args.length === 1) {
      node.args.forEach((arg, i) => {
        const type = this.#getExpressionResultType(arg);

        const param = func.params[i];

        this.#compileExpression(arg);

        if (type !== param.type) {
          throw new CompilerError(
            `Type mismatch in call to "${node.callee}": expected ${param.type.kind}, got ${type.kind} for argument ${i}`,
            arg.offset
          );
        }
      });
      this.#write(`(i32.const ${node.offset})`);
      this.#write(`(call $${node.callee})`);
      return;
    }

    if (node.args.length !== func.params.length) {
      throw new CompilerError(
        `Wrong number of arguments for function "${node.callee}"`,
        node.offset
      );
    }

    node.args.forEach((arg, i) => {
      const type = this.#getExpressionResultType(arg);

      const param = func.params[i];

      if (type !== param.type) {
        throw new CompilerError(
          `Type mismatch in call to "${node.callee}": expected ${param.type.kind}, got ${type.kind} for argument ${i}`,
          arg.offset
        );
      }

      this.#compileExpression(arg);
    });
    this.#write(`(call $${node.callee})`);
  }

  #compileBinaryExpression(node: BinaryExpression) {
    const left = this.#getExpressionResultType(node.left);
    const right = this.#getExpressionResultType(node.right);

    if (left !== right) {
      throw new CompilerError(
        `Type mismatch for "${node.operator}" operator: ${left.kind} and ${right.kind}`,
        node.offset
      );
    }

    this.#compileExpression(node.left);
    this.#compileExpression(node.right);
    this.#emitBinaryOperation(left, node.operator, node.offset);
  }

  #compileFixExpression(node: FixExpression) {
    const type = this.#getExpressionResultType(node.expression);

    if (!isNumericType(type)) {
      throw new CompilerError(
        `Type mismatch for "${node.operator}" operator: ${type.kind}`,
        node.offset
      );
    }

    switch (node.expression.kind) {
      case NodeKinds.IDENTIFIER: {
        const symbol = this.#scope!.resolve(node.expression.name);

        if (!symbol) {
          throw new CompilerError(
            `Identifier "${node.expression.name}" not found in scope`,
            node.offset
          );
        }

        const value = this.#locals.tmp(symbol.type, node.expression.offset);

        // Store the variable value in a temporary variable
        this.#emitLoadLocal(symbol.mangled);
        this.#emitStoreLocal(value);

        // Increment the variable value
        this.#emitLoadLocal(symbol.mangled);
        this.#emitConst(type, 1n);
        this.#emitBinaryOperation(type, node.operator, node.offset);
        this.#emitStoreLocal(symbol.mangled);

        if (node.prefix) {
          // Push the incremented value
          this.#emitLoadLocal(symbol.mangled);
        } else {
          // Push the stored value
          this.#emitLoadLocal(value);
        }
        break;
      }
      case NodeKinds.FIELD_EXPRESSION: {
        const field = this.#struct(
          node.expression.object,
          node.expression.member,
          node.expression.offset
        );

        const address = this.#locals.tmp(
          BuiltInTypes.int,
          node.expression.offset
        );
        const value = this.#locals.tmp(field.type, node.expression.offset);

        // Store the struct field address
        this.#emitStoreLocal(address);

        // Store the struct field value in a temporary variable
        this.#emitLoadLocal(address);
        this.#emitLoad(field.size);
        this.#emitStoreLocal(value);

        // Push the struct field address
        this.#emitLoadLocal(address);
        // Increment the struct field value
        this.#emitLoadLocal(value);
        this.#emitConst(type, 1n);
        this.#emitBinaryOperation(type, node.operator, node.offset);
        // Store the result in the struct field
        this.#emitStore(field.size);

        if (node.prefix) {
          // Push the incremented value
          this.#emitLoadLocal(address);
          this.#emitLoad(field.size);
        } else {
          // Push the stored value
          this.#emitLoadLocal(value);
        }
        break;
      }
      default:
        throw new CompilerError(
          `Type mismatch for "${node.operator}" operator: ${type.kind}`,
          node.offset
        );
    }
  }

  #compileUnaryExpression(node: UnaryExpression) {
    const condition = this.#getExpressionResultType(node.expression);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.expression.offset
      );
    }

    const type = this.#getExpressionResultType(node);

    if (!isValidType(type)) {
      throw new CompilerError(
        `Invalid result type "${type.kind}" for unary expression`,
        node.offset
      );
    }

    this.#compileExpression(node.expression);
    this.#emitUnaryOperation(type, node.operator, node.offset);
  }

  #compileTernaryExpression(node: TernaryExpression) {
    const condition = this.#getExpressionResultType(node.condition);

    if (condition !== BuiltInTypes.bool) {
      throw new CompilerError(
        `Invalid type "${condition}" as conditional expression`,
        node.condition.offset
      );
    }

    const type = this.#getExpressionResultType(node);

    if (!isValidType(type)) {
      throw new CompilerError(
        `Invalid result type "${type.kind}" for ternary expression`,
        node.offset
      );
    }

    this.#compileExpression(node.condition);
    this.#block(`(if (result ${WASM_TYPES[type.kind]})`, () => {
      this.#block("(then", () => {
        this.#compileExpression(node.consequent);
      });
      this.#block("(else", () => {
        this.#compileExpression(node.alternate);
      });
    });
  }

  #compileIdentifier(node: Identifier) {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for identifier compilation.",
        node.offset
      );
    }

    const symbol = this.#scope.resolve(node.name);

    if (!symbol) {
      throw new CompilerError(
        `Identifier '${node.name}' not found in scope.`,
        node.offset
      );
    }
    this.#emitLoadLocal(symbol.mangled);
  }

  #compileBoolLiteral(node: BoolLiteral) {
    this.#write(`(i32.const ${+node.value})`);
  }

  #compileNumberLiteral(node: NumberLiteral) {
    const type = NumericSuffixTypes[node.suffix];
    this.#emitConst(type, node.value);
  }

  #compileStringLiteral(node: StringLiteral) {
    const info = this.#strings.get(node.value);

    if (!info) {
      throw new CompilerError(
        `String literal "${node.value}" not found in collected literals`,
        node.offset
      );
    }

    this.#write(`(i32.const ${info.offset})`);
    this.#write(`(i32.const ${info.length})`);
    this.#write(`(call $__strpack)`);
  }

  #compileInterpolatedString(node: InterpolatedString) {
    node.parts.forEach((part, i) => {
      if (part.kind === NodeKinds.STRING_LITERAL) {
        this.#compileStringLiteral(part);
      } else {
        const type = this.#getExpressionResultType(part);

        if (type === BuiltInTypes.string) {
          this.#compileExpression(part);
        } else {
          this.#compileFormattedExpression(
            new FormattedExpression(part, DEFAULT_FORMAT, part.offset)
          );
        }
      }

      if (i > 0) this.#write("(call $__strcat)");
    });
  }

  #compileFormattedExpression(node: FormattedExpression) {
    const type = this.#getExpressionResultType(node.expression);

    if (!isNumericType(type)) {
      throw new CompilerError(
        `Invalid type "${type.kind}" for formatting`,
        node.offset
      );
    }

    this.#compileExpression(node.expression);

    const {
      padder = "0",
      padding,
      right = false,
      base,
      uppercase,
    } = node.format;

    this.#write(`(i32.const ${base})`);
    this.#write(`(call $${WASM_NUMERIC_FORMATTERS[type.kind]})`);

    if (padding) {
      this.#write(`(i32.const ${padder.charCodeAt(0)})`);
      this.#write(`(i32.const ${padding})`);
      this.#write(`(i32.const ${+right})`);
      this.#write("(call $__pad)");
    }

    if (uppercase) {
      this.#write("(call $__to_upper)");
    }
  }

  #isResultExpression(node: AnyAstNode): boolean {
    const type = this.#getExpressionResultType(node);
    return isValidType(type);
  }

  #getExpressionResultType(node: AnyAstNode): Type {
    if (!this.#scope) {
      throw new CompilerError(
        "No active scope for type inference",
        node.offset
      );
    }

    switch (node.kind) {
      case NodeKinds.BOOL_LITERAL:
        return BuiltInTypes.bool;
      case NodeKinds.NUMBER_LITERAL:
        return NumericSuffixTypes[node.suffix];
      case NodeKinds.STRING_LITERAL:
        return BuiltInTypes.string;
      case NodeKinds.INTERPOLATED_STRING:
        return BuiltInTypes.string;
      case NodeKinds.FORMATTED_EXPRESSION:
        return BuiltInTypes.string;
      case NodeKinds.IDENTIFIER: {
        const symbol = this.#scope.resolve(node.name);

        if (!symbol) {
          throw new CompilerError(
            `Identifier "${node.name}" not found for type inference`,
            node.offset
          );
        }
        return symbol.type;
      }
      case NodeKinds.CALL_EXPRESSION:
        const func = this.#functions.get(node.callee);

        if (!func) {
          throw new CompilerError(
            `Cannot infer return type for call to undeclared function "${node.callee}"`,
            node.offset
          );
        }
        return func.type!;
      case NodeKinds.FIELD_EXPRESSION:
        const symbol = this.#scope.resolve(node.object);

        if (!symbol) {
          throw new CompilerError(
            `Field "${node.object}" not found in scope during compilation`,
            node.offset
          );
        }

        if (symbol.type.kind !== TypeKinds.STRUCT) {
          throw new CompilerError(
            `Cannot assign to field "${node.member}" of non-object variable "${node.object}"`,
            node.offset
          );
        }

        const object = this.#structs.get(symbol.type.name);

        if (!object) {
          throw new CompilerError(
            `Object "${symbol.type.name}" not found in scope during compilation`,
            node.offset
          );
        }

        const member = object.fields.get(node.member);

        if (!member) {
          throw new CompilerError(
            `Struct "${node.object}" has no field named "${node.member}"`,
            node.offset
          );
        }
        return member.type;
      case NodeKinds.BINARY_EXPRESSION: {
        const type = this.#getExpressionResultType(node.left);

        switch (node.operator) {
          case TokenTypes.NOT_EQUALS:
          case TokenTypes.EQUALS:
          case TokenTypes.LTE:
          case TokenTypes.GTE:
          case TokenTypes.LT:
          case TokenTypes.GT:
            return BuiltInTypes.bool;
          default: {
            if (!WASM_OPERATION_OPERANDS[node.operator]?.includes(type)) {
              throw new CompilerError(
                `Invalid type for "${node.operator}" operation: ${type.kind}`,
                node.offset
              );
            }
            return type;
          }
        }
      }
      case NodeKinds.FIX_EXPRESSION: {
        return this.#getExpressionResultType(node.expression);
      }
      case NodeKinds.UNARY_EXPRESSION: {
        return this.#getExpressionResultType(node.expression);
      }
      case NodeKinds.TERNARY_EXPRESSION: {
        const left = this.#getExpressionResultType(node.consequent);
        const right = this.#getExpressionResultType(node.alternate);

        if (left !== right) {
          throw new CompilerError(
            `Type mismatch for ternary operator: ${left.kind} and ${right.kind}`,
            node.offset
          );
        }
        return left;
      }
      default:
        throw new CompilerError(
          `Cannot infer type for node: ${node.kind}`,
          node.offset
        );
    }
  }
}
