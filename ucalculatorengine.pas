{  CalcIt's Pascal port.
   Original code can be found in CalcIt's repository: https://github.com/leduyquang753/CalcIt
}

{%RunFlags MESSAGES+}
unit UCalculatorEngine;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
  Classes, SysUtils, StrUtils, UExpressionInvalidException, Operands, Functions, FGL{, DbgConsole};

type
  Variable = class
  public
    value: extended;
    constructor create(valueIn: extended);
  end;

  OpMap = specialize TFPGMapObject<String, Op>;
  VarMap = specialize TFPGMapObject<String, Variable>;
  FuncMap = specialize TFPGMapObject<String, Func>;

  NumberStack = class
  private
    values: array of extended;
  public
    constructor create;
    function peek: extended;
    procedure push(newValue: extended);
    function pop: extended;
    function isNotEmpty: boolean;
    procedure printDebug;
  end;

  OperandStack = class
  private
    values: array of Op;
  public
    constructor create;
    function peek: Op;
    function peekPriority: longint;
    procedure push(newValue: Op);
    function pop: Op;
    function isNotEmpty: boolean;
    procedure printDebug;
  end;

  Bracelet = class
  public
    opening: string;
    funcAssigned: Func;
    arguments: AoE;
    procedure pushArgument(argumentIn: extended);
    constructor create(openingIn: string; funcIn: Func);
    function getResult: extended;
    procedure printDebug;
  end;

  BraceletStack = class
  private
    values: array of Bracelet;
  public
    constructor create;
    function peek: Bracelet;
    procedure push(newValue: Bracelet);
    function pop: Bracelet;
    function isNotEmpty: boolean;
  end;

  { CalculatorEngine - The class that contains all of the calculation methods }

  CalculatorEngine = class
  public
    function calculate(expression: string): extended;
    function getVariable(variable: string): extended;
    function getVariableString(variable: string): string;
    constructor new;

  private
    // The variables.
    AtoZ: array['a'..'z'] of extended;
    ans: extended;
    preAns: extended;
    opRegistry: OpMap;
    varRegistry: VarMap;
    funcRegistry: FuncMap;

    procedure registerOperand(operand: Op);
    procedure registerFunction(funcIn: Func);
    function processNumberToken(var negativity, hadNegation, isVar, hadComma: boolean; var strIn: string; pos: longint; NS, TNS: NumberStack; OS, TOS: OperandStack): extended;
    function getVariableInternal(variable: string; pos: longint): extended;
    function performCalculation(input: string): extended;
  end;

implementation

uses Main;

resourcestring
  //msgDivByZero = 'Division by zero.';
  msgUnexpectedEnd = 'Unexpected end of expression.';
  msgUnexpectedEqual = 'Unexpected equal sign.';
  msgUnexpectedDigit = 'Unexpected digit.';
  //msgUnexpectedVariable = 'Unexpected variable.';
  //msgUnexpectedOpeningBrace = 'Unexpected opening brace.';
  msgUnexpectedClosingBrace = 'Unexpected closing brace.';
  //msgUnexpectedCharacter = 'Unexpected character.';
  msgUnmatchingBraces = 'Unmatching braces.';
  msgUnexpectedOperand = 'Unexpected operand.';
  //msgUnexpectedEqualNotOperator = 'Unexpected equal sign. Please note that = is not an operator.';
  msgUnexpectedPercent = 'Unexpected percent sign.';
  //msgUnexpectedPercentNotOperator = 'Unexpected percent sign. Please note that % is not an operator.';
  //msgUnexpectedPercentOnlyOne = 'Unexpected percent sign. Please note that only one percent sign is permitted at the end of a number.';
  //msgAndCalc = 'AND calculation only supports two integers, but given %s and %s.';
  //msgOrCalc = 'OR calculation only supports two integers, but given %s and %s.';
  //msgXorCalc = 'XOR calculation only supports two integers, but given %s and %s.';
  //msgInvalidNumbers = 'Invalid numbers: %s; %s.';
  //msgSuddenBrace = 'Sudden closing brace.';
  //msgUnmatchingBrace = 'Unmatching closing brace.';
  msgUnknownSymbol = 'Unknown symbol.';
  //msgMixedCharsAndNums = 'Mixed characters and numbers.';
  //msgInvalidOperatorPlacement = 'Invalid operator placement.';
  msgUnexpectedComma = 'Unexpected comma.';
  msgUnknownVariable = 'Unknown variable.';
  //msgUnknownStuff = 'Unknown stuff.';
  //msgAssignmentNotVariable = '%s is not a variable.';
  msgTrailingNegativePositiveSign = 'Trailing negative/positive sign(s).';
  msgNothingToCalculate = 'There is nothing to calculate and assign to the variables.';
  msgReservedVariable = 'Ans and PreAns are reserved variables and cannot be assigned.';
  msgInvalidVariable = 'Invalid variable name "%s", it must not start with a digit.';
  msgNonAlphanumericVariableName = 'Invalid variable name "%s", it must include only a÷z, 0÷9 and _ characters.';
  msgNotset = '[Not set]';                   
  msgEmptyVariableName = '[Empty variable name.]';
  msgInvalidVariableNameWindow = '[Invalid variable name.]';
  msgUnexpectedSemicolon = 'Unexpected semicolon.';
  msgUnknownFunction = 'Unknown function "%s".';

const
  positiveInfinity = 99999;
  negativeInfinity = -99999;

  rFlags = [rfReplaceAll, rfIgnoreCase];

var dotlessMulOp: DotlessMultiplication;

procedure CalculatorEngine.registerOperand(operand: Op);
var s: string;
begin
  for s in operand.characters do opRegistry.addOrSetData(s, operand);
end;

procedure CalculatorEngine.registerFunction(funcIn: func);
var s: string;
begin
  for s in funcIn.names do funcRegistry.addOrSetData(s, funcIn);
end;

constructor CalculatorEngine.new;
begin
  fillchar(AtoZ, sizeof(AtoZ), 0);
  ans := 0;
  preans := 0;
  randomize;

  dotlessMulOp := DotlessMultiplication.create;

  opRegistry := OpMap.create;
  registerOperand(Plus.create);
  registerOperand(Minus.create);
  registerOperand(Multiply.create);
  registerOperand(Divide.create);
  registerOperand(Exponentiation.create);
  registerOperand(Root.create);
  registerOperand(OpeningBrace.create);
  registerOperand(ClosingBrace.create);

  varRegistry := VarMap.create;
  varRegistry.add('pi', Variable.create(3.1415926535897932385));
  varRegistry.add('lnb', Variable.create(2.71828182845904523536));
  varRegistry.add('c0', Variable.create(299792458));

  funcRegistry := FuncMap.create;
  registerFunction(FuncSum.create);
  registerFunction(FuncSin.create);
  registerFunction(FuncCos.create);
  registerFunction(FuncTan.create);
  registerFunction(FuncCot.create);
  registerFunction(FuncArcSin.create);
  registerFunction(FuncArcCos.create);
  registerFunction(FuncArcTan.create);
  registerFunction(FuncArcCot.create);
  registerFunction(FuncFloor.create);
  registerFunction(FuncAbs.create);    
  registerFunction(FuncGCD.create);          
  registerFunction(FuncLCM.create);
  registerFunction(FuncFact.create);
  registerFunction(FuncLog.create);
  registerFunction(FuncLn.create);
  registerFunction(FuncP.create);
  registerFunction(FuncC.create);
  registerFunction(FuncRound.create);
  registerFunction(FuncDegToRad.create);
  registerFunction(FuncDegToGrad.create);
  registerFunction(FuncRadToDeg.create);
  registerFunction(FuncRadToGrad.create);
  registerFunction(FuncGradToDeg.create);
  registerFunction(FuncGradToRad.create);
  registerFunction(FuncMax.create);
  registerFunction(FuncMin.create);
  registerFunction(FuncAverage.create);
  registerFunction(FuncRandom.create);
  registerFunction(FuncRandomInt.create);
  registerFunction(FuncRandomInList.create);
  registerFunction(FuncIsGreater.create);
  registerFunction(FuncIsSmaller.create);
  registerFunction(FuncIsEqual.create);
  registerFunction(FuncIf.create);
end;

// Conditional string.
function CS(condition: boolean; value1, value2: string): string;
begin
  if condition then exit(value1) else exit(value2);
end;

// Conditional integer
function CI(condition: boolean; value1, value2: longint): longint;
begin
  if condition then exit(value1) else exit(value2);
end;

// Conditional extended
function CE(condition: boolean; value1, value2: extended): extended;
begin
  if condition then exit(value1) else exit(value2);
end;

function strInArray(stringIn: string; arrayIn: array of string): boolean;
var iterS: string;
begin
  for iterS in arrayIn do if stringIn = iterS then exit(true);
  exit(false);
end;

function charInArray(stringIn: char; arrayIn: array of char): boolean;
var iterS: string;
begin
  for iterS in arrayIn do if stringIn = iterS then exit(true);
  exit(false);
end;

function strInStr(subString, mainString: string): boolean;
begin
  exit(pos(subString, mainString) > 0);
end;

function isNumber(c: char): boolean;
begin
  exit((c>='0') and (c<='9'))
end;

function isChar(c: char): boolean;
begin
  exit(((c>='a') and (c<='z')) or (c = '_'));
end;

function areBracesMatch(opening, closing: string): boolean;
begin
  case opening of
    '(': exit(closing=')');
    '[': exit(closing=']');
    '{': exit(closing='}');
    '<': exit(closing='>');
  else exit(false);
  end;
end;

procedure performBacktrackCalculation(NS, TNS: NumberStack; OS, TOS: OperandStack; BS: BraceletStack; shouldCalculateAll: boolean);
var currentOp: Op; currentNum: extended; lastPriority: longint = positiveInfinity;
begin
  if not OS.isNotEmpty then exit;
  currentOp := OS.pop; currentNum := NS.pop;
  while shouldCalculateAll or not (currentOp is OpeningBrace) do begin
    if shouldCalculateAll and (currentOp is OpeningBrace) then begin
      while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
      lastPriority := positiveInfinity;
      if OS.isNotEmpty then currentOp := OS.pop else begin
        while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
        NS.push(currentNum);
        exit;
      end;
    end;
    if currentOp.priority <> lastPriority then while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
    if currentOp.reversed then currentNum := currentOp.calculate(NS.pop, currentNum) else begin
      TNS.push(currentNum);
      TOS.push(currentOp);
      currentNum := NS.pop;
    end;
    lastPriority := currentOp.priority;
    if OS.isNotEmpty then currentOp := OS.pop else begin
      while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
      NS.push(currentNum);
      exit;
    end;
  end;
  while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
  NS.push(currentNum);
end;

procedure performBacktrackSameLevelCalculation(NS, TNS: NumberStack; OS, TOS: OperandStack);
var currentOp: Op; currentNum: extended; lastPriority: longint;
begin
  if not OS.isNotEmpty then exit;
  currentOp := OS.pop; currentNum := NS.pop; lastPriority := currentOp.priority;
  while not (currentOp is OpeningBrace) do begin
    if currentOp.priority <> lastPriority then begin
      while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
      NS.push(currentNum);
      OS.push(currentOp);
      exit;
    end;
    if currentOp.reversed then currentNum := currentOp.calculate(NS.pop, currentNum) else begin
      TNS.push(currentNum);
      TOS.push(currentOp);
      currentNum := NS.pop;
    end;
    lastPriority := currentOp.priority;
    if OS.isNotEmpty then currentOp := OS.pop else begin
      while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
      NS.push(currentNum);
      exit;
    end;
  end;
  while TOS.isNotEmpty do currentNum := TOS.pop.calculate(currentNum, TNS.pop);
  NS.push(currentNum);
end;

function CalculatorEngine.processNumberToken(var negativity, hadNegation, isVar, hadComma: boolean; var strIn: string; pos: longint; NS, TNS: NumberStack; OS, TOS: OperandStack): extended;
var shouldDivide: boolean = false; res: extended;
begin
  if not isVar then begin
    strIn := stringReplace(strIn, ',', '.', rFlags);
    if strIn[length(strIn)] = '%' then begin
      shouldDivide := true;
      setLength(strIn, length(strIn)-1);
    end;
    res := strToFloat(strIn);
    if shouldDivide then res /= 100;
  end else begin
    res := getVariableInternal(strIn, pos);
  end;
  if negativity then begin
    NS.push(-1);
    OS.push(dotlessMulOp);
  end;
  negativity := false;
  hadNegation := false;
  hadComma := false;
  strIn := '';
  exit(res);
end;

function CalculatorEngine.performCalculation(input: string): extended;
var NS, TNS: NumberStack; OS, TOS: OperandStack; BS: BraceletStack;
    i: longint; c: char;
    status: boolean = false; // true: previous was number/closing brace; false: previous was operand/opening brace.
    negativity: boolean = false;
    hadNegation: boolean = false;
    isVar: boolean = false;
    hadClosingBrace: boolean = false;
    hadComma: boolean = false;
    currentToken: string = '';
    currentOp: Op;
    currentFunc: Func;
    currentBracelet: Bracelet;
begin
  NS := NumberStack.create;
  TNS := NumberStack.create;
  OS := OperandStack.create;
  TOS := OperandStack.create;
  BS := BraceletStack.create;
  for i:=1 to length(input) do begin
    c := input[i];
    if (c = '-') and not status then begin negativity := not negativity; hadNegation := true; end
    else if c = '%' then if not status or (currentToken[length(currentToken)] = '%') or isVar then raise ExpressionInvalidException.createNew(msgUnexpectedPercent, i)
      else currentToken += c
    else if c = ';' then begin
      if BS.isNotEmpty then begin
        if status then begin
          if length(currentToken) <> 0 then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS));
          performBacktrackCalculation(NS, TNS, OS, TOS, BS, false);
          BS.peek.pushArgument(NS.pop);
          status := false;
          hadClosingBrace := false;
        end else if OS.peek is OpeningBrace then begin
          BS.peek.pushArgument(0);
          status := false;            
          hadClosingBrace := false;
        end else raise ExpressionInvalidException.createNew(msgUnexpectedSemicolon, i);
      end else raise ExpressionInvalidException.createNew(msgUnexpectedSemicolon, i);
    end
    else if c = ',' then
      if length(currentToken) = 0 then begin
        if hadClosingBrace then begin
          if OS.isNotEmpty and (dotlessMulOp.priority < OS.peekPriority) then performBacktrackSameLevelCalculation(NS, TNS, OS, TOS);
          OS.push(dotlessMulOp);
          hadClosingBrace := false;
        end;
        currentToken := '0,';
        status := true;
        isVar := false;
        hadComma := true;
      end
      else if status then
        if isVar then raise ExpressionInvalidException.createNew(msgUnexpectedComma, i)
        else if hadComma then raise ExpressionInvalidException.createNew(msgUnexpectedComma, i)
        else begin
          currentToken += c;
          hadComma := true;
        end
      else begin end
    else if isNumber(c) then
      if length(currentToken) = 0 then begin
        if hadClosingBrace then begin
          if OS.isNotEmpty and (dotlessMulOp.priority < OS.peekPriority) then performBacktrackSameLevelCalculation(NS, TNS, OS, TOS);
          OS.push(dotlessMulOp);
          hadClosingBrace := false;
        end;
        currentToken := c;
        status := true;
        isVar := false;
      end
      else if status then
        if isVar then currentToken += c
        else if currentToken[length(currentToken)] = '%' then raise ExpressionInvalidException.createNew(msgUnexpectedDigit, i)
        else currentToken += c
      else begin
        currentToken := c;
        status := true;
        isVar := false;
      end
    else if isChar(c) then begin
      if hadClosingBrace or ((length(currentToken) <> 0) and not isVar) then begin
        if (length(currentToken) <> 0) and not isVar then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS));
        if OS.isNotEmpty and (dotlessMulOp.priority < OS.peekPriority) then performBacktrackSameLevelCalculation(NS, TNS, OS, TOS);
        OS.push(dotlessMulOp);
        hadClosingBrace := false;
      end;
      currentToken += c;
      isVar := true; status := true; hadClosingBrace := false;
    end
    else if opRegistry.tryGetData(c, currentOp) then begin
      if currentOp is OpeningBrace then begin
        if hadClosingBrace or ((length(currentToken) <> 0) and not isVar) then begin
          if (length(currentToken) <> 0) and not isVar then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS));
          if OS.isNotEmpty and (dotlessMulOp.priority < OS.peekPriority) then performBacktrackSameLevelCalculation(NS, TNS, OS, TOS);
          OS.push(dotlessMulOp);
          hadClosingBrace := false;
        end;
        if not funcRegistry.tryGetData(currentToken, currentFunc) then raise ExpressionInvalidException.createNew(format(msgUnknownFunction, [currentToken]), i-1);
        OS.push(currentOp);
        BS.push(Bracelet.create(c, currentFunc));
        status := false;
        currentToken := '';
      end
      else if currentOp is ClosingBrace then begin
        if not status then if not OS.isNotEmpty then begin
          NS.push(0);
          status := true;
          hadClosingBrace := true;
        end else if OS.peek is OpeningBrace then begin
          if BS.isNotEmpty and not areBracesMatch(BS.peek.opening, c) then raise ExpressionInvalidException.createNew(msgUnmatchingBraces, i);
          OS.pop;
          currentBracelet := BS.pop;
          currentBracelet.pushArgument(0);
          NS.push(currentBracelet.getResult);
          status := true;
          hadClosingBrace := true;
        end else raise ExpressionInvalidException.createNew(msgUnexpectedClosingBrace, i)
        else if not BS.isNotEmpty or areBracesMatch(BS.peek.opening, c) then begin
          if length(currentToken) <> 0 then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS));
          performBacktrackCalculation(NS, TNS, OS, TOS, BS, false);
          OS.pop;
          currentBracelet := BS.pop;
          currentBracelet.pushArgument(NS.pop);
          NS.push(currentBracelet.getResult);
          status := true;
          hadClosingBrace := true;
        end
        else raise ExpressionInvalidException.createNew(msgUnmatchingBraces, i);
      end
      else begin
        if status then begin
          if length(currentToken) <> 0 then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS))
          else if hadNegation then raise ExpressionInvalidException.createNew(msgUnexpectedOperand, i);
          if OS.isNotEmpty and (currentOp.priority < OS.peekPriority) then performBacktrackSameLevelCalculation(NS, TNS, OS, TOS);
          OS.push(currentOp);
          status := false;
          hadClosingBrace := false;
        end else if c = '+' then begin hadNegation := true; status := true; end else raise ExpressionInvalidException.createNew(msgUnexpectedOperand, i);
      end;
    end else raise ExpressionInvalidException.createNew(msgUnknownSymbol, i);
  end;
  if status then
    if (length(currentToken) <> 0) then NS.push(processNumberToken(negativity, hadNegation, isVar, hadComma, currentToken, i, NS, TNS, OS, TOS))
    else if hadNegation then raise ExpressionInvalidException.createNew(msgTrailingNegativePositiveSign)
    else begin end
  else raise ExpressionInvalidException.createNew(msgUnexpectedEnd);
  while BS.isNotEmpty do begin
    performBacktrackCalculation(NS, TNS, OS, TOS, BS, false);
    currentBracelet := BS.pop;
    currentBracelet.pushArgument(NS.pop);
    NS.push(currentBracelet.getResult);
  end;
  performBacktrackCalculation(NS, TNS, OS, TOS, BS, true);
  exit(NS.pop);
end;

function CalculatorEngine.calculate(expression: string): extended;
var
  input, s: string;
  oldAns: extended;
  toAssign: array of string;
  ps: longint;
  c: char;
begin
  input := lowercase(stringReplace(stringReplace(stringReplace(expression, ' ', '', rFlags), '        ', '', rFlags), sLineBreak, '', rFlags));
  setlength(toAssign, 0);
  while true do begin
    ps := pos('=', input);
    if ps = 0 then break;
    if ps = 1 then raise ExpressionInvalidException.createNew(msgUnexpectedEqual);
    if ps = 2 then begin
      if not isChar(input[1]) then raise ExpressionInvalidException.createNew(format(msgInvalidVariable, [input[1]]));
      setlength(toAssign, length(toAssign)+1);
      toAssign[length(toAssign)-1] := input[1];
    end else begin
      s := copy(input, 1, ps-1);
      if (s = 'ans') or (s = 'preans') then raise ExpressionInvalidException.createNew(msgReservedVariable);
      if isNumber(s[1]) then raise ExpressionInvalidException.createNew(format(msgInvalidVariable, [s]));
      for c in s do if not isChar(c) and not isNumber(c) then raise ExpressionInvalidException.createNew(format(msgNonAlphanumericVariableName, [s]));
      setlength(toAssign, length(toAssign)+1);
      toAssign[length(toAssign)-1] := s;
    end;
    delete(input, 1, ps);
  end;
  if length(input) = 0 then raise ExpressionInvalidException.createNew(msgNothingToCalculate);
  if input = '!' then begin
    for s in toAssign do if length(s) = 1 then AtoZ[s[1]] := 0 else varRegistry.remove(s);
    exit(0);
    preAns := ans;
    ans := 0;
  end;
  oldAns := self.ans;
  ans := self.performCalculation(input);
  for s in toAssign do
    if length(s) = 1 then AtoZ[s[1]] := ans else varRegistry.addOrSetData(s, Variable.create(ans));
  self.preAns := oldAns;
  exit(ans);
end;

function CalculatorEngine.getVariable(variable: string): extended;
begin
  variable := lowerCase(variable);
  case variable of
    'PreAns': exit(PreAns);
    'Ans': exit(Ans);
  else if (length(variable) = 1) and (variable[1] >= 'a') and (variable[1] <= 'z') then exit(AtoZ[variable[1]])
  end;
  exit(0);
end;

function CalculatorEngine.getVariableString(variable: string): string;
var p: variable; c: char;
begin
  variable := lowercase(variable);
  if length(variable) = 0 then exit(msgEmptyVariableName);
  if isNumber(variable[1]) then exit(msgInvalidVariableNameWindow);
  for c in variable do if not isNumber(c) and not isChar(c) then exit(msgInvalidVariableNameWindow);
  if (length(variable) = 1) and (variable[1] >= 'a') and (variable[1] <= 'z') then exit(formatNumber(AtoZ[variable[1]]));
  case variable of
    'ans': exit(formatNumber(ans));
    'preans': exit(formatNumber(preans));
  end;
  if varRegistry.tryGetData(variable, p) then exit(formatNumber(p.value));
  exit(msgNotSet);
end;

function CalculatorEngine.getVariableInternal(variable: string; pos: longint): extended;
var p: Variable;
begin
  if (length(variable) = 1) and (variable[1] >= 'a') and (variable[1] <= 'z') then exit(AtoZ[variable[1]]);
  case variable of
    'ans': exit(ans);
    'preans': exit(preans);
  end;
  if varRegistry.tryGetData(variable, p) then exit(p.value);
  raise ExpressionInvalidException.createNew(msgUnknownVariable, pos);
end;

{ STACK IMPLEMENTATIONS }
// NumberStack
constructor NumberStack.create;
begin
  setlength(values, 0);
end;

function NumberStack.peek: extended;
begin
  if length(values) = 0 then exit(0) else exit(values[length(values)-1]);
end;

procedure NumberStack.push(newValue: extended);
begin
  setlength(values, length(values)+1);
  values[length(values)-1] := newValue;
end;

function NumberStack.pop: extended;
var oldValue: extended;
begin
  if length(values) = 0 then exit(0);
  oldValue := values[length(values)-1];
  setlength(values, length(values)-1);
  exit(oldValue);
end;

function NumberStack.isNotEmpty: boolean;
begin
  exit(length(values) <> 0);
end;

procedure NumberStack.printDebug;
var n: extended; s: string = '';
begin
  for n in values do s += floattoStr(n) + ' ';
  mainWindow.Console.Append(s);
end;

// OperandStack
constructor OperandStack.create;
begin
  setlength(values, 0);
end;

function OperandStack.peek: Op;
begin
  if length(values) = 0 then exit(nil) else exit(values[length(values)-1]);
end;

function OperandStack.peekPriority: longint;
begin
  if length(values) = 0 then exit(negativeInfinity) else exit(values[length(values)-1].priority);
end;

procedure OperandStack.push(newValue: Op);
begin
  setlength(values, length(values)+1);
  values[length(values)-1] := newValue;
end;

function OperandStack.pop: Op;
var oldValue: Op;
begin
  if length(values) = 0 then exit(nil);
  oldValue := values[length(values)-1];
  setlength(values, length(values)-1);
  exit(oldValue);
end;

function OperandStack.isNotEmpty: boolean;
begin
  exit(length(values) <> 0);
end;

procedure OperandStack.printDebug;
var n: Op; s: string = '';
begin
  for n in values do s += n.characters[0] + ' ';
  mainWindow.Console.Append(s);
end;

// BraceletStack
constructor BraceletStack.create;
begin
  setlength(values, 0);
end;

function BraceletStack.peek: Bracelet;
begin
  if length(values) = 0 then exit(nil) else exit(values[length(values)-1]);
end;

procedure BraceletStack.push(newValue: Bracelet);
begin
  setlength(values, length(values)+1);
  values[length(values)-1] := newValue;
end;

function BraceletStack.pop: Bracelet;
var oldValue: Bracelet;
begin
  if length(values) = 0 then exit(nil);
  oldValue := values[length(values)-1];
  setlength(values, length(values)-1);
  exit(oldValue);
end;

function BraceletStack.isNotEmpty: boolean;
begin
  exit(length(values) <> 0);
end;

// Bracelet
constructor Bracelet.create(openingIn: string; funcIn: Func);
begin
  opening := openingIn;
  funcAssigned := funcIn;
  setlength(arguments, 0);
end;

procedure Bracelet.pushArgument(argumentIn: extended);
begin
  setlength(arguments, length(arguments)+1);
  arguments[length(arguments)-1] := argumentIn;
end;

function Bracelet.getResult: extended;
begin
  exit(funcAssigned.calculate(arguments));
end;

procedure Bracelet.printDebug;
var n: extended; s: string = '';
begin
  for n in arguments do s += floatToStr(n) + ' ';
  mainWindow.Console.Append(s);
end;

// Variable
constructor Variable.create(valueIn: extended);
begin
  value := valueIn;
end;

end.

