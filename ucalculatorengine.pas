{  CalcIt's Pascal port.
   Original code can be found in CalcIt's repository: https://github.com/leduyquang753/CalcIt
}

unit UCalculatorEngine;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
  Classes, SysUtils, StrUtils, UExpressionInvalidException, Math, Crt{, DbgConsole};

type

  { CalculatorEngine - The class that contains all of the calculation methods }

  CalculatorEngine = class
    function calculate(expression: string): extended;
    function getVariable(variable: integer): extended;
    constructor new;

  private
    // The variables.
    a: extended;
    b: extended;
    c: extended;
    d: extended;
    e: extended;
    f: extended;
    ans: extended;
    preAns: extended;
    function funcSplitExpression(expression: string): TStringList;
    function calculate(splitExpression: TStringList; variable: byte): extended;
    function calculate(splitExpression: TStringList; openingBrace: string): extended;
  end;

implementation

resourcestring
  msgDivByZero = 'Division by zero.';
  msgUnexpectedEnd = 'Unexpected end of expression.';
  msgUnexpectedEqual = 'Unexpected equal sign.';
  //msgUnexpectedEqualNotOperator = 'Unexpected equal sign. Please note that = is not an operator.';
  msgUnexpectedPercent = 'Unexpected percent sign.';
  msgUnexpectedPercentNotOperator = 'Unexpected percent sign. Please note that % is not an operator.';
  msgUnexpectedPercentOnlyOne = 'Unexpected percent sign. Please note that only one percent sign is permitted at the end of a number.';
  msgAndCalc = 'AND calculation only supports two integers, but given %s and %s.';
  msgOrCalc = 'OR calculation only supports two integers, but given %s and %s.';
  msgXorCalc = 'XOR calculation only supports two integers, but given %s and %s.';
  msgInvalidNumbers = 'Invalid numbers: %s; %s.';
  msgUnknownFunc = 'Unknown function.';
  msgSuddenBrace = 'Sudden closing brace.';
  msgUnmatchingBrace = 'Unmatching closing brace.';
  msgUnknownSymbol = 'Unknown symbol.';
  msgMixedCharsAndNums = 'Mixed characters and numbers.';
  msgInvalidOperatorPlacement = 'Invalid operator placement.';
  msgMultipleCommas = 'Multiple commas in the same number.';
  msgUnknownVar = 'Unknown variable.';
  msgUnknownStuff = 'Unknown stuff.';

const
  // These thing are used for the parser not to treat these as invalid.
  functions    : array [0..5] of string = ( 'sin', 'cos', 'tan', 'cot', 'log', 'ln');
  openingBraces: array [0..2] of char   = ( '(', '[', '{' );
  closingBraces: array [0..2] of char   = ( ')', ']', '}' );
  variables    : array[0..11] of string = ( 'a', 'b', 'c', 'd', 'e', 'f', '-a', '-b', '-c', '-d', '-e', '-f' );
  ansVars      : array [0..1] of string = ( 'ans', 'preans' );

  rFlags = [rfReplaceAll, rfIgnoreCase];

constructor CalculatorEngine.new;
begin
  a := 0;
  b := 0;
  c := 0;
  d := 0;
  e := 0;
  f := 0;
  ans := 0;
  preans := 0;
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

// Calculates the given expresion.
function calculateIndividual(param1, operand, param2: string): extended;
var number1, number2: extended; int1, int2: int64;
begin
  try
    number1 := strToFloat(stringReplace(CS(param1 = '', '0', copy(param1, 1, param1.length - CI(ansiEndsStr('%', param1), 1, 0))), ',', '.', rFlags)) / CI(ansiEndsStr('%', param1), 100, 1);
    number2 := strToFloat(stringReplace(CS(param2 = '', '0', copy(param2, 1, param2.length - CI(ansiEndsStr('%', param2), 1, 0))), ',', '.', rFlags)) / CI(ansiEndsStr('%', param2), 100, 1);
  except on Exception do raise ExpressionInvalidException.createNew(format(msgInvalidNumbers, [param1, param2]));
  end;

  case operand of
    '+': exit(number1 + number2);
    '-': exit(number1 - number2);
    '.': exit(number1 * number2);
    ':': begin
           if number2 = 0 then raise ExpressionInvalidException.createNew(msgDivByZero);
           exit(number1 / number2);
         end;
    '^': begin
           if number2 < 0 then
           else exit(power(number1, number2));
         end;
    'v': begin
           if number2 < 0 then
           else exit(power(number2, 1/number1));
         end;
    '&': begin
           try
             int1 := strtoint64(param1);
             int2 := strtoint64(param2);
             exit(int1 and int2);
           except on Exception do raise ExpressionInvalidException.createNew(format(msgAndCalc, [param1, param2]));
           end;
         end;
    '|': begin
           try
             int1 := strtoint64(param1);
             int2 := strtoint64(param2);
             exit(int1 or int2);
           except on Exception do raise ExpressionInvalidException.createNew(format(msgOrCalc, [param1, param2]));
           end;
         end;
    '!': begin
           try
             int1 := strtoint64(param1);
             int2 := strtoint64(param2);
             exit(int1 xor int2);
           except on Exception do raise ExpressionInvalidException.createNew(format(msgXorCalc, [param1, param2]));
           end;
         end;
     else exit(0);
  end;
end;

// Retrieves the operator's value. Used to determine which operation should be done first.
function getOperatorValue(operand: string): byte;
begin
  case operand of
    '|', '!': exit(1);
    '&'     : exit(2);
    '+', '-': exit(3);
    '.', ':': exit(4);
    '^'     : exit(5);
    'v'     : exit(6);
    else      exit(0);
  end;
end;

function isNumber(c: char): boolean;
begin
  exit((c>='0') and (c<='9'))
end;

function testBrace(c: char): byte;
begin
  if charInArray(c, openingBraces) then exit(1);
  if charInArray(c, closingBraces) then exit(2);
  exit(0);
end;

function getOpeningBrace(c: char): char;
var j:byte;
begin
  for j:=0 to 2 do begin
    if closingBraces[j] = c then exit(openingBraces[j]);
  end;
  exit('(');
end;

function getClosingBrace(c: char): char;
var j:byte;
begin
  for j:=0 to 2 do begin
    if openingBraces[j] = c then exit(closingBraces[j]);
  end;
  exit(')');
end;

function CalculatorEngine.funcSplitExpression(expression: string): TStringList;
var
  resultList: TStringList;
  parsed: string = '';
  current: string = '';
  // The current braces that are open.
  braces: string = '';

  // Whether something else from equal sign has been parsed. Used to prevent unexpected equal signs in the middle of the expression.
  metElse: boolean = false;
  // Whether the previous is a number.
  hadNumber: boolean = false;
  // Whether the previous is a sign.
  hadSign: boolean = true;
  // Whether the previous is a closing brace.
  hadClosingBrace: boolean = false;
  // Whether a comma is parsed in a number.
  metComma: boolean = false;
  // The current sign of the current number. true is positive; false is negative.
  sign: boolean = true;

  currentPos, iter: longint;
  charac: char;

begin
  resultList := TStringList.Create;

  // Scan every character in the expression.
  for currentPos:=1 to expression.length do begin
    charac := expression[currentPos];
    parsed += charac;

    case testBrace(charac) of
      // Opening brace
      1: begin
           if (current <> '') and (not strInArray(current, functions)) then raise ExpressionInvalidException.createNew(msgUnknownFunc, currentPos);
           resultList.add(CS(sign, '', '-') + current + charac);
           braces += charac;
           current := '';
           hadNumber := false;
           hadClosingBrace := false;
           sign := true;
           hadSign := true;
           metComma := false;
           metElse := true;
           continue;
         end;

      // Closing brace
      2: begin
           if braces.Length = 0 then raise ExpressionInvalidException.createNew(msgSuddenBrace, currentPos);
           if not ansiEndsStr(getOpeningBrace(charac) + '', braces) then raise ExpressionInvalidException.createNew(msgUnmatchingBrace, currentPos);
           if (current <> '') and (not hadNumber) and (current <> 'pi') and (not strInArray(current, variables)) and (not strInArray(current, ansVars)) then
             raise ExpressionInvalidException.createNew(msgUnknownSymbol, currentPos);
           if hadNumber or (strInArray(current, variables)) or (strInArray(current, ansVars)) then resultList.Add(CS(sign, '', '-') + current)
             else if not hadClosingBrace then resultList.add(current);
           resultList.add('' + charac);
           setLength(braces, braces.length-1);
           current := '';
           hadNumber := false;
           metElse := true;
           hadClosingBrace := true;
           metComma := false;
           hadSign := true;
           sign := true;
           continue;
         end;
    end;
    if isNumber(charac) then begin
      if (not hadNumber) and (current <> '') and (not strInArray(current, variables)) and (not strInArray(current, ansVars)) then raise ExpressionInvalidException.createNew(msgMixedCharsAndNums, currentPos);
      if ansiEndsStr('%', current) then raise ExpressionInvalidException.createNew(msgUnexpectedPercentNotOperator, currentPos);
      hadNumber := true;
      metElse := true;
      hadSign := false;
      hadClosingBrace := false;
      current += charac;
      continue;
    end;
    case charac of
      '+': begin
             if hadNumber or (current = 'pi') or (strInArray(current, variables)) or (strInArray(current, ansVars)) then begin
               resultList.add(CS(sign, '', '-') + current);
               sign := true;
               current := '';
               resultList.add('+');
               hadNumber := false;
               metElse := true;
               hadClosingBrace := false;
               hadSign := true;
               metComma := false;
               continue;
             end else if hadClosingBrace then begin
               sign := true;
               current := '';
               resultList.add('+');
               hadNumber := false;
               metElse := true;
               hadClosingBrace := false;
               hadSign := true;
               metComma := false;
               continue;
             end else if not hadSign then raise ExpressionInvalidException.createNew(msgInvalidOperatorPlacement, currentPos);
             continue;
           end;
      '-': begin
             if hadNumber or (current = 'pi') or (strInArray(current, variables)) or (strInArray(current, ansVars)) then begin
               resultList.add(CS(sign, '', '-') + current);
               sign := true;
               current := '';
               resultList.add('-');
               hadNumber := false;
               metElse := true;
               hadClosingBrace := false;
               hadSign := true;
               metComma := false;
               continue;
             end else if hadClosingBrace then begin
               sign := true;
               current := '';
               resultList.add('-');
               hadNumber := false;
               metElse := true;
               hadClosingBrace := false;
               hadSign := true;
               metComma := false;
               continue;
             end else if hadSign then begin
               sign := not sign;
               continue;
             end else raise ExpressionInvalidException.createNew(msgInvalidOperatorPlacement, currentPos);
           end;
      '.', ':', '^', 'v', '|', '&', '!':
        begin
          if hadNumber or (current = 'pi') or (strInArray(current, variables)) or (strInArray(current, ansVars)) then begin
            resultList.add(CS(sign, '', '-') + current);
            sign := true;
            current := '';
            resultList.add(charac);
            hadNumber := false;
            metElse := true;
            hadClosingBrace := false;
            hadSign := true;
            metComma := false;
            continue;
          end else if hadClosingBrace then begin
            sign := true;
            resultList.add(charac);
            hadNumber := false;
            metElse := true;
            hadClosingBrace := false;
            hadSign := true;
            metComma := false;
            continue;
          end else raise ExpressionInvalidException.createNew(msgInvalidOperatorPlacement, currentPos);
        end;
      ',': begin
             if metComma then raise ExpressionInvalidException.createNew(msgMultipleCommas, currentPos)
             else begin
               metComma := true;
               current += charac;
             end;
           end;
      '%': begin
             if current = '' then raise ExpressionInvalidException.createNew(msgUnexpectedPercent, currentPos);
             if (not hadNumber) and (current <> '') and (current <> 'pi') and (not strInArray(current, variables)) and (not strInArray(current, ansVars)) then raise ExpressionInvalidException.createNew('Unknown stuff', currentPos);
             if strInStr('%', current) then raise ExpressionInvalidException.createNew(msgUnexpectedPercentOnlyOne, currentPos);
             hadNumber := true;
             hadSign := false;
             hadClosingBrace := false;
             current += charac;
             continue;
           end;
      '=': begin
             if metElse then raise ExpressionInvalidException.createNew(msgUnexpectedEqual, currentPos)
             else begin
               if not strInArray(current, variables) then raise ExpressionInvalidException.createNew(msgUnknownVar, currentPos);
               resultList.add(current + '=');
               hadSign := true;
               current := '';
               continue;
             end;
           end;
      else begin
        if hadNumber then raise ExpressionInvalidException.createNew(msgMixedCharsAndNums, currentPos);
        hadNumber := false;
        hadSign := false;
        hadClosingBrace := false;
        current += charac;
        continue;
      end;
    end;
  end;

  // The final checks for the final component.
  if hadSign and (not hadClosingBrace) then raise ExpressionInvalidException.createNew(msgUnexpectedEnd, currentPos);
  if (not hadNumber) and (current <> '') and (current <> 'pi') and (not strInArray(current, variables)) and (not strInArray(current, ansVars)) then raise ExpressionInvalidException.createNew(msgUnknownStuff, currentPos);
  if current <> '' then begin
    resultList.add(CS((hadNumber or (strInArray(current, variables)) or (strInArray(current, ansVars))) and (not sign), '-', '') + current);
  end;
  if braces <> '' then for iter := braces.length downto 1 do resultList.add('' + getClosingBrace(braces[iter]));
  exit(resultList);
end;

function trimFirst(list: TStringList): TStringList;
var iter: longint; resultValue: TStringList;
begin
  resultValue := TStringList.Create;
  for iter := 1 to list.Count-1 do resultValue.add(list.strings[iter]);
  exit(resultValue);
end;

//function CalculatorEngine.calculate(splitExpression: TStringList; variable: byte): extended; forward;

function CalculatorEngine.calculate(splitExpression: TStringList; openingBrace: string): extended;
var
  // The current expression.
  input,
  // The copy of the expression to be modified to prevent concurrent modification to input.
  modified,
  sub: TStringList;
  s, s2, opening, str, toCheck: string;
  j, k, tmpc: longint;
  toAdd, outputValue, individual, sign, sinVal, cosVal: extended;

  // The current position, used for tracing the bracelets.
  currentPos,
  // The most important operator position to be calculated.
  maxPos,
  // The current max operator value.
  max,
  // The location of the first opening brace, if any.
  beginPos,
  // Number of sub-braces inside the outmost bracelet.
  subBraces: longint;

  // Whether it is seeking for the matching closing brace to get the bracelet to calculate.
  seeking,
  // Whether the current component is an operator.
  operand,
  matched,
  beginsWithMinus,
  hasMinusSign,
  endsWithPercent: boolean;

  value: byte;

begin
  // Check whether this is an assignment.
  if ansiEndsStr('=', splitExpression.Strings[0]) then begin
    case splitExpression.Strings[0][1] of
      'a': exit(self.calculate(trimFirst(splitExpression), 1));
      'b': exit(self.calculate(trimFirst(splitExpression), 2));
      'c': exit(self.calculate(trimFirst(splitExpression), 3));
      'd': exit(self.calculate(trimFirst(splitExpression), 4));
      'e': exit(self.calculate(trimFirst(splitExpression), 5));
      'f': exit(self.calculate(trimFirst(splitExpression), 6));
    end;
    exit(0);
  end;

  input := splitExpression;
  modified := nil;
  sub := nil;

  while true do begin
    if modified <> nil then input := modified;
    modified := TStringList.create;
    for tmpc := 0 to input.count-1 do modified.add(input.strings[tmpc]);
    currentPos := -1;
    maxPos := -1;
    max := 0;
    beginPos := -1;
    subBraces := -1;
    operand := true;
    seeking := false;

    // Scan every component in the expression.
    for tmpc := 0 to input.Count-1 do begin
      s := input.strings[tmpc];
      currentPos += 1;

      if seeking then begin
        if strInStr('(', s) or strInStr('[', s) or strInStr('{', s) then subBraces += 1
        else if strInStr(')', s) or strInStr(']', s) or strInStr('}', s) then begin
          subBraces -= 1;
          if subBraces = 0 then begin
            opening := modified.strings[beginPos];
            for j:=0 to currentPos-beginPos do modified.delete(beginPos);
            modified.insert(beginPos, stringReplace(floatToStr(self.calculate(sub, opening)), '.', ',', rFlags));
            seeking := false;
            break;
          end;
        end;
        sub.add(s);
        continue;
      end;

      // Replace the variable / constant to its value, if the component is it.
      matched := false;
      s2 := CS(s = '', '0', s);
      beginsWithMinus := s2[1] = '-';
      endsWithPercent := ansiEndsStr('%', s2);
      toCheck := CS(beginsWithMinus, copy(s2, 2, s2.length), s);
      toCheck := CS(endsWithPercent, copy(toCheck, 1, toCheck.length-1), toCheck);
      toAdd := 0;
      case toCheck of
        'ans': begin toAdd := self.ans; matched := true; end;
        'preans': begin toAdd := self.preAns; matched := true; end;
        'a': begin toAdd := self.a; matched := true; end;
        'b': begin toAdd := self.b; matched := true; end;
        'c': begin toAdd := self.c; matched := true; end;
        'd': begin toAdd := self.d; matched := true; end;
        'e': begin toAdd := self.e; matched := true; end;
        'f': begin toAdd := self.f; matched := true; end;
        'pi': begin toAdd := pi; matched := true; end;
      end;
      if beginsWithMinus then toAdd := -toAdd;
      if endsWithPercent then toAdd /= 100;
      if matched then begin
        modified.delete(currentPos);
        modified.insert(currentPos, stringReplace(floatToStr(toAdd), '.', ',', rFlags));
      end;
      if strInStr('(', s2) or strInStr('[', s2) or strInStr('{', s2) then begin
        sub := TStringList.create;
        beginPos := currentPos;
        seeking := true;
        subBraces := 1;
        continue;
      end;
      operand := not operand;
      if operand then begin
        value := getOperatorValue(s2[1]);
        if value > max then begin
          max := value;
          maxPos := currentPos;
        end;
      end;
    end;

    if currentPos = 0 then begin
      // The expression contains only one number. Perform the function (if any) and then return the result.
      str := stringReplace(modified.strings[0], ',', '.', rFlags);
      outputValue := strToFloat(stringReplace(CS(str = '', '0', copy(str, 1, str.length - CI(ansiEndsStr('%', str), 1, 0))), ',', '.', rFlags)) / CI(ansiEndsStr('%', str), 100, 1);
      if openingBrace.length < 2 then exit(outputValue);
      hasMinusSign := openingBrace[1] = '-';
      sign := CE(hasMinusSign, -1, 1);
      sincos(degtorad(outputValue*sign), sinVal, cosVal);
      case copy(openingBrace, CI(hasMinusSign, 2, 1), openingBrace.length - CI(hasMinusSign, 2, 1)) of
        'sin': exit(sinVal);
        'cos': exit(cosVal);
        'tan': exit(sinVal/cosVal);
        'cot': exit(cosVal/sinVal);
        'log': exit(log10(outputValue*sign));
        'ln' : exit(ln   (outputValue*sign));
      end;
      exit(outputValue*sign);
    end;
    if maxPos <> -1 then begin
      individual := calculateIndividual(modified.strings[maxPos-1], modified.strings[maxPos], modified.strings[maxPos+1]);
      for k := 1 to 3 do modified.delete(maxPos-1);
      modified.insert(maxpos-1, stringReplace(floatToStr(individual), '.', ',', rFlags));
    end;
  end;
end;

function CalculatorEngine.calculate(splitExpression: TStringList; variable: byte): extended;
var res: extended = 0;
begin
  res := self.calculate(splitExpression, '');
  case variable of
    1: self.a := res;
    2: self.b := res;
    3: self.c := res;
    4: self.d := res;
    5: self.e := res;
    6: self.f := res;
  end;
  exit(res);
end;

function CalculatorEngine.calculate(expression: string): extended;
var
  input: string;
  oldAns: extended;
  split: TStringList;

begin
  input := lowercase(stringReplace(stringReplace(stringReplace(expression, ' ', '', rFlags), '        ', '', rFlags), sLineBreak, '', rFlags));
  split := funcSplitExpression(input);
  oldAns := self.ans;
  ans := self.calculate(split, '');
  self.preAns := oldAns;
  exit(ans);
end;

function CalculatorEngine.getVariable(variable: integer): extended;
begin
  case variable of
    -1: exit(self.preAns);
     0: exit(self.ans);
     1: exit(self.a);
     2: exit(self.b);
     3: exit(self.c);
     4: exit(self.d);
     5: exit(self.e);
     6: exit(self.f);
  end;
  exit(0);
end;

end.
