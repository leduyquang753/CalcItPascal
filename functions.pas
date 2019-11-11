unit Functions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UExpressionInvalidException, Math;

type
  AoS = array of string;
  AoE = array of extended;

  {== The base function class ==}
  Func = class
  public
    names: AoS;
    function calculate(arguments: AoE): extended; virtual; abstract;
  end;

  {== Actual function classes ==}
  FuncSum = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncSin = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncCos = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncTan = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncCot = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;  
  FuncArcSin = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncArcCos = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncArcTan = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncArcCot = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end; 
  FuncFloor = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncAbs = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncGCD = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncLCM = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncFact = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncLog = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncLn = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncP = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncC = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;  
  FuncRound = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncDegToRad = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncDegToGrad = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncRadToDeg = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncRadToGrad = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncGradToDeg = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncGradToRad = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end; 
  FuncMax = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncMin = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncAverage = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncRandom = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncRandomInt = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncRandomInList = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncIsGreater = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncIsSmaller = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncIsEqual = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;
  FuncIf = class(Func) constructor create; function calculate(arguments: AoE): extended; override; end;

resourcestring
  msgDivByZero = 'Division by zero.';
  msgInvalidFunctionArgument = 'Invalid function argument.';
  msgInvalidFactorial = 'Factorial''s argument must not be negative.';
  msgInvalidLogarithmNonPositiveInput = 'Logarithm''s input must be positive.';   
  msgInvalidLogarithmNonPositiveBase = 'Logarithm''s base must be positive.';
  msgPermutationWrongNumberOfParameters = 'Wrong number of arguments for permutation function, there must be (only) two.';
  msgCombinationWrongNumberOfParameters = 'Wrong number of arguments for combination function, there must be (only) two.';
  msgPermutationNegativeParameters = 'Permutation function''s arguments must not be negative.';             
  msgCombinationNegativeParameters = 'Combination function''s arguments must not be negative.';
  msgRandomInvalidNumberOfArguments = 'Wrong number of arguments for random function, there must be at most two.';
  msgComparisonInvalidNumberOfArguments = 'Wrong number of arguments for comparison function, there must be (only) two.'; 
  msgIsEqualInvalidNumberOfArguments = 'Wrong number of arguments for equation checking function, there must be at least two.';
  msgIfInvalidNumberOfArguments = 'Wrong number of arguments for conditional function, there must be at most three.';

implementation

uses Main;

// FuncSum
constructor FuncSum.create;
begin
  setlength(names, 3);
  names[0] := '';
  names[1] := 'sum';
  names[2] := 'total';
end;

function FuncSum.calculate(arguments: AoE): extended;
var s, arg: extended;
begin               
  s := 0;
  for arg in arguments do s += arg;
  exit(s);
end;

// FuncSin
constructor FuncSin.create;
begin
  setlength(names, 2);
  names[0] := 'sin';
  names[1] := 'sine';
end;

function FuncSin.calculate(arguments: AoE): extended;
var s, arg: extended;
begin              
  s := 0;
  for arg in arguments do s += arg;
  exit(sin(degtorad(s)));
end;

// FuncCos
constructor FuncCos.create;
begin
  setlength(names, 2);
  names[0] := 'cos';
  names[1] := 'cosine';
end;

function FuncCos.calculate(arguments: AoE): extended;
var s, arg: extended;
begin             
  s := 0;
  for arg in arguments do s += arg;
  exit(cos(degtorad(s)));
end;

// FuncTan
constructor FuncTan.create;
begin
  setlength(names, 4);
  names[0] := 'tan';
  names[1] := 'tangent';
  names[2] := 'tang';
  names[3] := 'tg';
end;

function FuncTan.calculate(arguments: AoE): extended;
var s, arg: extended;
begin              
  s := 0;
  for arg in arguments do s += arg;
  if cos(degtorad(s)) = 0 then raise ExpressionInvalidException.createNew(msgDivByZero);
  exit(tan(degtorad(s)));
end;     

// FuncCot
constructor FuncCot.create;
begin
  setlength(names, 4);
  names[0] := 'cot';
  names[1] := 'cotg';
  names[2] := 'cotan';
  names[3] := 'cotangent';
end;

function FuncCot.calculate(arguments: AoE): extended;
var s, arg: extended;
begin                        
  s := 0;
  for arg in arguments do s += arg;
  if sin(degtorad(s)) = 0 then raise ExpressionInvalidException.createNew(msgDivByZero);
  exit(1/tan(degtorad(s)));
end;

// FuncSin
constructor FuncArcSin.create;
begin
  setlength(names, 4);
  names[0] := 'arcsin';
  names[1] := 'arcsine';
  names[2] := 'sin_1';
  names[3] := 'sine_1';
end;

function FuncArcSin.calculate(arguments: AoE): extended;
var s, arg: extended;
begin                                   
  s := 0;
  for arg in arguments do s += arg;
  if (s > 1) or (s < -1) then raise ExpressionInvalidException.createNew(msgInvalidFunctionArgument);
  exit(radtodeg(arcsin(s)));
end;

// FuncArcCos
constructor FuncArcCos.create;
begin
  setlength(names, 4);
  names[0] := 'arccos';
  names[1] := 'arccosine';
  names[2] := 'cos_1';
  names[3] := 'cosine_1';
end;

function FuncArcCos.calculate(arguments: AoE): extended;
var s, arg: extended;
begin                                
  s := 0;
  for arg in arguments do s += arg;
  if (s > 1) or (s < -1) then raise ExpressionInvalidException.createNew(msgInvalidFunctionArgument);
  exit(radtodeg(arccos(s)));
end;

// FuncArcTan
constructor FuncArcTan.create;
begin
  setlength(names, 8);
  names[0] := 'arctan';
  names[1] := 'arctangent';
  names[2] := 'arctang';
  names[3] := 'arctg';
  names[4] := 'tan_1';
  names[5] := 'tangent_1';
  names[6] := 'tang_1';
  names[7] := 'tg_1';
end;

function FuncArcTan.calculate(arguments: AoE): extended;
var s, arg: extended;
begin                             
  s := 0;
  for arg in arguments do s += arg;
  exit(radtodeg(arctan(s)));
end;

// FuncArcCot
constructor FuncArcCot.create;
begin
  setlength(names, 8);
  names[0] := 'arccot';
  names[1] := 'arccotg';
  names[2] := 'arccotan';
  names[3] := 'arccotangent';
  names[4] := 'cot_1';
  names[5] := 'cotg_1';
  names[6] := 'cotan_1';
  names[7] := 'cotangent_1';
end;

function FuncArcCot.calculate(arguments: AoE): extended;
var s, arg: extended;
begin                             
  s := 0;
  for arg in arguments do s += arg;
  if s = 0 then exit(90);
  exit(radtodeg(arctan(1/s)));
end;

// FuncFloor
constructor FuncFloor.create;
begin
  setlength(names, 2);
  names[0] := 'floor';
  names[1] := 'flr';
end;

function FuncFloor.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(int(s));
end;

// FuncAbs
constructor FuncAbs.create;
begin
  setlength(names, 2);
  names[0] := 'abs';
  names[1] := 'absolute';
end;

function FuncAbs.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(abs(s));
end;     

// FuncGCD
constructor FuncGCD.create;
begin
  setlength(names, 3);
  names[0] := 'gcd';
  names[1] := 'greatestcommondivisor';
  names[2] := 'greatest_common_divisor';
end;

function FuncGCD.calculate(arguments: AoE): extended;
var res, n, temp, i: int64;
begin
  if length(arguments) = 1 then exit(trunc(abs(arguments[0])));
  res := trunc(abs(arguments[0]));
  for i := 1 to length(arguments)-1 do begin
    n := trunc(abs(arguments[i]));
    while n <> 0 do begin
      temp := n;
      n := res mod n;
      res := temp;
    end;
  end;
  exit(res);
end;

// FuncLCM
constructor FuncLCM.create;
begin
  setlength(names, 3);
  names[0] := 'lcm';
  names[1] := 'lowestcommonmultiplier';
  names[2] := 'lowest_common_multiplier';
end;

function FuncLCM.calculate(arguments: AoE): extended;
var res, n, t, t2, temp: int64; i: longint;
begin
  if length(arguments) = 1 then exit(trunc(abs(arguments[0])));
  res := trunc(abs(arguments[0]));
  for i := 1 to length(arguments)-1 do begin
    n := trunc(abs(arguments[i]));
    t := n;
    t2 := res;
    while t2 <> 0 do begin
      temp := t2;
      t2 := n mod t2;
      n := temp;
    end;
    res := res*t div n;
  end;
  exit(res);
end;

// FuncFact
constructor FuncFact.create;
begin
  setlength(names, 2);
  names[0] := 'fact';
  names[1] := 'factorial';
end;

function FuncFact.calculate(arguments: AoE): extended;
var s, arg: extended; n, i: int64;
begin
  s := 0;
  for arg in arguments do s += arg;
  n := trunc(s);
  if n < 0 then raise ExpressionInvalidException.createNew(msgInvalidFactorial);
  s := 1; i := 0;
  while i < n do begin
    i += 1;
    s *= i;
  end;
  exit(s);
end;

// FuncLog
constructor FuncLog.create;
begin
  setlength(names, 3);
  names[0] := 'log';
  names[1] := 'logarithm';
  names[2] := 'logarid';
end;

function FuncLog.calculate(arguments: AoE): extended;
var s: extended; i: longint;
begin
  if length(arguments) = 1 then begin
    s := arguments[0];
    if s <= 0 then raise ExpressionInvalidException.createNew(msgInvalidLogarithmNonPositiveInput);
    exit(log10(s));
  end else begin
    if arguments[0] <= 0 then raise ExpressionInvalidException.createNew(msgInvalidLogarithmNonPositiveBase);
    s := 0;
    for i:=1 to length(arguments)-1 do s += arguments[i];
    if s <= 0 then raise ExpressionInvalidException.createNew(msgInvalidLogarithmNonPositiveInput);
    exit(logn(arguments[0], s));
  end;
end;

// FuncLn
constructor FuncLn.create;
begin
  setlength(names, 4);
  names[0] := 'ln';
  names[1] := 'loge';
  names[2] := 'natural_logarithm';
  names[3] := 'natural_logarid';
end;

function FuncLn.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  if s <= 0 then raise ExpressionInvalidException.createNew(msgInvalidLogarithmNonPositiveInput);
  exit(ln(s));
end;

// FuncP
constructor FuncP.create;
begin
  setlength(names, 3);
  names[0] := 'p';
  names[1] := 'permutation';
  names[2] := 'permut';
end;

function FuncP.calculate(arguments: AoE): extended;
var n, k: int64; res: extended = 1;
begin
  if length(arguments) <> 2 then raise ExpressionInvalidException.createNew(msgPermutationWrongNumberOfParameters);
  n := trunc(arguments[0]);
  k := trunc(arguments[1]);
  if (n < 0) or (k < 0) then raise ExpressionInvalidException.createNew(msgPermutationNegativeParameters);
  if k > n then exit(0);
  k := n-k;
  while k < n do begin
    k += 1;
    res *= k;
  end;
  exit(res);
end;

// FuncC
constructor FuncC.create;
begin
  setlength(names, 3);
  names[0] := 'c';
  names[1] := 'combination';
  names[2] := 'combin';
end;

function FuncC.calculate(arguments: AoE): extended;
var n, k, i: int64; res: extended = 1;
begin
  if length(arguments) <> 2 then raise ExpressionInvalidException.createNew(msgCombinationWrongNumberOfParameters);
  n := trunc(arguments[0]);
  k := trunc(arguments[1]);
  if (n < 0) or (k < 0) then raise ExpressionInvalidException.createNew(msgCombinationNegativeParameters);
  if k > n then exit(0);
  i := n-k;
  while i < n do begin
    i += 1;
    res *= i;
  end;
  i := 0;
  while i < k do begin
    i += 1;
    res /= i;
  end;
  exit(res);
end;

// FuncRound
constructor FuncRound.create;
begin
  setlength(names, 2);
  names[0] := 'round';
  names[1] := 'rnd';
end;

function FuncRound.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  if abs(frac(s)) >= 0.5 then
    if s > 0 then exit(int(s)+1) else exit(int(s)-1)
  else exit(int(s));
end;

// FuncDegToRad
constructor FuncDegToRad.create;
begin
  setlength(names, 5);
  names[0] := 'dtr';
  names[1] := 'degtorad';
  names[2] := 'deg_to_rad';
  names[3] := 'degreestoradians';
  names[4] := 'degrees_to_radians';
end;

function FuncDegToRad.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(degToRad(s));
end;

// FuncDegToGrad
constructor FuncDegToGrad.create;
begin
  setlength(names, 5);
  names[0] := 'dtg';
  names[1] := 'degtograd';
  names[2] := 'deg_to_grad';
  names[3] := 'degreestogradians';
  names[4] := 'degrees_to_gradians';
end;

function FuncDegToGrad.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(degToGrad(s));
end;

// FuncRadToDeg
constructor FuncRadToDeg.create;
begin
  setlength(names, 5);
  names[0] := 'rtd';
  names[1] := 'radtodeg';
  names[2] := 'rad_to_deg';
  names[3] := 'radianstodegrees';
  names[4] := 'radians_to_degrees';
end;

function FuncRadToDeg.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(radToDeg(s));
end;

// FuncRadToGrad
constructor FuncRadToGrad.create;
begin
  setlength(names, 5);
  names[0] := 'rtg';
  names[1] := 'radtograd';
  names[2] := 'rad_to_grad';
  names[3] := 'radianstogradians';
  names[4] := 'radians_to_gradians';
end;

function FuncRadToGrad.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(radToGrad(s));
end;

// FuncGradToDeg
constructor FuncGradToDeg.create;
begin
  setlength(names, 5);
  names[0] := 'gtd';
  names[1] := 'gradtodeg';
  names[2] := 'grad_to_deg';
  names[3] := 'gradianstodegrees';
  names[4] := 'gradians_to_degrees';
end;

function FuncGradToDeg.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(gradToDeg(s));
end;

// FuncGradToRad
constructor FuncGradToRad.create;
begin
  setlength(names, 5);
  names[0] := 'gtr';
  names[1] := 'gradtorad';
  names[2] := 'grad_to_rad';
  names[3] := 'gradianstoradians';
  names[4] := 'gradians_to_radisns';
end;

function FuncGradToRad.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(gradToRad(s));
end;

// FuncMax
constructor FuncMax.create;
begin
  setlength(names, 2);
  names[0] := 'max';
  names[1] := 'maximum';
end;

function FuncMax.calculate(arguments: AoE): extended;
var s, arg: extended; isFirst: boolean = true;
begin
  for arg in arguments do
    if isFirst then begin
      s := arg;
      isFirst := false;
    end else if arg > s then s := arg;
  exit(s);
end;

// FuncMin
constructor FuncMin.create;
begin
  setlength(names, 2);
  names[0] := 'min';
  names[1] := 'minimum';
end;

function FuncMin.calculate(arguments: AoE): extended;
var s, arg: extended; isFirst: boolean = true;
begin
  for arg in arguments do
    if isFirst then begin
      s := arg;
      isFirst := false;
    end else if arg < s then s := arg;
  exit(s);
end;

// FuncAverage
constructor FuncAverage.create;
begin
  setlength(names, 2);
  names[0] := 'avg';
  names[1] := 'average';
end;

function FuncAverage.calculate(arguments: AoE): extended;
var s, arg: extended;
begin
  s := 0;
  for arg in arguments do s += arg;
  exit(s/length(arguments));
end;

// FuncRandom
constructor FuncRandom.create;
begin
  setlength(names, 2);
  names[0] := 'random';
  names[1] := 'rand';
end;

function FuncRandom.calculate(arguments: AoE): extended;
begin
  case length(arguments) of
  1: exit(random*arguments[0]);
  2: exit(arguments[0]+(arguments[1]-arguments[0])*random);
  else raise ExpressionInvalidException.createNew(msgRandomInvalidNumberOfArguments);
  end;
end;

// FuncRandomInt
constructor FuncRandomInt.create;
begin
  setlength(names, 4);
  names[0] := 'randomint';
  names[1] := 'randint';
  names[2] := 'randominteger';
  names[3] := 'random_integer';
end;

function FuncRandomInt.calculate(arguments: AoE): extended;
begin
  case length(arguments) of
  1: exit(int(random*arguments[0]));
  2: exit(int(arguments[0]+(arguments[1]-arguments[0])*random));
  else raise ExpressionInvalidException.createNew(msgRandomInvalidNumberOfArguments);
  end;
end;

// FuncRandomInList
constructor FuncRandomInList.create;
begin
  setlength(names, 3);
  names[0] := 'randominlist';
  names[1] := 'random_in_list';
  names[2] := 'randinlist';
end;

function FuncRandomInList.calculate(arguments: AoE): extended;
begin
  exit(arguments[trunc(random*(length(arguments)-1))]);
end;

// FuncIsGreater
constructor FuncIsGreater.create;
begin
  setlength(names, 1);
  names[0] := 'isgreater';
end;

function FuncIsGreater.calculate(arguments: AoE): extended;
begin
  if length(arguments) <> 2 then raise ExpressionInvalidException.createNew(msgComparisonInvalidNumberOfArguments);
  if arguments[0] > arguments[1] then exit(1) else exit(0);
end;

// FuncIsSmaller
constructor FuncIsSmaller.create;
begin
  setlength(names, 1);
  names[0] := 'issmaller';
end;

function FuncIsSmaller.calculate(arguments: AoE): extended;
begin
  if length(arguments) <> 2 then raise ExpressionInvalidException.createNew(msgComparisonInvalidNumberOfArguments);
  if arguments[0] < arguments[1] then exit(1) else exit(0);
end;

// FuncIsEqual
constructor FuncIsEqual.create;
begin
  setlength(names, 1);
  names[0] := 'isequal';
end;

function FuncIsEqual.calculate(arguments: AoE): extended;
var n: extended;
begin
  if length(arguments) < 2 then raise ExpressionInvalidException.createNew(msgIsEqualInvalidNumberOfArguments);
  for n in arguments do if arguments[0] <> n then exit(0);
  exit(1);
end;

// FuncIf
constructor FuncIf.create;
begin
  setlength(names, 1);
  names[0] := 'if';
end;

function FuncIf.calculate(arguments: AoE): extended;
begin
  if length(arguments) > 3 then raise ExpressionInvalidException.createNew(msgIfInvalidNumberOfArguments);
  while length(arguments) < 3 do begin
    setlength(arguments, length(arguments)+1);
    arguments[length(arguments)-1] := 0;
  end;
  if arguments[0] > 0 then exit(arguments[1]) else exit(arguments[2]);
end;

end.

