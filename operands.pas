unit Operands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, UExpressionInvalidException;

type
  AoS = array of string;

  {=== The base operand class. ===}
  Op = class
    public
      characters: AoS;    // The characters bound to the operand.
      reversed: boolean;  // Whether calculation will be performed from right to left rather than left to right.
      priority: longint;  // Priority of the operand in calculation.
      function calculate(val1, val2: extended): extended; virtual; abstract;
  end;

  {=== Actual operand classes. ===}
  Plus = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  Minus = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  Multiply = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  DotlessMultiplication = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  Divide = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  Exponentiation = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  Root = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  OpeningBrace = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

  ClosingBrace = class(Op)
    constructor create;
    function calculate(val1, val2: extended): extended; override;
  end;

resourcestring
  msgDivByZero = 'Division by zero.';
  msgUnsupportedExponentiation = 'Unsupported exponentiation operation.';
  msgBraceCalculated = 'Brace is involved as an operator. This is a BUG, report it to the author.';

implementation
  // Plus
  constructor Plus.create;
  begin
    setlength(characters, 1);
    characters[0] := '+';
    reversed := false;
    priority := 1;
  end;

  function Plus.calculate(val1, val2: extended): extended;
  begin
    exit(val1+val2);
  end;

  // Minus
  constructor Minus.create;
  begin
    setlength(characters, 2);
    characters[0] := '-';
    characters[1] := '–';
    reversed := false;
    priority := 1;
  end;

  function Minus.calculate(val1, val2: extended): extended;
  begin
    exit(val1-val2);
  end;

  // Multiply
  constructor Multiply.create;
  begin
    setlength(characters, 5);
    characters[0] := '.';
    characters[1] := '*';
    characters[2] := 'x';
    characters[3] := '·';
    characters[4] := '×';
    reversed := false;
    priority := 2;
  end;

  function Multiply.calculate(val1, val2: extended): extended;
  begin
    exit(val1*val2);
  end;

  // Divide
  constructor Divide.create;
  begin
    setlength(characters, 4);
    characters[0] := ':';
    characters[1] := '/';
    characters[2] := '∕';
    characters[3] := '∕';
    reversed := false;
    priority := 2;
  end;

  function Divide.calculate(val1, val2: extended): extended;
  begin
    if val2 = 0 then raise ExpressionInvalidException(msgDivByZero);
    exit(val1/val2);
  end;

  // -- Power function
  function funcPow(base, exponent: extended): extended;
  var roundedExponent: int64;
  begin
    if (base = 0) then if exponent > 0 then exit(0) else raise ExpressionInvalidException.createNew(msgDivByZero);
    if exponent < 0 then exit(1/funcPow(base, -exponent));
    roundedExponent := round(exponent);
    if abs(roundedExponent-exponent) < 1E-11 then
      if (base > 0) or (roundedExponent and 1 = 1) then exit(power(base, roundedExponent)) else exit(-power(-base, roundedExponent))
    else if base > 0 then exit(power(base, exponent)) else raise ExpressionInvalidException.createNew(msgUnsupportedExponentiation);
  end;

  // Exponentiation
  constructor Exponentiation.create;
  begin
    setlength(characters, 1);
    characters[0] := '^';
    reversed := true;
    priority := 4;
  end;

  function Exponentiation.calculate(val1, val2: extended): extended;
  begin
    exit(funcPow(val1, val2));
  end;

  // Root
  constructor Root.create;
  begin
    setlength(characters, 1);
    characters[0] := '#';
    reversed := false;
    priority := 4;
  end;

  function Root.calculate(val1, val2: extended): extended;
  begin
    if val2 = 0 then raise ExpressionInvalidException.createNew('Level-0 root occured.');
    exit(funcPow(val2, 1/val1));
  end;

  // OpeningBrace
  constructor OpeningBrace.create;
  begin
    setlength(characters, 4);
    characters[0] := '(';
    characters[1] := '[';
    characters[2] := '{';
    characters[3] := '<';
    reversed := false;
    priority := -1;
  end;

  function OpeningBrace.calculate(val1, val2: extended): extended;
  begin
    raise ExpressionInvalidException.createNew(msgBraceCalculated);
    exit(0);
  end;

  // ClosingBrace
  constructor ClosingBrace.create;
  begin
    setlength(characters, 4);
    characters[0] := ')';
    characters[1] := ']';
    characters[2] := '}';
    characters[3] := '>';
    reversed := false;
    priority := -2;
  end;

  function ClosingBrace.calculate(val1, val2: extended): extended;
  begin
    raise ExpressionInvalidException.createNew(msgBraceCalculated);
    exit(0);
  end;

  // DotlessMultiplication
  constructor DotlessMultiplication.create;
  begin
    setlength(characters, 1);
    characters[0] := '.';
    reversed := false;
    priority := 3;
  end;

  function DotlessMultiplication.calculate(val1, val2: extended): extended;
  begin
    exit(val1*val2);
  end;
end.

