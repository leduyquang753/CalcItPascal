{$MODE objfpc}
uses CRT, SysUtils, UCalculatorEngineConsole, UExpressionInvalidException, StrUtils;
const
  msgError = 'ERROR: ';
  msgConsoleBegin = 'Type any expression to calculate or "exit".';
  rFlags = [rfReplaceAll, rfIgnoreCase];
var s: string; engine: CalculatorEngine;

function formatNumber(numberIn: extended): string;
begin
  exit(stringReplace(formatFloat('0.##########', numberIn), '.', ',', rFlags));
end;

procedure calculateIt;
var result: extended;
begin
  try
    if s = 'exit' then halt(0);
    result := engine.calculate(s);
    writeln('= ', formatNumber(result));
  except
    on e: ExpressionInvalidException do
      writeln(msgError, e.exceptionMessage);
  end;
  writeln;
end;

begin
  engine := CalculatorEngine.new;
  writeln(msgConsoleBegin);
  while true do begin readln(s); calculateIt; end;
end.