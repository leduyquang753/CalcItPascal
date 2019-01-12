unit UExpressionInvalidException;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  ExpressionInvalidException = class(Exception)
    exceptionMessage: string;
    position: longint;
    constructor createNew(msg: string);
    constructor createNew(msg: string; pos: longint);
  end;

implementation

constructor ExpressionInvalidException.createNew(msg: string);
begin
  self.exceptionMessage := msg;
  self.position := -1;
end;

constructor ExpressionInvalidException.createNew(msg: string; pos: longint);
begin
  self.exceptionMessage := msg;
  self.position := pos;
end;

end.

