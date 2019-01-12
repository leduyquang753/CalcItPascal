unit DbgConsole;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TDbgWindow }

  TDbgWindow = class(TForm)
    DebugConsole: TMemo;
    procedure log(msg: string);
  private

  public

  end;

var
  DbgWindow: TDbgWindow;

implementation

{$R *.lfm}

procedure TDbgWindow.log(msg: string);
begin
  self.DebugConsole.Append(msg);
end;

end.

