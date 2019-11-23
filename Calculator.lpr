program Calculator;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, UHelpBox, UVariables, DbgConsole, ULangSelector, Operands
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='CalcIt';
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TDbgWindow, DbgWindow);
  Application.CreateForm(TVariables, Variables);
  Application.CreateForm(TLangSelector, LangSelector);
  Application.CreateForm(THelpBox, HelpBox);
  Application.Run;
end.

