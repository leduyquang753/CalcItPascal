unit UHelpBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Main, UVariables;

type

  { THelpBox }

  THelpBox = class(TForm)
    OK: TButton;
    Content: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure OKClick(Sender: TObject);
    procedure updateBox;
  private

  public

  end;

var
  HelpBox: THelpBox;

implementation

{$R *.lfm}

{ THelpBox }

resourcestring
  msgHelp1  = 'CalcIt is a simple-to-use Java library/program used to perform single to medium-level mathematic expressions.';
  msgHelp2  = 'This program is a port to Delphi for a GUI and ease of use (no need to install a JRE).';
  msgHelp3  = 'Operators supported:';
  msgHelp4  = '    +  Addition';
  msgHelp5  = '    -  Subtraction';
  msgHelp6  = '    .  Multiplication';
  msgHelp7  = '    :  Division';
  msgHelp8  = '    ^  Exponentiation';
  msgHelp9  = '    v  Root';
  msgHelp10 = '    &  AND (for integers)';
  msgHelp11 = '    |  OR (for integers)';
  msgHelp12 = '    !  XOR (for integers)';
  msgHelp13 = 'Functions suported (syntax: <name>([value = 0])):';
  msgHelp14 = '    sin  Trigonometric sine';
  msgHelp15 = '    cos  Trigonometric cosine';
  msgHelp16 = '    tan  Trigonometric tangent';
  msgHelp17 = '    cot  Trigonometric cotangent';
  msgHelp18 = '    log  Base 10 logarithm';
  msgHelp19 = '    ln   Natural (base e) logarithm';
  msgHelp20 = 'The program takes operators'' priorities into account.';
  msgHelp21 = 'Version %s built at %s. Copyright © 2019 Lê Duy Quang';

const
  appVer = '0.2 (build 28)';
  appBuildTime = '14h26 12/1/2019 UTC';

function contentToInject: string;
begin
  exit(
  msgHelp1 + sLineBreak + sLineBreak + msgHelp2 + sLineBreak + sLineBreak + msgHelp3 + sLineBreak + msgHelp4 + sLineBreak + msgHelp5 + sLineBreak + msgHelp6 + sLineBreak + msgHelp7 + sLineBreak +
  msgHelp8 + sLineBreak + msgHelp9 + sLineBreak + msgHelp10 + sLineBreak + msgHelp11 + sLineBreak + msgHelp12 + sLineBreak + sLineBreak + msgHelp13 + sLineBreak + msgHelp14 + sLineBreak +
  msgHelp15 + sLineBreak + msgHelp16 + sLineBreak + msgHelp17 + sLineBreak + msgHelp18 + sLineBreak + msgHelp19 + sLineBreak + sLineBreak + msgHelp20 + sLineBreak + sLineBreak + msgHelp21
  );
end;

procedure THelpBox.FormResize(Sender: TObject);
begin
  Content.Width  := Self.Width  - 20;
  Content.Height := Self.Height - 55;
  OK.Left        := Self.Width  - 85;
  OK.Top         := Self.Height - 35;
end;

procedure THelpBox.OKClick(Sender: TObject);
begin
  Self.Close;
end;

procedure THelpBox.updateBox;
begin
  Content.Lines.BeginUpdate;
  Content.Lines.Clear;
  Content.Lines.Add(format(contentToInject, [appVer, appBuildTime]));
  Content.Lines.EndUpdate;
end;

procedure THelpBox.FormActivate(Sender: TObject);
begin
  self.updateBox;
end;

procedure THelpBox.FormCreate(Sender: TObject);
begin
  readLangConfig;
  MainWindow.Console.Append(msgConsoleBegin);
end;

end.

