unit UHelpBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FileInfo, DateUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Menus, Main, UVariables;

type

  { THelpBox }

  THelpBox = class(TForm)
    OK: TButton;
    Content: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  msgHelp = 'CalcIt Pascal is a simple-to-use application for evaluating simple to medium-level mathematical expressions.'                   + sLineBreak
          + 'Operator priorities are taken into account.'                                                                                    + sLineBreak
                                                                                                                                             + sLineBreak
          + 'For detailed documentation about how to use, please visit https://github.com/leduyquang753/CalcItPascal/blob/master/README.md.' + sLineBreak
                                                                                                                                             + sLineBreak
          + 'Version %s built at %s. Copyright © 2019 Lê Duy Quang. Licensed under MIT.';

var
  appVer: string = '0.2.1 (build 2)';
  appBuildTime: string = '12h23 13/1/2019 UTC';

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
  Content.Lines.Add(format(msgHelp, [appVer, appBuildTime]));
  Content.Lines.EndUpdate;
end;

procedure THelpBox.FormActivate(Sender: TObject);
begin
  self.updateBox;
end;

procedure THelpBox.FormCreate(Sender: TObject);
var Info: TVersionInfo; buildTime: TDateTime; tmpNum: longint;
begin
  DefaultFormatSettings.shortDateFormat := 'y/m/d';
  DefaultFormatSettings.longTimeFormat := 'HH:MM:SS';
  Info := TVersionInfo.Create;
  Info.Load(HINSTANCE);
  appVer := intToStr(Info.FixedInfo.FileVersion[0]) + '.' + intToStr(Info.FixedInfo.FileVersion[1]) + '.' + intToStr(Info.FixedInfo.FileVersion[2]) + ' (build ' + intToStr(Info.FixedInfo.FileVersion[3]) + ')';
  buildTime := strToDateTime({$I %DATE%} + ' ' + {$I %TIME%}, DefaultFormatSettings);
  appBuildTime := intToStr(hourOf(buildTime)) + 'h';
  tmpNum := minuteOf(buildTime);
  appBuildTime += CS(tmpNum < 10, '0', '') + intToStr(tmpNum) + ':';
  tmpNum := secondOf(buildTime);
  appBuildTime += CS(tmpNum < 10, '0', '') + intToStr(tmpNum) + '  ';
  tmpNum := dayOfWeek(buildTime);
  appBuildTime += intToStr(CI(tmpNum = 1, 8, tmpNum)) + ' | ' + intToStr(dayOfTheMonth(buildTime)) + '/' + intToStr(monthOfTheYear(buildTime)) + '/' + intToStr(yearOf(buildTime));
end;

procedure THelpBox.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then self.close;
end;

end.

