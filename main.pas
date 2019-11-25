unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {FileUtil, }Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCalculatorEngine, UExpressionInvalidException, StrUtils, LCLType, LResources,
  Translations, LCLTranslator, Menus, USettings, Math{, DbgConsole};

type

  { TMainWindow }

  TMainWindow = class(TForm)
    BtnClear: TButton;
    BtnSettings: TButton;
    Language: TButton;
    Help: TButton;
    BtnVariables: TButton;
    Calculate: TButton;
    ConsoleLabel: TLabel;
    Console: TMemo;
    Expression: TEdit;
    InputLabel: TLabel;
    Engine: CalculatorEngine;
    procedure BtnClearClick(Sender: TObject);
    procedure BtnSettingsClick(Sender: TObject);
    procedure CalculateClick(Sender: TObject);
    procedure ExpressionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ExpressionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure BtnVariablesClick(Sender: TObject);
    procedure calculateIt;
    function getVariable(variable: string): string;
    procedure LanguageClick(Sender: TObject);
    procedure updateVariables;
  end;

var
  MainWindow: TMainWindow;
  lastSuccessful: string = '';
  //dbgShown: boolean = true; 
  enterCalculatesLast: boolean = true;
  upDownBeginEnd: boolean = false;
  maxExpressions: longint = 64;

resourcestring
  msgError = 'ERROR: ';
  msgConsoleBegin = 'Type any expression to calculate, "/clear", "/help", "/vars" to view and select variables, or "/exit".';
  msgOverflow = 'Numbers in the calculation are too large, cannot compute.';

function Translate(POFileName: string): boolean;
procedure readLangConfig;
procedure writeLangConfig(confLang: string);
function formatNumber(num: extended): string;

implementation

{$R *.lfm}

uses UHelpBox, UVariables, ULangSelector;

const configVersion = -9999;

var langConfFileName, engineConfFileName, startupFileName, confDir: string; numberFormat: TFormatSettings;
    history: array of string; exprInProgress: string = '';
    historyPointer: longint = -1; historyHEAD: longint = -1;
    shouldCancelKey: boolean = false;

function Translate(POFileName: string): boolean;
var
  res: TLResource;
  ii: Integer;
  POFile: TPOFile;
  LocalTranslator: TUpdateTranslator;
begin
  Result:= false;
  res:=LazarusResources.Find('Calculator.' + POFileName, 'PO');
  if res = nil then EXIT;
  POFile:=TPOFile.Create(False);
  try
    POFile.ReadPOText(res.Value);
    Result:= Translations.TranslateResourceStrings(POFile);
    if not Result then EXIT;

    LocalTranslator := TPOTranslator.Create(POFile);

    LRSTranslator := LocalTranslator;
    for ii := 0 to Screen.CustomFormCount-1
      do LocalTranslator.UpdateTranslation(Screen.CustomForms[ii]);
    for ii := 0 to Screen.DataModuleCount-1
      do LocalTranslator.UpdateTranslation(Screen.DataModules[ii]);
    LRSTranslator:= nil;
    LocalTranslator.Destroy;
    Application.Title:=MainWindow.Caption;
  finally end;
end;

procedure readLangConfig;
var conf: text; confLang: string;
begin
  if not DirectoryExists(confDir) then createDir(confDir);
  if fileExists(langConfFileName) then begin
    assign(conf, langConfFileName);
    reset(conf);
    readln(conf, confLang);
    close(conf);
    translate(confLang);
  end else begin
    assign(conf, langConfFileName);
    rewrite(conf);
    writeln(conf, 'en');
    close(conf);
  end;
end;

procedure writeLangConfig(confLang: string);
var conf: text;
begin
  assign(conf, langConfFileName);
  rewrite(conf);
  writeln(conf, confLang);
  close(conf);
end;

procedure readBool(var conf: text; var bool: boolean);
var readData: string;
begin
  readln(conf, readData);
  bool := not (readData = '0');
end;

procedure writeBool(var conf: text; bool: boolean);
begin
  if bool then writeln(conf, '1') else writeln(conf, '0');
end;

procedure writeEngineConfig(engine: CalculatorEngine);
var conf: text;
begin
  assign(conf, engineConfFileName);
  rewrite(conf);
  writeln(conf, configVersion);
  writeBool(conf, engine.decimalDot);
  writeBool(conf, engine.enforceDecimalSeparator);
  writeBool(conf, engine.thousandDot);
  writeBool(conf, engine.mulAsterisk);
  writeBool(conf, engine.enforceMulDiv);
  writeBool(conf, engine.zeroUndefinedVars);
  writeBool(conf, enterCalculatesLast);
  writeBool(conf, upDownBeginEnd);
  writeln(conf, maxExpressions);
  close(conf);
end;

procedure readEngineConfig(engine: CalculatorEngine);
var conf: text; readConfigVersion: longint;
begin
  if not DirectoryExists(confDir) then createDir(confDir);
  if fileExists(engineConfFileName) then begin
    assign(conf, engineConfFileName);
    reset(conf);
    readln(conf, readConfigVersion);
    if readConfigVersion <> configVersion then begin close(conf); writeEngineConfig(engine); exit; end;
    readBool(conf, engine.decimalDot);
    readBool(conf, engine.enforceDecimalSeparator);
    readBool(conf, engine.thousandDot);
    readBool(conf, engine.mulAsterisk);
    readBool(conf, engine.enforceMulDiv);
    readBool(conf, engine.zeroUndefinedVars);     
    readBool(conf, enterCalculatesLast);
    readBool(conf, upDownBeginEnd);
    readln(conf, maxExpressions);
    close(conf);
  end else writeEngineConfig(engine);
end;

procedure writeStartupExpressions;
var f: text; s: string;
begin
  assign(f, startupFileName);
  rewrite(f);
  for s in Settings.BoxStartupExpressions.Lines do writeln(f, s);
  close(f);
end;

procedure loadStartupExpressions;
var f: text; readString, expr: string; c: char; hadSlash: boolean;
begin
  if not DirectoryExists(confDir) then createDir(confDir);
  if fileExists(startupFileName) then begin
    assign(f, startupFileName);
    reset(f);
    Settings.BoxStartupExpressions.Clear;
    while not eof(f) do begin
      readln(f, readString);
      expr := '';
      hadSlash := false;
      Settings.BoxStartupExpressions.Append(readString);
      for c in readString do if (c = '/') then if hadSlash then begin delete(expr, length(expr), 1); break; end else begin expr += c; hadSlash := true; end else begin expr += c; hadSlash := false; end;
      if expr <> '' then
        try MainWindow.engine.calculate(expr)
        except
          on e: ExpressionInvalidException do MainWindow.Console.Append(expr + sLineBreak + msgError + e.exceptionMessage + sLineBreak);
          on e: EOverflow do MainWindow.Console.Append(expr + sLineBreak + msgError + msgOverflow + sLineBreak);
        end;
    end;
  end else begin writeStartupExpressions; loadStartupExpressions; end;
end;

{ TMainWindow }

procedure TMainWindow.FormChangeBounds(Sender: TObject);
begin
  Console.Height       := Self.Height-110;
  Console.Width        := Self.Width - 20;
  InputLabel.Top       := Self.Height- 80;
  Expression.Top       := Self.Height- 64;
  Expression.Width     := Self.Width - 20;

  Help.Visible         := Self.Width > 366;
  BtnSettings.Visible  := Self.Width > 336;
  BtnClear.Visible     := Self.Width > 266;
end;

const rFlags = [rfReplaceAll, rfIgnoreCase];

procedure TMainWindow.calculateIt;
var calculatedResult: extended; currentExpression: String = ''; exprIn: String; c: char;
begin
  try
    case self.expression.text of
      '/help': begin
                HelpBox.ShowModal;
                self.expression.text := '';
                exit;
              end;
      '/vars': begin
                Variables.showForm(Self);
                Variables.show;
                self.expression.text := '';
                exit;
              end;
      '/exit': begin
                Application.Terminate;
                exit;
              end;
      '/clear': begin
                  BtnClearClick(nil);     
                  self.expression.text := '';
                  exit;
                end;
    end;
    if self.expression.text = '' then
      if enterCalculatesLast and (lastSuccessful <> '') then exprIn := lastSuccessful else exit
    else exprIn := self.expression.text;
    for c in exprIn do begin
      if c = '|' then
        if length(currentExpression) <> 0 then begin
          calculatedResult := self.Engine.calculate(currentExpression);
          self.console.append(sLineBreak + currentExpression);
          self.console.append('= ' + formatNumber(calculatedResult));
          currentExpression := '';
        end else continue
      else currentExpression += c;
    end;
    if length(currentExpression) <> 0 then begin
      calculatedResult := self.Engine.calculate(currentExpression);
      self.console.append(sLineBreak + currentExpression);
      self.console.append('= ' + formatNumber(calculatedResult));
    end;
    lastSuccessful := exprIn;
    if length(history) = maxExpressions then begin
      historyHEAD := (historyHEAD+1) mod maxExpressions;
      history[historyHEAD] := exprIn;
    end else begin
      historyHEAD += 1;            
      setlength(history, historyHEAD+1);
      history[historyHEAD] := exprIn;
    end;
    historyPointer := -1;
    self.expression.text := '';
  except
    on e: ExpressionInvalidException do begin
      self.console.append(sLineBreak + currentExpression);
      self.Console.Append(msgError + e.exceptionMessage);
      if e.position > -1 then begin
        self.Expression.SelStart:=e.position;
        self.Expression.SelLength := 0;
      end;
    end;
    on e: EOverflow do begin
      self.console.append(sLineBreak + currentExpression);
      self.Console.Append(msgError + msgOverflow);
    end;
  end;                                                               
  self.updateVariables;
end;

procedure updateNumberFormat(engine: CalculatorEngine);
begin
  numberFormat := DefaultFormatSettings;
  if engine.decimalDot then numberFormat.DecimalSeparator := '.' else numberFormat.decimalSeparator := ',';
  if engine.thousandDot then if engine.decimalDot then numberFormat.thousandSeparator := ',' else numberFormat.thousandSeparator := '.' else numberFormat.thousandSeparator := ' ';
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin                                                        
  confDir := getAppConfigDir(false);
  langConfFileName := confDir + '\calcitlang.dat';
  engineConfFileName := confDir + '\calcitengine.dat';
  startupFileName := confDir + '\startup.dat';
  Application.UpdateFormatSettings := false;
  DecimalSeparator := '.';
  self.KeyPreview := false;                 
  readLangConfig;
  Engine := CalculatorEngine.new;
  Application.Title:=MainWindow.Caption;
  readEngineConfig(engine);
  updateNumberFormat(engine);
  Settings := TSettings.Create(self);
  loadStartupExpressions;
  Settings.Init;
  Console.clear;
  Console.Append(msgConsoleBegin);
  //Console.Append(langConfFileName);
end;

procedure TMainWindow.HelpClick(Sender: TObject);
begin
  HelpBox.ShowModal;
end;

procedure TMainWindow.BtnVariablesClick(Sender: TObject);
begin
  Variables.showForm(Self);
  Variables.show;
end;

function TMainWindow.getVariable(variable: string): string;
begin
  exit(formatNumber(Engine.getVariable(variable)));
end;

function formatNumber(num: extended): string;
var formatted, toReturn: string; digitCount: integer = -1; c, mulSign: char; expo: longint;
begin
  if MainWindow.engine.mulAsterisk or MainWindow.engine.decimalDot or (not MainWindow.engine.decimalDot and MainWindow.engine.thousandDot) then mulSign := '*' else mulSign := '.';
  if (num <> 0) and (log10(abs(num)) < 0) then begin
    expo := ceil(-log10(abs(num))/3)*3;
    toReturn := stringReplace(formatFloat('#,##0.##########', num*power(10, expo), numberFormat), 'E', mulSign+'10^', rFlags);
    if expo <> 0 then toReturn += mulSign+'10^-' + intToStr(expo);
    exit(toReturn);
  end;
  formatted := stringReplace(formatFloat('#,##0.##########', num, numberFormat), 'E', mulSign+'10^', rFlags);
  toReturn := '';
  for c in formatted do
    if c = numberFormat.decimalSeparator then begin toReturn += c; digitCount := 0; end
    else if c = mulSign then begin toReturn += c; digitCount := -1; end
    else begin
      if digitCount = -1 then begin toReturn += c; continue; end
      else if digitCount = 10 then continue else begin toReturn += c; digitCount += 1; end;
    end;
  exit(toReturn);
end;

procedure TMainWindow.LanguageClick(Sender: TObject);
begin
  LangSelector.showModal;
end;

procedure TMainWindow.ExpressionKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_RETURN then begin
    self.calculateIt;
    key := 0;
    shouldCancelKey := true;
  end else if key = VK_UP then if upDownBeginEnd then begin key := VK_HOME; shouldCancelKey := true; end else begin
    if length(history) = 0 then begin key := 0; shouldCancelKey := true; exit; end;
    if historyPointer = -1 then begin
      exprInProgress := expression.text;
      historyPointer := historyHEAD;
    end else if (length(history)+historyPointer-1) mod length(history) = historyHEAD then begin key := VK_END; shouldCancelKey := true; exit; end else historyPointer := (length(history)+historyPointer-1) mod length(history);
    expression.text := history[historyPointer];
    key := VK_END; shouldCancelKey := true;
  end else if key = VK_DOWN then if upDownBeginEnd then begin key := VK_END; shouldCancelKey := true; end else begin
    if historyPointer = -1 then begin key := 0; shouldCancelKey := true; exit; end;
    if historyPointer = historyHEAD then begin expression.text := exprInProgress; historyPointer := -1; key := VK_END; shouldCancelKey := true; exit; end;
    historyPointer := (historyPointer+1) mod length(history);
    expression.text := history[historyPointer];
    key := VK_END; shouldCancelKey := true;
  end;
end;

procedure TMainWindow.ExpressionKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if shouldCancelKey then begin key := 0; shouldCancelKey := false; end;
end;

procedure TMainWindow.updateVariables;
begin
  Variables.updateStuff;
end;

procedure TMainWindow.CalculateClick(Sender: TObject);
begin
  self.calculateIt;
end;

procedure TMainWindow.BtnClearClick(Sender: TObject);
begin
  Console.Clear;
  Console.Append(msgConsoleBegin);
end;

procedure TMainWindow.BtnSettingsClick(Sender: TObject);
begin
  if Settings.ShowModal = mrOk then begin
    // PanelDecimalSeparator
    engine.decimalDot := Settings.RadioDecimalDot.Checked;
    engine.decimalDot := not Settings.RadioDecimalComma.Checked;
    engine.enforceDecimalSeparator := Settings.CheckEnforceDecimalSeparator.Checked;

    // PanelThousandSeparator
    engine.thousandDot := Settings.RadioCommaDot.Checked;
    engine.thousandDot := not Settings.RadioSpace.Checked;

    // PanelMulDivStyle
    engine.mulAsterisk := not Settings.RadioMulDot.Checked;
    engine.mulAsterisk := Settings.RadioMulAsterisk.Checked;
    engine.enforceMulDiv := Settings.CheckEnforceMulDivStyle.Checked;

    engine.zeroUndefinedVars := Settings.CheckDefaultZero.Checked;

    // Other settings
    enterCalculatesLast := Settings.CheckEnterWhenEmpty.Checked;
    upDownBeginEnd := Settings.RadioUpDownBeginEnd.Checked;

    if maxExpressions <> Settings.SpinMaxExpressions.Value then begin
      setlength(history, 0);
      historyHEAD := -1;
      historyPointer := -1;
      expression.text := exprInProgress;
      maxExpressions := Settings.SpinMaxExpressions.Value;
    end;

    writeEngineConfig(engine);
    updateNumberFormat(engine);
  end;
end;

initialization
  {$I internationalization\Calculator.lrs}

end.

