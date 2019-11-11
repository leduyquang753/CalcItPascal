unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {FileUtil, }Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCalculatorEngine, UExpressionInvalidException, StrUtils, LCLType, LResources,
  Translations, LCLTranslator, Menus{, DbgConsole};

type

  { TMainWindow }

  TMainWindow = class(TForm)
    Language: TButton;
    Help: TButton;
    BtnVariables: TButton;
    Calculate: TButton;
    ConsoleLabel: TLabel;
    Console: TMemo;
    Expression: TEdit;
    InputLabel: TLabel;
    Engine: CalculatorEngine;
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

resourcestring
  msgError = 'ERROR: ';
  msgConsoleBegin = 'Type any expression to calculate, "help", "vars" to view and select variables, or "exit".';

function Translate(POFileName: string): boolean;
procedure readLangConfig;
procedure writeLangConfig(confLang: string);
function formatNumber(num: extended): string;

implementation

{$R *.lfm}

uses UHelpBox, UVariables, ULangSelector;

var confFileName, confDir: string;

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
  if fileExists(confFileName) then begin
    assign(conf, confFileName);
    reset(conf);
    readln(conf, confLang);
    close(conf);
    translate(confLang);
  end else begin
    assign(conf, confFileName);
    rewrite(conf);
    writeln(conf, 'en');
    close(conf);
  end;
end;

procedure writeLangConfig(confLang: string);
var conf: text;
begin
  assign(conf, confFileName);
  rewrite(conf);
  writeln(conf, confLang);
  close(conf);
end;

{ TMainWindow }

procedure TMainWindow.FormChangeBounds(Sender: TObject);
begin
  Console.Height       := Self.Height-110;
  Console.Width        := Self.Width - 20;
  InputLabel.Top       := Self.Height- 80;
  Expression.Top       := Self.Height- 64;
  Expression.Width     := Self.Width - 20;
  Calculate.Left       := Self.Width - 85;
  Calculate.Top        := Self.Height- 35;
  BtnVariables.Left    := Self.Width -165;
  BtnVariables.Top     := Self.Height- 35;
  Help.Left            := Self.Width -195;
  Help.Top             := Self.Height- 35;
end;

const rFlags = [rfReplaceAll, rfIgnoreCase];

procedure TMainWindow.calculateIt;
var calculatedResult: extended;
begin
  {if not dbgShown then begin
    dbgWindow.show;
    dbgShown := true;
  end;}
  try
    case self.expression.text of
      'help': begin
                HelpBox.ShowModal;
                self.expression.text := '';
                exit;
              end;
      'vars': begin
                Variables.showForm(Self);
                Variables.show;
                self.expression.text := '';
                exit;
              end;
      'exit': begin
                Application.Terminate;
                exit;
              end;
    end;
    if (self.expression.text = '') then begin
      if (lastSuccessful <> '') then begin
        self.console.append(sLineBreak + lastSuccessful);
        calculatedResult := self.Engine.calculate(lastSuccessful);
        self.console.append('= ' + formatNumber(calculatedResult));
        self.updateVariables;
      end;
    end else begin
      self.console.append(sLineBreak + self.expression.text);
      calculatedResult := self.Engine.calculate(self.Expression.Text);
      self.console.append('= ' + formatNumber(calculatedResult));
      lastSuccessful := self.expression.text;
      self.expression.text := '';
      self.updateVariables;
    end;
  except
    on e: ExpressionInvalidException do begin
      self.Console.Append(msgError + e.exceptionMessage);
      if e.position > -1 then begin
        self.Expression.SelStart:=e.position;
        self.Expression.SelLength := 0;
      end;
    end;
  end;
end;

procedure TMainWindow.FormCreate(Sender: TObject);
begin                                                        
  confDir := getAppConfigDir(false);
  confFileName := confDir + '\calcitlang.dat';
  Application.UpdateFormatSettings := false;
  DecimalSeparator := '.';
  self.KeyPreview := false;
  Engine := CalculatorEngine.new;
  Application.Title:=MainWindow.Caption;
  //Console.Append(confFileName);
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
begin
  exit(
  stringReplace(
  stringReplace(formatFloat('#,##0.##########',
  num), ',', ' ', rFlags), '.', ',', rFlags));
end;

procedure TMainWindow.LanguageClick(Sender: TObject);
begin
  LangSelector.showModal;
end;

procedure TMainWindow.ExpressionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then begin
    self.calculateIt;
    key := 0;
  end;
end;

procedure TMainWindow.ExpressionKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then key := 0;
end;

procedure TMainWindow.updateVariables;
begin
  Variables.updateStuff;
end;

procedure TMainWindow.CalculateClick(Sender: TObject);
begin
  self.calculateIt;
end;

initialization
  {$I Calculator.lrs}

end.

