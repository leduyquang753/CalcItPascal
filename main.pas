unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UCalculatorEngine, UExpressionInvalidException, StrUtils, LCLType, LResources, Translations, LCLTranslator{, DbgConsole};

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
    procedure ExpressionKeyPress(Sender: TObject; var Key: char);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure BtnVariablesClick(Sender: TObject);
    procedure calculateIt;
    function getVariable(variable: integer): string;
    procedure LanguageClick(Sender: TObject);
    procedure updateVariables;
  private

  public

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

function formatNumber(numberIn: extended): string;
begin
  exit(stringReplace(formatFloat('0.##########', numberIn), '.', ',', rFlags));
end;

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
                Variables.showForm(self);
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
  confFileName := GetEnvironmentVariable('appdata') + '\CalcIt\calcitlang.dat';
  confDir := GetEnvironmentVariable('appdata') + '\CalcIt';
  Application.UpdateFormatSettings := false;
  DecimalSeparator := '.';
  Engine := CalculatorEngine.new;
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

function TMainWindow.getVariable(variable: integer): string;
begin
  exit(
  stringReplace(formatFloat('0.##########',
  self.Engine.getVariable(variable)), '.', ',', rFlags));
  //exit('0a');
end;

procedure TMainWindow.LanguageClick(Sender: TObject);
begin
  LangSelector.showModal;
end;

procedure TMainWindow.ExpressionKeyPress(Sender: TObject; var Key: char);
begin
  if (key = #13) or (key = #10) then begin
    key := #0;
    self.calculateIt;
  end;
end;

procedure TMainWindow.updateVariables;
begin
  Variables.AValue.Text := getVariable(1);
  Variables.BValue.Text := getVariable(2);
  Variables.CValue.Text := getVariable(3);
  Variables.DValue.Text := getVariable(4);
  Variables.EValue.Text := getVariable(5);
  Variables.FValue.Text := getVariable(6);
  Variables.AnsValue.Text := getVariable(0);
  Variables.PreAnsValue.Text := getVariable(-1);
end;

procedure TMainWindow.CalculateClick(Sender: TObject);
begin
  self.calculateIt;
end;

initialization
  {$I internationalization/Calculator.lrs}

end.
