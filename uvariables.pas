unit UVariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Main, StrUtils;

type

  { TVariables }

  TVariables = class(TForm)
    BoxVarVal: TEdit;
    BoxVarName: TEdit;
    ButtonA: TButton;
    ButtonJ: TButton;
    ButtonK: TButton;
    ButtonL: TButton;
    ButtonP: TButton;
    ButtonN: TButton;
    ButtonM: TButton;
    ButtonO: TButton;
    ButtonQ: TButton;
    ButtonR: TButton;
    ButtonB: TButton;
    ButtonS: TButton;
    ButtonT: TButton;
    ButtonW: TButton;
    ButtonV: TButton;
    ButtonU: TButton;
    ButtonX: TButton;
    ButtonY: TButton;
    ButtonZ: TButton;
    ButtonD: TButton;
    ButtonC: TButton;
    ButtonF: TButton;
    ButtonE: TButton;
    ButtonG: TButton;
    ButtonH: TButton;
    ButtonI: TButton;
    ButtonAns: TButton;
    ButtonPreAns: TButton;
    ButtonClose: TButton;
    BoxA: TEdit;
    BoxH: TEdit;
    BoxI: TEdit;
    BoxJ: TEdit;
    BoxK: TEdit;
    BoxL: TEdit;
    BoxP: TEdit;
    BoxO: TEdit;
    BoxN: TEdit;
    BoxM: TEdit;
    BoxQ: TEdit;
    BoxB: TEdit;
    BoxT: TEdit;
    BoxU: TEdit;
    BoxW: TEdit;
    BoxY: TEdit;
    BoxZ: TEdit;
    BoxV: TEdit;
    BoxX: TEdit;
    BoxS: TEdit;
    BoxR: TEdit;
    BoxAns: TEdit;
    BoxPreAns: TEdit;
    BoxC: TEdit;
    BoxD: TEdit;
    BoxE: TEdit;
    BoxF: TEdit;
    BoxG: TEdit;
    AtoZContainer: TScrollBox;
    procedure BoxVarNameChange(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure showForm(ParentIn: TMainWindow);
    procedure updateStuff;
    procedure VarButtonClick(Sender: TObject);
  private
    MainForm: TMainWindow;
    boxes: array['a'..'z'] of TEdit;
  public

  end;

var
  Variables: TVariables;

implementation

{$R *.lfm}{$R+}

const rFlags = [rfReplaceAll, rfIgnoreCase];

function formatNumber(numberIn: extended): string;
begin
  exit(
  stringReplace(
  formatFloat('0.##########', numberIn), '.', ',', rFlags));
  //exit('0a');
end;

function getVariable(mainInstance: TMainWindow; variable: string): string;
begin
  exit(
  formatNumber(
  mainInstance.Engine.getVariable(variable)));
  //exit('0a');
end;

{ TVariables }

procedure TVariables.showForm(ParentIn: TMainWindow);
begin
  Self.MainForm := ParentIn;
  Self.UpdateStuff;
end;

procedure TVariables.updateStuff;
var c: char;
begin
  for c:='a' to 'z' do boxes[c].Text := mainWindow.engine.getVariableString(c);
  BoxAns.Text := mainWindow.engine.getVariableString('Ans');
  BoxPreAns.Text := mainWindow.engine.getVariableString('PreAns');
  boxVarVal.text := mainWindow.engine.getVariableString(boxVarName.text);
end;

procedure TVariables.VarButtonClick(Sender: TObject);
var oldPos: longint; s: string;
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert((sender as TButton).Caption, s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 6;
end;

procedure TVariables.ButtonCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TVariables.BoxVarNameChange(Sender: TObject);
begin
  boxVarVal.text := mainWindow.engine.getVariableString(boxVarName.text);
end;

procedure TVariables.FormCreate(Sender: TObject);
begin
  boxes['a'] := BoxA;
  boxes['b'] := BoxB;
  boxes['c'] := BoxC;
  boxes['d'] := BoxD;
  boxes['e'] := BoxE;
  boxes['f'] := BoxF;
  boxes['g'] := BoxG;
  boxes['h'] := BoxH;
  boxes['i'] := BoxI;
  boxes['j'] := BoxJ;
  boxes['k'] := BoxK;
  boxes['l'] := BoxL;
  boxes['m'] := BoxM;
  boxes['n'] := BoxN;
  boxes['o'] := BoxO;
  boxes['p'] := BoxP;
  boxes['q'] := BoxQ;
  boxes['r'] := BoxR;
  boxes['s'] := BoxS;
  boxes['t'] := BoxT;
  boxes['u'] := BoxU;
  boxes['v'] := BoxV;
  boxes['w'] := BoxW;
  boxes['x'] := BoxX;
  boxes['y'] := BoxY;
  boxes['z'] := BoxZ;
  mainForm := mainWindow;
end;

procedure TVariables.FormResize(Sender: TObject);
begin
  buttonClose.Top := variables.ClientHeight-33;
  buttonPreAns.top := variables.clientHeight-85;
  buttonAns.top := variables.clientHeight-59;
  boxPreAns.top := variables.clientHeight-84;
  boxAns.top := variables.clientHeight-58;
  boxVarName.top := variables.ClientHeight-113;
  boxVarVal.top := variables.clientHeight-113;
  AtoZContainer.height := variables.clientHeight-131;
end;

end.

