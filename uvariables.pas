unit UVariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Main, StrUtils;

type

  { TVariables }

  TVariables = class(TForm)
    A: TButton;
    B: TButton;
    Ans: TButton;
    ButtonClose: TButton;
    AValue: TEdit;
    BValue: TEdit;
    CValue: TEdit;
    DValue: TEdit;
    EValue: TEdit;
    FValue: TEdit;
    AnsValue: TEdit;
    PreAnsValue: TEdit;
    PreAns: TButton;
    F: TButton;
    E: TButton;
    D: TButton;
    C: TButton;
    procedure AClick(Sender: TObject);
    procedure AnsClick(Sender: TObject);
    procedure BClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure CClick(Sender: TObject);
    procedure DClick(Sender: TObject);
    procedure EClick(Sender: TObject);
    procedure FClick(Sender: TObject);
    procedure PreAnsClick(Sender: TObject);
    procedure showForm(ParentIn: TMainWindow);
    procedure updateStuff;
  private
    MainForm: TMainWindow;

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

function getVariable(mainInstance: TMainWindow; variable: integer): string;
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
begin
  AValue.Text := getVariable(Self.MainForm, 1);
  BValue.Text := getVariable(Self.MainForm, 2);
  CValue.Text := getVariable(Self.MainForm, 3);
  DValue.Text := getVariable(Self.MainForm, 4);
  EValue.Text := getVariable(Self.MainForm, 5);
  FValue.Text := getVariable(Self.MainForm, 6);
  AnsValue.Text := getVariable(Self.MainForm, 0);
  PreAnsValue.Text := getVariable(Self.MainForm, -1);
end;

procedure TVariables.ButtonCloseClick(Sender: TObject);
begin
  Self.Close;
end;

var s: string; oldPos: integer;

procedure TVariables.CClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('C', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

procedure TVariables.DClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('D', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

procedure TVariables.EClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('E', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

procedure TVariables.FClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('F', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

procedure TVariables.PreAnsClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('PreAns', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 6;
end;

procedure TVariables.AClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('A', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

procedure TVariables.AnsClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('Ans', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 3;
end;

procedure TVariables.BClick(Sender: TObject);
begin
  oldPos := Self.MainForm.Expression.SelStart;
  s := Self.MainForm.Expression.Text;
  Insert('B', s, oldPos+1);
  Self.MainForm.Expression.Text := s;
  Self.MainForm.Expression.SelStart := oldPos + 1;
end;

end.

