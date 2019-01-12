unit ULangSelector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Main;

type

  { TLangSelector }

  TLangSelector = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    LangList: TListBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  LangSelector: TLangSelector;

const
  langs: array[1..2] of string = ('en', 'vi');
  langNames: array[1..2] of string = (
    'English (United States)',
    'Tiếng Việt (Việt Nam)'
  );

implementation

{$R *.lfm}

{ TLangSelector }

procedure TLangSelector.FormActivate(Sender: TObject);
var i: integer;
begin
  langList.Clear;
  for i:=1 to length(langNames) do self.LangList.AddItem(langNames[i], TObject.Create);
end;

procedure TLangSelector.btnCancelClick(Sender: TObject);
begin
  self.Close;
end;

procedure TLangSelector.btnOKClick(Sender: TObject);
var j: integer;
begin
  for j:=0 to langList.Count-1 do if langList.Selected[j] then begin
    Translate(langs[j+1]);
    writeLangConfig(langs[j+1]);
    MainWindow.updateVariables;
    break;
  end;
  self.Close;
end;

end.

