unit USettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, Spin, UCalculatorEngine;

type

  { TSettings }

  TSettings = class(TForm)
    BoxStartupExpressions: TMemo;
    LabelHistorySize: TLabel;
    SettingsButtonCancel: TButton;
    SettingsButtonOK: TButton;
    CheckEnterWhenEmpty: TCheckBox;
    CheckDefaultZero: TCheckBox;
    CheckEnforceDecimalSeparator: TCheckBox;
    CheckEnforceMulDivStyle: TCheckBox;
    LabelStartupExpressions: TLabel;
    LabelDecimalSeparator: TLabel;
    LabelMulDivStyle: TLabel;
    LabelThousandSeparator: TLabel;
    LabelUpDownBehavior: TLabel;
    PanelDecimalSeparator: TPanel;
    PanelMulDivStyle: TPanel;
    PanelThousandSeparator: TPanel;
    PanelUpDownBehavior: TPanel;
    RadioUpDownHistory: TRadioButton;
    RadioMulAsterisk: TRadioButton;
    RadioMulDot: TRadioButton;
    RadioSpace: TRadioButton;
    RadioDecimalDot: TRadioButton;
    RadioDecimalComma: TRadioButton;
    RadioCommaDot: TRadioButton;
    RadioUpDownBeginEnd: TRadioButton;
    SpinMaxExpressions: TSpinEdit;
    procedure Init;
  private

  public

  end;

var
  Settings: TSettings;

implementation

uses Main;

{ TSettings }

procedure TSettings.Init;
var engine: CalculatorEngine; s: string;
begin
  engine := MainWindow.Engine;

  // PanelDecimalSeparator
  RadioDecimalDot.Checked := engine.decimalDot;
  RadioDecimalComma.Checked := not engine.decimalDot;
  CheckEnforceDecimalSeparator.Checked := engine.enforceDecimalSeparator;

  // PanelThousandSeparator
  RadioCommaDot.Checked := engine.thousandDot;
  RadioSpace.Checked := not engine.thousandDot;

  // PanelMulDivStyle
  RadioMulDot.Checked := not engine.mulAsterisk;
  RadioMulAsterisk.Checked := engine.mulAsterisk;
  CheckEnforceMulDivStyle.Checked := engine.enforceMulDiv;

  CheckDefaultZero.Checked := engine.zeroUndefinedVars;

  // Other settings
  CheckEnterWhenEmpty.Checked := enterCalculatesLast;
  RadioUpDownHistory.Checked := not upDownBeginEnd;
  RadioUpDownBeginEnd.Checked := upDownBeginEnd;
  SpinMaxExpressions.Value := maxExpressions;

  CheckDefaultZero.Checked := engine.zeroUndefinedVars;

  BoxStartupExpressions.clear;
  for s in startupExpressions do BoxStartupExpressions.append(s);
end;

initialization
  {$I usettings.lrs}

end.

