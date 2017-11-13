unit SPropertes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, LCLIntf, LCLType, Buttons, Math,
  FPCanvas, TypInfo, Spin, UScale, SFigures;

type
  TProperty = class
    procedure deletePRP(); virtual; abstract;
    procedure returnPRP(); virtual; abstract;
    procedure OnChange(Sender: TObject); virtual; abstract;
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    Ch: PLongint;
    n: integer;
    constructor Create(s: string; t: PLongint; i: integer);
    procedure deletePRP(); override;
    procedure returnPRP(); override;
    procedure OnChange(Sender: TObject); override;
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create();
    procedure deletePRP(); override;
    procedure returnPRP(); override;
    procedure OnChange(Sender: TObject); override;
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create();
    procedure deletePRP(); override;
    procedure returnPRP(); override;
    procedure OnChange(Sender: TObject); override;
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

var
  PropertyPanel: TPanel;
  Propertys: ArrayOfProperty;

implementation

procedure RegisterProperty(PropertyO: TProperty);
begin
  SetLength(Propertys, Length(Propertys) + 1);
  Propertys[High(Propertys)] := PropertyO;
end;

{ OnChange }

procedure TPenStyleProperty.OnChange(Sender: TObject);
begin
  if Drawing then
  begin
    Figures[high(Figures)].P := CasePenStyle(PenStylesBox.ItemIndex);
    InvalidateHandler;
  end;
  PenStyle := CasePenStyle(PenStylesBox.ItemIndex);
end;

procedure TBrushStyleProperty.OnChange(Sender: TObject);
begin
  if Drawing then
  begin
    Figures[high(Figures)].B := CaseBrushStyle(BrushStylesBox.ItemIndex);
    InvalidateHandler;
  end;
  BrushStyle := CaseBrushStyle(BrushStylesBox.ItemIndex);
end;

procedure TSpinProperty.OnChange(Sender: TObject);
begin
  ch^ := SpinEdit.Value;
  if Length(Figures) > 0 then
    figures[high(figures)].getparams;
  InvalidateHandler;
end;

{ BoxDrowItem }

procedure TPenStyleProperty.PenStylesBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  Y: integer;
begin
  Y := ARect.Top + 7;
  (Control as TComboBox).Canvas.Pen.Style := CasePenStyle(Index);
  (Control as TComboBox).Canvas.Line(0, Y, 100, Y);
end;

procedure TBrushStyleProperty.BrushStylesBoxDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  PRect: TRect;
begin
  PRect.Left := ARect.Left + 8;
  PRect.Right := ARect.Right - 8;
  PRect.Top := ARect.Top + 4;
  PRect.Bottom := ARect.Bottom - 4;
  (Control as TComboBox).Canvas.Brush.Style := CaseBrushStyle(Index);
  (Control as TComboBox).Canvas.Brush.Color := clBlack;
  (Control as TComboBox).Canvas.Rectangle(PRect);
end;

{ deletePRP }
procedure TSpinProperty.deletePRP();
begin
  SpinLabel.Parent := nil;
  SpinEdit.Parent := nil;
end;

procedure TPenStyleProperty.deletePRP();
begin
  PenStylesBox.Parent := nil;
  PenStylesLabel.Parent := nil;
end;

procedure TBrushStyleProperty.deletePRP();
begin
  BrushStylesBox.Parent := nil;
  BrushStylesLable.Parent := nil;
end;

{ returnPRP }

procedure TSpinProperty.returnPRP();
begin
  SpinLabel.Parent := PropertyPanel;
  SpinEdit.Parent := PropertyPanel;
  SpinLabel.Top := n * 100;
  SpinEdit.Top := n * 100 + 10;
end;

procedure TPenStyleProperty.returnPRP();
begin
  PenStylesBox.Parent := PropertyPanel;
  PenStylesLabel.Parent := PropertyPanel;
  PenStylesLabel.Top := 110;
  PenStylesBox.Top := 120;
end;

procedure TBrushStyleProperty.returnPRP();
begin
  BrushStylesBox.Parent := PropertyPanel;
  BrushStylesLable.Parent := PropertyPanel;
  BrushStylesLable.Top := 210;
  BrushStylesBox.Top := 220;
end;

{ Create }
constructor TSpinProperty.Create(s: string; t: PLongint; i: integer);
begin
  n := i;
  SpinLabel := TLabel.Create(PropertyPanel);
  SpinLabel.Caption := s;
  SpinLabel.Align := alTop;
  SpinLabel.Top := n * 100;
  Ch := t;
  SpinEdit := TSpinEdit.Create(PropertyPanel);
  SpinEdit.Align := alTop;
  SpinEdit.MinValue := 1;
  SpinEdit.MaxValue := 50;
  SpinEdit.Value := Width;
  SpinEdit.Top := n * 100 + 10;
  SpinEdit.Alignment := taLeftJustify;
  SpinEdit.OnChange := @OnChange;
end;

constructor TPenStyleProperty.Create();
begin
  PenStylesLabel := TLabel.Create(PropertyPanel);
  PenStylesLabel.Top := 110;
  PenStylesLabel.Caption := 'Pen Style';
  PenStyleslabel.Align := alTop;
  PenStylesBox := TComboBox.Create(PropertyPanel);
  PenStylesBox.Align := alTop;
  PenStylesBox.ReadOnly := True;
  PenStylesBox.Top := 120;
  PenStylesBox.Items.CommaText := ',,,,';
  PenStylesBox.Style := csOwnerDrawFixed;
  PenStylesBox.ItemIndex := 0;
  PenStylesBox.OnDrawItem := @PenStylesBoxDrawItem;
  PenStylesBox.OnChange := @OnChange;
end;

constructor TBrushStyleProperty.Create();
begin
  BrushStylesLable := TLabel.Create(PropertyPanel);
  BrushStylesLable.Top := 210;
  BrushStylesLable.Caption := 'Brush Style';
  BrushStylesLable.Align := alTop;
  BrushStylesBox := TComboBox.Create(PropertyPanel);
  BrushStylesBox.Items.CommaText := ',,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 220;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
  BrushStylesBox.OnChange := @OnChange;
end;



initialization

  begin
    PropertyPanel := TPanel.Create(nil);
    RegisterProperty(TSpinProperty.Create('Width', @Width, length(Propertys)));
    RegisterProperty(TPenStyleProperty.Create());
    RegisterProperty(TBrushStyleProperty.Create());
    RegisterProperty(TSpinProperty.Create('Radius X', @RadX, length(Propertys)));
    RegisterProperty(TSpinProperty.Create('Radius Y', @RadY, length(Propertys)));
  end;

end.
