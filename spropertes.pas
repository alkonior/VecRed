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
    procedure showPRP(); virtual; abstract;
  end;

  ArrayOfProperty = array of TProperty;

  TSpinProperty = class(TProperty)
    SpinLabel: TLabel;
    SpinEdit: TSpinEdit;
    Ch: PLongint;
    n: integer;
    constructor Create(s: string; t: PLongint; i: integer);
    procedure deletePRP(); override;
    procedure showPRP(); override;
    procedure OnChange(Sender: TObject);
  end;

  TPenStyleProperty = class(TProperty)
    PenStylesLabel: TLabel;
    PenStylesBox: TComboBox;
    constructor Create();
    procedure deletePRP(); override;
    procedure showPRP(); override;
    procedure OnChange(Sender: TObject);
    procedure PenStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TBrushStyleProperty = class(TProperty)
    BrushStylesLable: TLabel;
    BrushStylesBox: TComboBox;
    constructor Create();
    procedure deletePRP(); override;
    procedure showPRP(); override;
    procedure OnChange(Sender: TObject);
    procedure BrushStylesBoxDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
  end;

  TButtonProperty = class(TProperty)
    Button: TButton;
    n: integer;
    constructor Create(s: string; i: integer);
    procedure deletePRP(); override;
    procedure showPRP(); override;
    procedure OnClick(Sender: TObject);
  end;

var
  PropertyPanel: TPanel;
  Propertys: ArrayOfProperty;
  NumberOfbuttonsOnProperty: integer;

implementation

procedure RegisterProperty(PropertyO: TProperty);
begin
  SetLength(Propertys, Length(Propertys) + 1);
  Propertys[High(Propertys)] := PropertyO;
end;

{ OnChange }

procedure TPenStyleProperty.OnChange(Sender: TObject);
begin
  PenStyle := CasePenStyle(PenStylesBox.ItemIndex);
  if Length(Figures) > 0 then
    Figures[high(Figures)].GetParams();
end;

procedure TBrushStyleProperty.OnChange(Sender: TObject);
begin
  BrushStyle := CaseBrushStyle(BrushStylesBox.ItemIndex);
  if Length(Figures) > 0 then
    Figures[high(Figures)].GetParams();
end;

procedure TSpinProperty.OnChange(Sender: TObject);
begin
  ch^ := SpinEdit.Value;
  if Length(Figures) > 0 then
    figures[high(figures)].getparams;
  InvalidateHandler;
end;

procedure TButtonProperty.OnClick(Sender: TObject);
var
  i, j: integer;
  f: array of TFigure;
begin
  case (Sender as TButton).tag of
    0:
    begin
      j := 0;
      if SelectedNumber > 0 then
      begin
        for i := 0 to length(Figures) - 1 do
        begin
          if Figures[i].Selected then
            FreeAndNil(Figures[i])
          else
          begin
            Figures[j] := Figures[i];
            Inc(j);
          end;
        end;
        SetLength(figures, j);
      end;
    end;
    1:
    begin
      j := 0;
      if SelectedNumber > 0 then
      begin
        for i := 0 to length(Figures) - 1 do
        begin
        end;
      end;
    end;
    2:
    begin
      j := 0;
      if SelectedNumber > 0 then
      begin
        for i := 0 to length(Figures) - 1 do
        begin
          if Figures[i].Selected then
            FreeAndNil(Figures[i])
          else
          begin
            Figures[j] := Figures[i];
            Inc(j);
          end;
        end;
        SetLength(figures, j);
      end;
    end;
  end;
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
  (Control as TComboBox).Canvas.Line(0, Y, 200, Y);
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

procedure TButtonProperty.deletePRP();
begin
  Button.Parent := nil;
end;


{ showPRP }

procedure TSpinProperty.showPRP();
begin
  SpinLabel.Parent := PropertyPanel;
  SpinEdit.Parent := PropertyPanel;
  SpinLabel.Top := n * 100;
  SpinEdit.Top := n * 100 + 10;
end;

procedure TPenStyleProperty.showPRP();
begin
  PenStylesBox.Parent := PropertyPanel;
  PenStylesLabel.Parent := PropertyPanel;
  PenStylesLabel.Top := 110;
  PenStylesBox.Top := 120;
end;

procedure TBrushStyleProperty.showPRP();
begin
  BrushStylesBox.Parent := PropertyPanel;
  BrushStylesLable.Parent := PropertyPanel;
  BrushStylesLable.Top := 210;
  BrushStylesBox.Top := 220;
end;

procedure TButtonProperty.showPRP();
begin
  Button.Parent := PropertyPanel;
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
  SpinEdit.MaxValue := 1000000;
  SpinEdit.Value := WidthOfFigure;
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
  BrushStylesBox.Items.CommaText := ',,,,,,,';
  BrushStylesBox.ReadOnly := True;
  BrushStylesBox.Top := 220;
  BrushStylesBox.Style := csOwnerDrawFixed;
  BrushStylesBox.ItemIndex := 0;
  BrushStylesBox.Align := alTop;
  BrushStylesBox.OnDrawItem := @BrushStylesBoxDrawItem;
  BrushStylesBox.OnChange := @OnChange;
end;

constructor TButtonProperty.Create(s: string; i: integer);
begin
  n := i;
  Button := TButton.Create(PropertyPanel);
  Button.Caption := s;
  Button.Align := alTop;
  Button.Top := n * 100;
  Button.Tag := NumberOfbuttonsOnProperty;
  NumberOfbuttonsOnProperty := NumberOfbuttonsOnProperty + 1;
  Button.OnClick := @OnClick;
end;


initialization

  begin
    PropertyPanel := TPanel.Create(nil);
    RegisterProperty(TSpinProperty.Create('Width', @WidthOfFigure, length(Propertys)));
    RegisterProperty(TPenStyleProperty.Create());
    RegisterProperty(TBrushStyleProperty.Create());
    RegisterProperty(TSpinProperty.Create('Radius X', @RadXOfFigure, length(Propertys)));
    RegisterProperty(TSpinProperty.Create('Radius Y', @RadYOfFigure, length(Propertys)));
    RegisterProperty(TButtonProperty.Create('Delete', Length(Propertys)));
    RegisterProperty(TButtonProperty.Create('All Top', Length(Propertys)));
    RegisterProperty(TButtonProperty.Create('All Bottom', Length(Propertys)));
  end;

end.
